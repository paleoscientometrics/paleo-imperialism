library(tidyverse)
library(igraph)
library(countrycode)
library(ggthemes)

pal <- c("#f0ffe9", "#ffe599", "#bbe487", "#4e9755", "#173109")

theme_set(theme_hc() %+replace% 
		  	theme(axis.title = element_text(face="bold"),
		  		  legend.title = element_text(face="bold"))
		  )

# Load data ---------------------------------------------------------------
load(file.path("data", "refs.RData"))
pbdb <- readRDS(file.path("data", "pbdb.rds"))
refs1990 <- all_refs[all_refs$pubyr > 1990,]$reference_no %>% unique()

dat <- completed_refs[completed_refs$reference_no %in% refs1990,]

dat <- dat[!is.na(dat$samp_code),]

dat <- merge(dat, pbdb[,c("reference_no", "collection_no")], all.x=TRUE, all.y=FALSE)

# Overall patterns --------------------------------------------------------

# * Number of collections per country ---------------------------------------
colls <- unique(dat[,c("collection_no", "code")])
total_colls <- length(unique(dat$collection_no))

colls_n <- data.frame(table(colls$code), stringsAsFactors = FALSE)
colnames(colls_n) <- c("code", "freq")
colls_n$freq <- colls_n$freq/total_colls

colls_n$imperial <- 0
colls_n$imperial[colls_n$code %in% imperialism$code] <- 1
colls_n <- colls_n[order(colls_n$freq, decreasing = TRUE),]

#n <- which(cumsum(colls_n$freq) > 0.8)[1]

topcountries <- colls_n[1:15,]
sum(topcountries$freq)
topcountries$country <- countrycode(topcountries$code, origin = "iso3c", destination = "country.name")

# * In foreign country ----------------------------------------------------
dat2 <- dat[dat$code != dat$code2,]
dat2 <- unique(dat2[,c("collection_no", "code")])

colls_n2 <- data.frame(table(dat2$code), stringsAsFactors = FALSE)
colnames(colls_n2) <- c("code", "foreign")
colls_n2$foreign <- colls_n2$foreign/total_colls

colls_foreign <-colls_n2[colls_n2$code %in% topcountries$code,]

topcountries <- merge(topcountries, colls_foreign)

# * Does not include any local researcher ---------------------------------
dat$local <- 1
dat$local[dat$code != dat$code2] <- 0
dat3 <- dat[!duplicated(dat[,c("code", "collection_no")]),]

local_sum <- tapply(dat3$local, dat3$collection_no, sum)
dat3 <- colls[colls$collection_no %in% names(which(local_sum == 0)),]

colls_n3 <- data.frame(table(dat3$code), stringsAsFactors = FALSE)
colnames(colls_n3) <- c("code", "parachute")
colls_n3$parachute <- colls_n3$parachute/total_colls

colls_parachute <-colls_n3[colls_n3$code %in% topcountries$code,]


topcountries <- merge(topcountries, colls_parachute)

# Final calculations for bar chart ----------------------------------------
topcountries$local <- topcountries$freq - topcountries$foreign
topcountries$foreign <- topcountries$foreign - topcountries$parachute

topcountries <- topcountries %>%  select(local, foreign, parachute, country, imperial) %>% 
	pivot_longer(cols=c("local", "foreign", "parachute"), names_to = c("type"), values_to="freq")

topcountries$type <- factor(topcountries$type, levels=c("local", "foreign", "parachute"))

ggplot(topcountries, aes(x=reorder(country, freq), y=freq*100, fill=type)) +
	geom_bar(stat="identity") +
	labs(x="", y=" % contribution to fossil collections", fill="Fieldwork")+
	scale_fill_manual(values=pal[c(3:5)], 
					  labels=c("In same country", "In a foreign country", "In a foreign country \nw/o local collaboration")) +
	coord_flip() +
	guides(fill=guide_legend(ncol=2)) 

ggsave(file.path("figs", "Fig_01_parachute_science.svg"), 
	   height=5, w=6)


# Over time ---------------------------------------------------------------
countries <- c("Brazil", "Argentina", "Mexico", "China", "Japan", "India",
			   "South Africa")

individual <- unique(dat[dat$aff_country %in% countries,])
individual <- merge(individual, all_refs[,c("reference_no", "pubyr")], all.x=TRUE, all.y=FALSE)

individual$type <- "international"
individual[individual$aff_code == individual$samp_code,]$type <- "local"

refs.ind <- unique(individual$reference_no[individual$type == "international"])

for(i in 1:length(refs.ind)){
	country <- individual[individual$reference_no == refs.ind[i],]$aff_country %>%  unique()
	temp <- dat[dat$reference_no==refs.ind[i],]
	
	local <- any(unique(temp$samp_country) %in% unique(temp$aff_country))
	
	if(!local) individual[individual$reference_no == refs.ind[i],]$type <- "parachute"
}

summary.countries <- individual %>% select(reference_no, pubyr, aff_country, type) %>% 
	distinct() %>% 
	filter(pubyr > 1989) %>% 
	distinct() %>% 
	group_by(aff_country, pubyr, type) %>% 
	tally() %>%
	ungroup()

#moving average
library(zoo)
fnrollmean <- function (x, n=3) {
	if (length(x) < n) {
		rep(NA,length(x)) 
	} else {
		rollmean(x,n,align="center",na.pad=TRUE)
	}
}

summary.countries <- summary.countries %>%  
	group_by(aff_country, type) %>% 
	mutate(rM=fnrollmean(n)) 

summary.countries$aff_country <- factor(summary.countries$aff_country, levels=countries)

ggplot(summary.countries, aes(x=pubyr, y=rM, col=type)) +
	geom_line(size=1) +
	facet_wrap(~aff_country, scales = "free_y") +
	scale_color_manual(values=pal[3:5], breaks=c("local", "international", "parachute"),
					   labels=c("In same country", "In a foreign country w. local collaboration", 
					   		 "In a foreign country w/o local collaboration")) +
	labs(x="Year", y="No. of publications", col="Fieldwork") +
	guides(col=guide_legend(nrow=2)) +
	theme(axis.title.y=element_text(angle=90))

ggsave(file.path("figs", "Supplementary", "Fig_S_regional_hubs_time.svg"),
	   w=9, h=9)

# Get samp_countries ------------------------------------------------------
individual <- unique(individual[,c("reference_no", "pubyr", "samp_country", "aff_country", "type")])

# China
temp <- individual[individual$aff_country == "China",]

temp <- temp %>% 
	filter(type != "local") %>% 
	group_by(pubyr, samp_country, type) %>% 
	tally()

temp2 <- temp
temp2[temp2$samp_country != "Myanmar (Burma)",]$samp_country <- "Other"
temp2 <- temp2 %>% group_by(samp_country, pubyr) %>% 
	tally() %>% 
	group_by(samp_country) %>% 
	mutate(rM=fnrollmean(n))

p1 <- p1 <- ggplot(temp2, aes(x=pubyr, y=rM, col=samp_country)) +
	geom_line(size=1) +
	scale_color_manual(values=pal[c(4,3)]) +
	labs(x="Year", y="Number of publications", col="Countries")+
	theme(axis.title.y=element_text(angle=90)) 

tlab <- temp2[temp2$pubyr==2019,] 

p1 <- p1 +	xlim(1990, 2025) +
	ggrepel::geom_label_repel(data=tlab, aes(x=pubyr, y=rM, label=samp_country), hjust=0, size=3, nudge_x = 2, inherit.aes = FALSE) +
	scale_color_manual(values=rev(RColorBrewer::brewer.pal(2, "Dark2"))) +
	theme(legend.position ="none")

# Japan
temp <- individual[individual$aff_country == "Japan",]

temp <- temp %>% 
	filter(type != "local") %>% 
	group_by(pubyr, samp_country, type) %>% 
	tally()

temp2 <- temp
temp2$samp_country[!temp2$samp_country %in% names( sort(table(temp$samp_country), decreasing = TRUE)[1:6])] <- "Other"
temp2 <- temp2 %>% group_by(samp_country, pubyr) %>% 
	tally() %>% 
	group_by(samp_country) %>% 
	mutate(rM=fnrollmean(n))

temp2$samp_country[temp2$samp_country %in% c("Canada", "New Zealand")] <- "Other"

p2 <- ggplot(temp2, aes(x=pubyr, y=rM, col=samp_country)) +
	geom_line(size=1) +
	#scale_color_manual(values=pal[c(4,3)]) +
	labs(x="Year", y="Number of publications", col="Countries")+
	theme(axis.title.y=element_text(angle=90))

tlab <- temp2[temp2$pubyr==2019,] 
tlab[6,] <- temp2[temp2$samp_country == "Thailand" & temp2$pubyr==2018,]

p2 <- p2 +xlim(1990, 2025) +
	ggrepel::geom_label_repel(data=tlab, aes(x=pubyr, y=rM, label=samp_country), hjust=0, size=3, nudge_x = 2) +
	scale_color_manual(values=rev(RColorBrewer::brewer.pal(8, "Dark2")),
					   breaks=c("Other", "Mongolia", "Myanmar (Burma)", "China", 
					   		 "Thailand", "United States", 
					   		 "Russia")) +
	theme(legend.position ="none")

# Argentina
temp <- individual[individual$aff_country == "Argentina",]

temp <- temp %>% 
	filter(type != "local") %>% 
	group_by(pubyr, samp_country, type) %>% 
	tally()
temp$samp_country[temp$samp_country=="Aruba"] <- "Antarctica"

temp2 <- temp
temp2$samp_country[!temp2$samp_country %in% names( sort(table(temp$samp_country), decreasing = TRUE)[1:6])] <- "Other"
temp2 <- temp2 %>% group_by(samp_country, pubyr) %>% 
	tally() %>% 
	group_by(samp_country) %>% 
	mutate(rM=fnrollmean(n))


p3 <- ggplot(temp2, aes(x=pubyr, y=rM, col=samp_country)) +
	geom_line(size=1) +
	#scale_color_manual(values=pal[c(4,3)]) +
	labs(x="Year", y="Number of publications", col="Countries")+
	theme(axis.title.y=element_text(angle=90))

tlab <- temp2[temp2$pubyr==2019,] 

tlab <- rbind(tlab, 
	  temp2[temp2$pubyr == 2017 & !temp2$samp_country %in% tlab$samp_country ,])

p3 <- p3 +xlim(1990, 2025) +
	ggrepel::geom_label_repel(data=tlab, aes(x=pubyr, y=rM, label=samp_country), hjust=0, size=3, nudge_x = 2) +
	scale_color_manual(values=rev(RColorBrewer::brewer.pal(8, "Dark2"))) +
	theme(legend.position ="none")

# South Africa
temp <- individual[individual$aff_country == "South Africa",]

#countries involved
temp3 <- temp$reference_no[temp$type != "local"]
temp3 <- dat[dat$reference_no %in% temp3, c("reference_no", "aff_country", "samp_country")]

temp <- temp %>% 
	filter(type != "local") %>% 
	group_by(pubyr, samp_country, type) %>% 
	tally()

temp2 <- temp
temp2$samp_country[!temp2$samp_country %in% names( sort(table(temp$samp_country), decreasing = TRUE)[1:5])] <- "Other"
temp2 <- temp2 %>% group_by(samp_country, pubyr) %>% 
	tally() %>% 
	group_by(samp_country) %>% 
	mutate(rM=fnrollmean(n))

#countries involved continued
temp3 <- temp3[temp3$samp_country %in% c("Tanzania", "Botswana", "Tanzania"),]
ref <- table(temp3$reference_no) #check which one has collaborations
temp3 <- temp3[temp3$reference_no %in% names(ref[ref!=1]),]
table(temp3$aff_country)
table(temp3$aff_country,temp3$samp_country)

p4 <- ggplot(temp2, aes(x=pubyr, y=rM, col=samp_country)) +
	geom_line(size=1) +
	#scale_color_manual(values=pal[c(4,3)]) +
	labs(x="Year", y="Number of publications", col="Countries")+
	theme(axis.title.y=element_text(angle=90))

tlab <- na.omit(temp2[order(temp2$samp_country, 
							temp2$pubyr, decreasing = TRUE),])
tlab <- tlab[duplicated(tlab$samp_country)==FALSE,]


p4 <- p4 +xlim(1990, 2025) +
	ggrepel::geom_label_repel(data=tlab, aes(x=pubyr, y=rM, label=samp_country), hjust=0, size=3, nudge_x = 2) +
	scale_color_manual(values=rev(RColorBrewer::brewer.pal(8, "Dark2"))) +
	theme(legend.position ="none")

library(patchwork)

svg(file.path("figs","Supplementary","Fig_S_regional_hubs_countries.svg"), w=8, h=8)
p1+p2+p3+p4 + plot_layout(ncol=2) +
	plot_annotation(tag_prefix = "(", tag_levels = "a", tag_suffix = ")") &
	theme(plot.tag = element_text(size=10))
dev.off()

