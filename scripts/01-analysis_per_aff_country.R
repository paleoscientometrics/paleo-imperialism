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
dat <- completed_refs

#other data
income <- read.csv(file.path("data", "2019_income_classification_worldbank.csv"))
gdp <- read.csv(file.path("data", "2020-05-28_GDP_WorldBank.csv"))
research_fund <- read.csv(file.path("data", "2020-05-28_RD_WorldBank.csv"))
researchers <- read.csv(file.path("data", "2021-02-06_researchers_WorldBank.csv"), skip=4)
population <-read.csv(file.path("data", "2021-03-13_population_worldbank.csv"), skip=4)
gpi <- read.csv(file.path("data","2019_GPI.csv"), sep = ",") #Global Peace Index 
epi <- read.csv(file.path("data","2019_EPI.csv"), sep = ",") # english proficiency 
imperialism <- readxl::read_xls("data/imperialism.xls", sheet=4)

dat$code <- countrycode::countrycode(dat$aff_country, origin="country.name", destination = "iso3c")
imperialism$code <- countrycode::countrycode(imperialism$country, origin="country.name", destination = "iso3c")

dat <- merge(dat, pbdb[,c("reference_no", "collection_no")], all.x=TRUE, all.y=FALSE)


# Shape data --------------------------------------------------------------

population <- population %>%  pivot_longer(starts_with("X"), names_to="year") %>% 
	mutate(year = gsub("X", "", year)) %>% 
	filter(year != "") %>%
	mutate(year = as.numeric(year)) %>% 
	filter(year >= 1990) %>% 
	group_by(Country.Code) %>% 
	summarise(population = mean(value, na.rm=TRUE)) %>% 
	rename(code=Country.Code)

researchers <- researchers %>%  pivot_longer(starts_with("X"), names_to="year") %>% 
	mutate(year = gsub("X", "", year)) %>% 
	filter(year != "") %>%
	mutate(year = as.numeric(year)) %>% 
	filter(year >= 1990) %>% 
	group_by(Country.Code) %>% 
	summarise(mean = mean(value, na.rm=TRUE)) %>% 
	rename(code=Country.Code)

researchers <- merge(researchers, population)
researchers$researchers <- researchers$population/10^6 * researchers$mean
# Overall patterns --------------------------------------------------------

# * Number of collections per country ---------------------------------------
colls <- unique(dat[,c("collection_no", "code")])

colls_n <- data.frame(prop.table(table(colls$code)), stringsAsFactors = FALSE)
colnames(colls_n) <- c("code", "freq")
colls_n$imperial <- 0
colls_n$imperial[colls_n$code %in% imperialism$code] <- 1
colls_n <- colls_n[order(colls_n$freq, decreasing = TRUE),]

n <- which(cumsum(colls_n$freq) > 0.8)[1]

topcountries <- colls_n[1:n,]
sum(topcountries$freq)
topcountries$country <- countrycode(topcountries$code, origin = "iso3c", destination = "country.name")

# * In foreign country ----------------------------------------------------
dat2 <- dat[dat$samp_country != dat$aff_country,]
dat2 <- unique(dat2[,c("collection_no", "code")])

colls_n2 <- data.frame(table(dat2$code), stringsAsFactors = FALSE)
colnames(colls_n2) <- c("code", "foreign")
colls_n2$foreign <- colls_n2$foreign/nrow(colls)

colls_foreign <-colls_n2[colls_n2$code %in% topcountries$code,]

topcountries <- merge(topcountries, colls_foreign)

# * Does not include any local researcher ---------------------------------
dat$local <- 1
dat$local[dat$samp_country != dat$aff_country] <- 0
dat3 <- dat[!duplicated(dat[,c("code", "collection_no")]),]

local_sum <- tapply(dat3$local, dat3$collection_no, sum)
dat3 <- colls[colls$collection_no %in% names(which(local_sum == 0)),]

colls_n3 <- data.frame(table(dat3$code), stringsAsFactors = FALSE)
colnames(colls_n3) <- c("code", "parachute")
colls_n3$parachute <- colls_n3$parachute/nrow(colls)

colls_parachute <-colls_n3[colls_n3$code %in% topcountries$code,]


topcountries <- merge(topcountries, colls_parachute)

# Final calculations for bar chart ----------------------------------------
topcountries$local <- topcountries$freq - topcountries$foreign
topcountries$foreign <- topcountries$foreign - topcountries$parachute

topcountries <- topcountries %>%  select(local, foreign, parachute, country) %>% 
	pivot_longer(cols=c("local", "foreign", "parachute"), names_to = c("type"), values_to="freq")

topcountries$type <- factor(topcountries$type, levels=c("local", "foreign", "parachute"))

ggplot(topcountries, aes(x=reorder(country, freq), y=freq*100, fill=type)) +
	geom_bar(stat="identity") +
	labs(x="", y="Proportion of fossil collections", fill="Fieldwork")+
	scale_fill_manual(values=pal[c(3:5)], 
					  labels=c("In same country", "In a foreign country", "In a foreign country \nw/o local collaboration")) +
	coord_flip() +
	guides(fill=guide_legend(ncol=2)) 

ggsave(file.path("figs", "Fig_01_parachute_science.svg"), 
	   height=5, w=6)
