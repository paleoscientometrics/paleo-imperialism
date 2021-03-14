library(tidyverse)
library(igraph)
library(countrycode)
library(ggplot2)

pal <- c("#f0ffe9", "#ffe599", "#bbe487", "#4e9755", "#173109")

# Load data ---------------------------------------------------------------
load(file.path("data", "refs.RData"))
df <- completed_refs
df <- df[!is.na(df$aff_country),]

# each country gets an id
countries <- unique(c(df$samp_country, df$aff_country))
countries <- gsub("&", "and", countries)
countries <- data.frame(id=1:length(countries),
						label=countries)
countries$region <- countrycode::countryname(countries$label, destination = "continent") #may change
countries <- countries[order(countries$region),]

regs <- unique(countries$region)
cols <- palette.colors(length(regs))
countries$cols <- plyr::mapvalues(countries$region, from=regs, to=cols)

# By region
df$samp_region <- countrycode::countryname(df$samp_country, destination="region23")
df$aff_region <- countrycode::countryname(df$aff_country, destination="region23")

df$samp_continent <- countrycode::countryname(df$samp_country, destination="continent")
df$aff_continent <- countrycode::countryname(df$aff_country, destination="continent")

# each region gets an id
regions <- unique(c(df$samp_region, df$aff_region))
regions <- data.frame(id=1:length(regions),
					  label=regions)

head(regions)

# Create edge list --------------------------------------------------------

# Create edge list --------------------------------------------------------

edges <- df %>%  
	group_by(samp_country, aff_country) %>% 
	summarise(weight = n()) %>% 
	ungroup()


#add node id
edges <- edges %>% 
	left_join(countries, by = c("aff_country" = "label")) %>% 
	rename(from = id)

edges <- edges %>% 
	left_join(countries %>%  select(id, label), 
			  by = c("samp_country" = "label")) %>% 
	rename(to = id)

edges <- na.omit(edges)
# edges <- dplyr::select(edges, from, to, weight)


# Plot --------------------------------------------------------------------
edges <- edges[edges$samp_country != "",] 
edges2 <- edges[edges$samp_country != edges$aff_country,]

df2 <- countries %>%  filter(!is.na(region))
df2 <- df2 %>% left_join(edges %>%  select(aff_country, weight), 
						 by=c("label"="aff_country"))

df2$weight[is.na(df2$weight)] <- 0
df2 <- df2[df2$label %in% unique(edges2$samp_country, edges2$aff_country),]

df2 <- df2 %>%  group_by(label, region) %>% 
	summarise(weight=sum(weight))

df2 <- df2[order(df2$region, df2$weight, decreasing = T),]
regs <- unique(df2$region)

df2$y <- 0
df2$x <- 0

seq_max <- c(20,22,21, 22, 18)
x_mid <- c(5,4,2,3,1)
f <- c(3,5,8,5,10)

for(i in 1:length(regs)){
	temp <- df2[df2$region == regs[i],]
	
	df2[df2$region == regs[i],]$y <- seq(seq_max[i],1, length.out = nrow(temp))
	df2[df2$region == regs[i],]$x <- jitter(rep(x_mid[i], nrow(temp)), factor=f[i])
}

df2[df2$label=="USA",]$y <-  23

edges2 <- edges2 %>% 
	left_join(df2 %>%  select(label, y,x), by=c("samp_country" = "label")) %>% 
	rename(x2 = x, y2=y) %>% 
	left_join(df2 %>%  select(label, y,x), by=c("aff_country" = "label")) %>% 
	rename(x1=x, y1=y)

p.breaks <- df2 %>% group_by(region) %>% 
	summarise(x=median(x)) %>% 
	arrange(x)

p0 <- ggplot() 


p1 <- ggplot() + geom_curve(data=na.omit(edges2[edges2$x2 > edges2$x1,]), 
							aes(x=x1, y=y1, xend=x2, yend=y2, col=region), 
							curvature = 0.3, 
							alpha=0.1) +
	geom_curve(data=na.omit(edges2[edges2$x1 > edges2$x2,]), 
			   aes(x=x1, y=y1, xend=x2, yend=y2, col=region), 
			   curvature = -0.3, alpha=0.1) +
	geom_point(data=df2, 
			   aes(x=x, y=y, 
			   	size=weight, col=ifelse(weight>25,region, "Other"))) +
	scale_x_continuous(breaks=p.breaks$x, limits=c(0,6)) +
	scale_size_continuous(breaks=c(0,50,100,500,1000,
								   2000,5000,7500), 
						  range=c(1,15)) +
	scale_color_manual(values=c("Africa" = "#00798c", "Oceania"="#d1495b", 
								"Asia"="#edae49", "Europe"="#66a182", 
								"Americas"="#2e4057", "Other"="lightgrey"))


x11(w=15, h=10);p1 + theme_minimal()
