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
	set.seed(42)
	df2[df2$region == regs[i],]$x <- jitter(rep(x_mid[i], nrow(temp)), factor=f[i])
}

df2[df2$label=="USA",]$y <-  24
df2[df2$label=="China",]$y <- 22
df2[df2$label=="Germany",]$y <- 22.8

edges2 <- edges2 %>% 
	left_join(df2 %>%  select(label, y,x), by=c("samp_country" = "label")) %>% 
	rename(x2 = x, y2=y) %>% 
	left_join(df2 %>%  select(label, y,x), by=c("aff_country" = "label")) %>% 
	rename(x1=x, y1=y)

p.breaks <- df2 %>% group_by(region) %>% 
	summarise(x=median(x)) %>% 
	arrange(x)


edges2$region[edges2$weight < 16] <- "Other"

edges2$region <- factor(edges2$region,levels=c(regs, "Other"))
df2$weight_alpha <- df2$weight
df2$weight_alpha[df2$label %in% df2$label[order(df2$weight, decreasing = T)][1:25]] <- 9999

curvature = 0.3
alpha = 0.2
breaks <- c(50,100,500,1000,
			2000, 5000, 10000)
lab_size <- 4

p1 <- ggplot() + geom_curve(data=na.omit(edges2[edges2$x2 > edges2$x1,]), 
							aes(x=x1, y=y1, xend=x2, yend=y2, col=region
								), 
							curvature = curvature, 
							alpha=alpha,
							size=0.5) +
	geom_curve(data=na.omit(edges2[edges2$x1 > edges2$x2,]), 
			   aes(x=x1, y=y1, xend=x2, yend=y2, col=region), 
			   curvature = -curvature, alpha=alpha) +
	geom_point(data=df2, 
			   aes(x=x, y=y, 
			   	size=weight, col=region,
			   	alpha=weight_alpha)) +
	geom_text(data=df2[df2$x < 3,], aes(x=x, y=y, 
							label=ifelse(label %in% df2$label[order(df2$weight, decreasing = T)][1:25], label, NA)),
			 hjust=1, vjust=0, size=lab_size, fontface="bold", nudge_x = -0.1) + 
	geom_text(data=df2[df2$x > 3,], aes(x=x, y=y, 
										label=ifelse(label %in% df2$label[order(df2$weight, decreasing = T)][1:25], label, NA)),
			  hjust=0, vjust=0, size=lab_size, nudge_y = 0.2, nudge_x = 0.1, fontface="bold") +
	scale_x_continuous(breaks=p.breaks$x, limits=c(0,6)) +
	scale_size_continuous(breaks=breaks, 
						  range=c(1,15)) +
	scale_color_manual(values=c("Other"="#e5e5e5", 
								"Africa" = "#00798c", 
								"Oceania"="#d1495b", 
								"Asia"="#edae49", 
								"Europe"="#66a182", 
								"Americas"="#2e4057")) +
	scale_alpha_continuous(breaks=c(0, 50,100,200,500, 1000,2000, 9999), 
								range=c(0.1,1)) +
	guides(alpha="none") +
	labs(col="Region", size="Number of \noutgoing nodes")

top15 <- sort(tapply(edges2$weight, edges2$samp_country, sum), decreasing = T)[1:25]
top15 <- top15[!names(top15) %in% df2$label[order(df2$weight, decreasing = T)][1:25]]
top15 <- df2[df2$label %in% names(top15),]
top15$label <- gsub(" ", "\n", top15$label)


p2 <- p1 + theme_void() +
	geom_text(data=top15, aes(x=x, y=y, label=label), size=3, hjust=1,
					vjust=0)
ggsave(file.path("figs", "Fig_02_network_global.svg"), p2, width=15, h=10)


# Parachute science -------------------------------------------------------


