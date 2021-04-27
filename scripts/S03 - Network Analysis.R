library(tidyverse)
library(igraph)
library(countrycode)
library(ggplot2)

palv <- viridis::viridis(5)
names(palv) <- c("Europe", "Americas", "Oceania", "Asia", "Africa")
palv["Europe"] <- "#75428f"
palv["Americas"] <- "#365091"
palv["Africa"] <- "#fdaa25"
palv["Asia"] <- "#027218"

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

svg(file.path("figs","Supplementary", "Fig_S1_network_analysis.svg"), w=6, h=6)
par(mar=c(1,1,1,1), mfrow=c(2,2))

start=0
#Network
for (reg in c("Asia", "Europe", "South America", "Africa")){
	
	edges <- df[grep(reg, df$samp_region),] %>% 
		group_by(aff_region, samp_region) %>% 
		summarise(weight = n()) %>% 
		ungroup()
	
	edges_sub <- edges[grep(reg, edges$samp_region),]
	
	#remove uncessary edges
	edges_sub <- edges_sub[edges_sub$weight > 25,]
	
	edges_sub[edges_sub$weight <50 & !edges_sub$aff_region %in% c("Middle Africa", "Southern Africa", "Western Africa"),]$aff_region <- "Others"
	
	vert <- edges_sub[,c("aff_region", "weight")] %>% 
		group_by(aff_region) %>% 
		summarise(weight=sum(weight))
	
	g <- graph_from_data_frame(edges_sub, directed=TRUE, vertices = vert)
	
	#check
	#plot(g)
	
	clrs <- V(g)$name

	clrs[grep("Africa", clrs)] <- palv["Africa"]
	clrs[grep("America", clrs)] <- palv["Americas"]
	clrs[grep("Europe", clrs)] <- palv["Europe"]
	clrs[grep("Asia", clrs)] <- palv["Asia"]
	clrs[grep("Australia", clrs)] <- palv["Oceania"]
	clrs[grep("Others", clrs)] <- "darkgrey"
	
	V(g)$color <- clrs
	V(g)$size <- V(g)$weight/sum(V(g)$weight)*100
	V(g)$label <- ifelse(V(g)$weight > 10, V(g)$name, NA)
	
	
	
	# The labels are currently node IDs.
	
	# Setting them to NA will render no labels:
	
	V(g)$label.color <- "black"
	E(g)$width <- E(g)$weight/sum(E(g)$weight)*20
	E(g)$edge.color <- "gray80"
	
	V(g)$frame.color <- "white"
	
	
	plot(g, 
		 layout=layout_with_gem, edge.curved=.1,
		 vertex.label.cex 	=0.8)
	start = start +1
	title(main=paste0("(", letters[start], ") ", reg),adj=0, font.main=1, cex.main=1)

}
dev.off()
