library(tidyverse)
library(igraph)
library(countrycode)
library(ggplot2)
library(patchwork)
library(ggthemes)

palv <- viridis::viridis(5)
names(palv) <- c("Europe", "Americas", "Oceania", "Asia", "Africa")
palv["Europe"] <- "#75428f"
palv["Americas"] <- "#365091"
palv["Africa"] <- "#fdaa25"
palv["Asia"] <- "#027218"
pal <- c("#f0ffe9", "#ffe599", "#bbe487", "#4e9755", "#173109")

theme_set(theme_hc() %+replace% 
		  	theme(axis.title = element_text(face="bold"),
		  		  legend.title = element_text(face="bold"),
		  		  axis.title.y = element_text(angle=90))
)


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

# Stats -------------------------------------------------------------------
reshape.df <- function(x){
	x <- sort(x, decreasing = TRUE)[1:10]
	x <- data.frame(value=x)
	x$code <- rownames(x)
	x$country <- countrycode(x$code, "iso3c", "country.name")
	return(x)
}

for (reg in c("Asia", "Europe", "South America", "Africa")){
	
	gr <- df[grep(reg, df$samp_region),c("aff_code", "samp_code")]
	gr <- gr[gr$aff_code != gr$samp_code,] # remove self nodes
	gr <- graph_from_data_frame(gr, directed = T)
	
	gr$degree <- igraph::degree(gr) # the degree of a node in a network is the number of connections it has to other nodes and the degree distribution is the probability distribution of these degrees over the whole network. 
	gr$betweenness <- igraph::betweenness(gr) # detecting the amount of influence a node has over the flow of information in a graph. 
	
	
	degree_top15 <- reshape.df(gr$degree)
	betweenness_top15 <- reshape.df(gr$betweenness)
	
	p1 <- ggplot(degree_top15, aes(x=reorder(country, value), y=value)) +
		geom_bar(stat="identity", fill=pal[4]) + coord_flip(expand=FALSE) +
		labs(y="Degree", x="") +
		ggthemes::theme_hc()
	
	p2 <- ggplot(betweenness_top15, aes(x=reorder(country, value), y=value)) +
		geom_bar(stat="identity", fill=pal[4]) + coord_flip(expand=FALSE) +
		labs(y="Betweenness", x="") +
		ggthemes::theme_hc()
	
	svg(file.path("figs","Supplementary",sprintf("Fig_S_network_stats_%s.svg", reg)),w=8, h=3)
	p <- p1+p2 + plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
		theme(plot.tag = element_text(size=10))
	print(p)
	dev.off()
}

# Africa ------------------------------------------------------------------

df.africa <- df[grep("Africa", df$samp_region),]

df.africa <- df.africa %>% 
	#filter(samp_country != aff_country) %>% 
	group_by(samp_country, aff_country) %>% 
	tally() %>% 
	arrange(desc(n)) 

top.africa <- df.africa %>% group_by(samp_country) %>% 
	summarise(total = sum(n)) %>% 
	slice_max(order_by=total, n=5) %>% pull(samp_country)

top.researchers.africa <- df.africa %>%  filter(samp_country %in% top.africa) %>% 
	group_by(aff_country) %>% 
	tally() %>% 
	slice_max(order_by = n, n=5) %>% pull(aff_country)


df.africa$aff_country[!df.africa$aff_country %in% top.researchers.africa] <- "Other"
df.africa <- df.africa[df.africa$samp_country %in% top.africa,]
df.africa$samp_country <- factor(df.africa$samp_country, levels=rev(top.africa))
unique(df.africa$aff_country)

df.africa$aff_country <- factor(df.africa$aff_country, levels=rev(c("Other", rev(top.researchers.africa))))

p1 <- ggplot(df.africa, 
	   aes(x=samp_country, y=n, fill=aff_country)) +
	geom_bar(stat="identity", width=0.6) +
	labs(x="", y="Number of publications", fill="Researchers\nfrom") +
	scale_fill_manual(values=RColorBrewer::brewer.pal(9, "PiYG")) +
	guides(fill=guide_legend(ncol=3)) +
	coord_flip() 

# Asia ------------------------------------------------------------------

df.asia <- df[grep("Asia", df$samp_region),]

df.asia <- df.asia %>% 
	#filter(samp_country != aff_country) %>% 
	group_by(samp_country, aff_country) %>% 
	tally() %>% 
	arrange(desc(n)) 

top.asia <- df.asia %>% group_by(samp_country) %>% 
	summarise(total = sum(n)) %>% 
	slice_max(order_by=total, n=5) %>% pull(samp_country)

top.researchers.asia <- df.asia %>%  filter(samp_country %in% top.asia) %>% 
	group_by(aff_country) %>% 
	tally() %>% 
	slice_max(order_by = n, n=5) %>% pull(aff_country)


df.asia$aff_country[!df.asia$aff_country %in% top.researchers.asia & df.asia$aff_country != df.asia$samp_country] <- "Other"
df.asia <- df.asia[df.asia$samp_country %in% top.asia,]
df.asia$samp_country <- factor(df.asia$samp_country, levels=rev(top.asia))

unique(df.asia$aff_country)
df.asia$aff_country <- factor(df.asia$aff_country, 
							  levels=rev(c("Other", "Myanmar (Burma)", "Mongolia", "India", rev(top.researchers.asia))))

p2 <- ggplot(df.asia, 
	   aes(x=samp_country, y=n, fill=aff_country)) +
	geom_bar(stat="identity", width=0.6) +
	labs(x="", y="Number of publications", fill="Researchers\nfrom") +
	scale_fill_manual(values=RColorBrewer::brewer.pal(9, "PiYG")) +
	guides(fill=guide_legend(ncol=3)) +
	coord_flip() 

svg(file.path("figs", "Supplementary", "Fig_S_top_africa_asia.svg"), w=8, h=6)
p1 + p2 + plot_layout(ncol=1) +
	plot_annotation(tag_prefix = "(", tag_levels = "a", tag_suffix = ")") &
	theme(plot.tag=element_text(size=10))
dev.off()


