library(tidyverse)
library(igraph)
library(countrycode)
library(ggplot2)
library(patchwork)

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

df2[df2$label=="United States",]$y <-  24
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


edges2$region[edges2$weight < 26] <- "Other"

edges2$region <- factor(edges2$region,levels=c(regs, "Other"))
df2$weight_alpha <- df2$weight
df2$weight_alpha[df2$label %in% df2$label[order(df2$weight, decreasing = T)][1:25]] <- 9999

curvature = 0.3
alpha = 0.2
breaks <- c(50,100,500,1000,
			2000, 5000, 10000)
lab_size <- 4

palv <- viridis::viridis(5)
names(palv) <- c("Europe", "Americas", "Oceania", "Asia", "Africa")
palv["Europe"] <- "#75428f"
palv["Americas"] <- "#365091"
palv["Africa"] <- "#fdaa25"
palv["Asia"] <- "#027218"

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
	scale_color_manual(values=c("Other"="#e5e5e560", 
								palv)) +
	scale_alpha_continuous(breaks=c(0, 50,100,200,500, 1000,2000, 9999), 
								range=c(0.3,1)) +
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

# Igraph: statistics ------------------------------------------------------
gr <- df[,c("aff_code", "samp_code")]
gr <- gr[gr$aff_code != gr$samp_code,] # remove self nodes
gr <- graph_from_data_frame(gr, directed = T)

gr$degree <- igraph::degree(gr) # the degree of a node in a network is the number of connections it has to other nodes and the degree distribution is the probability distribution of these degrees over the whole network. 
gr$betweenness <- igraph::betweenness(gr) # detecting the amount of influence a node has over the flow of information in a graph. 
gr$closeness <- igraph::closeness(gr) #the more central a node is, the closer it is to all other nodes. 


reshape.df <- function(x){
	x <- sort(x, decreasing = TRUE)[1:15]
	x <- data.frame(value=x)
	x$code <- rownames(x)
	x$country <- countrycode(x$code, "iso3c", "country.name")
	return(x)
}

degree_top15 <- reshape.df(gr$degree)
betweenness_top15 <- reshape.df(gr$betweenness)
closeness_top15 <- reshape.df(gr$closeness)

p3 <- ggplot(degree_top15, aes(x=reorder(country, value), y=value)) +
	geom_bar(stat="identity", fill=pal[4]) + coord_flip(expand=FALSE) +
	labs(y="Degree", x="") +
	ggthemes::theme_hc()

p4 <- ggplot(betweenness_top15, aes(x=reorder(country, value), y=value)) +
	geom_bar(stat="identity", fill=pal[4]) + coord_flip(expand=FALSE) +
	labs(y="Betweenness", x="") +
	ggthemes::theme_hc()

svg(file.path("figs", "Supplementary", "Fig_S2_network_stats.svg"), w=8, h=3)
p3 + p4 + plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") & 
	theme(plot.tag = element_text(size = 10))
dev.off()

# Parachute science -------------------------------------------------------
df3 <- df
df3 <- df3[!df3$samp_country == "Antarctica",]

df3$foreign <- 1

#references with at least one local researcher is 0
df3$foreign[df3$samp_country == df3$aff_country] <- 0
df3$foreign[df3$reference_no %in% df3$reference_no[df3$foreign==0]] <- 0

ref_foreign <- tapply(df3$foreign, df3$reference_no, sum)
ref_foreign  <- ref_foreign[ref_foreign > 0]

parachuters <- df3[df3$reference_no %in% names(ref_foreign),]
parachuters <- unique(parachuters[,c("reference_no", "samp_country")])

outgoing <- table(parachuters$samp_country)

ref_local <- tapply(df3$foreign, df3$reference_no, sum)
ref_local <- ref_local[ref_local==0]

locals <- df3[df3$reference_no %in% names(ref_local),]
locals <- unique(locals[,c("reference_no", "samp_country")])

locals <- table(locals$samp_country)

outgoing <- as.data.frame(outgoing)
colnames(outgoing) <- c("country", "outgoing")

locals <- as.data.frame(locals)
colnames(locals) <- c("country", "local")

refs_countries <- locals %>% 
	left_join(outgoing) 

refs_countries[is.na(refs_countries)] <- 0

refs_countries$prop <- refs_countries$local/refs_countries$outgoing
refs_countries$index <- log(refs_countries$prop)

refs_countries$code <- countrycode::countrycode(refs_countries$country, origin="country.name", destination="iso3c")

#

# Plot --------------------------------------------------------------------

#Tile grid
# Load grid data
worldtilegrid <- read.csv(file.path("data", "worldtilegrid.csv"))
worldtilegrid$alpha.2[worldtilegrid$alpha.2=="GB"] <- "UK"

refs_countries$total <- refs_countries$local + refs_countries$outgoing 

pari <- merge(worldtilegrid, 
			  refs_countries[,c("code", "index")],
			  by.x="alpha.3", by.y="code" )

theme_map <- theme_minimal() + 
	theme(panel.grid = element_blank(), axis.text = element_blank(), 
		  axis.title = element_blank(),
		  #legend.text = element_text(angle=45, hjust=0.5),
		  legend.position = "bottom",
		  legend.title = element_text(face="bold"))


p1 <- ggplot(pari, aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1)) +
	geom_rect(aes(fill=index), color = "#ffffff") + 
	scale_fill_gradient2(high = "#fddeee30", mid="#de75aeff", low="#c5197dff",
						 midpoint = 1,
						 na.value = "grey80")+
	labs(fill="Parachute Index") +
	geom_text(aes(x = x, y = y, label = ifelse(index < 0, alpha.2, "")),
			  col="#ffffff", 
			  nudge_x = 0.5, nudge_y = -0.5, size = 3) +	
	scale_y_reverse() +
	guides(color=FALSE)+
	coord_equal()+
	theme_map

bottom10 <- refs_countries %>% 
	filter(!code %in% c("SJM", "GLD")) %>%
	filter(total > 29) %>% 
	slice_min(order_by=index, n=10)

p2 <- ggplot(bottom10, aes(x=reorder(country, -index), y=index)) +
	geom_bar(stat="identity", fill="#c5197dff") +
	scale_y_continuous(trans="reverse") +
	labs(x="", y=" Parachute Index")+
	coord_flip() +
	ggthemes::theme_hc() +
	theme(axis.title = element_text(face="bold"),
		  legend.title = element_text(face="bold"))

svg(file.path("figs", "Fig_03_parachute_index.svg"), 
	height=8, width=7)
p1 + p2 + plot_layout(ncol=1, heights=c(0.55, 0.25)) +
	plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") & 
	theme(plot.tag = element_text(size = 10))
dev.off()
	