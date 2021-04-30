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

# Taxa per countries -----------------------------------------------------------------
fullpbdb <- chronosphere::fetch("pbdb") #get download of pbdb with taxonomy information

countries <- c("Myanmar", "Dominican Republic", "Mongolia", "Kazakhstan", "Morocco")
cc <- countrycode(countries, "country.name", "iso3c")
cc2 <- countrycode(cc, "iso3c", "iso2c")

df.mmr <- df[df$samp_code %in% cc,]
refs.mmr <- all_refs[all_refs$reference_no %in% df.mmr$reference_no,]

pbdb <- readRDS(file.path("data", "pbdb.rds"))
pbdb.mmr <- pbdb[pbdb$cc %in% cc2 & pbdb$reference_no %in% df.mmr$reference_no,]


pbdb.mmr <- merge(pbdb.mmr, fullpbdb[,c("phylum", "order", "family","genus", "collection_no", "identified_name")], all.x=TRUE, all.y=FALSE)
pbdb.mmr <- merge(df.mmr, pbdb.mmr[,c("reference_no", "phylum", "order", "family", "genus", "identified_name")], all.x=TRUE, all.y=FALSE)

pbdb.mmr$categories <- NA
pbdb.mmr$categories[pbdb.mmr$phylum == "Arthropoda"] <- "Arthropods"
pbdb.mmr$categories[pbdb.mmr$phylum == "Chordata" ] <- "Vertebrates"
pbdb.mmr$categories[is.na(pbdb.mmr$categories)] <- "Fungi and invertebrates"

pbdb.mmr <- unique(pbdb.mmr[,c("reference_no", "aff_country", "categories", "samp_country")]) #get unique values

pbdb.mmr <- pbdb.mmr %>% group_by(aff_country, samp_country, categories) %>% 
  tally() %>% 
  group_by(aff_country, samp_country) %>% 
  mutate(total = sum(n)) %>% 
  filter(total >= 25) 
pbdb.mmr$categories <- factor(pbdb.mmr$categories, 
                              levels=c("Arthropods", "Vertebrates", "Fungi and invertebrates"))

ggplot(pbdb.mmr, aes(x=reorder(aff_country,total), y=n, fill=categories)) +
  geom_bar(stat="identity", width=0.8) +
  labs(x="", fill="") +
  facet_grid(samp_country~., scales = "free", space = "free_y") +
  scale_fill_manual(values=pal[5:3])+
  guides(fill=guide_legend(nrow=2)) +
  coord_flip() +
  theme(strip.background = element_rect(fill=paste0(pal[5], "20")))

ggsave(file.path("figs", "Supplementary", "Fig_S_taxa_per_country.svg"), w=8, h=10)
