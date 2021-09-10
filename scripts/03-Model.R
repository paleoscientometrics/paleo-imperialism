## ---------------------------
##
## Project: Colonialism in paleontology
##
## Purpose of script: Path analysis to identify causal factors
## leading to research output in paleontology
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2021
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2021-03-13
## Last Modified: 2021-09-04
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
library(tidyverse)
library(countrycode)
library(piecewiseSEM)
library(patchwork)

pal <- c("#f0ffe9", "#ffe599", "#bbe487", "#4e9755", "#173109")

# Load data ---------------------------------------------------------------
load(file.path("data", "refs.RData"))
pubs <- setNames(data.frame(table(completed_refs$aff_code)), c("code", "npubs"))

imperialism <- read.csv("data/imperialism.csv")

gdp <- read.csv(file.path("data", "2021-02-03_GDP_percapita_WorldBank.csv"), skip=4)
epi <- read.csv(file.path("data", "2019_EPI.csv"))
gpi <- read.csv(file.path("data", "2019_GPI.csv"))
hdi <- read.csv(file.path("data", "2021_HDI_UNDP.csv"))
native <- readLines(file.path("data", "native.txt"))

# Transform data ----------------------------------------------------------
colnames(gdp)[1:2] <-  c("country", "code")

# * Add country code if missing -------------------------------------------
hdi$code <- countrycode(hdi$Country, "country.name", "iso3c")
hdi <- hdi[!is.na(hdi$code),] #remove all without a code

gpi$code <- countrycode(gpi$Countries, "country.name", "iso3c")
gpi <- setNames(gpi[,c(3,2)], c("code", "gpi"))

epi$code <- countrycode(epi$countries, "country.name", "iso3c")
epi <- setNames(epi[,c(5,3)], c("code", "epi"))
epi <- rbind(epi,cbind(code=countrycode(native, "country.name", "iso3c"), epi=75)) #add native speaker data
epi$epi <- as.numeric(epi$epi)

imperialism$code <- countrycode(imperialism$country, "country.name", "iso3c")

# * Calculate mean values for each index ----------------------------------
gdp <- gdp %>% select(-Indicator.Name) %>% 
	pivot_longer(cols=X1990:X2019) %>% 
	mutate(year=as.numeric(gsub("X", "", name))) %>% 
	group_by(code) %>% 
	summarise(gdp = mean(value, na.rm = TRUE)) 

hdi <- hdi %>% 
	pivot_longer(cols=X1990:X2019) %>% 
	mutate(year=as.numeric(gsub("X", "", name))) %>% 
	group_by(code) %>% 
	summarise(hdi = mean(value, na.rm = TRUE)) 

# * Merge data --------------------------------------------------------------
multimerge       <- function(x, y, by="code"){
	df            <- merge(x, y, by= by, all.x= T, all.y= T)
	return(df)
}

df <- Reduce(multimerge, list(pubs, gdp, hdi, epi, gpi))
df$imperialism <- 0
df$imperialism[df$code %in% imperialism$code] <- 1
nrow(df)

df <- na.omit(df)
nrow(df)

# Data deficient countries ------------------------------------------------
dd <- pubs[!pubs$code %in% df$code,]
dd <- Reduce(multimerge, list(dd, gdp, hdi, epi, gpi))
dd <- dd[!is.na(dd$npubs),]
dd <- dd[dd$npubs>25,]

dd$country <- countrycode(dd$code, "iso3c", "country.name") 

dd <- merge(dd, completed_refs[completed_refs$samp_code %in% dd$code,] %>% 
	group_by(samp_code, aff_country) %>% 
	tally() %>% 
	group_by(samp_code) %>% 
	summarise(t=paste0(aff_country, "(",n, ")", collapse=", ")),
	by.x="code", by.y="samp_code")

income <- read.csv("data/2019_income_classification_worldbank.csv")

#save data
write.csv(dd[order(dd$npubs, decreasing=T),c("country", "npubs", "t")], "output/data_deficient.csv",
		  row.names = F)

# income classification
dd <- merge(dd, income[,c(1,3)])

dd %>% group_by(classification) %>% 
	summarise(n=sum(npubs)) %>% 
	ungroup() %>% 
	mutate(prop=n/sum(n))

# Models ------------------------------------------------------------------

df$logpubs <- log(df$npubs)

# * Stand-alone -----------------------------------------------------------


mod.fin <- lm(logpubs~ hdi + epi + imperialism + gpi, df)
summary(mod.fin)

mod.hdi <- lm(hdi ~ gdp + imperialism + gpi, df)
mod.hdi <- step(mod.hdi)
mod.hdi

mod.epi <- lm(epi ~ hdi + gdp + imperialism + gpi, df)
mod.epi <- step(mod.epi)

# * CPA -------------------------------------------------------------------
model.list <- list(mod.fin, mod.hdi, mod.epi)

res <- as.psem(model.list)
summary(res)
plot(res)

coefs_res <-coefs(res)

# Plot --------------------------------------------------------------------
coords <- data.frame(name=c("logpubs", "imperialism", "hdi", "gdp", "gpi", "epi"),
					 labels =c("Research output\n in paleontology", "Imperial\nlegacy", 
					 		  "HDI", "GDP", "GPI", "EPI"),
					 x=c(2, 3,1,1,1.5, 2),
					 y=c(1,2,2,3,3, 2))

coefs_res$x1 <- plyr::mapvalues(coefs_res$Predictor, coords$name, coords$x) %>% as.numeric()
coefs_res$x2 <- plyr::mapvalues(coefs_res$Response, coords$name, coords$x)%>% as.numeric()

coefs_res$y1 <- plyr::mapvalues(coefs_res$Predictor, coords$name, coords$y)%>% as.numeric()
coefs_res$y2 <- plyr::mapvalues(coefs_res$Response, coords$name, coords$y)%>% as.numeric()

#Path model
p1 <- ggplot() +
	geom_segment(data=coefs_res, aes(x=x1, y=y1, xend=x2, yend=y2, size=abs(Std.Estimate), 
									 col=ifelse(Std.Estimate < 0, "-", "+"), alpha=ifelse(P.Value < 0.05, "sig", "nsig")))+
	geom_label(data=coords, aes(x=x, y=y, label=labels), 
			   hjust=0.5, label.r=unit(0.05, "lines"), 
			   label.padding = unit(0.3, "lines"), size=3, col=pal[5]) +
	 # geom_text(data=coefs_res, aes(x=(x1+x2)/2, y=(y1+y2)/2, 
	 # 							  label=round(Std.Estimate, 2)), size=3) +
	coord_cartesian(xlim=c(0.8,3.2), ylim=c(0.8, 3.2)) +
  scale_color_manual(values=pal[c(3,4)])+
  scale_alpha_manual(values=c(0.3, 1)) +
	theme_void() +
  theme(legend.position = "none")

direct <- coefs_res[coefs_res$Response == "logpubs",]
direct <- setNames(direct[,c(2,8)], c("var", "estimate"))

direct$influence <- "direct"

indirect <- coefs_res[!coefs_res$Response == "logpubs",]
indirect <- setNames(indirect[,c(2,8)], c("var", "estimate"))
indirect$influence <- "indirect"

influence <- rbind(direct, indirect)
influence$labs <- plyr::mapvalues(influence$var, coords$name, coords$labels)


p2 <- ggplot(influence, aes(x=labs, y=estimate, fill=influence)) +
  #geom_hline(yintercept = 0, linetype="dashed", col="darkgrey") +
  geom_bar(stat="identity", width=0.5) +
  labs(x="", y="Standardised effect size", fill="Influence") +
  scale_fill_manual(values=pal[c(4,3)]) +
  ggthemes::theme_hc() +
  theme(axis.title = element_text(face=2),
        legend.title = element_text(face=2),
        legend.position = "top")

svg(file.path("figs", "Fig_04_model.svg"), width=6, h=8)
p1 + p2 +
  plot_layout(ncol=1, heights = c(0.8,0.2)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
dev.off()
