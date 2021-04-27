library(tidyverse)
library(countrycode)
library(piecewiseSEM)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Load data ---------------------------------------------------------------
load(file.path("data", "refs.RData"))
pubs <- setNames(data.frame(table(completed_refs$aff_code)), c("code", "npubs"))
imperialism <- readxl::read_xls("data/imperialism.xls", sheet=4)

gdp <- read.csv(file.path("data", "2021-02-03_GDP_percapita_WorldBank.csv"), skip=4)
research_fund <- read.csv(file.path("data", "2020-05-28_RD_WorldBank.csv"))
epi <- read.csv(file.path("data", "2019_EPI.csv"))
gpi <- read.csv(file.path("data", "2019_GPI.csv"))
hdi <- read.csv(file.path("data", "2021_HDI_UNDP.csv"))
native <- readLines(file.path("data", "native.txt"))

population <- read.csv(file.path("data", "2021-03-13_population_worldbank.csv"), skip=4)

# Transform data ----------------------------------------------------------
colnames(gdp)[1:2] <- colnames(research_fund)[1:2] <- colnames(population)[1:2] <- c("country", "code")

# * Add country code if missing -------------------------------------------
hdi$code <- countrycode(hdi$Country, "country.name", "iso3c")
hdi <- hdi[!is.na(hdi$code),] #remove all without a code

gpi$code <- countrycode(gpi$Countries, "country.name", "iso3c")
gpi <- setNames(gpi[,c(3,2)], c("code", "gpi"))

epi$code <- countrycode(epi$countries, "country.name", "iso3c")
epi <- setNames(epi[,c(5,3)], c("code", "epi"))

imperialism$code <- countrycode(imperialism$country, "country.name", "iso3c")

# * Calculate mean values for each index ----------------------------------
gdp <- gdp %>% select(-Indicator.Name) %>% 
	pivot_longer(cols=X1990:X2019) %>% 
	mutate(year=as.numeric(gsub("X", "", name))) %>% 
	group_by(code) %>% 
	summarise(gdp = mean(value, na.rm = TRUE)) 

research_fund <- research_fund %>% select(-Indicator.Name) %>% 
	pivot_longer(cols=X1990:X2019) %>% 
	mutate(year=as.numeric(gsub("X", "", name))) %>% 
	group_by(code) %>% 
	summarise(research = mean(value, na.rm = TRUE)) 

population <- population %>% 
	pivot_longer(cols=X1990:X2019) %>% 
	mutate(year=as.numeric(gsub("X", "", name))) %>% 
	group_by(code) %>% 
	summarise(pop = mean(value, na.rm = TRUE)) 

research_fund <- merge(research_fund, population) %>% #research funding per capita
	mutate(research=research/pop) %>% 
	select(code, research)

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

df <- Reduce(multimerge, list(pubs, gdp, hdi, research_fund, epi, gpi))
df$imperialism <- 0
df$imperialism[df$code %in% imperialism$code] <- 1
df <- na.omit(df)
# Models ------------------------------------------------------------------

# * Stand-alone -----------------------------------------------------------
mod.fin <- lm(npubs ~ gdp + hdi + research + gpi + epi + imperialism, df)
mod.fin <- step(mod.fin)

mod.gdp <- lm(gdp ~ hdi + gpi + epi + imperialism, df)
mod.gdp <- step(mod.gdp)

mod.epi <- lm(epi ~ gdp + hdi + research, df) 

mod.research <- lm(research ~ gdp + epi + imperialism, df)
mod.research <- step(mod.research)

# * SEM -------------------------------------------------------------------
model.list <- list(mod.fin, mod.research)

res <- as.psem(model.list)
summary(res)

res2 <- update(res, mod.gdp)
res3 <- update(res2, mod.epi)

AIC(res, res2)
AIC(res2, res3)

coefs_res <-coefs(res)
plot(res) # to view

# Plot --------------------------------------------------------------------
coords <- data.frame(name=c("npubs", "research", "imperialism", "hdi", "gdp", "epi"),
					 labels =c("Number of\npublications", "Research\nfunding", "Imperialist\nbackground", 
					 		  "HDI", "GDP", "EEP"),
					 x=c(2, 2,1,3,1.5,2.5),
					 y=c(1,2,2,2,3,3))

coefs_res$x1 <- plyr::mapvalues(coefs_res$Predictor, coords$name, coords$x) %>% as.numeric()
coefs_res$x2 <- plyr::mapvalues(coefs_res$Response, coords$name, coords$x)%>% as.numeric()

coefs_res$y1 <- plyr::mapvalues(coefs_res$Predictor, coords$name, coords$y)%>% as.numeric()
coefs_res$y2 <- plyr::mapvalues(coefs_res$Response, coords$name, coords$y)%>% as.numeric()


ggplot() +
	geom_segment(data=coefs_res, aes(x=x1, y=y1, xend=x2, yend=y2))+
	geom_label(data=coords, aes(x=x, y=y, label=labels), 
			   hjust=0.5, label.r=unit(0.05, "lines"), 
			   label.padding = unit(0.3, "lines"), size=3) +
	geom_text(data=coefs_res, aes(x=(x1+x2)/2, y=(y1+y2)/2, 
								  label=round(Std.Estimate, 2)), size=3) +
	coord_cartesian(xlim=c(0.5,3.5), ylim=c(0.5, 3.5)) +
	theme_void()

ggsave(file.path("figs", "Fig_04_model.svg"), w=8, h=6)
