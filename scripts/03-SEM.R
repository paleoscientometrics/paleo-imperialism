library(tidyverse)
library(countrycode)
library(piecewiseSEM)

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

mod.research <- lm(research ~ gdp + epi + hdi + gpi + imperialism, df)
mod.research <- step(mod.research)

# * SEM -------------------------------------------------------------------
model.list <- list(mod.fin, mod.gdp, mod.research)

res <- as.psem(model.list)
summary(res)

plot(res)
