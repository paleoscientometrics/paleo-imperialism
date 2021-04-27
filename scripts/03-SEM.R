library(tidyverse)
library(countrycode)
library(piecewiseSEM)

# Load data ---------------------------------------------------------------
load(file.path("data", "refs.RData"))
pubs <- setNames(data.frame(table(completed_refs$aff_code)), c("code", "npubs"))

gdp <- read.csv(file.path("data", "2020-05-28_GDP_WorldBank.csv"))
research_fund <- read.csv(file.path("data", "2020-05-28_RD_WorldBank.csv"))
epi <- read.csv(file.path("data", "2019_EPI.csv"))
gpi <- read.csv(file.path("data", "2019_GPI.csv"))
hdi <- read.csv(file.path("data", "2021_HDI_UNDP.csv"))
native <- readLines(file.path("data", "native.txt"))

# Transform data ----------------------------------------------------------
colnames(gdp)[1:2] <- colnames(research_fund)[1:2] <- c("country", "code")


# * Add country code if missing -------------------------------------------
hdi$code <- countrycode(hdi$Country, "country.name", "iso3c")
hdi <- hdi[!is.na(hdi$code),] #remove all without a code

gpi$code <- countrycode(gpi$Countries, "country.name", "iso3c")
gpi <- setNames(gpi[,c(3,2)], c("code", "gpi"))

epi$code <- countrycode(epi$countries, "country.name", "iso3c")
epi <- setNames(epi[,c(5,3)], c("code", "epi"))

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

