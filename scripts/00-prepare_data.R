## ---------------------------
##
## Project: Colonial history and global economics distort our understanding of deep-time biodiversity
##
## Purpose of script: Reshape data to be used for analyses
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2021
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2021-03-13
## Last Modified: 2021-12-30
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(chronosphere)
library(sp)
library(spdep)
library(rworldmap)

# Load data -----------------------------------------------------------

pbdb <- chronosphere::fetch("pbdb")
all_refs <- read.csv("data/PBDB_refs.csv")
completed_refs <- read.csv("data/aff-data-complete.csv")

# Find countries for those not in there -----------------------------------

no_countries <- pbdb[pbdb$cc == "",c("lng", "lat")]
no_countries <- no_countries[!(is.na(no_countries$lng)|is.na(no_countries$lat)),]
no_countries <- unique(no_countries)

spts = SpatialPoints(no_countries[,1:2])
w <- rworldmap::getMap()
proj4string(spts) <- proj4string(w)

#planar coordinates
spts <- spTransform( spts, CRS('+proj=moll') ) 
w2 <- spTransform( w, CRS('+proj=moll') ) 

#find nearest country
dist_cc <- as.data.frame(rgeos::gDistance(spts, w2,byid=TRUE))

new_dist <- rep(NA, ncol(dist_cc))
cc <- rownames(dist_cc)
cc[cc=="Indian Ocean Territories"] <- "Australia"
cc[cc=="Saint Martin"] <- "The Netherlands"

for(i in 1:ncol(dist_cc)){
	temp <- dist_cc[,i]
	new_dist[i] <- cc[which.min(temp)]
}

no_countries <- cbind(no_countries, name=new_dist)
no_countries$cc <-countrycode::countrycode(no_countries$name, origin = 'country.name', 
										   destination = 'iso2c')

for (i in 1:nrow(no_countries)){
	n <- which(pbdb$lng == no_countries$lng[i] & pbdb$lat == no_countries$lat[i])
	
	pbdb$cc[n] <- no_countries$cc[i]
}

pbdb$country <- countrycode::countrycode(pbdb$cc, origin="iso2c", destination = "country.name")
pbdb$country[pbdb$cc=="UK"] <- "United Kingdom"
pbdb$country[pbdb$cc=="AA"] <- "Antarctica"
pbdb$country[pbdb$cc=="FA"] <- "Faroe Islands"
pbdb$country[pbdb$country=="United States Minor Outlying Islands (the)"] <- "United States"

# Make corrections to mistakes --------------------------------------------

completed_refs$aff_country <- plyr::mapvalues(completed_refs$aff_country, 
											  c("Brasil", "Chi", "Columbia", "Denkmark", "ISA", "Morroco", "Northern Ireland", "Phillipines", "Yemen Arab Republic", "Pland", "Rondebosch"),
											  c("Brazil", "China", "Colombia", "Denmark", "USA", "Morocco", "UK", "Philippines", "Yemen", "Poland", "South Africa"))

completed_refs$aff_country[completed_refs$reference_no=="74387"] <- c("Argentina", "Peru","France")

temp <- completed_refs
temp <- temp[!temp$samp_country %in% c("", "ODP Site"),]
n <- grep("Cura.+", temp$samp_country)
temp$samp_country[n] <- "Curacao"


# Recode country to ISO3 --------------------------------------------------

temp$samp_code <- countrycode::countrycode(temp$samp_country, origin="country.name", destination="iso3c")
temp$aff_code <- countrycode::countrycode(temp$aff_country, origin="country.name", destination="iso3c")

temp$aff_country <- countrycode::countrycode(temp$aff_code, origin = "iso3c", destination = "country.name")
temp$samp_country <- countrycode::countrycode(temp$samp_code, origin = "iso3c", destination = "country.name")

completed_refs <- temp

# Save data ---------------------------------------------------------------

saveRDS(pbdb[,c("collection_no", "lng", "lat", "reference_no", "country", "cc")], file="data/pbdb.rds")
save(all_refs, completed_refs, file="data/refs.RData")
