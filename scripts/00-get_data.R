library(chronosphere)
library(sp)
library(spdep)
library(rworldmap)

# Download data -----------------------------------------------------------

pbdb <- chronosphere::fetch("pbdb")

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


# Save data ----------------------------------------------------------------

saveRDS(pbdb[,c("collection_no", "lng", "lat", "reference_no", "country", "cc")], file="data/pbdb.rds")

all_refs <- read.csv("https://raw.githubusercontent.com/paleoscientometrics/paleo-aff-initiative/main/data_archive/PBDB_refs.csv")

all_refs <- all_refs[all_refs$pubyr > 1989 & all_refs$pubyr < 2021,]

completed_refs <- read.csv("https://raw.githubusercontent.com/paleoscientometrics/paleo-aff-initiative/main/data_archive/aff-data-complete.csv")

completed_refs$aff_country <- plyr::mapvalues(completed_refs$aff_country, 
											  c("Brasil", "Chi", "Columbia", "Denkmark", "ISA", "Morroco", "Northern Ireland", "Phillipines", "Yemen Arab Republic", "Pland", "Rondebosch"),
											  c("Brazil", "China", "Colombia", "Denmark", "USA", "Morocco", "UK", "Philippines", "Yemen", "Poland", "South Africa"))

completed_refs$aff_country[completed_refs$reference_no=="74387"] <- c("Argentina", "Peru","France")

save(all_refs, completed_refs, file="data/refs.RData")
