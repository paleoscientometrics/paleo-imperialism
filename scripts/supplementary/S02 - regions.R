## ---------------------------
##
## Project: Colonial history and global economics distortour understanding of deep-time biodiversity
##
## Purpose of script: Create a region list
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
## ---------------------------V
library(countrycode)

regions <- na.omit(unique(codelist_panel[,c("continent", "region23")]))
regions$countries <- NA

for(i in 1:nrow(regions)){
	temp <- codelist_panel[codelist_panel$continent==regions$continent[i] & 
						   	codelist_panel$region23==regions$region23[i],]
	temp <- unique(temp$country.name.en)
	temp <- temp[!is.na(temp)]
	regions$countries[i] <- paste(sort(temp), collapse=", ")
}

regions <- regions[order(regions$continent, regions$region23),]
write.csv(regions, "regions.csv", row.names = FALSE)
