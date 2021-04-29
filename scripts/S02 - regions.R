library(countrycode)

View(codelist)

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
