library(chronosphere)
library(countrycode)
library(divDyn)

pbdb <- fetch("pbdb")

# add countries
pbdb$iso3c <- countrycode(pbdb$cc, "iso2c", "iso3c")
pbdb$iso3c[pbdb$cc=="UK"] <- countrycode("United Kingdom", "country.name", "iso3c")
pbdb <- pbdb[!is.na(pbdb$iso3c),]

income_class <- read.csv(file.path("data", "2019_income_classification_worldbank.csv"))
income_class$region <- countrycode(income_class$code, "iso3c", "region23")
imperial <- read.csv(file.path("data", "imperialism.csv"))
imperial$code <- countrycode(imperial$country, "country.name", "iso3c")
income_class$colonialism <- 0
income_class$colonialism[income_class$code %in% imperial$code] <- 1

dat <- merge(pbdb, income_class, by.x="iso3c", by.y="code", all.x=T, all.y=F)

need<-c("collection_no","collection_name","accepted_name","accepted_rank","identified_name","identified_rank","early_interval","late_interval","max_ma","min_ma",
		"reference_no","phylum","class","order","family","genus","lng","lat","paleolng","paleolat","formation","lithology1","lithification1","environment","created","zone",
		"iso3c", "country", "classification", "reference_no", "region", "colonialism")
dat <- dat[,need]

# Taxonomic filtering ----
dat <- dat[dat$accepted_rank%in%c("genus","species"), ]
dat <- dat[dat$genus!="", ]
nrow(dat)

# sampled phyla
# levels(factors(dat$phylum))
#A. phyla
marineNoPlant <-c("","Agmata","Annelida","Bilateralomorpha","Brachiopoda","Bryozoa","Calcispongea","Chaetognatha","Cnidaria","Ctenophora","Echinodermata","Entoprocta","Foraminifera","Hemichordata","Hyolitha",
				  "Mollusca","Nematoda","Nematomorpha","Nemertina","Onychophora","Petalonamae","Phoronida","Platyhelminthes","Porifera","Problematica","Rhizopodea","Rotifera","Sarcomastigophora","Sipuncula","Uncertain","Vetulicolia","")

# logical vector of rows indicating these
bByPhyla <- dat$phylum%in%marineNoPlant

#B. classes
#levels(factor(noNeed$class))
needClass <-c("Acanthodii","Actinopteri","Actinopterygii","Agnatha","Cephalaspidomorphi","Chondrichthyes","Cladistia","Coelacanthimorpha","Conodonta","Galeaspida","Myxini","Osteichthyes","Petromyzontida","Plagiostomi","Pteraspidomorphi","Artiopoda","Branchiopoda","Cephalocarida","Copepoda","Malacostraca","Maxillopoda","Megacheira","Merostomoidea",
			  "Ostracoda","Paratrilobita","Pycnogonida","Remipedia","Thylacocephala","Trilobita","Xiphosura")
# logical vector of rows indicating occurrences
bNeedClass <- dat$class%in%needClass

#C. mammals
# mammals <- dat[dat$class=="Mammalia", ]
# levels(factor(mammals$order))
needMammalOrd <-c("Cetacea","Sirenia")
bMammalOrder <- dat$order%in%needMammalOrd

# the carnivores
# carnivores <- dat[dat$order=="Carnivora", ]
# levels(factor(carnivores$family))
needFam <-c("Otariidae","Phocidae","Desmatophocidae")
bNeedMamFam <- dat$family%in%needFam

# D. Reptiles
# reptiles <- dat[dat$class=="Reptilia", ]
# levels(factor(reptiles$order))

needReptOrd<-c("Eosauropterygia","Hupehsuchia","Ichthyosauria","Placodontia","Sauropterygia","Thalattosauria")
# the logical vector for the total data
bRept <- dat$order%in%needReptOrd

# E. Sea turtles
# turtles <- dat[dat$order=="Testudines", ]
# levels(factor(turtles$family))

needTurtleFam <-c("Cheloniidae","Protostegidae","Dermochelyidae","Dermochelyoidae","Toxochelyidae","Pancheloniidae")
bTurtle <- dat$family%in%needTurtleFam

# subset
dat <- dat[bByPhyla|bNeedClass|bMammalOrder|bNeedMamFam|bRept|bTurtle, ]
nrow(dat)

# resolve the potential homonymy problem
dat$clgen <-paste(dat$class, dat$genus)

# filter by environment
levels(factor((dat$environment)))

omitEnv <-c("\"floodplain\"","alluvial fan","cave","\"channel\"","channel lag","coarse channel fill","crater lake","crevasse splay","dry floodplain","delta plain","dune","eolian indet.","fine channel fill","fissure fill","fluvial indet.","fluvial-lacustrine indet.","fluvial-deltaic indet.","glacial","interdune","karst indet.","lacustrine - large","lacustrine - small","lacustrine delta front","lacustrine delta plain","lacustrine deltaic indet.","lacustrine indet.","lacustrine interdistributary bay","lacustrine prodelta","levee","loess","mire/swamp","pond","sinkhole","spring","tar","terrestrial indet.","wet floodplain")

# omit the occurrences
dat <- dat[!dat$environment%in%omitEnv, ]
nrow(dat)

# omit fossils from unlithified sediments
# As these are more frequent in younger sites and occur heterogeneously, sampling
# bias can be reduced by omitting such collections from the data.
dat <- dat[dat$lithification1!="unlithified", ]

# Stratigraphic binning ----
data(stages)
data(keys)

# the 'stg' entries (lookup)
stgMin <- categorize(dat[ ,"early_interval"], keys$stgInt)
stgMax <- categorize(dat[ ,"late_interval"], keys$stgInt)

# convert to numeric
stgMin <- as.numeric(stgMin)
stgMax <- as.numeric(stgMax)					 


# convert to numeric
stgMin <- as.numeric(stgMin)
stgMax <- as.numeric(stgMax)

# empty container
dat$stg <- rep(NA, nrow(dat))

# select entries, where
stgCondition <- c(
	# the early and late interval fields indicate the same stg
	which(stgMax==stgMin),
	# or the late_intervar field is empty
	which(stgMax==-1))

# in these entries, use the stg indicated by the early_interval
dat$stg[stgCondition] <- stgMin[stgCondition] 
table(dat$stg)

# Cambrian, Na and Kiessling, 2015
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))
source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R")

# Ordovician, Kiessling
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))
source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R")

table(dat$stg)

# Diversity ----
# A. Raw patterns
ddStages <-divDyn(dat,bin="stg",tax="clgen")
ddStages.high <- divDyn(dat[dat$classification %in% c("H", "UM"),], bin="stg",tax="clgen")
ddStages.low <- divDyn(dat[dat$classification %in% c("L", "LM"),], bin="stg",tax="clgen")

# the plot
tsplot(stages,boxes="sys",shading="sys",xlim=4:95,ylim=c(0,2800),ylab="Richness (raw data)",xlab="Age (Ma)")
lines(stages$mid, ddStages$divCSIB,col="blue",lwd=2)
lines(stages$mid, ddStages.high$divCSIB,col="#00BB33",lwd=1)
lines(stages$mid, ddStages.low$divCSIB,col="red",lwd=1)

legend("top",inset=c(0.01,0.01),
	   legend=c("Lower middle and low income", "High and upper middle","All data"),
	   col=c("red", "#00BB33","blue"),lwd=c(1,2),bg="white",cex=0.8)

# B. Subsampling
it=100
sqsStagesPlot <-subsample(dat,bin="stg",tax="clgen",coll="collection_no",q=0.7,iter=it,ref="reference_no",singleton="ref",type="sqs",duplicates=FALSE,excludeDominant=TRUE,largestColl =TRUE,output="dist",na.rm=TRUE)
sqsStagesPlot.high <-subsample(dat[dat$classification %in% c("H", "UM"),],bin="stg",tax="clgen",coll="collection_no",q=0.7,iter=it,ref="reference_no",singleton="ref",type="sqs",duplicates=FALSE,excludeDominant=TRUE,largestColl =TRUE,output="dist",na.rm=TRUE)
sqsStagesPlot.low <-subsample(dat[dat$classification %in% c("L", "LM"),],bin="stg",tax="clgen",coll="collection_no",q=0.7,iter=it,ref="reference_no",singleton="ref",type="sqs",duplicates=FALSE,excludeDominant=TRUE,largestColl =TRUE,output="dist",na.rm=TRUE)

tsplot(stages,boxes="sys",shading="sys",xlim=4:95,ylim=c(0,1500),ylab="Richness (corrected SIB)",xlab="Age (Ma)")
lines(stages$mid, sqsStagesPlot$divCSIB[ ,51],col="blue",lwd=2)
lines(stages$mid, sqsStagesPlot.high$divCSIB[ ,51],col="#00BB33",lwd=2)
lines(stages$mid, sqsStagesPlot.low$divCSIB[ ,51],col="red",lwd=2)
legend("top",inset=c(0.01,0.01),
       legend=c("Lower middle and low income", "High and upper middle","All data"),
       col=c("red", "#00BB33","blue"),lwd=c(1,2),bg="white",cex=0.8)

# C. Europe and North America
ddStages <-divDyn(dat,bin="stg",tax="clgen")

reg<- unique(grep("Europe", income_class$region, value=T))
regions <- c(reg, "North America", "Eastern Asia", "Australia and New Zealand")

ddStages.eu <- divDyn(dat[dat$region %in% regions,], bin="stg",tax="clgen")
ddStages.neu <- divDyn(dat[!dat$region %in% regions,], bin="stg",tax="clgen")

tsplot(stages,boxes="sys",shading="sys",xlim=4:95,ylim=c(0,2800),ylab="Richness (raw data)",xlab="Age (Ma)")
lines(stages$mid, ddStages$divCSIB,col="blue",lwd=2)
lines(stages$mid, ddStages.eu$divCSIB,col="#00BB33",lwd=1)
lines(stages$mid, ddStages.neu$divCSIB,col="red",lwd=1)

legend("top",inset=c(0.01,0.01),
       legend=c("All data", "Europe, North America,  Eastern Asia & Australia and New Zealand", "Rest of the World"),
       col=c("blue", "#00BB33", "red"),lwd=c(1,2),bg="white",cex=0.8)

cor.test(ddStages$divCSIB, ddStages.eu$divCSIB)

# Subsampling
it=100
sqsStagesPlot.eu<-subsample(dat[dat$region %in% regions,],bin="stg",tax="clgen",coll="collection_no",q=0.7,iter=it,ref="reference_no",singleton="ref",type="sqs",duplicates=FALSE,excludeDominant=TRUE,largestColl =TRUE,output="dist",na.rm=TRUE)
sqsStagesPlot.neu <-subsample(dat[!dat$region %in% regions,],bin="stg",tax="clgen",coll="collection_no",q=0.7,iter=it,ref="reference_no",singleton="ref",type="sqs",duplicates=FALSE,excludeDominant=TRUE,largestColl =TRUE,output="dist",na.rm=TRUE)

tsplot(stages,boxes="sys",shading="sys",xlim=4:95,ylim=c(0,1500),ylab="Richness (corrected SIB)",xlab="Age (Ma)")
lines(stages$mid, sqsStagesPlot$divCSIB[ ,51],col="blue",lwd=2)
lines(stages$mid, sqsStagesPlot.eu$divCSIB[ ,51],col="#00BB33",lwd=2)
lines(stages$mid, sqsStagesPlot.neu$divCSIB[ ,51],col="red",lwd=2)
legend("top",inset=c(0.01,0.01),
       legend=c("All data", "Europe, North America,  Eastern Asia & Australia and New Zealand", "Rest of the World"),
       col=c("blue", "#00BB33", "red"),lwd=c(1,2),bg="white",cex=0.8)



tsplot(stages,boxes="sys",shading="sys",xlim=4:95,ylim=c(0,1.5),ylab="Extinction Rate",xlab="Age (Ma)")
lines(stages$mid, sqsStagesPlot$extPC[ ,51],col="blue",lwd=2)
lines(stages$mid, sqsStagesPlot.eu$extPC[ ,51],col="#00BB33",lwd=2)
lines(stages$mid, sqsStagesPlot.neu$extPC[ ,51],col="red",lwd=2)
legend("top",inset=c(0.01,0.01),
       legend=c("All data", "Europe, North America,  Eastern Asia & Australia and New Zealand", "Rest of the World"),
       col=c("blue", "#00BB33", "red"),lwd=c(1,2),bg="white",cex=0.8)

# Colonialism
ddStages <-divDyn(dat,bin="stg",tax="clgen")
ddStages.col <- divDyn(dat[dat$colonialism==1,], bin="stg",tax="clgen")
ddStages.ncol <- divDyn(dat[dat$colonialism==0,], bin="stg",tax="clgen")

# the plot
tsplot(stages,boxes="sys",shading="sys",xlim=4:95,ylim=c(0,2800),ylab="Richness (raw data)",xlab="Age (Ma)")
lines(stages$mid, ddStages$divCSIB,col="blue",lwd=2)
lines(stages$mid, ddStages.col$divCSIB,col="#00BB33",lwd=1)
lines(stages$mid, ddStages.ncol$divCSIB,col="red",lwd=1)

legend("top",inset=c(0.01,0.01),
       legend=c("No colonising history", "Cololnising history","All data"),
       col=c("red", "#00BB33","blue"),lwd=c(1,2),bg="white",cex=0.8)

# B. Subsampling
it=100
sqsStagesPlot <-subsample(dat,bin="stg",tax="clgen",coll="collection_no",q=0.7,iter=it,ref="reference_no",singleton="ref",type="sqs",duplicates=FALSE,excludeDominant=TRUE,largestColl =TRUE,output="dist",na.rm=TRUE)
sqsStagesPlot.col <-subsample(dat[dat$colonialism==1,],bin="stg",tax="clgen",coll="collection_no",q=0.7,iter=it,ref="reference_no",singleton="ref",type="sqs",duplicates=FALSE,excludeDominant=TRUE,largestColl =TRUE,output="dist",na.rm=TRUE)
sqsStagesPlot.ncol <-subsample(dat[dat$colonialism==0,],bin="stg",tax="clgen",coll="collection_no",q=0.7,iter=it,ref="reference_no",singleton="ref",type="sqs",duplicates=FALSE,excludeDominant=TRUE,largestColl =TRUE,output="dist",na.rm=TRUE)

tsplot(stages,boxes="sys",shading="sys",xlim=4:95,ylim=c(0,1500),ylab="Richness (corrected SIB)",xlab="Age (Ma)")
lines(stages$mid, sqsStagesPlot$divCSIB[ ,51],col="blue",lwd=2)
lines(stages$mid, sqsStagesPlot.col$divCSIB[ ,51],col="#00BB33",lwd=2)
lines(stages$mid, sqsStagesPlot.ncol$divCSIB[ ,51],col="red",lwd=2)
legend("top",inset=c(0.01,0.01),
       legend=c("No colonising history", "Cololnising history","All data"),
       col=c("red", "#00BB33","blue"),lwd=c(1,2),bg="white",cex=0.8)
