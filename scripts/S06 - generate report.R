## ---------------------------
##
## Project: Colonialism in paleontology
##
## Purpose of script: Create a document to list all references used in the study
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2021
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2021-08-27
## Last Modified:
##
## ---------------------------
##
## Notes: This script was written during the time that the refer package
## was being developed. As such, it might only work with the earliest version.  
##
## ---------------------------

# Load library ------------------------------------------------------------
library(refer) # v0.0.1

# Download updated pbdb references ----------------------------------------

cats <- c("colls", "occs")
for(i in cats){
	url <- sprintf("https://paleobiodb.org/data1.2/occs/refs.csv?interval=Cryogenian,Holocene&select=%s&show=formatted,entname", i)
	download.file(url, destfile = sprintf("data/references_%s.csv", i))
}

fnames <- lapply(cats, function(x)sprintf("data/references_%s.csv",x))

dats <- lapply(fnames, function(x) read.csv(x, encoding = "UTF-8"))
fullrefs <- do.call(rbind, dats)

# Get references used in study --------------------------------------------

load("data/refs.RData")
refs1990 <- unique(all_refs[all_refs$pubyr > 1990,]$reference_no)
dat <- completed_refs[completed_refs$reference_no %in% refs1990,]
refs <- unique(dat$reference_no)

dir.create("report") #folder for report

# Find and compile missing references -------------------------------------
missing <- refs[which(!refs %in% fullrefs$reference_no)]
length(missing)
missing <- all_refs[all_refs$reference_no %in% missing,]

# Create bib file from missing references
f <- "data/missing.ris"

# Add header
xfun::write_utf8('Provider: The Paleobiology Database
Database: The Paleobiology Database
Compile: Nussaïbah B. Raja
Content: text/plain; charset="utf-8"', f)

# Template RIS file
template <- c("
TY  - %s
ID  - ref:%s
AU  - %s
PY  - %s
TI  - %s
T2  - %s
VL  - %s
IS  - %s
SP  - %s
LA  - %s\nKW  - data")

# Write to file

for(i in 1:nrow(missing)){
	temp <- missing[i,]
	
	bib <- sprintf(template, 
				   toupper(substr(temp$publication_type, 1,4)),
				   temp$reference_no,
				   gsub(",", " and ", paste0(temp$author1init, " ", temp$author1last, " and ", temp$author2init, " ", temp$author2last, " and ", temp$otherauthors)),
				   temp$pubyr,
				   temp$reftitle,
				   temp$pubtitle,
				   temp$pubvol,
				   temp$pubno,
				   paste(temp$firstpage, temp$lastpage, sep="--"),
				   temp$language)
	
	bib <- gsub("NA", "", bib)
	
	write(bib,file=f,append=TRUE)
}

rbibutils::bibConvert("report/missing.ris", "report/missing.bib", informat="ris", outformat = "bib")

system("biber --tool -V report/missing.bib")

file.remove(f)
file.remove("report/missing.bib.blg")
file.rename("report/missing_bibertool.bib", f)

refer::subset_bib("data/pbdb_refs.bib", refs, "report/references.bib")

# Enterers ----------------------------------------------------------------

enterers <- unique(fullrefs$enterer[fullrefs$reference_no %in% refs])
enterers
xfun::write_utf8(paste(sort(enterers), collapse=", "), "report/enterernames.txt")

# Create report ---------------------------------------------------

# Create generic metafile
inputFile <- refer::create_metadata(path="report", edit=F, overwrite = F)

# Generate report
refer::report(inputFile=inputFile,
			  data_refs = "references.bib",
			  output_path = file.path("."),
			  output_file = "report.pdf",
			  enterer_names=enterers)

# Create sample data to add to report -------------------------------------

samp <- completed_refs[30:40,]
samp
write.csv(samp,"report/sample_data.csv", row.names = F)
