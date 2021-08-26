library(refer)

# url <- "https://paleobiodb.org/data1.2/occs/refs.csv?interval=Ediacaran,Holocene&select=occs"
# download.file(url, destfile = "data/references.csv")

fullrefs <- read.csv("data/references.csv", encoding = "UTF-8")

# Get references associated
load("data/refs.RData")

refs <- unique(completed_refs$reference_no)

dir.create("report")
subset_bib("data/pbdb_refs.bib", refs, "report/references.bib")

# Generate report
#Create generic metafile
inputFile <- create_metadata(path="report", edit=F, overwrite = F)

report(inputFile=inputFile,
       data_refs = "references.bib",
       output_path = file.path("."),
       output_file = "report.pdf",
       enterer_names=c("Enterer 1", "Enterer 2"))

