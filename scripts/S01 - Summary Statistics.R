
# Load data ---------------------------------------------------------------

load(file.path("data", "refs.RData"))
unknowns <- read.csv(file.path("data", "unknown.csv"))

# Number of publications
length(unique(completed_refs$reference_no))
length(unique(completed_refs$reference_no)) - 11037
length(unique(completed_refs$reference_no)) + length(unique(unknowns$reference_no))

# Reference details -------------------------------------------------------
all_refs <- subset(all_refs, reference_no %in% completed_refs$reference_no)

single <- all_refs[!is.na(all_refs$author2last),]
nrow(single) #single author pubs

#number of countries per reference 
ncount <- completed_refs[completed_refs$reference_no %in% single$reference_no,]
ncount <- unique(ncount)
ncount <- table(ncount$reference_no)
length(ncount[ncount > 1])

table(ncount)

