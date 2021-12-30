# Attachments ----
to_install <- c("chronosphere", "countrycode", "plyr", "rgeos", "rworldmap", "sp", "spdep", "ggrepel", "ggthemes", "igraph", "patchwork", "RColorBrewer", "tidyverse", "zoo", "ggplot2", "viridis", "piecewiseSEM", "divDyn", "rbibutils", "xfun")
for (i in to_install) {
	message(paste("looking for ", i))
	if (!requireNamespace(i)) {
		message(paste("     installing", i))
		install.packages(i)
	}
}