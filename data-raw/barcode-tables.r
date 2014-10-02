# Download barcode tables from TCGA

library(RCurl)
library(XML)

.barcodes <- getURL("https://tcga-data.nci.nih.gov/datareports/codeTablesReport.htm")
.barcodes <- readHTMLTable(.barcodes, stringsAsFactors = FALSE)

fix_colnames <- function(x) structure(x, names = make.names(names(x)))
.barcodes <- lapply(.barcodes, fix_colnames)

logicals <- c("Yes" = TRUE, "No" = FALSE)
.barcodes$dataType$Available     <- .barcodes[.barcodes$dataType$Available]
.barcodes$platformCode$Available <- .barcodes[.barcodes$platformCode$Available]

save(.barcodes, file = "R/sysdata.rda")
