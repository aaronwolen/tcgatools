# Save barcode tables downloaded from TCGA 

tables <- dir("data-raw/barcode-tables", full.names = TRUE)
names(tables) <- sub(".csv", "", basename(tables))

.barcodes <- lapply(tables, read.csv, na.strings = "", colClasses = "character")

logicals <- c("Yes" = TRUE, "No" = FALSE)
.barcodes$dataType$Available     <- .barcodes[.barcodes$dataType$Available]
.barcodes$platformCode$Available <- .barcodes[.barcodes$platformCode$Available]

save(.barcodes, file = "R/sysdata.rda")
