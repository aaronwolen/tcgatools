# Save barcode tables downloaded from TCGA 
library(dplyr)
options(stringsAsFactors = FALSE)

center.types <- c(BCR  = "Biospecimen Core Resource",
                  GCC  = "Genome Characterization Center",
                  CGCC = "Cancer Genomic Characterization Center",
                  GSC  = "Genome Sequencing Center",
                  GDAC = "Genome Data Analysis Center",
                  DCC  = "Data Coordinating Center",
                  COM  = "Commercial Center") # My best guess for COM

analytes <- c(D = "DNA",
              G = "WGA.genomeplex",
              H = "miRNA",
              R = "RNA",
              T = "totalRNA",
              W = "WGA.replig",
              X = "WGA.replig2")

tables <- dir("data-raw/barcode-tables", full.names = TRUE)
names(tables) <- sub(".csv", "", basename(tables))

tables <- lapply(tables, read.csv, na.strings = "", colClasses = "character")

logicals <- c("Yes" = TRUE, "No" = FALSE)
tables$dataType$Available     <- tables[tables$dataType$Available]
tables$platformCode$Available <- tables[tables$platformCode$Available]

lower_names <- function(x) structure(x, names = tolower(names(x)))
tables <- lapply(tables, lower_names)

.bc <- list(

  tss = rename(tables$tissueSourceSite,
               code = tss.code,
               tss = source.site, 
               disease = study.name),

  center = rename(tables$centerCode,
                  center.type.short = center.type,
                  center.name.short = short.name,
                  center.name.long = display.name) %>%
             mutate(center.type.long = center.types[center.type.short]) %>%
             select(-center.name) %>% # Remove center URL
             select(code, contains("name"), contains("type")),

  disease = rename(tables$diseaseStudy,
                 disease.short = study.abbreviation,
                 disease.long = study.name),
  
  analyte = rename(tables$portionAnalyte,
                   analyte.long = definition) %>%
            mutate(analyte.short = analytes[code])
)

save(.bc, file = "R/sysdata.rda")
