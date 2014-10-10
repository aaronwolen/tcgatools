# tcgatools

A collection of small tools for working with data from [The Cancer Genome Atlas](http://cancergenome.nih.gov/).

## Installation

```r
install.packages("devtools")
devtools::install_github("aaronwolen/tcgatools")
```

## Features

Currently, the only useful functionality provided is the ability to parse and annotate TCGA barcodes.

```r
barcodes <- c("TCGA-G9-6361-01A-21R-1965-07",
              "TCGA-HC-7740-11A-01R-2118-07")
```

Parse: 
```r
parse_barcodes(barcodes)

##   project tss participant sample vial portion analyte plate center
##      TCGA  G9        6361     01    A      21       R  1965     07
##      TCGA  HC        7740     11    A      01       R  2118     07
```

Annotate: 

```r
parse_barcodes(barcodes, annotations = "short")
```

|project |tss                               |disease |bcr |participant |sample |vial |portion |analyte |plate |center |center.type |
|:-------|:---------------------------------|:-------|:---|:-----------|:------|:----|:-------|:-------|:-----|:------|:-----------|
|TCGA    |Roswell Park                      |PRAD    |IGC |6361        |01     |A    |21      |RNA     |1965  |UNC    |CGCC        |
|TCGA    |International Genomics Consortium |PRAD    |IGC |7740        |11     |A    |01      |RNA     |2118  |UNC    |CGCC        |
