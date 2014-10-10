#' Parse TCGA barcodes
#' 
#' Extract and expand information embedded in TCGA sample barcodes. Details
#' about the barcode format is provided
#' \href{https://wiki.nci.nih.gov/display/TCGA/TCGA+Barcode}{here}.
#' 
#' @export
#' @param x character vector of barcodes
#' @param verbose report barcode type match similarities
#' 
#' @examples
#' barcodes <- c('TCGA-10-7321-11A-01R-2263-07', 'TCGA-Q5-7321-11A-01D-2263-32')
#' parse_barcodes(barcodes, annotate = TRUE)   

parse_barcodes <- function(x, annotate = FALSE, labels = "long", verbose = FALSE) {
  
  nparts <- count_barcode_parts(x)
  types <- .type[sapply(.type, length) == nparts]
  
  bparts <- data.frame(str_split_fixed(x, "-", nparts), stringsAsFactors = FALSE)
  
  type.hits <- lapply(types, Map, f = str_detect, string = bparts)
  type.hits <- lapply(type.hits, data.frame)
  type.hits <- lapply(type.hits, apply, 2, mean)
  type.hits <- do.call("rbind", type.hits)
  if (verbose) cat(type.hits)
  type.hits <- rowSums(type.hits)
  
  if (all(type.hits != nparts))
    stop("No matching barcode type was found.", call. = FALSE)
  
  if (sum(type.hits == nparts) > 1)
    stop("Barcodes matched multiple barcode types.", call. = FALSE)
  
  names(bparts) <- names(.type[[names(type.hits)[type.hits == nparts]]])
  
  # extract vial and analyte
  if ("sample" %in% names(bparts)) 
    bparts <- extract_split(bparts, "sample", "vial")
  if ("portion" %in% names(bparts)) 
    bparts <- extract_split(bparts, "portion", "analyte")

  if (!annotate) return(bparts)

  # tissue source site
  bparts <- bc_annotate(bparts, .bc$tss, "tss", "code", labels)
  
  # disease
  if ("disease" %in% names(bparts) & labels == "short")
    bparts <- bc_annotate(bparts, .bc$disease, "disease", "disease.long", labels)
  
  # analyte
  if ("analyte" %in% names(bparts))
    bparts <- bc_annotate(bparts, .bc$analyte, "analyte", "code", labels)

  # center
  if ("center" %in% names(bparts))
    bparts <- bc_annotate(bparts, .bc$center, "center", "code", labels)

  return(bparts)
}
