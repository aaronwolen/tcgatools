#' Parse TCGA barcodes
#' 
#' Extract and expand information embedded in TCGA sample barcodes. Details
#' about the barcode format is provided
#' \href{https://wiki.nci.nih.gov/display/TCGA/TCGA+Barcode}{here}.
#' 
#' @export
#' @param x character vector of barcodes
#' @param annotations character string indicating whether barcodes should be
#'   only be parsed (\code{annotations = "none"}), or annotated using either
#'   \code{"short"} or \code{"long"} values
#' 
#' @examples
#' barcodes <- c('TCGA-10-7321-11A-01R-2263-07', 'TCGA-Q5-7321-01A-01D-2263-32')
#' parse_barcodes(barcodes, annotations = "long")

parse_barcodes <- function(x, annotations = "none") {
  
  stopifnot(length(x) > 0 & is.character(x))
  annotations <- match.arg(tolower(annotations), c("none", "short", "long"))
  
  nparts <- count_barcode_parts(x)
  types <- .type[sapply(.type, length) == nparts]
  bparts <- data.frame(str_split_fixed(x, "-", nparts), stringsAsFactors = FALSE)
  
  # identify type
  type.hits <- lapply(types, Map, f = str_detect, string = bparts)
  type.hits <- lapply(type.hits, data.frame)
  type.hits <- lapply(type.hits, apply, 2, mean)
  type.hits <- do.call("rbind", type.hits)

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

  if (annotations == "none") return(bparts)

  # tissue source site
  bparts <- bc_annotate(bparts, .bc$tss, "tss", "code", annotations)
  
  # disease
  if ("disease" %in% names(bparts) & annotations == "short")
    bparts <- bc_annotate(bparts, .bc$disease, "disease", "disease.long", annotations)
  
  # sample type
  if ("sample" %in% names(bparts))
    bparts <- bc_annotate(bparts, .bc$sample, "sample", "code", annotations)

  # analyte
  if ("analyte" %in% names(bparts))
    bparts <- bc_annotate(bparts, .bc$analyte, "analyte", "code", annotations)

  # center
  if ("center" %in% names(bparts))
    bparts <- bc_annotate(bparts, .bc$center, "center", "code", annotations)

  return(bparts)
}
