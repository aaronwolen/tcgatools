# Count number of parts in a vector of barcodes
count_barcode_parts <- function(x) {
  n <- str_count(x, "-") + 1
  if (!all(diff(n) == 0))
    stop("Barcodes must have the same number of components", call. = FALSE)
  return(n[1])
}



# Extract data matching a regex pattern and split into a separate column
extract_split <- function(df, col, into, pattern = "[[:alpha:]]$") {
  
  stopifnot(length(col) == 1)
  j <- which(names(df) == col)
  if (!all(str_detect(df[[col]], pattern))) return(df)
  
  pieces <- list(str_replace(df[[col]], pattern, ""),
                 str_extract(df[[col]], pattern))

  names(pieces) <- c(col, into)
  data.frame(append(df[-j], pieces, after = j - 1), stringsAsFactors = FALSE)
}
