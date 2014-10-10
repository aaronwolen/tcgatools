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
  data.frame(append(df[-j], pieces, after = j - 1), 
             stringsAsFactors = FALSE, row.names = NULL)
}



# Perform an inner-join between df.x and df.y
# - insert that matching variables from df.y into df.x starting at the
#   position of the by.x variable, which is removed
# - remove by.y if it equals "code"
# - remove by.x columns with ".short" if labels = "long" (and vice versa)
bc_annotate <- function(df.x, df.y, by.x, by.y, labels) {
  
  i <- match(by.x, names(df.x))
  
  match <- df.y[match(df.x[[by.x]], df.y[[by.y]]), ]
  if (by.y == "code") match <- subset(match, select = -code)
  
  rm <- switch(labels, short = "long", long = "short")
  match <- match[which(!grepl(rm, names(match)))]
  names(match) <- str_replace(names(match), "(.name)?.(short|long)", "")
  
  data.frame(append(df.x[-i], match, i - 1), stringsAsFactors = FALSE) 
}
