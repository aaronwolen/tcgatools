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
