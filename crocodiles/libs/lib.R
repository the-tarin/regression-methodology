split_ranges_automatically <- function(df) {
  for (col in colnames(df)) {
    if (any(grepl("–", df[[col]]))) {
      df[[col]] <- as.character(df[[col]])
      split_vals <- strsplit(df[[col]], split = "–")
      low_val <- sapply(split_vals, function(x) as.numeric(x[1]))
      high_val <- sapply(split_vals, function(x) ifelse(length(x) > 1, as.numeric(x[2]), as.numeric(x[1])))
      df[[paste0(col, "_low")]] <- low_val
      df[[paste0(col, "_high")]] <- high_val
    }
  }
  
  df <- df %>%
    select(-matches("[-]"))
  
  return(df)
}