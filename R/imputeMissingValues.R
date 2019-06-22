imputeMissingValues <- function(data, method = NULL){
  columns <- colnames(data)
  numeric_columns <- columns[(sapply(data, class) == 'numeric') | (sapply(data, class) == 'integer')]
  character_columns <- columns[sapply(data, class) == 'character']
  missingValuesCounts <- colSums(is.na(data))
  missingValueFeatures <- names(missingValuesCounts)
  for(feature in missingValueFeatures){
    if(feature %in% numeric_columns){
      missing_value_rows <- is.na(data[, feature])
      data[missing_value_rows, feature] <- mean(data[!missing_value_rows, feature], na.rm = TRUE)
    }
    if(feature %in% character_columns){
      missing_value_rows <- is.na(data[, feature])
      data[missing_value_rows, feature] <- getColMode(data[, feature])
    }
  }
  return (data)
}