removeMissingValuesFeatures <- function(data, cutOffPer = 30){
  missingValuesCounts <- colSums(is.na(data))
  missingValuesPer <- round(missingValuesCounts*100/nrow(data),0)
  missingValueFeatures <- names(missingValuesCounts)
  cols_to_drop <- missingValueFeatures[missingValuesPer > cutOffPer]
  data <- data[, !(colnames(data) %in% cols_to_drop)]
  return(data)
}