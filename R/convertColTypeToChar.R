convertColTypeToChar <- function(data, col){
  data[, col] <- as.character(data[, col])
  return (data)
}