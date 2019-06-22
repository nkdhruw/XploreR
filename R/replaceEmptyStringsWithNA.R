replaceEmptyStringsWithNA <- function(data){
  data[data == ''] <- NA
  return(data)
}