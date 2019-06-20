featureBoxplot <- function(data, feature, splitBy = NULL){
  errorMessage <- NULL 
  featureClass <- class(data[, feature])
  print(featureClass)
  if((featureClass != 'numeric') & (featureClass != 'integer')){
    errorMessage <- 'Column should have numeric data type'
    return(list(errorMessage = errorMessage))
  }
  if(is.null(splitBy)){
    y <- data[, feature]
    p <- plot_ly(y = y, type = 'box') 
    return(list(errorMessage = errorMessage, p = p))
  } else {
    splitByFactors <- unique(data[, splitBy])
    if(length(splitByFactors) > 50){
      errorMessage <- 'No. of factors in the splitBy column should be less than 50'
      return(list(errorMessage = errorMessage))
    }else{
      p <- plot_ly(y = data[,feature], color = data[, splitBy], type='box') %>%
        layout(xaxis = list(title = feature),
               yaxis = list (title = "Range"))
      return(list(errorMessage = errorMessage, p = p))
    }
  }
  
}