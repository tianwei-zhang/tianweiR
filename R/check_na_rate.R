#' Check the NA value rate for each column in a dataframe
#' @param x a data frame
#' @return a data frame with variable name and the NA rate

check_na_rate=function(x){
  na_rate=scales::percent(colSums(is.na(x))/nrow(x))
  names(na_rate)=colnames(x)
  return(as.data.frame(na_rate))
}