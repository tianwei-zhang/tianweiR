#' Normalize a continuous variable to a binary varibale based on median
#' @param data a dataframe with some numeric columns
#' @param col_to_change column index to change from nuermic to binary
#' @param comparator > or >= median
#' @return a dataframe with changed columns
#' @examples 
#' normalize_numeric_2_binary(iris,c(1,2),'>')
#' @export
normalize_numeric_2_binary=function(data,col_to_change,comparator){
  for (i in col_to_change){
    
    col_median=median(data[,i])
    print(paste(colnames(data)[i],col_median))
    if(identical(comparator,'>=')){
          data[,i]=as.numeric(data[,i]>=col_median)
    }else if(identical(comparator,'>')){
      data[,i]=as.numeric(data[,i]>col_median)
      
    }else{
      cat('Wrong comparator \n')
    }
    
  }
  
  return(data)
}