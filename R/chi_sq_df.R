#' Apply chi-square test for any combination of two columns in a data set
#' @param data A dataframe whose columns will be tested for chi-square indepedence. 
#' @return a matrix with the p-values of the chi-square test. Each column or row represents a variable in the input dataframe
#' @examples 
#' chi_sq_test_df(iris)
#' @export

chi_sq_test_df=function(data){
  list_var=colnames(data)
  output=matrix(ncol = length(list_var),nrow = length(list_var))
  colnames(output)=list_var
  rownames(output)=list_var
  for(i in 1:length(list_var)){
    for(j in 1:length(list_var)){
     var_x=list_var[i]
     var_y=list_var[j]
     value_x=data%>%select_(paste0('`',var_x,'`'))
     value_x=as.matrix(value_x)[,1]
     value_y=data%>%select_(paste0('`',var_y,'`'))
     value_y=as.matrix(value_y)[,1]
     
     value_x=as.factor(value_x)
     value_y=as.factor(value_y)
     test_result=chisq.test(value_x,value_y)
      output[i,j]=test_result$p.value   
    }
  }

  return(output)
}
