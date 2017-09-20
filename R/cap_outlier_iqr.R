#' Cap outlier by IQR method
#'
#' @param data Dataframe to cap outliers
#' @param cols_index Which columns to check for outliers
#' @param iqr_threshold Number of IQRs to define outliers. Default=1.5
#' @param zero_min Use 0 as the minimum value. Default is false 
#' @return data with outliers capped
#' @examples 
#' cap_outlier_IQR(data,c(1,3,4),iqr_threshold=2,zero_min=T)
#' @export
cap_outlier_iqr=function(data,cols_index,iqr_threshold=1.5,zero_min=F){
  for (i in cols_index){
    
    # compute 1st, 3rd quartiles and IQR for a column
    col_3rd_quartile=quantile(data[,i],probs = 0.75)
    col_1st_quartile=quantile(data[,i],probs = 0.25)
    col_iqr=IQR(data[,i])
    
    # compute upper and lower bounds for outliers
    upper_bound=col_3rd_quartile+iqr_threshold*col_iqr
    if(zero_min){
      lower_bound=0
    }else{
      lower_bound=col_1st_quartile-iqr_threshold*col_iqr
    }
    
    # compute row index of outliers
    index_greater_upper=data[,i]>=upper_bound
    index_lower_lower=data[,i]<=outlier_min
    
    # cap outliers
    data[index_greater_upper,i]=upper_bound
    data[index_lower_lower,i]=lower_bound
  }
  
  return(data)
}