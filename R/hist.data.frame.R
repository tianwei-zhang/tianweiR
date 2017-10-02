### need to test and debug later

hist.data.frame=function(data,n){

  data=as.data.frame(data)
  num_col_index=seq(1:ncol(data))[sapply(data,is.numeric)]
  plot_row=ceiling(length(num_col_index)/1)
  # par(mfrow=c(plot_row,1))
  for(i in num_col_index){
    hist(data[,i],n,main = paste('Distribution of',colnames(data)[i]),xlab=colnames(data)[i])
    # print(i)
  }
  
}