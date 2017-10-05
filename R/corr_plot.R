#' Create a level plot to show correlations
#' 
#' @param data Data frame object
#' @param increment increment for the legend label
#' @param size size of the axis labels
#' @return the level plot
#' @export
#' @examples
#' corr_plot(iris,0.1,1)


corr_plot=function(data,increment,size,title=""){
  library(lattice)
  temp=cor(data.matrix(data))
  p=levelplot(temp,at=seq(-1,1,increment),scales=list(x=list(rot=90),cex=size),xlab="",ylab="",main=title )
  p
  return(p)
}

