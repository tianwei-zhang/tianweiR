#' Cross validation with a classification model built by caret
#' @param df Dataframe containing the outcome and features to train model
#' @param method Model type used by caret
#' @param n number of cross validations to run
#' @param formula a string to represent model formula used by caret
#' @param data_test test data set
#' @param outcome_var a string to indicate the outcome variable name
#' @examples 
#' iris_select=iris%>%filter(Species %in% c('setosa','versicolor'))
#' iris_select$Species=as.character(iris_select$Species)
#' library(caret)
#' train_index=createDataPartition(iris_select$Species,times = 1,p = 0.8)$Resample1
#' data_test=iris_select[-train_index,]
#' data_train=iris_select[train_index,]
#' cv_caret(data_train,'rf',n=5,formula= 'Species~.',data_test=data_test,'Species')
#' @export

cv_caret=function(df,method,n,formula,data_test,outcome_var){
  # set a random seed for separate training data into cv groups
  old <- .Random.seed
  on.exit( { .Random.seed <<- old } )
  set.seed(2)
  
  # create bootstrap/cv groups
  df$bootstrap_group=ceiling(runif(nrow(df),0,1)*n)
  
  # determine which outcome to test for a classification problem
  outcome1=unique(data_test[,which(colnames(data_test)==outcome_var)])[1]
  # cat(paste('Predicting if a data point is going to be',outcome1))
  for(i in 1:n){
    
    # each time, remove 1 group from training data
    df_model=df%>%filter(bootstrap_group!=i)%>%select(-bootstrap_group)
    # create the model
    lm_boot=train(
      as.formula(formula),
      data = df_model,
      metric='Accuracy',
      method=method
      
    )
    
    # plot the ROC curve
    if(i==1){
      
      plot(roc(data_test[,which(colnames(data_test)==outcome_var)]==outcome1, unlist(predict(lm_boot,data_test,type='prob')[outcome1]), direction="<"),
           col="black", lwd=3, main=paste0("ROC for ",method),print.auc=F)
    }else{
      plot(roc(data_test[,which(colnames(data_test)==outcome_var)]==outcome1, unlist(predict(lm_boot,data_test,type='prob')[outcome1]), direction="<"),
           col=i, lwd=3, main=paste0("ROC for ",method),print.auc=F,add=T)
    }
    
    
  }
  
}