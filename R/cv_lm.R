#' Cross validation with a lm model
#'
#' @param df Dataframe containing the outcome and features for training model
#' @param family Model type used by glm
#' @param n number of cross validations to run
#' @param formula model formula used by glm
#' @param data_test test data set
#' @param outcome_var a string to indicate the outcome variable name
#' @examples 
#' iris_select=iris%>%filter(Species %in% c('setosa','versicolor'))
#' iris_select$Species=as.character(iris_select$Species)
#' library(caret)
#' train_index=createDataPartition(iris_select$Species,times = 1,p = 0.8)$Resample1
#' data_test=iris_select[-train_index,]
#' data_train=iris_select[train_index,]
#' cv_lm(data_train,'binomial',n=5,formula= 'Species~.',data_test=data_test,'Species')
#' @export

cv_lm=function(df,family,n,formula,data_test,outcome_var){
  # set a random seed for separate training data into cv groups
  old <- .Random.seed
  on.exit( { .Random.seed <<- old } )
  set.seed(2)
  

  # create bootstrap/cv groups

  df$bootstrap_group=ceiling(runif(nrow(df),0,1)*n)
  
  # determine which outcome to test for a classification problem
  outcome1=unique(data_test[,which(colnames(data_test)==outcome_var)])[1]
  df[,which(colnames(df)==outcome_var)]=df[,which(colnames(df)==outcome_var)]==outcome1
  data_test[,which(colnames(data_test)==outcome_var)]=data_test[,which(colnames(data_test)==outcome_var)]==outcome1
  for(i in 1:n){
    # each time, remove 1 group from training data
    
    df_model=df%>%filter(bootstrap_group!=i)%>%select(-bootstrap_group)
    # create the model
    lm_boot=glm(as.formula(formula),data =df_model ,family = family)
    if(i==1){
      plot(roc(data_test[,which(colnames(data_test)==outcome_var)], predict(lm_boot,data_test,type='response'), direction="<"),
           col="black", lwd=3, main="ROC for logistic regression",print.auc=F)
    }else{
      plot(roc(data_test[,which(colnames(data_test)==outcome_var)], predict(lm_boot,data_test,type='response'), direction="<"),
           col=i, lwd=3, main="ROC for logistic regression",print.auc=F,add=T)
    }
    
    
  }
}


