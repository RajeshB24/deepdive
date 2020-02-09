

#' @title Variable importance for models in this library
#'
#' @param model Model object
#' @param x    a data frame with input variables
#' @param y    a data frame with ouptut variable
#' @param showPlot logical. True will show importance plot. Default True
#' @param seed Set seed with this parameter. Incase of sin activation sometimes changing seed can yeild better results. Default is 2
#'
#' @return returns variable importance data frame
#' @export
#' @importFrom  data.table rbindlist
#' @import rpart
#' @importFrom treeClust rpart.predict.leaves

variableImportance<-function(model,x,y,showPlot=T,seed=2){

  set.seed(seed)
inVars<-colnames(x)


if(model$modelType=="multiClass"){

  y<-dummy_cols(y)[-1]
  names(y)<-stringr::str_remove(names(y),"y_")
}

modelPred<- predict(model,x)

if(model$modelType=="multiClass"){

names(modelPred)<-stringr::str_remove(names(modelPred),"pred_")


modelPred<-modelPred[,!names(modelPred)%in%"ypred"]
for(i in 1:ncol(modelPred)){
  modelPred[,i]<-as.numeric(modelPred[,i])
}

}
modelError<-sqrt(mean((as.matrix(y-modelPred)^2)))



#variable Shuffle

errorDiff<-unlist(lapply(inVars, function(c){
    shuffler(x,c,y,model,modelError,seed)

  }))

ImportanceDf<-data.frame(variable=inVars,
                         errorDiff=errorDiff
                         )

ImportanceDf$Importance<-ImportanceDf$errorDiff/max(ImportanceDf$errorDiff)

ImportanceDf<-ImportanceDf[order(errorDiff,decreasing = T),]



if(showPlot==T){

plot_importance<-barplot(ImportanceDf$errorDiff,
        names.arg = ImportanceDf$variable
        )
}

return(ImportanceDf)

}




