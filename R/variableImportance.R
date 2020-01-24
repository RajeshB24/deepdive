#seed=2
#model<-modelnet
#Use shuffle distortion to rmse impact


shuffler<-function(inputdf,colname,model,modelError,seed){
  set.seed(seed)
  shuffleDf<-inputdf
  shuffleDf[,colname]<- sample(inputdf[,colname],nrow(inputdf))

  shufflePred<- predict(model,shuffleDf)

  if(model$modelType=="regress"){

    shuffleError<-sqrt(mean((as.matrix(y-shufflePred)^2)))

  }

  errorDiff<-shuffleError-modelError
  return(errorDiff)

}

varaiableImportance<-function(model,x,y,showPlot=T,seed=2){

  set.seed(seed)
inVars<-colnames(x)


modelPred<- predict(model,x)

if(model$modelType=="regress"){

modelError<-sqrt(mean((as.matrix(y-modelPred)^2)))

}

#variable Shuffle

errorDiff<-unlist(lapply(inVars, function(c){
    shuffler(x,c,model,modelError,seed)

  }))

ImportanceDf<-data.frame(variable=inVars,
                         errorDiff=errorDiff
                         )

ImportanceDf$RelativeImportance<-ImportanceDf$errorDiff/max(ImportanceDf$errorDiff)

ImportanceDf<-ImportanceDf[order(errorDiff,decreasing = T),]



if(showPlot==T){

plot_importance<-barplot(ImportanceDf$errorDiff,
        names.arg = ImportanceDf$variable
        )
}

return(ImportanceDf)

}



