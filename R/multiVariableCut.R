




multiVariableCut<-function(x,cutVarSizePercent,networkCount,seed){
  varName=names(x)

  cutSize=round(cutVarSizePercent*length(varName),0)

  varCut<-lapply(1:networkCount, function(s){set.seed(s)
    sample(varName,cutSize)})

  return(varCut)
}


