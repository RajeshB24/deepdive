
multiDataCut<-function(x,y,networkCount,cutDataSizePercent,cutVarSizePercent,seed){

  inputLen= nrow(x)


  cutSize=round(cutDataSizePercent*inputLen,0)

  cutIndex<-lapply(1:networkCount,function(s){
    set.seed(s)
    sample(1:inputLen,cutSize)

  })


  varCut<- multiVariableCut(x,cutVarSizePercent,networkCount,seed)

  xCut<-lapply(1:networkCount, function(nc){
    x[cutIndex[[nc]],varCut[[nc]]]

  })

  yCut<-lapply(cutIndex,function(idx){
    data.frame(y[idx,])
  })



  for(i in 1:networkCount){
    names(yCut[[i]])<-names(y)
  }



  set.seed(seed)
  return(list(xCut=xCut,
              yCut=yCut,
              varCut=varCut))

}
