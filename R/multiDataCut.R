
#' Random Data Cut for deepforest
#'
#' @param x
#' @param y
#' @param networkCount
#' @param cutDataSizePercent
#' @param cutVarSizePercent
#' @param seed
#'
#' @return
#' @noRd
#'
#' @examples
multiDataCut<-function(x,y,networkCount,cutDataSizePercent,cutVarSizePercent,seed){

  inputLen= nrow(x)


  cutSize=round(cutDataSizePercent*inputLen,0)

  cutIndex<-lapply(1:networkCount,function(s){
    set.seed(s*seed)

    sample(1:inputLen,cutSize)

  })


  varCut<- multiVariableCut(x,cutVarSizePercent,networkCount,seed)

  xCut<-lapply(1:networkCount, function(nc){
   xcut<- data.frame(x[cutIndex[[nc]],varCut[[nc]]])
   names(xcut)<-varCut[[nc]]
   xcut

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

