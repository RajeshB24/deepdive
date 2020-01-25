




#' Random Variable cuts for deepforest
#'
#' @param x
#' @param cutVarSizePercent
#' @param networkCount
#' @param seed
#'
#' @return
#' @noRd
#'
#' @examples
multiVariableCut<-function(x,cutVarSizePercent,networkCount,seed){
  varName=names(x)

  cutSize=round(cutVarSizePercent*length(varName),0)

  varCut<-lapply(1:networkCount, function(s){set.seed(s*seed)
    sample(varName,cutSize)})

  return(varCut)
}


