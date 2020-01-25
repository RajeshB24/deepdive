

#' Predict Function for DeepForest
#'
#' @param object deepforest model object
#' @param newData pass dataframe for prediction
#' @param ... further arguments passed to or from other methods.
#' @return
#' @export
#' @export predict.deepforest
#' @importFrom  data.table rbindlist
#' @import rpart
#' @importFrom treeClust rpart.predict.leaves
#' @importFrom graphics barplot
#' @importFrom stats formula predict runif
#' @examples
#'
#'
predict.deepforest<-function(object,
                             newData,...){


  if (!inherits(object, "deepforest")) stop("Not a legitimate \"deepforest\" object")

  varCut=object$varcut
  modelGroup=object$chosenModels
  treeAugment=object$treeAugment



  predlist<-  lapply(1:length(modelGroup)
                     , function(o){

                       if(treeAugment==F){
                       as.matrix(predict.deepnet(modelGroup[[o]],newData[,varCut[[o]]]))}else{

                         xFit<-newData[,varCut[[o]]]


                         augmentPred<- predict.deeptree(modelGroup[[o]],
                           newData =xFit)

                        as.matrix(augmentPred)

                       }

                     })


  networkCount<-length(modelGroup)

predarray<-array(unlist(predlist),c(nrow(newData),ncol(predlist[[1]]),networkCount))

predDf<-data.frame(apply(predarray, 1:2, mean))
names(predDf)<-colnames(predlist[[1]])

  return(predDf)
}


