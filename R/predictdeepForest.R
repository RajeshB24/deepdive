


#' Predict Function for DeepForest
#'
#' @param object
#' @param newData
#'
#' @return
#' @export
#' @export predict.deepforest
#' @importFrom  data.table rbindlist
#' @import rpart
#' @importFrom treeClust rpart.predict.leaves
#' @examples
predict.deepforest<-function(object,
                             newData){

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

names(predDf)<-paste0('pred_',names(y))
  return(predDf)
}


