

#' Predict Function for DeepForest
#'
#' @param object deepforest model object
#' @param newData pass dataframe for prediction
#' @param ... further arguments passed to or from other methods.
#' @return returns predictions vector or dataframe
#' @export
#' @export predict.deepforest
#' @importFrom  data.table rbindlist
#' @import rpart
#' @importFrom treeClust rpart.predict.leaves
#' @importFrom graphics barplot
#' @importFrom stats formula predict runif

predict.deepforest<-function(object,
                             newData,...){


  if (!inherits(object, "deepforest")) stop("Not a legitimate \"deepforest\" object")

  varCut=object$varcut
  modelGroup=object$chosenModels
  treeAugment=object$treeAugment

  o<-1
  predlist<-  lapply(1:length(modelGroup)
                     , function(o){

                       if(treeAugment==F){


                         xFit<-data.frame(newData[,varCut[[o]]])
                         names(xFit)<-varCut[[o]]


                       deepnetPred<-predict.deepnet(modelGroup[[o]],xFit)




                       deepnetPred<-deepnetPred[,names(deepnetPred)!="ypred"]
                       for( i in names(deepnetPred)){

                         deepnetPred[,i]   <-as.numeric(deepnetPred[,i] )

                       }
                    deepnetPred

                         }else{

                         xFit<-data.frame(newData[,varCut[[o]]])
                         names(xFit)<-varCut[[o]]

                         augmentPred<- predict.deeptree(modelGroup[[o]],
                           newData =xFit)

                         if(object$modelType=="multiClass"){
                         augmentPred<-augmentPred[,names(augmentPred)!="ypred"]


                        for( i in names(augmentPred)){

                          augmentPred[,i]   <-as.numeric(augmentPred[,i] )

                        }}
                       augmentPred


                       }

                     })


  networkCount<-length(modelGroup)

predarray<-array(unlist(predlist),c(nrow(newData),ncol(predlist[[1]]),networkCount))

predDf<-data.frame(apply(predarray, 1:2, mean))
names(predDf)<-colnames(predlist[[1]])


if(object$modelType=="multiClass"){
  predDf$ypred<-stringr::str_remove_all( names(predDf),"y_")[max.col(predDf)]
  names(predDf)=stringr::str_remove_all(names(predDf),"y_")
}



  return(predDf)
}


