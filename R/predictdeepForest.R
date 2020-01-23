



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
#' @examples
predict.deepforest<-function(object,
                             newData){

  varCut=object[[1]]
  modelGroup=object[[2]]
  treeAugment=object[[3]]



  predlist<-  lapply(1:length(modelGroup)
                     , function(o){

                       if(treeAugment==F){
                       as.matrix(predict.deepnet(modelGroup[[o]],newData[,varCut[[o]]]))}else{

                         xFit<-newData[,varCut[[o]]]
                         AugmentedModel=modelGroup[[o]]

                         augmentPred<- predict.deeptree(AugmentedModel$modelGrpNet,
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


#average output : ignore Dead Networks
#weighted average : based denormalised Cost
#master learner

#ErrorCoverPercent : percent of error cover over best neuron.
#That is choose only  the best neuron or provide error cover to choose networks perfoming close to best. Higher error cover for better regularisation.

#Stoping rule on iteration : Stop on threshold ,stop on NA

#If both var percent and dat percent are 1 then fit tree only ones




#Boosted Model

