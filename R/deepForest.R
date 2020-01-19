


#' Title
#'
#' @param x
#' @param y
#' @param networkCount
#' @param layerChoice
#' @param unitsChoice
#' @param cutVarSizePercent
#' @param cutDataSizePercent
#' @param activation
#' @param reluLeak
#' @param modelType
#' @param iterations
#' @param eta
#' @param seed
#' @param gradientClip
#' @param regularisePar
#' @param optimiser
#' @param parMomentum
#' @param inputSizeImpact
#' @param parRmsPropZeroAdjust
#' @param parRmsProp
#' @param treeLeaves
#' @param treeMinSplitPercent
#' @param treeMinSplitCount
#' @param treeCp
#' @param errorCover
#' @param treeAugment
#'
#' @return
#' @export
#'
#' @examples
deepforest<-function(x,y,
                     networkCount=3,
                     layerChoice=c(2:3),
                     unitsChoice=c(4:10),
                     cutVarSizePercent=0.6,
                     cutDataSizePercent=0.6,
                     activation = 'relu',
                     reluLeak=0,
                     modelType ='regress',
                     iterations = 500,
                     eta = 10 ^-2,
                     seed=2,
                     gradientClip=0.8,
                     regularisePar=0,
                     optimiser="adam",
                     parMomentum=0.9,
                     inputSizeImpact=1,
                     parRmsPropZeroAdjust=10^-8,
                     parRmsProp=0.9999,
                     treeLeaves=NA,
                     treeMinSplitPercent=0.3,
                     treeMinSplitCount=100,
                     treeCp=0.01 ,
                     errorCover=0.2,
                     treeAugment=T
){


  multiLayerList<-multiLayerIntialiser(layerChoice,
                                       unitsChoice,
                                       networkCount,seed)

  dataCut<-multiDataCut(x,y,networkCount,cutDataSizePercent,cutVarSizePercent,seed)

  xCut=dataCut$xCut
  yCut=dataCut$yCut
  varCut=dataCut$varCut



  multiSeed<-c(1:networkCount)


  modelForest<-lapply(1:networkCount, function(nc){

    if(treeAugment==F){

      idx<-which(sapply(1:ncol(xCut[[nc]]), function(m){
        length(unique(xCut[[nc]][,m]))
      })>1)


                 deepnet(xCut[[nc]][,idx],
                         yCut[[nc]],
                         hiddenLayerUnits=multiLayerList[[nc]],
                         activation ,
                         reluLeak,
                         modelType ,
                         iterations ,
                         eta ,
                         seed,
                         gradientClip,
                         regularisePar,
                         optimiser="adam",
                         parMomentum,
                         inputSizeImpact,
                         parRmsPropZeroAdjust,
                         parRmsProp)


      }else{

                 idx<-which(sapply(1:ncol(xCut[[nc]]), function(m){
                   length(unique(xCut[[nc]][,m]))
                 })>1)


                 modelGrpNet<-      deeptree(     xCut[[nc]][,idx],
                                         yCut[[nc]],
                                         hiddenLayerUnits=multiLayerList[[nc]],
                                         activation ,
                                         reluLeak,
                                         modelType,
                                         iterations,
                                         eta,
                                         seed,
                                         gradientClip,
                                         regularisePar,
                                         optimiser,
                                         parMomentum,
                                         inputSizeImpact,
                                         parRmsPropZeroAdjust,
                                         parRmsProp,
                                         treeLeaves,
                                         treeMinSplitPercent,
                                         treeMinSplitCount,
                                         treeCp ,
                                         stackPred=NA    )




                 list(modelGrpNet=modelGrpNet)
               } })





  if(treeAugment==F){

    fitPerf<-unlist(lapply(1:length(modelForest), function(mf){
      sqrt(mean((modelForest[[mf]]$fitted$pred_y-yCut[[mf]]$y)^2))/mean(abs(yCut[[mf]]$y))
    }))
    }else{

      fitPerf<-unlist(lapply(1:length(modelForest), function(mf){

        AugmentedModel<-modelForest[[mf]]






        xFit<-xCut[[mf]][,varCut[[mf]]]
        row.names(xFit)<-1:nrow(xFit)
        yFit<-yCut[[mf]]
        row.names(yFit)<-1:nrow(yFit)


        augmentPred<- predict.deeptree(AugmentedModel$modelGrpNet,
                                       newData=xFit)


        sqrt(mean((augmentPred$ypred-yFit$y)^2))/mean(abs(yFit$y))
      }))

    }




  bestFit<- min(fitPerf,na.rm = T)

  errorCoverFit=bestFit+errorCover
  chosenIndex<-which(!is.na(fitPerf)& fitPerf<=errorCoverFit)
  chosenModels<- modelForest[chosenIndex]

  deepforestmod<-list(varCut[chosenIndex],chosenModels,treeAugment)
  class(deepforestmod)<-'deepforest'

  return(deepforestmod)
}
