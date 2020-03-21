


#' @title Build or train bagged deeptree or deepnet of multiple architecture
#' @description Build or train bagged deeptree or deepnet of multiple architecture.Based on error choice either select best model or average multiple model with random variable cut,data cut and architechture
#' @param x a data frame with input variables
#' @param y a data frame with ouptut variable
#' @param networkCount Integer, Number of deepnet or deeptree to build
#' @param layerChoice  vector, different layer choices
#' @param unitsChoice  vector , number of units choice
#' @param cutVarSizePercent ratio, percentage of variable to for each network
#' @param cutDataSizePercent ratio, percentage of data to for each network
#' @param activation choose from "sigmoid","relu","sin","cos","none".Activations will be randomly chosen from chosen. Default is relu and sin
#' @param reluLeak  numeric. Applicable when activation is "relu". Specify value between 0 any number close to zero below 1. Eg: 0.01,0.001 etc
#' @param modelType  one of "regress","binary","multiClass". "regress" for regression will create a linear single unit output layer. "binary" will create a single unit sigmoid activated layer. "multiClass" will create layer with units corresponding to number of output classes with softmax activation.
#' @param iterations integer. This indicates number of iteratios or epochs in backpropagtion .The default value is 500.
#' @param eta numeric.Hyperparameter,sets the Learning rate for backpropagation. Eta determines the convergence ability and speed of convergence.
#' @param seed  numeric. Set seed with this parameter. Incase of sin activation sometimes changing seed can yeild better results. Default is 2
#' @param gradientClip numeric. Hyperparameter numeric value which limits gradient size for weight update operation in backpropagation. Default is 0.8 . It can take any postive value.
#' @param regularisePar  numeric. L2 Regularisation Parameter .
#' @param optimiser one of "gradientDescent","momentum","rmsProp","adam". Default value "adam"
#' @param parMomentum numeric. Applicable for optimiser "mometum" and "adam"
#' @param inputSizeImpact numeric. Adjusts the gradient size by factor of percentage of rows in input. For very small data set setting this to 0 could yeild faster result. Default is 1.
#' @param parRmsPropZeroAdjust  numeric. Applicable for optimiser "rmsProp" and "adam"
#' @param parRmsProp  numeric.Applicable for optimiser "rmsProp" and "adam"
#' @param treeLeaves vector.Optional , leaves numbers from externally trained tree model can be supplied here. If supplied then model will not build a explicit tree and just fit a neural network to mentioned leaves.
#' @param treeMinSplitPercent numeric. This parameter controls depth of tree setting min split count for leaf subdivision as percentage of observations. Final minimum split will be chosen as max of count calculted with treeMinSplitPercent and treeMinSplitCount. Default 0.3. Range 0 to 1.
#' @param treeMinSplitCount numeric. This parameter controls depth of tree setting min split count.Final minimum split will be chosen as max of count calculted with treeMinSplitPercent and treeMinSplitCount. Default 30
#' @param treeCp complexity parameter. \code{\link{rpart.control}}
#' @param errorCover Ratio. Deault is 0.2 i.e all models within 20 percent error of best model will be selected.
#' @param treeAugment logical. If True fits deeptree and if False fits deepnet. Default is T
#' @param printItrSize numeric. Number of iterations after which progress message should be shown. Default value 100 and for iterations below 100 atleast 5 messages will be seen
#' @param showProgress logical. True will show progress and F will not show progress
#' @param stopError Numeric. Rmse at which iterations can be stopped. Default is 0.01, can be set as NA in case all iterations needs to run.
#' @param miniBatchSize integer. Set the mini batch size for mini batch gradient
#' @param useBatchProgress logical. Applicable for miniBatch , setting T will use show rmse in Batch and F will show error on full dataset. For large dataset set T
#' @return returns model object which can be passed into \code{\link{predict.deepforest}}
#' @export
#' @import rpart
#' @import data.table
#' @importFrom stringr str_remove_all
#' @importFrom treeClust rpart.predict.leaves
#' @importFrom graphics barplot
#' @importFrom stats formula predict runif
#' @examples
#'
#' require(deepdive)
#'
#'x<-data.frame(x1=runif(10),x2=runif(10))
#'y<-data.frame(y=10*x$x1+20*x$x2+20)
#'
#'mdeepf<-deepforest(x,y,
#'                   networkCount=2,
#'                   layerChoice=c(2:3),
#'                   unitsChoice=c(4:10),
#'                   cutVarSizePercent=0.6,
#'                   cutDataSizePercent=0.6,
#'                   activation = c('relu',"sin"),
#'                   reluLeak=0.01,
#'                   modelType ='regress',
#'                   iterations = 10,
#'                   eta = 10 ^-2,
#'                   seed=2,
#'                   gradientClip=0.8,
#'                   regularisePar=0,
#'                   optimiser="adam",
#'                   parMomentum=0.9,
#'                   inputSizeImpact=1,
#'                   parRmsPropZeroAdjust=10^-8,
#'                   parRmsProp=0.9999,
#'                   treeLeaves=NA,
#'                   treeMinSplitPercent=0.3,
#'                   treeMinSplitCount=100,
#'                   treeCp=0.01 ,
#'                   errorCover=0.2,
#'                   treeAugment=TRUE,
#'                   printItrSize=100,
#'                   showProgress=TRUE,
#'                   stopError=0.01,
#'                   miniBatchSize=64,
#'                   useBatchProgress=TRUE)


deepforest<-function(x,y,
                     networkCount=3,
                     layerChoice=c(2:3),
                     unitsChoice=c(4:10),
                     cutVarSizePercent=0.6,
                     cutDataSizePercent=0.6,
                     activation = c('sigmoid',"sigmoid"),
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
                     treeAugment=TRUE,
                     printItrSize=100,
                     showProgress=TRUE,
                     stopError=0.01,
                     miniBatchSize=NA,
                     useBatchProgress=TRUE
){



  multiLayerList<-multiLayerIntialiser(layerChoice,
                                       unitsChoice,
                                       networkCount,seed)


  activationList<- multiActivationList(multiLayerList,activation,seed)

  dataCut<-multiDataCut(x,y,networkCount,cutDataSizePercent,cutVarSizePercent,seed)

  xCut=dataCut$xCut
  yCut=dataCut$yCut
  varCut=dataCut$varCut


  multiSeed<-seed*c(1:networkCount)


  modelForest<-lapply(1:networkCount, function(nc){

    if(treeAugment==F){

      idx<-which(sapply(1:ncol(xCut[[nc]]), function(m){
        length(unique(xCut[[nc]][,m]))
      })>1)

      if(length(idx)>1){
        xSelect<-xCut[[nc]][,idx]
      }else{
        xSelect<-data.frame(xCut[[nc]][,idx])
        names(xSelect)<-names(xCut[[nc]])[idx]
      }



                 deepnet(xSelect,
                         data.frame( yCut[[nc]]),
                         hiddenLayerUnits=multiLayerList[[nc]],
                         activationList[[nc]] ,
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
                         parRmsProp,
                         printItrSize,
                         showProgress,
                         stopError,
                         miniBatchSize,
                         useBatchProgress,
                         ignoreNAerror=T)


      }else{

                 idx<-which(sapply(1:ncol(xCut[[nc]]), function(m){
                   length(unique(xCut[[nc]][,m]))
                 })>1)


                 if(length(idx)>1){
                  xSelect<-xCut[[nc]][,idx]
                 }else{
                  xSelect<-data.frame(xCut[[nc]][,idx])
                  names(xSelect)<-names(xCut[[nc]])[idx]
                 }

                  deeptree(    xSelect,
                                        data.frame( yCut[[nc]]),
                                         hiddenLayerUnits=multiLayerList[[nc]],
                                         activationList[[nc]] ,
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
                                         stackPred=NA ,
                                         printItrSize,
                                         showProgress,
                                         stopError,
                                         miniBatchSize,
                                         useBatchProgress,
                                         ignoreNAerror=T)


               } })


      fitPerf<-unlist(lapply(1:length(modelForest), function(mf){

        fitModel<-modelForest[[mf]]


        xFit<-data.frame(xCut[[mf]][,varCut[[mf]]])
        names(xFit)<-varCut[[mf]]

        row.names(xFit)<-1:nrow(xFit)
        yFit<-yCut[[mf]]
        row.names(yFit)<-1:nrow(yFit)

        if(treeAugment==T){

        augmentPred<- predict.deeptree(fitModel,
                                       newData=xFit)


       if(modelType=="multiClass"){

            multiClassPred<-augmentPred[,names(augmentPred)!="ypred"]
            yClass<-dummy_cols(yFit)
            yClass<-yClass[,names(yClass)!="y"]
            names(yClass)<-str_remove_all(names(yClass),"y_")

            for(idx in 1:ncol(multiClassPred)){
              multiClassPred[,idx]<-as.numeric(multiClassPred[,idx])
            }


          selectCol<-names(multiClassPred)[names(yClass)%in%names(multiClassPred)]
          sqrt(mean((
            as.matrix(multiClassPred[,selectCol]-yClass[,selectCol])
            )^2))/mean(as.matrix(abs(yClass)))
       } else{

         sqrt(mean((augmentPred$ypred-yFit$y)^2))/mean(abs(yFit$y))

       }


        }else{
          netPred<- predict.deepnet(fitModel,
                                         newData=xFit)
          if(modelType=="multiClass"){


            multiClassPred<-netPred[,names(netPred)!="ypred"]
            yClass<-dummy_cols(yFit)
            yClass<-yClass[,names(yClass)!="y"]
            names(yClass)<-str_remove_all(names(yClass),"y_")

            for(idx in 1:ncol(multiClassPred)){
              multiClassPred[,idx]<-as.numeric(multiClassPred[,idx])
            }



            sqrt(mean((
              as.matrix(multiClassPred[,names(yClass)]-yClass)
            )^2))/mean(as.matrix(abs(yClass)))



          }else{

            sqrt(mean((netPred$ypred-yFit$y)^2))/mean(abs(yFit$y))
          }

        }

      }))



  bestFit<- min(fitPerf,na.rm = T)

  errorCoverFit=bestFit+errorCover
  chosenIndex<-which(!is.na(fitPerf)& fitPerf<=errorCoverFit)
  chosenModels<- modelForest[chosenIndex]

  deepforestmod<-list(varcut=varCut[chosenIndex],
                      chosenModels=chosenModels,
                      treeAugment=treeAugment,
                      modelType=modelType
                      )
  class(deepforestmod)<-'deepforest'

  return(deepforestmod)
}
