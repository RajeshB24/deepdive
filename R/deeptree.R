


#' @title  Descision Tree augmented by Artificial Neural Network
#' @description  This models divides the input space by fitting a tree followed by artificial neural network to each of leaf. Decision tree model is built using rpart package and neural network using deepdive.Feature of stacking predictions from other models is also made available.
#'
#' @param x a data frame with input variables
#' @param y a data frame with ouptut variable
#' @param hiddenLayerUnits a numeric vector, length of vector indicates number of hidden layers and each element in vector indicates corresponding hidden units Eg: c(6,4) for two layers, one with 6 hiiden units and other with 4 hidden units. Note: Output layer is automatically created.
#' @param activation one of "sigmoid","relu","sin","cos","none". The default is "sigmoid". Choose a activation per hidden layer
#' @param reluLeak numeric. Applicable when activation is "relu". Specify value between 0 any number close to zero below 1. Eg: 0.01,0.001 etc
#' @param modelType  one of "regress","binary","multiClass". "regress" for regression will create a linear single unit output layer. "binary" will create a single unit sigmoid activated layer. "multiClass" will create layer with units corresponding to number of output classes with softmax activation.
#' @param iterations integer. This indicates number of iteratios or epochs in backpropagtion .The default value is 500.
#' @param eta numeric.Hyperparameter,sets the Learning rate for backpropagation. Eta determines the convergence ability and speed of convergence.
#' @param seed numeric. Set seed with this parameter. Incase of sin activation sometimes changing seed can yeild better results. Default is 2
#' @param gradientClip numeric. Hyperparameter numeric value which limits gradient size for weight update operation in backpropagation. Default is 0.8 . It can take any postive value.
#' @param regularisePar numeric. L2 Regularisation Parameter .
#' @param optimiser one of "gradientDescent","momentum","rmsProp","adam". Default value "adam"
#' @param parMomentum numeric. Applicable for optimiser "mometum" and "adam"
#' @param inputSizeImpact numeric. Adjusts the gradient size by factor of percentage of rows in input. For very small data set setting this to 0 could yeild faster result. Default is 1.
#' @param parRmsPropZeroAdjust numeric. Applicable for optimiser "rmsProp" and "adam"
#' @param parRmsProp numeric.Applicable for optimiser "rmsProp" and "adam"
#' @param treeLeaves vector.Optional , leaves numbers from externally trained tree model can be supplied here. If supplied then model will not build a explicit tree and just fit a neural network to mentioned leaves.
#' @param treeMinSplitPercent numeric. This parameter controls depth of tree setting min split count for leaf subdivision as percentage of observations. Final minimum split will be chosen as max of count calculted with treeMinSplitPercent and treeMinSplitCount. Default 0.3. Range 0 to 1.
#' @param treeMinSplitCount numeric. This parameter controls depth of tree setting min split count.Final minimum split will be chosen as max of count calculted with treeMinSplitPercent and treeMinSplitCount. Default 30
#' @param treeCp complexity parameter. \code{\link{rpart.control}}
#' @param stackPred vector.Predictions from buildnet or other models can be supplied here. If for certain leaf stackPrep accuracy is better then stackpred predictions will be chosen.
#' @param printItrSize numeric. Number of iterations after which progress message should be shown. Default value 100 and for iterations below 100 atleast 5 messages will be seen
#' @param showProgress logical. True will show progress and F will not show progress
#' @param stopError Numeric. Rmse at which iterations can be stopped. Default is 0.01, can be set as NA in case all iterations needs to run.
#' @param miniBatchSize integer. Set the mini batch size for mini batch gradient
#' @param useBatchProgress logical. Applicable for miniBatch , setting T will use show rmse in Batch and F will show error on full dataset. For large dataset set T
#' @param ignoreNAerror logical. Set T if iteration needs to be stopped when predictions become NA
#'
#' @return returns model object which can be passed into \code{\link{predict.deeptree}}
#' @export
#' @import rpart
#' @import data.table
#' @importFrom graphics barplot
#' @importFrom stats formula predict runif
#' @examples
#'
#' require(deepdive)
#'
#' x <- data.frame(x1 = runif(10),x2 = runif(10))
#'
#' y<- data.frame(y=20*x$x1 +30* x$x2 +10)
#'
#' deepTreeMod<-deeptree(x,
#'y,
#'hiddenLayerUnits=c(4,4),
#'activation = c('relu',"sin"),
#'reluLeak=0.01,
#'modelType ='regress',
#'iterations = 1000,
#'eta = 0.4,
#'seed=2,
#'gradientClip=0.8,
#'regularisePar=0,
#'optimiser="adam",
#'parMomentum=0.9,
#'inputSizeImpact=1,
#'parRmsPropZeroAdjust=10^-8,
#'parRmsProp=0.9999,
#'treeLeaves=NA,
#'treeMinSplitPercent=0.4,
#'treeMinSplitCount=100,
#'stackPred =NA,
#'stopError=4,
#'miniBatchSize=64,
#'useBatchProgress=TRUE,
#'ignoreNAerror=FALSE)

#'


deeptree<-function(     x,
                        y,
                        hiddenLayerUnits=c(2,2),
                        activation = c("sigmoid","sigmoid"),
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
                        treeMinSplitCount=30,
                        treeCp=0.01 ,
                        stackPred=NA,
                        printItrSize=100,
                        showProgress=TRUE,
                        stopError=0.01,
                        miniBatchSize=NA,
                        useBatchProgress=TRUE,
                        ignoreNAerror=FALSE){



  if(is.na(treeLeaves)){


    preBuiltTree<-F
    treeMod<-rpart(formula(paste0(names(y),"~.")),
                          cbind.data.frame(x,y),
                          control = rpart.control(minsplit =max(treeMinSplitCount,
                                                round(treeMinSplitPercent*nrow(x),0)),cp=0.01))

    treeLeaves<-treeMod$where

  }else{
    preBuiltTree<-T
    treeMod=NA
  }
  treeLeavesGroup<-unique(treeLeaves)

  x<-data.frame(x)


  y=data.frame(y)

 if( is.null(names(y))){
   names(y)<-"y"
 }


  #split data
  x<- lapply(treeLeavesGroup, function(tp){
   sx=data.frame( x[treeLeaves==tp,])
   names(sx)<-names(x)
   sx
  })

  y<-lapply(treeLeavesGroup, function(tp){
    y[treeLeaves==tp,]
  })

  if(!is.na(sum(stackPred))){
    stackPred<-lapply(treeLeavesGroup, function(tp){
      stackPred[treeLeaves==tp,]
    })


 stackErrorsplit<-unlist(lapply(1:length(y), function(er){
   sum((stackPred[[er]]-y[[er]])^2)
 }))}



#Below code removes columns which have only 1 unique value in X
xColList<-lapply(1:length(y),function(s){

  idx<-which(sapply(1:ncol(x[[s]]), function(m){
  length(unique(x[[s]][,m]))
})>1)

return(names(x[[s]])[idx])
})


  modelGroup<-lapply(1:length(y),function(s){

           if(length(unique(y[[s]]))==1){
            tpred=unique(y[[s]])
            names(tpred)="treepred"
            return(tpred)
           }else{

             deepNetMod<-  deepnet(data.frame(x[[s]][,xColList[[s]]]),
                       data.frame(y[[s]]),
                       hiddenLayerUnits,
                       activation ,
                       reluLeak,
                       modelType,
                       iterations,
                       eta ,
                       seed,
                       gradientClip,
                       regularisePar,
                       optimiser,
                       parMomentum,
                       inputSizeImpact,
                       parRmsPropZeroAdjust,
                       parRmsProp,
                       printItrSize,
                       showProgress,
                       stopError,
                       miniBatchSize,
                       useBatchProgress,
                       ignoreNAerror)

              class(deepNetMod)="deepnet"
              return(deepNetMod)
             }
    })

if(!is.na(stackPred)&modelType=="regress"){



  treeErrorlist<-unlist(lapply(1:length(modelGroup), function(fl){



    pred_y<-predict(modelGroup[[fl]],x[[fl]])$ypred


    sum((modelGroup[[fl]]$fitted-y[[fl]])^2)


  }))


 useStackPred<-ifelse(is.na(treeErrorlist),T, stackErrorsplit<treeErrorlist)


}else{
  useStackPred<-F
}

  if(!preBuiltTree){

    deeptreeMod<-list(treeLeavesGroup=treeLeavesGroup,modelGroup=modelGroup,xColList=xColList,
                      treeMod=treeMod,
                      preBuiltTree=preBuiltTree,
                      useStackPred=useStackPred,
                      modelType=modelType)

 }else{


   deeptreeMod<-list(treeLeavesGroup=treeLeavesGroup,
              modelGroup=modelGroup,
              xColList=xColList,
              treeMod=NA,
              preBuiltTree=preBuiltTree,
              useStackPred=useStackPred,
              modelType=modelType)
 }

  class(deeptreeMod)<-'deeptree'
  return(deeptreeMod)

}



