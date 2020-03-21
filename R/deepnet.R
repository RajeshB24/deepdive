
#' @title Build and train an Artificial Neural Network of any size
#' @description Build and train Artifical Neural Network of any depth in a single line code. Choose the hyperparameters to improve the accuracy or generalisation of model.
#' @param x  a data frame with input variables
#' @param y  a data frame with ouptut variable
#' @param hiddenLayerUnits a numeric vector, length of vector indicates number of hidden layers and each element in vector indicates corresponding hidden units Eg: c(6,4) for two layers, one with 6 hiiden units and other with 4 hidden units. Note: Output layer is automatically created.
#' @param activation one of "sigmoid","relu","sin","cos","none". The default is "sigmoid". Choose a activation per hidden layer
#' @param reluLeak  numeric. Applicable when activation is "relu". Specify value between 0 any number close to zero below 1. Eg: 0.01,0.001 etc
#' @param modelType one of "regress","binary","multiClass". "regress" for regression will create a linear single unit output layer. "binary" will create a single unit sigmoid activated layer. "multiClass" will create layer with units corresponding to number of output classes with softmax activation.
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
#' @param printItrSize numeric. Number of iterations after which progress message should be shown. Default value 100 and for iterations below 100 atleast 5 messages will be seen
#' @param showProgress logical. True will show progress and F will not show progress
#' @param stopError  Numeric. Rmse at which iterations can be stopped. Default is 0.01, can be set as NA in case all iterations needs to run.
#' @param miniBatchSize integer. Set the mini batch size for mini batch gradient
#' @param useBatchProgress logical. Applicable for miniBatch , setting T will use show rmse in Batch and F will show error on full dataset. For large dataset set T
#' @param ignoreNAerror logical. Set T if iteration needs to be stopped when predictions become NA
#' @param normalise logical. Set F if normalisation not required.Default T
#'
#' @return returns model object which can be passed into \code{\link{predict.deepnet}}
#' @export
#'@importFrom fastDummies dummy_cols
#'@importFrom graphics barplot
#'@importFrom stats formula predict runif
#' @examples
#' require(deepdive)
#'
#' x <- data.frame(x1 = runif(10),x2 = runif(10))
#' y<- data.frame(y=20*x$x1 +30*x$x2+10)
#'
#' #train
#' modelnet<-deepnet(x,y,c(2,2),
#' activation = c('relu',"sigmoid"),
#' reluLeak = 0.01,
#' modelType = "regress",
#' iterations =5,
#' eta=0.8,
#' optimiser="adam")
#'
#' #predict
#' predDeepNet<-predict.deepnet(modelnet,newData=x)
#'
#' #evaluate
#'sqrt(mean((predDeepNet$ypred-y$y)^2))
#'
#'
deepnet<- function(x,
                    y,
                    hiddenLayerUnits=c(2,2),
                    activation =c('sigmoid',"relu"),
                    reluLeak=0,
                    modelType = c('regress'),
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
                    printItrSize=100,
                    showProgress=TRUE,
                    stopError=0.01,
                    miniBatchSize=NA,
                    useBatchProgress=FALSE,
                    ignoreNAerror=FALSE,
                    normalise=TRUE
) {

  if(modelType=="multiClass"){
    y[,1]<-as.character(y[,1])

  }

  if(is.na(miniBatchSize)){
    miniBatchSize=nrow(x)
  }


  set.seed(seed)

   xcolnames<-names(x)
   if(is.character(y)){
     y<-as.factor(y)
   }
   if(modelType=="multiClass"){
     y<-dummy_cols(y)[,-1]

   }


  xType<-sapply(x,class)

  xfctrChrColsIdx<-which(xType%in%c("character","factor"))
  if(length(xfctrChrColsIdx)>0L){
    x<-dummy_cols(x)
    x<-x[,-xfctrChrColsIdx]
  }

  #Input Normalisation

  if(normalise==T){
  inColMax=sapply(x, max )
  inColMin=sapply(x, min)}else{
    inColMax=rep(1,ncol(x))
    inColMin=rep(0, ncol(x))
  }


  for(i in 1:ncol(x)){
    x[,i]<- (x[,i]-inColMin[i])/(inColMax[i]-inColMin[i])
  }

  #Output Normalisation

  if(normalise==T){
    outColMax=sapply(y, max )
    outColMin=sapply(y, min)}else{
      outColMax=rep(1,ncol(y))
      outColMin=rep(0, ncol(y))
    }

  for(i in 1:ncol(y)){
    y[,i]<- (y[,i]-outColMin[i])/(outColMax[i]-outColMin[i])
  }


  inputColCount = ncol(x)
  outputColCount = ncol(y)


  interVariableCount <- c(inputColCount + 1, hiddenLayerUnits)
  interOutCount <- c(hiddenLayerUnits, outputColCount)


  weightMatrix <<-
    lapply(c(1:length(interVariableCount)),
           function(i){ weightInitialiser(i,interVariableCount,interOutCount,seed,activation)
           })

  #Bias units only for hidden layer and output layer

  baisUnits<- lapply(1:length(interOutCount), function(b){

    if(b==1){NA
    }else{
      as.matrix(runif(interOutCount[b]))
    }
  })


 # y<-as.matrix(y)

#  x<-as.matrix(cbind(const = rep(1, nrow(x)), x))


  x<-cbind(const = rep(1, nrow(x)), x)



  AllWeights<-list(weightMatrix=weightMatrix,
                   baisUnits=baisUnits)


previousWeightUpdate<-  lapply(c(1:length(interVariableCount)),
                           function(i){
                             weightUpdateInitialiser(i,interVariableCount,interOutCount,seed)
                           })


previousBiasUpdate<-lapply(1:length(interOutCount), function(b){

  if(b==1){NA
  }else{
    as.matrix(runif(interOutCount[b],0,0))
  }
})


previousWeightAdapt<-  lapply(c(1:length(interVariableCount)),
                               function(i){
                                 weightUpdateInitialiser(i,interVariableCount,interOutCount,seed)
                               })


previousBiasAdapt<-lapply(1:length(interOutCount), function(b){

  if(b==1){NA
  }else{
    as.matrix(runif(interOutCount[b],0,0))
  }
})


printItrSize<-min(round(iterations/5,0),printItrSize)
msgIter<-seq(0,iterations,by=printItrSize)

if(max(msgIter)<iterations){
  msgIter<-c(msgIter,iterations)
}


  for (itr in 1:iterations) {


    if(itr==1){
      prevBatch=0
    }

    miniBatchIndex<-miniBatchCreate(nrow(x),miniBatchSize,prevBatch)
    prevBatch<-miniBatchIndex$bacthNo

    batchupper=miniBatchIndex[,"batchupper"]
    batchlower=miniBatchIndex[,"batchlower"]



    AllWeights <- backProp(as.matrix(x[batchlower:batchupper,]),
                           as.matrix(y[batchlower:batchupper,]),
                           weightMatrix, activation,reluLeak,
                                modelType, eta,
                                gradientClip,baisUnits,
                                regularisePar,itr,optimiser,
                                parMomentum,parRmsProp,parRmsPropZeroAdjust,
                                previousWeightUpdate,
                                previousBiasUpdate ,
                                previousWeightAdapt,
                                previousBiasAdapt,
                                inputSizeImpact)

    weightMatrix<-AllWeights$weightMatrix
    baisUnits<-AllWeights$baisUnits
    previousWeightUpdate    <-AllWeights$previousWeightUpdate
    previousBiasUpdate      <-AllWeights$previousBiasUpdate
    previousWeightAdapt    <-AllWeights$previousWeightAdapt
    previousBiasAdapt      <-AllWeights$previousBiasAdapt

    if(itr %in% msgIter){

       if(useBatchProgress==T){
         itry<- as.matrix(y[batchlower:batchupper,])
         feedList <- feedForward(as.matrix(x[batchlower:batchupper,]),
                                 weightMatrix,
                                 activation,
                                 reluLeak,
                                 modelType,baisUnits)
    }else{
      itry<-as.matrix(y)
      feedList <- feedForward(as.matrix(x),
                              weightMatrix,
                              activation,
                              reluLeak,
                              modelType,baisUnits)
      }



      feedOut <- feedList$a_output
      itrypred <- feedOut[[length(feedOut)]]



      for(ci in 1:ncol(itrypred)){

        itrypred[,ci]<-itrypred[,ci]*(outColMax[ci]-outColMin[ci])+outColMin[ci]

        }

      for(ci in 1:ncol(y)){

        itry[,ci]<-itry[,ci]*(outColMax[ci]-outColMin[ci])+outColMin[ci]}

      #rmse

      costFun<-sqrt(mean((itrypred - itry) ^ 2))

}

      if(showProgress==T){
    if(itr %in% msgIter){

    message(paste0("iteration ",
                ifelse(itr==iterations,itr, itr-1)
                 ,": ",costFun))
    }


      }
    if(itr %in% msgIter ){
      if(is.na(costFun)){
        if(ignoreNAerror==F){
        warning("NA error.Try changing eta/specify reluleak if relu is used/If minibatch is used try increasing the size")

        break();}
      }
      if(is.nan(costFun)){
        warning("NaN error.Try chaning eta")
        break();
      }


      if( !is.na(stopError)& !is.na(costFun)){
      if(costFun<=stopError){
        message(paste0("iteration ",itr,": ",costFun))
        message("Reached stop error. Reduce Change or set StopError to NA if would like more iterations ")
        break();

      }}

      }

    }


 # feedListOut<-feedForward(x,weightMatrix,activation,reluLeak,modelType,baisUnits)

  #feedOut<-feedListOut$a_output
  #ypred<-feedOut[[length(feedOut)]]

  #for(i in 1:ncol(y)){

   # ypred[,i]<-ypred[,i]*(outColMax[i]-outColMin[i])+outColMin[i]}



  #ypred<-data.frame(ypred)
  #names(ypred)<-paste0('pred_',names(ypred))

  deepnetmod<-list(#fitted=ypred,
                   weightMatrix=weightMatrix,
                   activation=activation,
                   modelType=modelType,
                   outColMax=outColMax,
                   outColMin=outColMin,
                   inColMax=inColMax,
                   inColMin=inColMin,
                   baisUnits=baisUnits,
                   reluLeak=reluLeak,
                   xcolnames=xcolnames)
  class(deepnetmod)<-"deepnet"

  return(deepnetmod)

}

