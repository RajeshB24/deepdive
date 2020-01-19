
#' @title Build and train an Artificial Neural Network of any size
#' @description Build and train Artifical Neural Network of any depth in a single line code. Choose the hyperparameters to improve the accuracy or generalisation of model.This is built by using AndrewNg Deep learning course as reference.
#' @param x  a data frame with input variables
#' @param y  a data frame with ouptut variable
#' @param hiddenLayerUnits a numeric vector, length of vector indicates number of hidden layers and each element in vector indicates corresponding hidden units Eg: c(6,4) for two layers, one with 6 hiiden units and other with 4 hidden units. Note: Output layer is automatically created.
#' @param activation one of "sigmoid","relu","sin","cos","none". The default is "sigmoid". This activation choice will be applied to all hidden layers.
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
#'
#' @return returns model object which can be passed into \code{\link{predict.deepnet}}
#' @export
#'@importFrom fastDummies dummy_cols
#' @examples
#' \dontrun{
#' x <- data.frame(a = runif(1000)*100,
#' b = runif(1000)*200,
#' c = runif(1000)*100,
#' d = runif(1000)*200)
#' y<- data.frame(y=20*x$a +30* x$b+10*x$c +10)
#'
#' #train
#' modelnet<-deepnet(x,y,c(2,2),activation = "relu",
#' reluLeak = 0,
#' modelType = "regress",
#' iterations =4000,
#' eta=0.8,
#' optimiser="adam")
#'
#' #predict
#' predDeepNet<-predict.deepnet(modelnet,newData=x)
#'
#' #evaluate
#'sqrt(mean((predDeepNet$pred_y-y$y)^2))
#'
#' }
deepnet<- function(x,
                    y,
                    hiddenLayerUnits,
                    activation = c('sigmoid'),
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
                    parRmsProp=0.9999
) {


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
  inColMax=sapply(x, max )
  inColMin=sapply(x, min)


  for(i in 1:ncol(x)){
    x[,i]<- (x[,i]-inColMin[i])/(inColMax[i]-inColMin[i])
  }

  #Output Normalisation
  outColMax=sapply(y, max)
  outColMin=sapply(y, min)

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


  y<-as.matrix(y)

  x<-as.matrix(cbind(const = rep(1, nrow(x)), x))


#  msgIter<-seq(0,iterations,by=max(1,min(round(iterations/10),100)))
 msgIter<-seq(0,iterations,by=iterations/100)


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

  for (itr in 1:iterations) {



    AllWeights <- backProp(x,y, weightMatrix, activation,reluLeak,
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

    feedList <- feedForward(x, weightMatrix, activation,reluLeak, modelType,baisUnits)
    feedOut <- feedList$a_output
    ypred <- feedOut[[length(feedOut)]]

    curcost<-sum((ypred - y) ^ 2)

#if(i==1){costFun=curcost}else if(curcost<costFun){
    costFun <- curcost
#}else{

 # if(modelType=="regress"){
  #  print("Suggestion: Reduce eta ")
#  break()}else{
 # costFun <- curcost}
  #}

    if(itr %in% msgIter){
    print(paste0("iteration ",itr,": ",costFun))}
  }


  feedListOut<-feedForward(x,weightMatrix,activation,reluLeak,modelType,baisUnits)

  feedOut<-feedListOut$a_output
  ypred<-feedOut[[length(feedOut)]]

  for(i in 1:ncol(y)){
    ypred[,i]<-ypred[,i]*(outColMax[i]-outColMin[i])+outColMin[i]

  }

  ypred<-data.frame(ypred)
  names(ypred)<-paste0('pred_',names(ypred))

  deepnetmod<-list(fitted=ypred,
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

