

#' Predict Function for Deepnet
#'
#' @param object deepnet model object
#' @param newData pass dataframe for prediction
#' @param ... further arguments passed to or from other methods.
#'
#' @return returns predictions vector or dataframe
#' @export predict.deepnet
#' @export
#' @importFrom graphics barplot
#' @importFrom stats formula predict runif


predict.deepnet<-function(object,
                          newData,...){

  if (!inherits(object, "deepnet")) stop("Not a legitimate \"deepnet\" object")
  newData<-data.frame(newData)
  weightMatrix=object[["weightMatrix"]]
  activation=object[["activation"]]
  modelType=object[["modelType"]]
  baisUnits=object[["baisUnits"]]
  reluLeak=object[["reluLeak"]]
  inColMin=object[['inColMin']]

  inColMax=object[['inColMax']]
  xcolnames=object[['xcolnames']]

  if(length(xcolnames)>1){
  newData<-newData[,xcolnames]
  }

  xType<-sapply(newData,class)


  xfctrChrColsIdx<-which(xType%in%c("character","factor"))

  if(length(xfctrChrColsIdx)>0L){
    newData<-fastDummies::dummy_cols(newData)
    newData<-newData[,-xfctrChrColsIdx]

  }


  newData<-data.frame(newData)

  for(i in 1:ncol(newData)){
    newData[,i]<- (newData[,i]-inColMin[i])/(inColMax[i]-inColMin[i])
    }


  newData<-as.matrix(cbind(const = rep(1, nrow(newData)), newData))


  if(ncol(newData)>2){
   reqmat<-matrix(1,ncol = nrow(weightMatrix[[1]]))


  colnames(reqmat)<-row.names(weightMatrix[[1]])


  newData<-plyr::rbind.fill(as.data.frame(reqmat),
                        as.data.frame(newData))


  newData<-as.matrix(newData[-1,])
  newData<- newData[,colnames(reqmat)]
  }

  newData[is.na(newData)]<-0



  feedList<- feedForward( newData, weightMatrix, activation,reluLeak, modelType,baisUnits)


  feedOut <- feedList$a_output
  zin <- feedList$z_in
  ypred = feedOut[[length(feedOut)]]

  outColMax<-object[['outColMax']]

  outColMin<-object[['outColMin']]

  for(i in 1:ncol(ypred)){
    ypred[,i]<-ypred[,i]*(outColMax[i]-outColMin[i])+outColMin[i]

    ypred[,i]= ypred[,i]
  }

  ypred<-data.frame(ypred)
  names(ypred)<-names(ypred)


  if(modelType=="multiClass"){
    ypred$ypred<-stringr::str_remove_all( names(ypred),"y_")[max.col(ypred)]
    names(ypred)=stringr::str_remove_all(names(ypred),"y_")
  }




  return(ypred)
}


