

#' Title
#'
#' @param object
#' @param newData
#'
#' @return
#' @export predict.deepnet
#' @export
#' @examples
predict.deepnet<-function(object,
                          newData){

  newData<-data.frame(newData)
  weightMatrix=object[["weightMatrix"]]
  activation=object[["activation"]]
  modelType=object[["modelType"]]
  baisUnits=object[["baisUnits"]]
  reluLeak=object[["reluLeak"]]
  inColMin=object[['inColMin']]

  inColMax=object[['inColMax']]
  xcolnames=object[['xcolnames']]

  newData<-newData[,xcolnames]

  xType<-sapply(newData,class)


  xfctrChrColsIdx<-which(xType%in%c("character","factor"))

  if(length(xfctrChrColsIdx)>0L){
    newData<-fastDummies::dummy_cols(newData)
    newData<-newData[,-xfctrChrColsIdx]
  }



  for(i in 1:ncol(newData)){
    newData[,i]<- (newData[,i]-inColMin[i])/(inColMax[i]-inColMin[i])
  }


  newData<-as.matrix(cbind(const = rep(1, nrow(newData)), newData))


   reqmat<-matrix(1,ncol = nrow(weightMatrix[[1]]))

  colnames(reqmat)<-row.names(weightMatrix[[1]])



  newData<-plyr::rbind.fill(as.data.frame(reqmat),
                        as.data.frame(newData))


  newData<-as.matrix(newData[-1,])

  newData[is.na(newData)]<-0

  newData<- newData[,colnames(reqmat)]

  feedList<- feedForward( newData, weightMatrix, activation,reluLeak, modelType,baisUnits)


  feedOut <- feedList$a_output
  zin <- feedList$z_in
  ypred = feedOut[[length(feedOut)]]

  outColMax<-object[['outColMax']]

  outColMin<-object[['outColMin']]

  for(i in 1:ncol(y)){
    ypred[,i]<-ypred[,i]*(outColMax[i]-outColMin[i])+outColMin[i]

    ypred[,i]= ypred[,i]
  }

  ypred<-data.frame(ypred)
  names(ypred)<-paste0('pred_',names(ypred))



  if(modelType=="multiClass"){
    ypred$pred_y<-stringr::str_remove_all( names(ypred),"pred_y_")[max.col(ypred)]
  }

  return(ypred)
}


