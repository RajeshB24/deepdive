

#' Title
#'
#' @param object
#' @param newData
#' @param treeLeaves
#' @param stackPred
#'
#' @return
#' @export
#'
#' @examples
predict.deeptree<-function(object,newData,treeLeaves=NA,stackPred=NA){




  library(data.table)
  library(rpart)

  useStackPred=object$useStackPred

  if(length(useStackPred)<length(object$treeLeavesGroup)){
    useStackPred<-rep(useStackPred,length(object$treeLeavesGroup))
  }

  if(sum(useStackPred)>0){
    if(is.na(sum(stackPred))){
      print('stackPred is required')
      break;
    }
  }
  treeLeavesGroup<-object[['treeLeavesGroup']]
  modelGroup<-object[['modelGroup']]
  xCols=object[["xColList"]]

if(object[["preBuiltTree"]]){

  if(is.na(treeLeaves)){
    print("missing treeLeaves attribute")
    break();

  }}else{

    treeLeaves=treeClust::rpart.predict.leaves(object[["treeMod"]],newData)


  }


  newDataSplit<- lapply(treeLeavesGroup, function(tp){
    newData[treeLeaves==tp,]
  })

  if(sum(useStackPred)>0){
  stackPred<-lapply(treeLeavesGroup, function(tp){
    stackPred[treeLeaves==tp,]
  })
}



  predlist<-  lapply(1:length(modelGroup)
                     , function(mg){

                       if(!useStackPred[[mg]]){
                       predict.deepnet(modelGroup[[mg]],newDataSplit[[mg]][,xCols[[mg]]])
                       }else{
                         stackPred[[mg]]
                       }

                     })

  predlist<-lapply(1:length(predlist), function(pl){
    data.frame( cbind(predlist[[pl]],rowname= as.numeric(row.names(newDataSplit[[pl]]))))

  })


  ypred<-data.frame(rbindlist(predlist,use.names = F))

  ypred<-ypred[order(ypred$rowname),]
  names(ypred)[1]<-c('pred_y')

  return(data.frame(ypred=ypred[,'pred_y']))
}
