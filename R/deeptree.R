#Special Case neural network : This model expects groups identified through intial simple tree model. This could be leaf labels or predictions. The algorithm then further trains a ANN per group.



deeptree<-function(     x,
                        y,
                        hiddenLayerUnits,
                        activation = 'relu',
                        reluLeak=0,
                        modelType ='regress',
                        iterations = 500,
                        eta = 10 ^(-(2+length(hiddenLayerUnits))),
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
                        stackPred=NA    ){

library(rpart)




  if(is.na(treeLeaves)){


    preBuiltTree<-F
    treeMod<-rpart::rpart(formula(paste0(names(y),"~.")),
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

  #split data
  x<- lapply(treeLeavesGroup, function(tp){
    x[treeLeaves==tp,]
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
xColList<-lapply(1:length(y),function(s){idx<-which(sapply(1:ncol(x[[s]]), function(m){
  length(unique(x[[s]][,m]))
})>1)

return(names(x[[s]])[idx])
})



  modelGroup<-lapply(1:length(y),function(s){

               deepnet(data.frame(x[[s]][,xColList[[s]]]),
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
                       parRmsProp)})



  treeErrorlist<-unlist(lapply(1:length(modelGroup), function(fl){
    sum((modelGroup[[fl]]$fitted-y[[fl]])^2)
  }))

  if(is.list(stackPred)){
 useStackPred<-ifelse(is.na(treeErrorlist),T, stackErrorsplit<treeErrorlist)

  }else{
    useStackPred<-F
}

  if(!preBuiltTree){

  return(list(treeLeavesGroup=treeLeavesGroup,modelGroup=modelGroup,xColList=xColList,
              treeMod=treeMod,
              preBuiltTree=preBuiltTree,
              useStackPred=useStackPred))}else{

  return(list(treeLeavesGroup=treeLeavesGroup,
              modelGroup=modelGroup,
              xColList=xColList,
              treeMod=NA,
              preBuiltTree=preBuiltTree,
              useStackPred=useStackPred))
  }

}



