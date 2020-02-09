


#' Predict Function for Deeptree
#'
#' @param object deeptree model object
#' @param newData pass dataframe for prediction
#' @param treeLeaves  Pass vector with tree leaves if fit outside deeptree. default NA.
#' @param stackPred   Pass stackPred of prediction data if it was passed in deeptree
#' @param ... further arguments passed to or from other methods.
#'
#'
#' @return returns predictions vector or dataframe
#' @export
#' @export predict.deeptree
#' @importFrom  data.table rbindlist
#' @import rpart
#' @importFrom treeClust rpart.predict.leaves
#' @importFrom graphics barplot
#' @importFrom stats formula predict runif
#'

predict.deeptree <-
  function(object,
           newData,
           treeLeaves = NA,
           stackPred = NA,
           ...) {
    if (!inherits(object, "deeptree"))
      stop("Not a legitimate \"deeptree\" object")

    useStackPred = object$useStackPred

    if (length(useStackPred) < length(object$treeLeavesGroup)) {
      useStackPred <- rep(useStackPred, length(object$treeLeavesGroup))
    }

    if (sum(useStackPred) > 0) {
      if (is.na(sum(stackPred))) {
        warning('stackPred is required')

      }
    }
    treeLeavesGroup <- object[['treeLeavesGroup']]
    modelGroup <- object[['modelGroup']]
    xCols = object[["xColList"]]

    if (object[["preBuiltTree"]]) {
      if (is.na(treeLeaves)) {
        warning("missing treeLeaves attribute")


      }
    } else{
      treeLeaves = rpart.predict.leaves(object[["treeMod"]], newData)


    }

    newDataSplit <- lapply(treeLeavesGroup, function(tp) {
     sdata<-data.frame( newData[treeLeaves == tp, ])
      names(sdata)<-names(newData)
      sdata
    })

    if (sum(useStackPred) > 0) {
      stackPred <- lapply(treeLeavesGroup, function(tp) {
        stackPred[treeLeaves == tp, ]
      })
    }

    predlist <-  lapply(1:length(modelGroup)
                        , function(mg) {
                          if (!useStackPred[[mg]]) {
                            if(class( modelGroup[[mg]])=="deepnet"){
                              if(length(xCols[[mg]])==1){
                              predict.deepnet(modelGroup[[mg]],
                                      newDataSplit[[mg]])}else{
                                        predict.deepnet(modelGroup[[mg]],
                                                        newDataSplit[[mg]][, xCols[[mg]]])
                                      }
                            }else {
                                ypred= data.frame(ypred=rep(modelGroup[[mg]],
                                                             nrow(newDataSplit[[mg]])))
                                ypred
                              }


                          } else{
                            stackPred[[mg]]
                          }

                        })

    predlist <- lapply(1:length(predlist), function(pl) {
      data.frame(cbind(predlist[[pl]], rowname = as.numeric(row.names(newDataSplit[[pl]]))))

    })


    ypred <- data.frame(rbindlist(predlist, use.names = T,
                                  fill = T,
    ))




    ypred[is.na(ypred)]=0
    ypred <- ypred[order(ypred$rowname), ]

    if(object$modelType=="regress"){
      names(ypred)[1]<-"ypred"
    }else if(object$modelType=="multiClass"){

      ypred$ypred<-stringr::str_remove_all(ypred$ypred,"y..s.._")

      names(ypred)<-stringr::str_remove_all(names(ypred),"y..s.._")

      if(length(unique(ypred$ypred))>sum(!names(ypred)%in%c('rowname','ypred'))){
        for(coli in unique(ypred$ypred)[!unique(ypred$ypred)%in% names(ypred)]  ){

          ypred[,coli]<-ifelse(ypred$ypred==coli,
                               1,0)
        }

      }


      colnames<- names(ypred)[!names(ypred)%in%"rowname"]

    ypred<-data.frame(ypred[,!names( ypred)%in%"rowname"])

    names(ypred)<-colnames

    ProbCols<- names(ypred)[!names(ypred)%in%"ypred"]

    ypred$ProbSum <-rowSums(ypred[,ProbCols])

    for( icol in 1:length(ProbCols) ){
    ypred[,icol]<-ifelse(ypred$ProbSum==0 &
                                as.character(ypred[,icol])==
                                  as.character(ypred$ypred)
                                ,1,as.character(ypred[,icol])
                                )
    }

}

   finColNames<-names(ypred)
   finColNames<-finColNames[!finColNames%in%c("rowname","ProbSum")]

   ypred=ypred[,finColNames]
   names(ypred)<-finColNames

    return(data.frame(ypred))
  }
