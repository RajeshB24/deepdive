


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
        print('stackPred is required')

      }
    }
    treeLeavesGroup <- object[['treeLeavesGroup']]
    modelGroup <- object[['modelGroup']]
    xCols = object[["xColList"]]

    if (object[["preBuiltTree"]]) {
      if (is.na(treeLeaves)) {
        print("missing treeLeaves attribute")


      }
    } else{
      treeLeaves = rpart.predict.leaves(object[["treeMod"]], newData)


    }


    newDataSplit <- lapply(treeLeavesGroup, function(tp) {
      newData[treeLeaves == tp, ]
    })

    if (sum(useStackPred) > 0) {
      stackPred <- lapply(treeLeavesGroup, function(tp) {
        stackPred[treeLeaves == tp, ]
      })
    }



    predlist <-  lapply(1:length(modelGroup)
                        , function(mg) {
                          if (!useStackPred[[mg]]) {
                            predict.deepnet(modelGroup[[mg]], newDataSplit[[mg]][, xCols[[mg]]])
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

    if(object$modelGroup[[1]]$modelType=="regress"){
      names(ypred)[1]<-"pred_y"
    }

    if(object$modelGroup[[1]]$modelType=="multiClass"){
      ypred$pred_y<-stringr::str_remove_all(ypred$pred_y,"pred_y..s.._")
    }


    return(data.frame(ypred))
  }
