#Below intialises weight updates for momentum model


#' Title
#'
#' @param i
#' @param interVariableCount
#' @param interOutCount
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
weightUpdateInitialiser <- function(i,interVariableCount,interOutCount,seed) {
  set.seed(seed)

  weightUpdateMatrix<-matrix(runif(interVariableCount[i] * interOutCount[i],0,0),
                         nrow = interVariableCount[i])
  return(weightUpdateMatrix)
}

