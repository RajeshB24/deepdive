
#' Title
#'
#' @param i
#' @param interVariableCount
#' @param interOutCount
#' @param seed
#'
#' @return
#'
#'
#' @examples
weightUpdateInitialiser <- function(i,interVariableCount,interOutCount,seed) {
  set.seed(seed)

  weightUpdateMatrix<-matrix(runif(interVariableCount[i] * interOutCount[i],0,0),
                         nrow = interVariableCount[i])
  return(weightUpdateMatrix)
}

