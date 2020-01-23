#Below function returns intial weights for all layers


#' Title
#'
#' @param i
#' @param interVariableCount
#' @param interOutCount
#' @param seed
#' @param activation
#'
#' @return
#' @export
#'
#' @examples
weightInitialiser <- function(i,interVariableCount,interOutCount,seed,activation) {
  set.seed(seed)

                         weightMatrix<-matrix(runif(interVariableCount[i] * interOutCount[i],0,1)^10,
                                              nrow = interVariableCount[i])

  return(weightMatrix)
}

