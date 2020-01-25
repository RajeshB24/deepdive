
#' weight Initialisation for deepnet
#'
#' @param i
#' @param interVariableCount
#' @param interOutCount
#' @param seed
#' @param activation
#'
#' @return
#' @noRd
#'
#' @examples
weightInitialiser <- function(i,interVariableCount,interOutCount,seed,activation) {
  set.seed(seed)

                         weightMatrix<-matrix(runif(interVariableCount[i] * interOutCount[i],0,1)^10,
                                              nrow = interVariableCount[i])

  return(weightMatrix)
}

