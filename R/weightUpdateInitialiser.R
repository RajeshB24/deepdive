#Below intialises weight updates for momentum model


weightUpdateInitialiser <- function(i,interVariableCount,interOutCount,seed) {
  set.seed(seed)

  weightUpdateMatrix<-matrix(runif(interVariableCount[i] * interOutCount[i],0,0),
                         nrow = interVariableCount[i])
  return(weightUpdateMatrix)
}

