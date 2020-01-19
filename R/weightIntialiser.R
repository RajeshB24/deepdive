#Below function returns intial weights for all layers


weightInitialiser <- function(i,interVariableCount,interOutCount,seed,activation) {
  set.seed(seed)

if(activation=="sigmoid"){
  weightMatrix<-matrix(runif(interVariableCount[i] * interOutCount[i],-1,1),
                       nrow = interVariableCount[i])}else{
                         weightMatrix<-matrix(runif(interVariableCount[i] * interOutCount[i],0,1)^10,
                                              nrow = interVariableCount[i])
                       }
  return(weightMatrix)
}

