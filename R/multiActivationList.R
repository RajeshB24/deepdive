

#' Activation List for Deepforest
#'
#' @param multiLayerList
#' @param activation
#'
#' @return
#' @export
#'
#' @examples
multiActivationList<-function(multiLayerList,activation,seed){

layerLengths<-sapply(multiLayerList , function(l){
       length(l)})


activationList<-lapply(1:length(layerLengths), function(m){
  set.seed(m*seed)
  sample(activation,layerLengths[m],replace = T)
})


return(activationList)

}
