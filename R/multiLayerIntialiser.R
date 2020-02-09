

#' Multi Layer Initialisation for deepforest
#'
#' @param layerChoice
#' @param unitsChoice
#' @param networkCount
#' @param seed
#'
#' @return
#' @noRd
#'
#' @examples
#'
multiLayerIntialiser<-function(layerChoice,
                               unitsChoice,
                               networkCount,seed){
  set.seed(seed)
  allnets<- sample(layerChoice,networkCount,replace = T)
  multiLayerList<-lapply(allnets, function(l){sample(unitsChoice,l,replace = T)})

  return(multiLayerList)
}


