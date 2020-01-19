#MultiLayer and MultiUnits

multiLayerIntialiser<-function(layerChoice,
                               unitsChoice,
                               networkCount,seed){
  allnets<- sample(layerChoice,networkCount,replace = T)
  multiLayerList<-lapply(allnets, function(l){sample(unitsChoice,l,replace = T)})
  return(multiLayerList)
}

