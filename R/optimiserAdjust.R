
#' OptimserAdjustments to Gradent for deepnet
#'
#' @param i
#' @param dw
#' @param db
#' @param optimiser
#' @param parMomentum
#' @param parRmsProp
#' @param parRmsPropZeroAdjust
#' @param previousWeightUpdate
#' @param previousBiasUpdate
#' @param previousWeightAdapt
#' @param previousBiasAdapt
#'
#' @return
#' @noRd
#'
#' @examples
optimiserAdjust<-function(i,dw,db,optimiser,parMomentum,parRmsProp,parRmsPropZeroAdjust,
                          previousWeightUpdate,
                          previousBiasUpdate,
                          previousWeightAdapt,
                          previousBiasAdapt
                          ){



  if(optimiser%in%c("momentum","adam")){
    prev_Mw=previousWeightUpdate[[i]]
    prev_Mb=previousBiasUpdate[[i]]
    Mw=parMomentum*prev_Mw+(1-parMomentum)*dw
    Mb=parMomentum*prev_Mb+(1-parMomentum)*db

    previousWeightUpdate[[i]]<-Mw
    previousBiasUpdate[[i]]<-Mb}

  if(optimiser%in%c("rmsProp","adam")){
    prev_Rw=previousWeightAdapt[[i]]
    prev_Rb=previousBiasAdapt[[i]]

    Rw=parRmsProp*prev_Rw+(1-parRmsProp)*(dw)^2
    Rb=parRmsProp*prev_Rb+(1-parRmsProp)*(db)^2

    previousWeightAdapt[[i]]<-Rw
    previousBiasAdapt[[i]]<-Rb}

  if(optimiser=="momentum"){
    dw=Mw
    db=Mb
  }else if(optimiser=="rmsProp"){
    dw=dw/(sqrt(Rw)+parRmsPropZeroAdjust)
    db=db/(sqrt(Rb)+parRmsPropZeroAdjust)
  }else if(optimiser=="adam"){
    dw=Mw/(sqrt(Rw)+parRmsPropZeroAdjust)
    db=Mb/(sqrt(Rb)+parRmsPropZeroAdjust)
  }




return(list(dw=dw,
            db=db,
            previousWeightUpdate=previousWeightUpdate,
            previousBiasUpdate=previousBiasUpdate,
            previousWeightAdapt=previousWeightAdapt,
            previousBiasAdapt=previousBiasAdapt
            ))

}

