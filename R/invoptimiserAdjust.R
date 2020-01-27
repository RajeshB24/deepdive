
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
invoptimiserAdjust<-function(i,dw,optimiser,parMomentum,parRmsProp,parRmsPropZeroAdjust,
                          previousWeightUpdate,

                          previousWeightAdapt

                          ){



  if(optimiser%in%c("momentum","adam")){
    prev_Mw=previousWeightUpdate[[i]]

    Mw=parMomentum*prev_Mw+(1-parMomentum)*dw


    previousWeightUpdate[[i]]<-Mw
    }

  if(optimiser%in%c("rmsProp","adam")){
    prev_Rw=previousWeightAdapt[[i]]


    Rw=parRmsProp*prev_Rw+(1-parRmsProp)*(dw)^2


    previousWeightAdapt[[i]]<-Rw
    }

  if(optimiser=="momentum"){
    dw=Mw

  }else if(optimiser=="rmsProp"){
    dw=dw/(sqrt(Rw)+parRmsPropZeroAdjust)

  }else if(optimiser=="adam"){
    dw=Mw/(sqrt(Rw)+parRmsPropZeroAdjust)

  }




return(list(dw=dw,

            previousWeightUpdate=previousWeightUpdate,

            previousWeightAdapt=previousWeightAdapt

            ))

}

