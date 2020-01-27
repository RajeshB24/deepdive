
#' Weight Update to Gradients fro deepnet
#'
#' @param i
#' @param AllWeights
#' @param feedOut
#' @param x
#' @param y
#' @param ypred
#' @param zin
#' @param activation
#' @param reluLeak
#' @param eta
#' @param gradientClip
#' @param regularisePar
#' @param itr
#' @param optimiser
#' @param parMomentum
#' @param parRmsProp
#' @param parRmsPropZeroAdjust
#' @param sizeImpact
#'
#' @return
#'
#' @noRd
#' @examples
invweightUpdate <- function(i,
                         AllWeights,
                         feedOut,
                         x,
                         y,
                         ypred,
                         zin,
                         activation,reluLeak,
                         eta,
                         gradientClip,
                         regularisePar,
                         itr,
                         optimiser,
                         parMomentum,parRmsProp,parRmsPropZeroAdjust,
                         sizeImpact,
                         modelType
                         ) {

  oldWeights<-AllWeights$weightMatrix
  weightMatrix<-AllWeights$weightMatrix

  previousWeightUpdate <-AllWeights$previousWeightUpdate


  previousWeightAdapt <-AllWeights$previousWeightAdapt


  zNxtLayerExp<-AllWeights$zNxtLayerExp



nxtLayerWeights<-oldWeights[[i+1]]


  #  nxtLayerWeights<-weightMatrix[[i+1]]


     #Layer Expected Output Calculated Guess

    acurLayerExp=( 1/( nrow(nxtLayerWeights)-1))*
                 ((zNxtLayerExp-matrix(rep(nxtLayerWeights[nrow(nxtLayerWeights),],nrow(zNxtLayerExp)),
                            ncol = ncol(zNxtLayerExp),byrow = T))%*%
                 t( 1/nxtLayerWeights[-nrow(nxtLayerWeights),]))

    if(activation[i]=="relu"){

      zcurLayerExp=ifelse(acurLayerExp<0,acurLayerExp/reluLeak,acurLayerExp)
    }else if(activation[i]=="none"){
      zcurLayerExp=acurLayerExp
    }


  #Inverse Gradient

  if(i>1){

  hw=corpcor::pseudoinverse(cbind(feedOut[[i - 1]],1))%*%(zcurLayerExp-zin[[i]])
  }else{
    hw=corpcor::pseudoinverse(cbind(x,1))%*%(zcurLayerExp-zin[[i]])
  }


  #gradient Clipping
 # gradientMax= abs(gradientClip*weightMatrix[[i]])


 # hw= pmin(gradientMax,abs(hw))*(hw/abs(hw))


  if(optimiser %in% c("momentum","rmsProp","adam")){


    adjOut<-invoptimiserAdjust(i,hw,optimiser,parMomentum,parRmsProp,
                              parRmsPropZeroAdjust,
                              previousWeightUpdate,
                              previousWeightAdapt
                             )

    hw=adjOut$dw

    previousWeightUpdate=adjOut$previousWeightUpdate

    previousWeightAdapt=adjOut$previousWeightAdapt


  }







  weightMatrix[[i]] <- weightMatrix[[i]] +  sizeImpact*eta * hw
                                         - regularisePar*weightMatrix[[i]]


  return(list(weightMatrix=weightMatrix,
              previousWeightUpdate=previousWeightUpdate,
             previousWeightAdapt=previousWeightAdapt,
              ypred=ypred,
            zNxtLayerExp=zcurLayerExp
              ))
}
