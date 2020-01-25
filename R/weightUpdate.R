
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
weightUpdate <- function(i,
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
                         sizeImpact) {

  weightMatrix<-AllWeights$weightMatrix
  baisUnits     <-AllWeights$baisUnits
  previousWeightUpdate <-AllWeights$previousWeightUpdate
  previousBiasUpdate   <-AllWeights$previousBiasUpdate

  previousWeightAdapt <-AllWeights$previousWeightAdapt
  previousBiasAdapt   <-AllWeights$previousBiasAdapt


  hw <- as.matrix(y-ypred )



  if (i == 1) {
     #input to first layer
    #hin <- as.matrix(cbind(const = rep(1, nrow(x)), x))
    hin<-x


  } else{
     #input to hidden layer
        hin <- feedOut[[i - 1]]
  }

  for (wn in length(weightMatrix):min(length(weightMatrix), i + 1)) {


    if(activation[i]=="relu"){

      hw = hw %*% t(weightMatrix[[wn]])  * ifelse(zin[[wn - 1]] < 0,reluLeak, 1)
    }else if(activation[i]=="none"){

      hw = hw %*% t(weightMatrix[[wn]])


    }else if(activation[i]=="sigmoid"){

      hw = (hw %*% t(weightMatrix[[wn]]))  * (feedOut[[wn - 1]]*(1-feedOut[[wn - 1]]))
    }else if(activation[i]=='sin'){

      hw = (hw %*% t(weightMatrix[[wn]]))  * cos(zin[[wn - 1]])
    }else if(activation[i]=='cos'){

      hw = (hw %*% t(weightMatrix[[wn]]))  * sin(zin[[wn - 1]])
    }


  }

  bw=matrix(colSums(hw),byrow = T)
  hw = t(t(hw) %*% hin)

  gradientMax= abs(gradientClip*weightMatrix[[i]])
  hw= pmin(gradientMax,abs(hw))*(hw/abs(hw))


#  if(optimiser %in% "momentum"){
#     prev_hw<-previousWeightUpdate[[i]]
 #     prev_bw<-previousBiasUpdate[[i]]

#     hw=parMomentum*prev_hw+(1-parMomentum)*hw
 #      bw=parMomentum*prev_bw+(1-parMomentum)*bw

  #     previousWeightUpdate[[i]]=hw
   #    previousBiasUpdate[[i]]=bw }


  if(optimiser %in% c("momentum","rmsProp","adam")){


    adjOut<-optimiserAdjust(i,hw,bw,optimiser,parMomentum,parRmsProp,
                              parRmsPropZeroAdjust,
                              previousWeightUpdate,
                              previousBiasUpdate,
                              previousWeightAdapt,
                              previousBiasAdapt)

    hw=adjOut$dw
    bw=adjOut$db

    previousWeightUpdate=adjOut$previousWeightUpdate
    previousBiasUpdate=adjOut$previousBiasUpdate
    previousWeightAdapt=adjOut$previousWeightAdapt
    previousBiasAdapt=adjOut$previousBiasAdapt

  }







  weightMatrix[[i]] <- weightMatrix[[i]] +  sizeImpact*eta * hw
                                         - regularisePar*weightMatrix[[i]]
  #For first bias has been included in x
  if(i>1){
  baisUnits[[i]]<-baisUnits[[i]]+sizeImpact*eta*bw
  }


  return(list(weightMatrix=weightMatrix,
              baisUnits=baisUnits,
              previousWeightUpdate=previousWeightUpdate,
              previousBiasUpdate=previousBiasUpdate,
              previousWeightAdapt=previousWeightAdapt,
              previousBiasAdapt=previousBiasAdapt,
              ypred=ypred
              ))
}
