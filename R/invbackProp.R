
1
#' BackProp for deepnet

#' @return
#'
#' @noRd


invbackProp <- function(x,
                     y,
                     weightMatrix,
                     activation,
                     reluLeak,
                     modelType,
                     eta,
                     gradientClip,
                     regularisePar,
                     itr,
                     optimiser,
                     parMomentum,
                     parRmsProp,
                     parRmsPropZeroAdjust,
                     previousWeightUpdate,
                     previousWeightAdapt,
                     inputSizeImpact) {

  oldWeights=weightMatrix
  sizeImpact = min(1, 1 / (inputSizeImpact * nrow(x)))
  feedList <-invfeedForward(cbind(x,1), weightMatrix, activation, reluLeak, modelType)

  feedOut <- feedList$a_output
  zin <- feedList$z_in

  ypred = feedOut[[length(feedOut)]]


  for (i in length(weightMatrix):1) {

    if (i == length(weightMatrix)) {

      #Output layer weight updates



        if(modelType=="regress"){

          inputToLayer=feedOut[[i - 1]]

        dw = corpcor::pseudoinverse(cbind(inputToLayer,1)) %*% (y - ypred)
          }



      if (optimiser %in% c("momentum", "rmsProp", "adam")) {


        if (optimiser %in% c("momentum", "adam")) {
          prev_Mw = previousWeightUpdate[[i]]

          Mw = parMomentum * prev_Mw + (1 - parMomentum) * dw


          previousWeightUpdate[[i]] <- Mw

        }


        if (optimiser %in% c("rmsProp", "adam")) {
          prev_Rw = previousWeightAdapt[[i]]

          Rw = parRmsProp * prev_Rw + (1 - parRmsProp) * (dw) ^ 2


          previousWeightAdapt[[i]] <- Rw

        }

        if (optimiser == "momentum") {
          dw = Mw

        } else if (optimiser == "rmsProp") {
          dw = dw / (sqrt(Rw) + parRmsPropZeroAdjust)


        } else if (optimiser == "adam") {
          dw = Mw / (sqrt(Rw) + parRmsPropZeroAdjust)

        }



      }



      weightMatrix[[i]] = weightMatrix[[i]] + sizeImpact * eta * dw - regularisePar *weightMatrix[[i]]


       if(modelType=="regress"){
         zNxtLayerExp=y
       }else{
         #write reverse code based on activation function
       }

      AllWeights <- list(

        weightMatrix = weightMatrix,
        previousWeightUpdate = previousWeightUpdate,
        previousWeightAdapt = previousWeightAdapt,
        ypred = ypred,
        zNxtLayerExp=zNxtLayerExp,
        oldWeights=oldWeights
      )


    } else{

      #hidden and input layer weight updates

      AllWeights <- invweightUpdate(
        i,
        AllWeights,
        feedOut,
        x,
        y,
        ypred,
        zin,
        activation,
        reluLeak,
        eta,
        gradientClip,
        regularisePar,
        itr,
        optimiser,
        parMomentum,
        parRmsProp,
        parRmsPropZeroAdjust,
        sizeImpact,modelType
      )



    }

  }
  return(AllWeights)
}
