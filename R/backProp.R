
#' BackProp for deepnet

#' @return
#'
#' @noRd

backProp <- function(x,y,
                     weightMatrix,
                     activation,reluLeak,
                     modelType,
                     eta,gradientClip,baisUnits,
                     regularisePar,
                     itr,optimiser,parMomentum,
                     parRmsProp,parRmsPropZeroAdjust,
                     previousWeightUpdate,
                     previousBiasUpdate,
                     previousWeightAdapt,
                     previousBiasAdapt,
                     inputSizeImpact ) {

  sizeImpact= min(1,1/(inputSizeImpact*nrow(x)))
  feedList <-  feedForward(x, weightMatrix, activation,reluLeak, modelType,baisUnits)

  feedOut <- feedList$a_output
  zin <- feedList$z_in

  ypred = feedOut[[length(feedOut)]]
  #costFun <- sum((ypred - y) ^ 2)

  for (i in length(weightMatrix):1) {

    if (i == length(weightMatrix)) {
      #Output layer weight updates


      if(optimiser=="gradientDescent"){
      dw=t(t(y-ypred) %*%feedOut[[i -1]])
      db=sum(( y-ypred))
      }
      else if(optimiser %in% c("momentum","rmsProp","adam")){

             dw=t(t(y-ypred) %*%feedOut[[i -1]])
             db=sum((y-ypred))


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
               previousBiasAdapt[[i]]<-Rb
               }

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



         }



      weightMatrix[[i]] = weightMatrix[[i]] + sizeImpact* eta *dw - regularisePar*weightMatrix[[i]]



      baisUnits[[i]] = baisUnits[[i]] + sizeImpact* eta * db

      AllWeights<-list(weightMatrix=weightMatrix,
                       baisUnits=baisUnits,
                       previousWeightUpdate=previousWeightUpdate,
                       previousBiasUpdate=previousBiasUpdate,
                       previousWeightAdapt=previousWeightAdapt,
                       previousBiasAdapt=previousBiasAdapt,
                       ypred=ypred)


    } else{

      AllWeights <- weightUpdate(i, AllWeights, feedOut,
                                 x, y, ypred,
                                 zin,activation,reluLeak,
                                 eta,gradientClip,
                                 regularisePar,itr,
                                 optimiser,
                                 parMomentum,parRmsProp,parRmsPropZeroAdjust,
                                 sizeImpact)



    }

  }
  return(AllWeights)
}

