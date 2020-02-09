
#' shuffler for varialble importance
#'
#' @param x
#' @param colname
#' @param y
#' @param model
#' @param modelError
#' @param seed
#'
#' @return
#' @noRd
#'
#' @examples
shuffler<-function(x,colname,y,model,modelError,seed){


  set.seed(seed)
  shuffleDf<-x


  shuffleDf[,colname]<- x[sample(1:nrow(x),nrow(x)),colname]

 if( sum(shuffleDf[,colname]==x[,colname])==nrow(x)){
   set.seed(seed*100)

   shuffleDf[,colname]<- x[sample(1:nrow(x),nrow(x)),colname]
 }

  if( sum(shuffleDf[,colname]==x[,colname])==nrow(x)){


   warning( paste0(colname," did not shuffle.Try changing Seed"))
  }

  shufflePred<- predict(model,shuffleDf)



  if(model$modelType=="multiClass"){
    names(shufflePred)<-stringr::str_remove(names(shufflePred),"pred_")

    shufflePred<-shufflePred[,!names(shufflePred)%in%"ypred"]

    for(i in 1:ncol(shufflePred)){
      shufflePred[,i]<-as.numeric(shufflePred[,i])
    }

  }



  shuffleError<-sqrt(mean((as.matrix(y-shufflePred)^2)))



  errorDiff<-shuffleError-modelError
  return(errorDiff)

}
