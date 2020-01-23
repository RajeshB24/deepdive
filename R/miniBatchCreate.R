
#' Title
#'
#' @param xrows
#' @param miniBatchSize
#' @param prevBatch
#'
#' @return
#' @export
#'
#' @examples
miniBatchCreate<-function(xrows,miniBatchSize,prevBatch){

  maxBatch<-ceiling(xrows/miniBatchSize)

  if(prevBatch==maxBatch){
    prevBatch=0
  }
  bacthNo=prevBatch+1

  batchupper<-bacthNo*miniBatchSize

  batchlower<-batchupper-miniBatchSize+1

  batchupper<-min(xrows,batchupper)
  miniBatchIndex<-data.frame(batchlower=batchlower,
                             batchupper=batchupper,
                             bacthNo=bacthNo)

  return(miniBatchIndex)
}



