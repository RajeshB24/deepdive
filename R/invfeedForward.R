


#' FeedForward for deepnet

#' @return
#' @noRd
#'

invfeedForward <-
  function(xBiased,
           weightMatrix,
           activation,
           reluLeak,
           modelType) {
    a_output = list()
    z_in = list()


    for (i in 1:length(weightMatrix)) {

      if (i == 1) {
        a_input <- xBiased

        z_input <- as.matrix(a_input) %*% weightMatrix[[i]]
      } else{
        z_input <- as.matrix(cbind(a_input,1)) %*% weightMatrix[[i]]


      }





      if (i < length(weightMatrix)) {
        if (activation[i] == "relu") {
          a_input <- ifelse(z_input < 0, reluLeak, z_input)

        } else if (activation[i] == "none") {
          a_input <- z_input

        }

      } else{
        if (modelType == "regress") {
          a_input <- z_input
        } else if (modelType == "binary") {
          a_input <- (1 / (1 + exp(-z_input)))

        } else if (modelType == "multiClass") {
          a_input <-  exp(z_input) / rowSums(exp(z_input))
        }

      }





      a_output[[i]] <- a_input
      z_in[[i]] <- z_input
    }




    return(list(a_output = a_output,
                z_in = z_in))
  }
