#' discrete convolution
#' @param VectorSmall the Small Vector
#' @param VectorBig the Big Vector
#' @return wert of discrete convolution
#' @export
discrete_convolution <- function(VectorSmall,VectorBig){
  lengthSmall <- length(VectorSmall)
  lengthBig <- length(VectorBig)
  Tem <- matrix(0.0,(lengthBig + lengthSmall -1), lengthSmall)
  for (i in 1:lengthSmall) {
    Tem[i:(lengthBig-1+i),i] <- VectorBig * VectorSmall[i]
  }
  Result <- rowSums(Tem)
  return(Result)
}

