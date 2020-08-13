#' judge location translate matrix
#' @param lengthV forget
#' @param Vectorr Vector, input(x)
#' @param judge numic, judge wert
#' @return 2-matrix of location
#' @export
makeLTM <- function(Vectorr, lengthV, judge){
  Location <- matrix(0, lengthV, 2)
  Location[,1][which(Vectorr <= judge)] <- 1
  Location[,2][which(Vectorr > judge)] <- 1
  return(Location)
}

#' function for a 2 Piecewise
#' @param fct1 Function, first function, x <= judge
#' @param fct2 Function, second function, x > judge
#' @param Vectorr Vector, input(x)
#' @param judge numic, judge wert
#' @param ... other parameters'
#' @return Result output(y)
#' @export
fctPiecewiseFunction <- function(fct1, fct2, Vectorr, judge, ...){
  lengthV <- length(Vectorr)
  TranslatMatrix <- makeLTM(Vectorr, lengthV, judge)
  ResultTerm <- matrix(0, lengthV, 2)
  ResultTerm[,1] <- fct1(Vectorr, ...)
  ResultTerm[,2] <- fct2(Vectorr, ...)
  Result <- rowSums(ResultTerm * TranslatMatrix)
  return(Result)
}

#' Finding the minimum value of the corresponding position of two equal large vectors
#' @param VctA vector, numic, first Vector
#' @param VctB vector, numic, second Vector
#' @return vector, result
#' @examples
#' A = c(1, 2, 3)
#' B = c(2, 2, 2)
#' minVector(A, B)
#' @export
minVector <- function(VctA, VctB){
  VctC = VctA - VctB
  VctD = VctB
  VctD[which(VctC < 0.0)] = VctA[which(VctC < 0.0)]
  return(VctD)
}


#' Finding the maximum value of the corresponding position of two equal large vectors
#'
#' @param VctA vector, numic, first Vector
#' @param VctB vector, numic, second Vector
#' @return vector, result
#' @examples
#' A = c(1, 2, 3)
#' B = c(2, 2, 2)
#' maxVector(A, B)
#' @export
maxVector <- function(VctA, VctB){
  VctC = VctA - VctB
  VctD = VctA
  VctD[which(VctC < 0.0)] = VctB[which(VctC < 0.0)]
  return(VctD)
}

#' Finding the minimum value of one vectors and one wert
#'
#' @param WrtA numic
#' @param VctB vector, numic
#' @return vector, result
#' @examples
#' a = 2
#' B = c(1, 2, 3)
#' maxVector(a, B)
#' @export
minSVector <- function(WrtA, VctB){
  VctC = WrtA - VctB
  VctD = VctB
  VctD[which(VctC < 0.0)] = WrtA
  return(VctD)
}

#' Finding the maximum value of one vectors and one wert
#'
#' @param WrtA numic
#' @param VctB vector, numic
#' @return vector, result
#' @examples
#' a = 2
#' B = c(1, 2, 3)
#' maxVector(a, B)
#' @export
maxSVector <- function(WrtA, VctB){
  VctC = WrtA - VctB
  VctD = VctB
  VctD[which(VctC > 0.0)] = WrtA
  return(VctD)
}

#' rep a colVector to a matrix
#' @param colWert numic vector
#' @param dim2 intger, number of cols
#' @return a matrix
#' @export
colRep <- function(colWert, dim2) return(matrix(rep(colWert, dim2), ncol = dim2))

#' Adaptive quadrature of functions of one variable over a finite or infinite interval.
#' @importFrom stats integrate
#' @param lower the limits of integration. Can be infinite.
#' @param upper the limits of integration. Can be infinite.
#' @param f an R function taking a numeric first argument and returning a numeric vector of the same length. 
#' Returning a non-finite element will generate an error.
#' @param ... additional arguments to be passed to f.
#' @return value   the final estimate of the integral.
#' @export
integrateMAP2 <- function(lower, upper, f, ...){
  return((integrate(f,lower,upper, ...))$value)
}
#' Adaptive quadrature of functions of one variable over a finite or infinite interval.
#' @importFrom purrr map2
#' @param lower vector the limits of integration. Can be infinite.
#' @param upper vector the limits of integration. Can be infinite.
#' @param f an R function taking a numeric first argument and returning a numeric vector of the same length. 
#' Returning a non-finite element will generate an error.
#' @param ... additional arguments to be passed to f.
#' @return value a vector the final estimate of the integral.
#' @export
integrateVector <- function(f, lower, upper, ...){
  Tem <- map2(lower, upper, integrateMAP2, f, ...)
  Tem <- as.matrix(as.data.frame(Tem))
  attr(Tem,"dimnames") <- NULL
  return(as.vector(Tem))
}
