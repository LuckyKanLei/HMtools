#' caculate the absolut abweichung
#' @param A vector, numica
#' @param B vector, numica
#' @return Num, sum(abs(A-B))
#' @examples
#' absolutabweichungfunk(c(1,5), c(2,3))
#' @export
absolutabweichungfunk <- function(A, B){  ##A,B sind moglichkeit
  return(sum(abs(A - B)))
}

#' caculate the absolut abweichung for vector
#' @param A vector, numica
#' @param B vector, numica
#' @return Num, abs(A-B)
#' @examples
#' absolutabweichungfunk(c(1,5), c(2,3))
#' @export
absolutabweichungVector <- function(A, B){  ##A,B sind moglichkeit
  return(abs(A - B))
}

#' optimisation with 0.618
#' @param paramekl numica, the small wert
#' @param paramegr numica, the big wert
#' @param DatenReferenz numica, the Ideal optimal solution
#' @param abweichfk Function, Deviation calculation function, default absolute error
#' @param processfk Function, Self function
#' @param umkriesn Num, how many search
#' @param ... other paramater for processfk
#' @return Num, the best wert Optimal solution
#' @export
eindim618funk <- function(processfk, paramekl, paramegr, DatenReferenz, abweichfk = absolutabweichungfunk, umkriesn, ...){
  paramelk <- paramekl
  paramerc <- paramegr
  abweischunglk <- abweichfk(DatenReferenz,processfk(paramelk, ...))
  abweischungrc <- abweichfk(DatenReferenz,processfk(paramerc, ...))
  for(i in 1:umkriesn){
    parametemrc <- paramelk + 0.618*(paramerc - paramelk)
    parametemlk <- paramerc  +0.618*(paramelk - paramerc)
    abweischungtemrc <- abweichfk(DatenReferenz, processfk(parametemrc, ...))
    abweischungtemlk <- abweichfk(DatenReferenz, processfk(parametemlk, ...))
    if(abweischungtemrc<abweischungtemlk){
      paramelk <- parametemlk
    }
    if(abweischungtemlk<abweischungtemrc){
      paramerc <- parametemrc
    }
  }
  return(mean(paramelk,paramerc))
}

#' optimisation with 0.618 for vector
#' @param LowerBound numica vector, the small wert
#' @param UpperBound numica vector, the big wert
#' @param idealReferenz numica or vector, the Ideal optimal solution
#' @param abweichfk Function, Deviation calculation function, default absolute error
#' @param processfk Function, Self function
#' @param umkriesn Num, how many search
#' @param ... other paramater for processfk
#' @return numica vector, the best wert Optimal solution
#' @export
eindim618Vector <- function(processfk, LowerBound, UpperBound, idealReferenz, abweichfk = absolutabweichungVector, umkriesn = 5, ...){
  lLow <- length(LowerBound)
  lUpp <- length(UpperBound)
  lRef <- length(idealReferenz)
  if(lLow != lUpp) stop("Please make sure LowerBound and UpperBound haven same size.")
  if((lRef == 1)) idealReferenz <- rep(idealReferenz, lLow) else{
    if(lRef != lLow) stop("Please make sure idealReferenz is a numical wert or the same size vector with LowerBound and UpperBound")
  }
  paramelk <- LowerBound
  paramerc <- UpperBound
  abweischunglk <- abweichfk(idealReferenz,processfk(paramelk, ...))
  abweischungrc <- abweichfk(idealReferenz,processfk(paramerc, ...))
  for(i in 1:umkriesn){
    parametemrc <- paramelk + 0.618*(paramerc - paramelk)
    parametemlk <- paramerc + 0.618*(paramelk - paramerc)
    abweischungtemrc <- abweichfk(idealReferenz, processfk(parametemrc, ...))
    abweischungtemlk <- abweichfk(idealReferenz, processfk(parametemlk, ...))
    judgeRC <- (abweischungtemrc < abweischungtemlk)
    paramelk <- parametemlk * judgeRC + paramelk * (!judgeRC)
    paramerc <- parametemrc * (!judgeRC) + paramerc * judgeRC
  }
  return(0.5 *(paramelk + paramerc))
}

