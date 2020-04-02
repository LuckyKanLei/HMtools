#' caculate interpolation x in X, find y in Y
#' @param bktprm input x
#' @param BKTPRMLST X
#' @param ZLPRMLST Y
#' @return y
#' @examples
#' interpolation(2.5, 1:3, c(2, 8, 13))
#' @export
interpolation <- function(bktprm, BKTPRMLST, ZLPRMLST){
  constZero = 0.00000001
  n = length(BKTPRMLST)
  if(bktprm < BKTPRMLST[1]){
    zp <- (bktprm - BKTPRMLST[1]) * (ZLPRMLST[1] - ZLPRMLST[2]) /
      (BKTPRMLST[1] - BKTPRMLST[2] + constZero) + ZLPRMLST[1]

  }
  if(bktprm > BKTPRMLST[n]){
    zp <- (bktprm - BKTPRMLST[n-1]) * (ZLPRMLST[n-1] - ZLPRMLST[n]) /
      (BKTPRMLST[n-1] - BKTPRMLST[n] + constZero) + ZLPRMLST[n-1]

  }
  if(bktprm >= BKTPRMLST[1]) {
    for(i in 1:n){
      if(bktprm < BKTPRMLST[i]){
        zp <- (bktprm - BKTPRMLST[i-1])*(ZLPRMLST[i-1] - ZLPRMLST[i]) /
          (BKTPRMLST[i-1] - BKTPRMLST[i]+0.000000001) + ZLPRMLST[i-1]
        break()
      }
    }
  }
  return(zp)
}
