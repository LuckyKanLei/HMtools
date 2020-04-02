#' caculate abstand
#' @param a n* 2(latiyude, longitude) matix of station
#' @param b n * 2(latiyude, longitude) 2-array of Grid
#' @return c
#' @export
fctlengthtem <- function(a,b){
  return((a-b)^2)
}

#' Station-Data interpolation to Grid-Data with IDW
#' @param StationLocation n* 2(latiyude, longitude) matix of station
#' @param GridLocation n * 2(latiyude, longitude) 2-array of Grid
#' @param StationData Ntime * Nstation * Nfild 3-array of data from station
#' @param StationN how many Station are relevant for a grid
#' @return Ntime * Nstation * Nfild 3-array of data for Grids
#' @export
Station2GridIDW <- function(StationLocation, GridLocation, StationData, StationN = 5){

  periodN <- dim(StationData)[1]
  stationN <- dim(StationData)[2]
  fieldN <- dim(StationData)[3]
  gridN <- dim(GridLocation)[1]

  GridData <- array(0.0,dim = c(periodN, gridN, fieldN))
  LengthG2STemLong <- outer(GridLocation$longitude, StationLocation$longitude, fctlengthtem)
  LengthG2STemLati <- outer(GridLocation$latitude, StationLocation$latitude, fctlengthtem)
  LengthG2STem <- LengthG2STemLong + LengthG2STemLati
  LengthG2S <- LengthG2STem^0.5
  LengthG2Ssamllst5S <- array(0.0, c(gridN, stationN))
  for (i in 1:gridN) {
    LengthG2Ssamllst5S[i,order(LengthG2S[i,])[1:StationN]] <- LengthG2S[i,order(LengthG2S[i,])[1:StationN]]
  }
  LengthSum <- rowSums(LengthG2Ssamllst5S)
  WeightG2S5S <- array(0.0, c(gridN, stationN))
  for (i in 1:gridN) {
    WeightG2S5S[i,] <- LengthG2Ssamllst5S[i,] / LengthSum[i]
  }

  for (i in 1:fieldN) {
    GridData[,,i] <- StationData[,,i] %*% t(WeightG2S5S)
  }

  return(GridData)
}
