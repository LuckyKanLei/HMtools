#' caculate abstand
#' @param a n* 2(latiyude, longitude) matix of station
#' @param b n * 2(latiyude, longitude) 2-array of Grid
#' @return c
#' @export
fctlengthtem <- function(a,b){
  return((a-b)^2)
}

#' Station-Data interpolation to Grid-Data with IDW
#' @param StationLocation n* 3(ID, Latitude, Longitude) 2-array of station
#' @param GridLocation n * 3(ID, Latitude, Longitude) 2-array of Grid
#' @param StationData 3-array(periodN, stationN, fieldN) of data from station
#' @param cacuStationN = 5 how many Station are relevant for a grid
#' @param wightCoefficient = 2 wight Coefficient k, 1-5
#' @return Ntime * Nstation * Nfild 3-array of data for Grids
#' @examples
#' GridMetroData <- Station2GridIDW(SLC, GLC, SD)
#' @export
Station2GridIDW <- function(StationLocation, GridLocation, StationData,
                            cacuStationN = 5, wightCoefficient = 2){

  periodN <- dim(StationData)[1]
  stationN <- length(StationLocation$ID)
  fieldN <- dim(StationData)[3]
  gridN <- length(GridLocation$ID)
  message("gridN: ", gridN, "\nstationN: ", stationN, "\nperiodN: ", periodN, "\nfieldN: ", fieldN)
  stationN2 <- dim(StationData)[2]
  if(stationN != stationN2) stop("Make sure that the station data is a three-dimensional array (PeriodN, StationN, FieldN.)",
                                 "\n  ***now StationN is: ", stationN2, " and ", stationN)

  GridData <- array(0.0,dim = c(periodN, gridN, fieldN))
  LengthG2STemLong <- outer(GridLocation$Longitude, StationLocation$Longitude, fctlengthtem)
  LengthG2STemLati <- outer(GridLocation$Latitude, StationLocation$Latitude, fctlengthtem)
  LengthG2STem <- sqrt(LengthG2STemLong + LengthG2STemLati)
  LengthG2S <- LengthG2STem^(-wightCoefficient)
  LengthG2Ssamllst5S <- array(0.0, c(gridN, stationN))
  for (i in 1:gridN) {
    LengthG2Ssamllst5S[i,order(LengthG2S[i,])[1:cacuStationN]] <- LengthG2S[i,order(LengthG2S[i,])[1:cacuStationN]]
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
