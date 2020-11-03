#' Station-Data interpolation to Grid-Data with point-Krige
#' @param StationLocation n* 3(ID, Latitude, Longitude) 2-array of station
#' @param GridLocation n * 3(ID, Latitude, Longitude) 2-array of Grid
#' @param StationData 3-array(periodN, stationN, fieldN) of data from station
#' @param cacuStationN = 5 how many Station are relevant for a grid
#' @param wightCoefficient = 2 wight Coefficient k, 1-5
#' @param sAnstgVario = 4.25 linearen Variogrammes s
#' @importFrom plyr join
#' @return Ntime * Nstation * Nfild 3-array of data for Grids
#' @examples
#' GridMetroData <- Station2GridKrige(SLC, GLC, SD)
#' @export
Station2GridKrige <- function(StationLocation, GridLocation, StationData,
                              cacuStationN = 5, wightCoefficient = 2, sAnstgVario = 4.25){

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
  LengthG2S <- sqrt(LengthG2STemLong + LengthG2STemLati) * sAnstgVario ## V*

  G2Ssamllst5S = LengthG5S <- array(0.0, c(gridN, cacuStationN))
  for (i in 1:gridN) {
    G2Ssamllst5S[i,] <- order(LengthG2S[i,])[1:cacuStationN]
    LengthG5S[i,] <- LengthG2S[i,G2Ssamllst5S[i,]]
  }
  # G2Ssamllst5S[1,] <- G2Ssamllst5S[2,] <- c(1:5)
  # LengthG5S <- LengthG2S


  arG2Ssamllst5S <- array(t(as.matrix(G2Ssamllst5S)), dim = length(as.matrix(G2Ssamllst5S)))
  dfG2Ssamllst5S <- data.frame(ID = arG2Ssamllst5S)

  Grid5StationXY <- join(dfG2Ssamllst5S, StationLocation)
  maLat <- matrix(Grid5StationXY$Latitude, gridN, cacuStationN, byrow = T)
  maLon <- matrix(Grid5StationXY$Longitude, gridN, cacuStationN, byrow = T)
  arSolveOprtor <- arLgrgeOprtor <- array(0, dim = c(gridN, cacuStationN + 1, cacuStationN + 1))
  arLat1 <- array(maLat, dim = c(gridN, cacuStationN, cacuStationN ))
  arLat2 <- aperm(arLat1, c(1,3,2))
  LatTem <- fctlengthtem(arLat1, arLat2)

  arLon1 <- array(maLon, dim = c(gridN, cacuStationN, cacuStationN ))
  arLon2 <- aperm(arLon1, c(1,3,2))
  LonTem <- fctlengthtem(arLon1, arLon2)
  arDistance <- sqrt(LatTem + LonTem) * sAnstgVario
  arLgrgeOprtor[,1:cacuStationN, 1:cacuStationN] <- arDistance
  arLgrgeOprtor[,1+cacuStationN, 1:cacuStationN] <- 1.
  arLgrgeOprtor[,1:cacuStationN, 1+cacuStationN] <- 1.
  Gweight <- matrix(0,gridN, cacuStationN + 1)
  WeightG2S <- matrix(0,gridN,stationN)
  for (i in 1:gridN) {
    arSolveOprtor[i,,] <- solve(arLgrgeOprtor[i,,]) ## V-1
    Gweight[i,] <- arSolveOprtor[i,,] %*% c(LengthG5S[i,],1) ## for Standart Fehler
    WeightG2S[i,G2Ssamllst5S[i,]] <- Gweight[i,1:cacuStationN]
  }
  for (i in 1:fieldN) {
    GridData[,,i] <- StationData[,,i] %*% t(WeightG2S)
  }

  return(GridData)
}
