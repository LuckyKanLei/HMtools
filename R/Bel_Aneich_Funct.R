#' @title loesen die Bodenstationen
#' @importFrom raster rowFromY colFromX
#' @importFrom stats na.omit
#' @param Rd_Data Radar Daten in raster.
#' @param BS_Data Bodenstation Daten in data.frame mit(am mindesten) "Lat", "Lon", "Value".
#' @param pixeln_Weit Pixeln weit vom Such, 1 = 9-Pixeln, 2 = 25-Pixeln des Umfangs.
#' @param Schwellewert Schwellewert fuer loesen.
#' @return neu Daten in data.frame
#' @export
delete_BS_A <- function(Rd_Data, BS_Data, pixeln_Weit = 1, Schwellewert = 0.1) {
  BS_Data$Value <- replace(BS_Data$Value, BS_Data$Value < Schwellewert, NA)
  BS_Data <- na.omit(BS_Data)
  Pixel_N <- (2 * pixeln_Weit + 1)^2
  BS_row <- rowFromY(Rd_Data, BS_Data$Lat)
  BS_col <- colFromX(Rd_Data, BS_Data$Lon)
  n_BS <- dim(BS_Data)[1]
  Pixeln <- matrix(NA, n_BS, Pixel_N)
  for (i in 1:n_BS) {
    Pixeln[i,] <- Rd_Data[(BS_row[i]-pixeln_Weit):(BS_row[i]+pixeln_Weit),(BS_col[i]-pixeln_Weit):(BS_col[i]+pixeln_Weit)]
  }

  Max_value <- as.numeric(apply(Pixeln, 1, function(a) max(a, na.rm = T)))
  BS_Data$Value <- replace(BS_Data$Value, Max_value < Schwellewert, NA)
  BS_Data <- na.omit(BS_Data)
  return(BS_Data)
}

#' @title Faktoren oder Differenz in Bodenstationen
#' @importFrom raster rowFromY colFromX
#' @param Rd_Data Radar Daten in raster.
#' @param BS_Data Bodenstation Daten in data.frame mit(am mindesten) "Lat", "Lon", "Value".
#' @param methond "A" oder "D", A = Faktorenverfahren, D = Differenyverfahren.
#' @param pixeln_Weit Pixeln weit vom Such, 1 = 9-Pixeln, 2 = 25-Pixeln des Umfangs.
#' @return Faktoren der Bodenstationen in data.frame
#' @export
faktor_BS <- function(Rd_Data, BS_Data, methond = "A", pixeln_Weit = 1) {
  Pixel_N <- (2 * pixeln_Weit + 1)^2
  BS_row <- rowFromY(Rd_Data, BS_Data$Lat)
  BS_col <- colFromX(Rd_Data, BS_Data$Lon)
  n_BS <- dim(BS_Data)[1]
  Pixeln <- matrix(NA, n_BS, Pixel_N)
  for (i in 1:n_BS) {
    Pixeln[i,] <- Rd_Data[(BS_row[i]-pixeln_Weit):(BS_row[i]+pixeln_Weit),(BS_col[i]-pixeln_Weit):(BS_col[i]+pixeln_Weit)]
  }

  ## suchen die änlichste Daten aus 9-Pixeln Gebiet ##

  Diff <- abs(Pixeln - matrix(rep(BS_Data$Value, Pixel_N), n_BS, Pixel_N))

  Min_Loc <- cbind(1:n_BS, as.numeric(apply(Diff, 1, which.min)))
  Ra_in_BS <- Pixeln[Min_Loc]
  ## Aneichungfaktoren in BS ####
  if(methond == "A"){
    Ra_in_BS <- replace(Ra_in_BS, Ra_in_BS < 0.1, NA)
    AF_BS <- BS_Data$Value / Ra_in_BS
  } else {
    AF_BS <- BS_Data$Value - Ra_in_BS
  }
  return(AF_BS)
}

#' @title Faktoren oder Differenz in Bodenstationen
#' @importFrom raster rowFromY colFromX
#' @param Rd_Data Radar Daten in raster.
#' @param BS_Data Bodenstation Daten in data.frame mit(am mindesten) "Lat", "Lon", "Value".
#' @param pixeln_Weit Pixeln weit vom Such, 1 = 9-Pixeln, 2 = 25-Pixeln des Umfangs.
#' @return Beste Daten der Bodenstationen in data.frame
#' @export
umfang_best <- function(Rd_Data, BS_Data, pixeln_Weit = 1) {
  Pixel_N <- (2 * pixeln_Weit + 1)^2
  BS_row <- rowFromY(Rd_Data, BS_Data$Lat)
  BS_col <- colFromX(Rd_Data, BS_Data$Lon)
  n_BS <- dim(BS_Data)[1]
  Pixeln <- matrix(NA, n_BS, Pixel_N)
  for (i in 1:n_BS) {
    Pixeln[i,] <- Rd_Data[(BS_row[i]-pixeln_Weit):(BS_row[i]+pixeln_Weit),(BS_col[i]-pixeln_Weit):(BS_col[i]+pixeln_Weit)]
  }
  Diff <- abs(Pixeln - matrix(rep(BS_Data$Value, Pixel_N), n_BS, Pixel_N))

  Min_Loc <- cbind(1:n_BS, as.numeric(apply(Diff, 1, which.min)))
  Ra_in_BS <- Pixeln[Min_Loc]

  return(Ra_in_BS)
}

#' @title Faktoren oder Differenz in Bodenstationen
#' @importFrom raster rowFromY colFromX xyFromCell ncell
#' @importFrom geosphere distm
#' @param Rd_Raster Radar Daten in raster.
#' @param BS_lon_lat Bodenstation Daten in data.frame nur mit "Lon" und "Lat".
#' @param Such_radius Wie weit sucht man die Bodenstation zu interpolatieren.
#' @return Gewichten füer jeden Pixel.
#' @export
radar_Gewicht <- function(BS_lon_lat, Rd_Raster, Such_radius = 1250){
  a <- log(0.5) * 20 / Such_radius
  n_cell_Ra <- ncell(Rd_Raster)
  Mat_d <- distm(BS_lon_lat, xyFromCell(Rd_Raster, 1:n_cell_Ra))
  Mat_d <- replace(Mat_d, Mat_d == 0, 0.001)
  Mat_d_Suchradius <- replace(Mat_d, Mat_d > Such_radius, NA)
  Temp_e <- exp(a * Mat_d)
  Mat_g <- Temp_e / (1 - Temp_e)
  Mat_g_such <- replace(Mat_g, Mat_d > Such_radius, NA)
  Mat_g_1 <- as.matrix(apply(Mat_g_such, 2, function(a) a / sum(a, na.rm = T)))
  return(Mat_g_1)
}

#' @title Interpolation fuer  oder Differenz in Bodenstationen
#' @param Mat_g Output aus "radar_Gewicht".
#' @param AF_BS Output aus Funktion "faktor_BS"
#' @return Interpolatiert Faktoren.
#' @export
interpolat_AF <- function(Mat_g, AF_BS){
  mat_Dim <- dim(Mat_g)
  Mat_Value <- matrix(rep(AF_BS, mat_Dim[2]), mat_Dim[1], mat_Dim[2])
  AF_Ra <- apply(Mat_g * Mat_Value, 2, function(a) sum(a, na.rm = T))
  NA_Index <- which(as.logical(apply(Mat_g, 2, function(a) all(is.na(a)))))
  AF_Ra[NA_Index] <- NA
  return(AF_Ra)
}

#' @title Interpolation mit original Kriging
#' @importFrom raster xFromCell yFromCell
#' @param Rd_Data Radar Daten in raster.
#' @param BS_Data Bodenstation Daten in data.frame mit(am mindesten) "Lat", "Lon", "Value".
#' @param cacuStationN wie viele Station zu berücksichtigen.
#' @param sAnstgVario Parameter für Kringing.
#' @return Interpolatiert Value.
#' @export
interpolat_Kriging<- function(BS_Data, Rd_Data, cacuStationN = 5, sAnstgVario = 4.25) {
  stationN <- dim(BS_Data)[1]
  n_cell_Ra <- ncell(Rd_Data)

  GridData <- Station2GridKrige(data.frame(ID = 1:stationN, Longitude = BS_Data$Lon, Latitude = BS_Data$Lat),
                                data.frame(ID = 1:n_cell_Ra, Longitude = xFromCell(Rd_Data, 1:n_cell_Ra), Latitude = yFromCell(Rd_Data, 1:n_cell_Ra)),
                                array(BS_Data$Value, dim = c(1,stationN,1)),
                                cacuStationN = cacuStationN, sAnstgVario = sAnstgVario)
  Rd_Data@data@values <- GridData
  return(Rd_Data)
}






