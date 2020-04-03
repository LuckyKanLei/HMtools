#' location infomation of hydro station.
#' @format 'data.frame':	stationN obs. of  3 variables
#' \describe{
#' (The field names in these lists must be consistent and besause they are the only index.)
#' \item{ID}{id code of station}
#' \item{Latitude}{Latitude of station}
#' \item{Longitude}{Longitude of station}
#' }
"SLC"


#' location infomation of grids.
#' @format 'data.frame':	gridN obs. of  3 variables
#' \describe{
#' (The field names in these lists must be consistent and besause they are the only index.)
#' \item{ID}{id code of grids}
#' \item{Latitude}{Latitude of grids}
#' \item{Longitude}{Longitude of grids}
#' }
"GLC"

#' metrol infomation from hydro station.
#' @format 3-array(periodN, stationN, fieldN) of data from station
"SD"

#' grid-data of gridID
#' @format 'data.frame' or 2-array
#' \describe{
#' mit attributs
#' \item{NOdata}{is NA}
#' }
"GID"

#' grid-data of landuse.
#' @format 'data.frame' or 2-array
#' \describe{
#' mit attributs
#' \item{NOdata}{is NA}
#' }
"OGL"

#' paramters set of landuse.
#' @format 'data.frame' or 2-array(typN, paramN)
"LLib"
