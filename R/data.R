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



#' string for message
#' @format 'chr'
"AtomMess"

#' string for message
#' @format 'chr'
"CheckOkMess"

#' string for message
#' @format 'chr'
"BoundryString"

#' string for message
#' @format 'chr'
"WaringString"

#' string for message
#' @format 'chr'
"viewNote"

#' string for message
#' @format 'chr'
"checkNote"





# AtomMess <- "(It is a atom element, not a List)"
# CheckOkMess <- "Checked OK!\n"
# BoundryString <- "****************************************************\n"
# WaringString <- "|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|o|\n"
# viewNote <- "The \"VIEW\" mode returns a list of Arguments and output values(Out) for easy access to the input requirements and the output data structure.\n"
# checkNote <- paste0("If \"CHECK\" mode reports an error or errors, please correct the error or errors and run \"CHECK\" mode again.\n",
#                     "You can also view the specific structure of input Arguments through mode \"VIEW\".")
# use_data(AtomMess,
#          CheckOkMess,
#          BoundryString,
#          WaringString,
#          viewNote,
#          checkNote, internal = T)
