#' High resolution data reclassfy to low resolution data: N*N data to 1 data, and in the table(ID, Code, )
#' @importFrom utils read.table
#' @param OriginalWert original wert grid-data aus ArcGIS, there are 6 row ist meta data
#' @param classifyN intger how many data to 1 data
#' @param GridID 2-array, the gridID in geological
#' @param leftDropN how many grid in left in High resolution data would deleate
#' @param upDropN how many grid in up in High resolution data would deleate
#' @return a new data in datafram(crid ID, wert, rate) mit low resolution data
#' @examples
#' GL = fctClassify(OGL, GID, 20, 0, 5)
#' @export
fctClassify <- function(OriginalWert, GridID, classifyN, leftDropN = 0, upDropN = 0){
  infoGridRowN = dim(GridID)[1]  #the rows number of FLOWDRIC
  infoGridColN = dim(GridID)[2]   #the clows number of FLOWDRIC
  if(leftDropN != 0) OriginalWert <- OriginalWert[, -(1:leftDropN)]
  if(upDropN != 0) OriginalWert <- OriginalWert[-(1:upDropN),]
  TableWert = array(0.0,c(1,3))
  for (i in 1:infoGridColN) {
    for (j in 1:infoGridRowN) {
      if(!is.na(GridID[j,i])){
        LanduseTem = as.matrix(OriginalWert[(1 + (j - 1) * classifyN):(j * classifyN),
                                            (1 + (i - 1) * classifyN):(i * classifyN)])
        LanduseArtTem = table(unlist(LanduseTem))
        lengthTem = length(LanduseArtTem)
        MatrixTem = array(GridID[j,i], c(lengthTem,3))
        MatrixTem[,2] = as.integer(dimnames(LanduseArtTem)[[1]])
        RateTem = as.integer(LanduseArtTem)
        rateTem2 = sum(RateTem)
        MatrixTem[,3] = RateTem / rateTem2
        TableWert = rbind(TableWert,MatrixTem)
      }
      else next
    }
  }
  TableWert <- TableWert[-1,]
  TableWert <- as.data.frame(TableWert)
  names(TableWert) = c("GridID", "Wert", "Rate")
  return(TableWert)
}


#' read grid-data from ArcGIS
#' @importFrom utils read.table
#' @param filePath filename grid-data aus ArcGIS, there are 6 row ist meta data
#' @param ... other paramter
#' @return a datafaram mit 6 attributs
#' @export
read.grid <- function(filePath, ...){
  MetaData = read.table(filePath, nrows = 6)
  NODATA_value = MetaData[6,2]
  GridWert = read.table(filePath, skip = 6, na.strings = NODATA_value)
  for (i in 1:6) attr(GridWert, as.character(MetaData[i,1])) = MetaData[i,2]
  return(GridWert)
}

#' read grid-data from ArcGIS
#' @importFrom plyr join
#' @param GridCodeTable datafram mit GridID, wert, Rate. name of Wert must same with first column of ParamLib.(output from fctClassfy)
#' @param gridN intger, how many grid.
#' @param ParamLib datafram, Data set corresponding to each type.
#' @return a datafram(gridN, paramN)
#' @examples
#' GL <- fctClassify(OGL, GID, 20, 0, 5)
#' names(GL)[2] <- "Code"
#' GLP <- gridParamJion(GL, 311, LLib)
#' @export
gridParamJion <- function(GridCodeTable, gridN, ParamLib){
  if(names(GridCodeTable)[2] != names(ParamLib)[1]) stop("Name of Wert(2.col name of GridCodeTable)  must same with first name col of ParamLib.",
                                                         "\nnow is: ", names(GridCodeTable)[2], " and ", names(ParamLib)[1])
  paramN <- dim(ParamLib)[2]
  GridParamTem = join(GridCodeTable, ParamLib)[,-2]
  GridParam = array(0.0, c(gridN, paramN))
  GridParam[,1] = 1:gridN
  for (i in 2:paramN) {
    GridParam[,i] = tapply(GridParamTem[,2] * GridParamTem[,1 + i],
                           GridParamTem[,1], sum)
  }
  colnames(GridParam) = names(ParamLib)
  GridParam = as.data.frame(GridParam)
}
