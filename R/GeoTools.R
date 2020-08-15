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

#' Aggregate a array(dim >=3)* object to create a lower resolution (larger cells).
#' Aggregation groups rectangular areas to create larger cells.
#' The value for the resulting cells is computed with a user-specified function.
#' @param OriginalWert Original array Wert
#' @param aggregateN integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically).
#' Or two integers (horizontal and vertical aggregation factor) or three integers (when also aggregating over layers).
#' @param aggregateFunction = colMeans, function used to aggregate values. Function must support vectorization of column as input, like base::colMeans.
#' @return a lower resolution array(same dimensions)
#' @export
aggregateArray <- function(OriginalWert, aggregateN, aggregateFunction = colMeans){
  dimOri <- dim(OriginalWert)
  if(length(dimOri) > 3){
    dim(OriginalWert) <- c(dimOri[1:2], prod(dimOri[-(1:2)]))
    warning("OriginalWert is not a 3-dimensional array, but a ", length(dimOri), "-dimensional array.
    In the calculation process, OriginalWert will be converted to a three-dimensional array before calculation.
    Therefore, there may be errors. Please check the results.")
  }
  dimO <- dim(OriginalWert)
  if(length(aggregateN) == 1) aggregateN <- c(aggregateN, aggregateN)
  nrows <- as.integer(dimO[1] / aggregateN[1])
  ncols <- as.integer(dimO[2] / aggregateN[2])
  tem3A <- array(NA, c(nrows, ncols, dimO[3]))
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      tem2A <- OriginalWert[((i - 1) * aggregateN[1] + 1):(i * aggregateN[1]),
                            ((j - 1) * aggregateN[2] + 1):(j * aggregateN[2]),]
      dim(tem2A) <- c(prod(aggregateN), dimO[3])
      tem3A[i,j,] <- aggregateFunction(tem2A)
    }
  }
  aggA <- tem3A
  dim(aggA) <- c(nrows, ncols, dimOri[-(1:2)])
  return(aggA)
}

#' caculate area for every grids
#' @importClassesFrom raster RasterLayer
#' @importFrom raster area
#' @param BasinRaster RasterLayer, values Are 1
#' @param IDGrid RasterLayer, values are ID
#' @param classifyN numica, classsify N
#' @return Grid Area in RasterLayer
#' @export
fctGridArea <- function(BasinRaster, IDGrid, classifyN){
  infoGridRowN = IDGrid@nrows  #the rows number of FLOWDRIC
  infoGridColN = IDGrid@ncols   #the clows number of FLOWDRIC

  GridAreaRate = IDGrid
  for (i in 1:infoGridColN) {
    for (j in 1:infoGridRowN) {
      if(!is.na(IDGrid[j,i])){
        GTem = as.matrix(BasinRaster[(1 + (j - 1) * classifyN):(j * classifyN),
                                     (1 + (i - 1) * classifyN):(i * classifyN)])
        ARate <- sum(!is.na(GTem))
        GridAreaRate[j,i] <- ARate / (classifyN^2)
      }
      else next
    }
  }
  GArea <- area(IDGrid)
  GridArea <- GridAreaRate * GArea
  return(GridArea)
}

#' caculate acculation from DirGrid
#' @importClassesFrom raster RasterLayer
#' @importFrom raster flowPath
#' @param DirGrid RasterLayer,from raster::terrain(dem, opt = "flowdir")
#' @param IDGrid RasterLayer,same size with DirGrid
#' @return RasterLayer, acculation
#' @export
fctAcculation <- function(DirGrid, IDGrid) {
  gN = length(IDGrid[!is.na(IDGrid)])
  AccBGrid <- IDGrid
  # CacuM <- diag(gN)
  CacuM <- matrix(0, gN, gN)
  for (i in IDGrid[!is.na(IDGrid)]) {
    PathL <- flowPath(DirGrid, which(IDGrid@data@values == i))
    CacuM[i, IDGrid@data@values[PathL]] = 1
  }
  AccIDL <- colSums(CacuM)
  AccBGrid[!is.na(IDGrid)] <- AccIDL
  return(AccBGrid)
}

#' caculate next grid from DirGrid
#' @importClassesFrom raster RasterLayer
#' @importFrom raster flowPath
#' @param IDGrid RasterLayer,same size with DirGrid
#' @param DirGrid RasterLayer,from raster::terrain(dem, opt = "flowdir")
#' @return RasterLayer, next grid ID
#' @export
fctNextGrid <- function(IDGrid, DirGrid) {
  NextGrid <- IDGrid
  for (i in IDGrid[!is.na(IDGrid)]) {
    PathL <- flowPath(DirGrid, which(IDGrid@data@values == i))
    NextGrid@data@values[PathL[1]] <- IDGrid@data@values[PathL[2]]
  }
  return(NextGrid)
}

#' caculate ID of Next Grid
#' @importClassesFrom raster RasterLayer
#' @importFrom raster flowPath
#' @param DirGrid RasterLayer,from raster::terrain(dem, opt = "flowdir")
#' @param IDGrid RasterLayer,same size with DirGrid
#' @return 2-array(gridN,2) gridID and Next Grid ID
#' @export
fctNextGrid <- function(DirGrid, IDGrid) {
  gN = length(IDGrid[!is.na(IDGrid)])
  NextGridID <- array(0,c(gN, 2))
  for (i in IDGrid[!is.na(IDGrid)]) {
    PathL <- fctFindNext(DirGrid, which(IDGrid@data@values == i))
    NextGridID[i,] = IDGrid@data@values[PathL[1:2]]
  }
  return(NextGridID)
}

#' find the netx grid ID starting at a given point.
#' @importFrom  raster raster cellFromXY rowFromCell colFromCell cellFromRowCol
#' @param x	RasterLayer of flow direction (as can be created with terrain
#' @param p	starting point. Either two numbers: x (longitude) and y (latitude) coordinates; or a single cell number
#' @param ... other parameter
#' @return numeric (start ID and next ID)
#' @export
fctFindNext <- function (x, p, ...) {

  r <- raster(x)
  if (length(p) > 1) {
    p <- cellFromXY(r, p[1:2])
  }
  cell <- p
  row <- rowFromCell(r, cell)
  col <- colFromCell(r, cell)
  nr <- nrow(r)
  nc <- ncol(r)
  path <- cell

  fd <- x[cell]
  row <- if (fd %in% c(32, 64, 128))
    row - 1
  else if (fd %in% c(8, 4, 2))
    row + 1
  else row
  col <- if (fd %in% c(32, 16, 8))
    col - 1
  else if (fd %in% c(128, 1, 2))
    col + 1
  else col
  cell <- cellFromRowCol(r, row, col)
  # if (is.na(x[cell]))
  #   break
  # if (cell %in% path)
  #   break
  path <- c(path, cell)
  return(path)

}




