#' High resolution data reclassfy to low resolution data: N*N data to 1 data
#' @importFrom utils read.table
#' @param filePath filename
#' @param classifyN intger how many data to 1 data
#' @param GridID 2-array, the gridID in geological
#' @param leftDropN how many grid in left in High resolution data would deleate
#' @param upDropN how many grid in up in High resolution data would deleate
#' @return a new data mit low resolution data
#' @export
fctClassify <- function(filePath, GridID, classifyN, leftDropN = 0, upDropN = 0){
  infoGridRowN = dim(GridID)[1]  #the rows number of FLOWDRIC
  infoGridColN = dim(GridID)[2]   #the clows number of FLOWDRIC
  MetaData = read.table(filePath, nrows = 6)
  valueNoData = MetaData[6,2]
  colN = MetaData[1,2]
  rowN = MetaData[2,2]
  OriginalWert = read.table(filePath, skip = 6 + upDropN, na.strings = valueNoData)[,(1 + leftDropN):colN]
  GridWert = array(valueNoData,c(1,3))
  for (i in 1:infoGridColN) {
    for (j in 1:infoGridRowN) {
      if(GridID[j,i] != valueNoData){
        LanduseTem = as.matrix(OriginalWert[(1 + (j - 1) * classifyN):(j * classifyN), (1 + (i - 1) * classifyN):(i * classifyN)])
        LanduseArtTem = table(unlist(LanduseTem))
        lengthTem = length(LanduseArtTem)
        MatrixTem = array(GridID[j,i], c(lengthTem,3))
        MatrixTem[,2] = as.integer(dimnames(LanduseArtTem)[[1]])
        RateTem = as.integer(LanduseArtTem)
        rateTem2 = sum(RateTem)
        MatrixTem[,3] = RateTem / rateTem2
        GridWert = rbind(GridWert,MatrixTem)
      }
      else next
    }
  }
  return(GridWert[-1,])
}
