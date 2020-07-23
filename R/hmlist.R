
#' @title getData.list
#' @importFrom methods new
#' @export hm.list
hm.list <- setClass("hm.list", contains = "list")
#' @title getData.list
#' @description Extract the data required by Locat in Data for list
#' @import purrr
#' @param Data the data, from it get the required data
#' @param i index
#' @return Locat whith the required data
#' @export
'[.hm.list' <- function(Data, i){
  Out <- map(Data, getListI, i)
  class(Out) <- class(Data)
  return(Out)
}
#' @title getListI
#' @description Extract the data required by Locat in Data for list
#' @import purrr
#' @param List the data, from it get the required data
#' @param i index
#' @return Locat whith the required data
#' @export
getListI <- function(List, i){
  Out <- map(List, getI, i)
  # class(Out) <- class(List)
  return(Out)
}
#' @title putData.list
#' @description Extract the data required by Locat in Data for list
#' @import purrr
#' @param Data the data, from it get the required data
#' @param i index
#' @param value value
#' @return Locat whith the required data
#' @export
'[<-.hm.list' <- function(Data, i, value){
  DaNa <- names(Data)
  PuNa <- names(value)
  INa <- intersect(DaNa,PuNa)
  # indexPI <- as.integer(map(INa, function(a, b)which(b %in% a), PuNa))
  # PutData <- PutData[indexPI]
  for (j in 1:length(INa)) {
    eval(parse(text = paste0("Data$", INa[j], " <- ", "putList(", "value$", INa[j], ",", "Data$", INa[j], ",", i,")")))
  }
  return(Data)
}
#' @title putData.list
#' @description Extract the data required by Locat in Data for list
#' @param Data the data, from it get the required data
#' @param i index
#' @param PutData PutData
#' @return Locat whith the required data
#' @export
putList <- function(PutData, Data, i){
  DaNa <- names(Data)
  PuNa <- names(PutData)
  INa <- intersect(DaNa,PuNa)
  for(j in 1:length(INa)){
    dimN <- length(dim(parse(text = paste0("Data$",INa[j]))))
    if(dimN == 2) eval(parse(text = paste0("Data$", INa[j], "[i,]", " <- ", "PutData$", INa[j])))
    else if(dimN == 3) eval(parse(text = paste0("Data$", INa[j], "[i,,]", " <- ", "PutData$", INa[j])))
    else eval(parse(text = paste0("Data$", INa[j], "[i]", " <- ", "PutData$", INa[j])))

  }
  return(Data)
}
