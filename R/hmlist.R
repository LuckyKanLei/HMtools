
#' @title hm.list s4 class
#' @importFrom methods new
#' @export hm.list
hm.list <- setClass("hm.list", contains = "list")

#' @title subsetting for hm.list
#' @description Extract the data
#' @importFrom  purrr map
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
#' @title subsetting geben for hm.list
#' @description Extract the data required by Locat in Data for list
#' @param Data the data, from it get the required data
#' @param i index
#' @param value value
#' @return new data
#' @export
'[<-.hm.list' <- function(Data, i, value){
  class(Data) <- "list"
  class(value) <- "list"

  DaNa <- names(Data)
  PuNa <- names(value)
  INa <- intersect(DaNa,PuNa)
  # indexPI <- as.integer(map(INa, function(a, b)which(b %in% a), PuNa))
  # PutData <- PutData[indexPI]

  for (j in INa) {
    Data[[j]] <- putList(Data[[j]], i, value[[j]])
    # eval(parse(text = paste0("Data$", INa[j], " <- ", "putList(", "value$", INa[j], ",", "Data$", INa[j], ",", i,")")))
  }
  class(Data) <- "hm.list"
  return(Data)
}
#' @title putList
#' @description put the data to a hm.list
#' @param Data the data, from it get the required data
#' @param i index
#' @param PutData PutData
#' @return list
#' @export
putList <- function(Data, i, PutData){
  DaNa <- names(Data)
  PuNa <- names(PutData)
  INa <- intersect(DaNa,PuNa)
  for(j in INa){
    dimN <- length(dim(Data[[j]]))
    if(dimN == 2) Data[[j]][i,] <- PutData[[j]]
    else if(dimN == 3) Data[[j]][i,,] <- PutData[[j]]
    else Data[[j]][i] <- PutData[[j]]

    # dimN <- length(dim(parse(text = paste0("Data$",INa[j]))))
    # if(dimN == 2) eval(parse(text = paste0("Data$", INa[j], "[i,]", " <- ", "PutData$", INa[j])))
    # else if(dimN == 3) eval(parse(text = paste0("Data$", INa[j], "[i,,]", " <- ", "PutData$", INa[j])))
    # else eval(parse(text = paste0("Data$", INa[j], "[i]", " <- ", "PutData$", INa[j])))

  }
  return(Data)
}

#' @title mergeData
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeData.hm.list <- function(Ori, New){
  clNa <- class(Ori)
  class(Ori) <- "list"
  class(New) <- "list"
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  DNNa <- setdiff(NeNa, OrNa)
  for (i in DNNa) {
    Ori[[i]] <- New[[i]]
  }
  for(i in INa){
    Ori[[i]] <- mergeList(Ori[[i]], New[[i]])
  }
  class(Ori) <- clNa
  return(Ori)
}
#' @title mergeData
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeList <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  DNNa <- setdiff(NeNa, OrNa)
  for (i in NeNa) {
    Ori[[i]] <- New[[i]]
  }
  return(Ori)
}

