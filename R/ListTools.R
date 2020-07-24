# cropList <- function(df, li, i){
#   dfNa <- names(df)
#   liNa <- names(li)
#   judgeNa <- as.logical(map(dfNa, function(dfNa, liNa) any(dfNa == liNa), liNa))
#   if(any(!judgeNa)) {
#     stop(paste0("The list missing: ", paste(dfNa[which(!judgeNa)], collapse = ", "), ".\n"))
#   }
#   df <- as.data.frame(map(dfNa, function(dfNam, li, i) eval(parse(text = paste0("li$", dfNam, "[", i, ",]"))),li, i))
#   names(df) <- dfNa
#   return(df)
# }

#' @title getI
#' @description get the ith data of one demision data or get the ith row of two demisions data
#' @import purrr
#' @param Data Data
#' @param i index
#' @return the ith data of one demision data or get the ith row of two demisions data
#' @export
getI <- function(Data, i){
  Dim <- dim(Data)
  dimN <- length(Dim)
  if(dimN == 2 & Dim[1] > 1) return(Data[i,])
  else if(dimN == 3 & Dim[1] > 1) return(Data[i,,])
  else if(dimN == 2 & Dim[1] == 1) return(Data[1,])
  else if(dimN == 3 & Dim[1] == 1) return(Data[1,,])
  else return(Data[i])
}
#' @title putI
#' @description get the ith data of one demision data or get the ith row of two demisions data
#' @import purrr
#' @param Data Data
#' @param i index
#' @param PutD Put Data
#' @return put the ith data of one demision data or put the ith row of two demisions data
#' @export
putI <- function(Data, i, PutD){
  dimN <- length(dim(Data))
  if(dimN == 2) {
    Data[i,] <- PutD
    return(Data)}

  else if(dimN == 3) {
    Data[i,,] <- PutD
    return(Data)}
  else return(Data)
}

#' @title getData
#' @description Extract the data required by Locat in Data
#' @import purrr
#' @param Data the data, from it get the required data
#' @param Locat the loction, the data will in it put
#' @param i index
#' @return Locat whith the required data
#' @export
getData <- function(Data, Locat, i) UseMethod("getData", Locat)
#' @title getData.list
#' @description Extract the data required by Locat in Data for list
#' @import purrr
#' @param Data the data, from it get the required data
#' @param Locat the loction, the data will in it put
#' @param i index
#' @return Locat whith the required data
#' @export
getData.list <- function(Data, Locat, i){
  DaNa <- names(Data)
  LoNa <- names(Locat)
  INa <- intersect(DaNa,LoNa)

  indexID <- as.integer(map(INa, function(a, b)which(b %in% a), DaNa))
  Data <- Data[indexID]

  indexIL <- as.integer(map(INa, function(a, b)which(b %in% a), LoNa))
  Locat0 <- Locat[indexIL]
  # browser()
  Out <- map2(Data, Locat0, getData, i)
  Locat <- leftjoinData(Locat, Out)
  return(Locat)
}
#' @title getData.data.frame
#' @description Extract the data required by Locat in Data for data.frame
#' @import purrr
#' @param Data the data, from it get the required data
#' @param Locat the loction, the data will in it put
#' @param i index
#' @return Locat whith the required data
#' @export
getData.data.frame <- function(Data, Locat, i){
  DaNa <- names(Data)
  LoNa <- names(Locat)
  INa <- intersect(DaNa,LoNa)

  indexI <- as.integer(map(INa, function(a, b)which(b %in% a), DaNa))
  Data <- Data[indexI]
  # browser()
  Out <- as.data.frame(map(Data, getI, i))
  Locat <- leftjoinData(Locat, Out)
  return(Out)
}
#' @title getData.default
#' @description Extract the data required by Locat in Data for matrix or array
#' @import purrr
#' @param Data the data, from it get the required data
#' @param Locat the loction, the data will in it put
#' @param i index
#' @return Locat whith the required data
#' @export
getData.default <- function(Data, Locat, i){
  # browser()
  Out <- getI(Data, i)
  names(Out) <- names(Locat)
  return(Out)
}

#' @title putData
#' @description put the data required by Locat in Data into Locat
#' @import purrr
#' @param Data the data, from it get the required data
#' @param Locat the loction, the data will in it put
#' @param i index
#' @return Locat whith the required data
#' @export
putData <- function(Data, Locat, i) UseMethod("putData", Data)
#' @title putData.list
#' @description put the data required by Locat in Data into Locat for list
#' @import purrr
#' @param Data the data, from it get the required data
#' @param Locat the loction, the data will in it put
#' @param i index
#' @return Locat whith the required data
#' @export
putData.list <- function(Data, Locat, i){
  DaNa <- names(Data)
  LoNa <- names(Locat)
  INa <- intersect(DaNa,LoNa)

  indexID <- as.integer(map(INa, function(a, b)which(b %in% a), DaNa))
  Data <- Data[indexID]

  indexIL <- as.integer(map(INa, function(a, b)which(b %in% a), LoNa))
  Locat0 <- Locat[indexIL]
  # browser()
  Out <- map2(Data, Locat0, putData, i)
  Locat <- leftjoinData(Locat, Out)
  return(Locat)
}
#' @title putData.data.frame
#' @description put the data required by Locat in Data into Locat for data,frame
#' @import purrr
#' @param Data the data, from it get the required data
#' @param Locat the loction, the data will in it put
#' @param i index
#' @return Locat whith the required data
#' @export
putData.data.frame <- function(Data, Locat, i){
  DaNa <- names(Data)
  LoNa <- names(Locat)
  INa <- intersect(DaNa,LoNa)

  indexI <- as.integer(map(INa, function(a, b)which(b %in% a), DaNa))
  Data <- Data[indexI]
  indexIL <- as.integer(map(INa, function(a, b)which(b %in% a), LoNa))
  Locat0 <- Locat[indexIL]
  for (j in 1:length(INa)) {
    Locat0[[j]][i,] <- Data[[j]]
  }
  Locat <- leftjoinData(Locat, Locat0)
  return(Locat)
}
#' @title putData.default
#' @description put the data required by Locat in Data into Locat for other datda, exp. matrix or array
#' @import purrr
#' @param Data the data, from it get the required data
#' @param Locat the loction, the data will in it put
#' @param i index
#' @return Locat whith the required data
#' @export
putData.default <- function(Data, Locat, i){
  if(length(Data) > 1) {
    Locat[i,] <- Data
  } else {
    Locat[i] <- Data
  }
  return(Locat)
}

#' @title mergeData
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeData <- function(Ori, New) UseMethod("mergeData", Ori)
#' @title mergeData.list
#' @description merge the data from Ori and New bei elments names for list
#' @import purrr
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeData.list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  DONa <- setdiff(OrNa, NeNa)
  DNNa <- setdiff(NeNa, OrNa)
  for (i in DNNa) {
    Ori[[i]] <- New[[i]]
  }
  for(i in INa){
    Ori[[i]] <- mergeData(Ori[[i]], New[[i]])
  }
  return(Ori)
}

# mergeData.list <- function(Ori, New){
#   OrNa <- names(Ori)
#   NeNa <- names(New)
#   INa <- intersect(OrNa, NeNa)
#   DONa <- setdiff(OrNa, NeNa)
#   DNNa <- setdiff(NeNa, OrNa)
#   indexOR <- which(OrNa %in% DONa)
#   OutOri <- Ori[indexOR]
#   indexNE <- which(NeNa %in% DNNa)
#   OutNew <- New[indexNE]
#   indexORI <- as.integer(map(INa, function(a, b)which(b %in% a), OrNa))
#   indexNEI <- as.integer(map(INa, function(a, b)which(b %in% a), NeNa))
#   TemORI <- Ori[indexORI]
#   TemNEI <- New[indexNEI]
#   OutI <- map2(TemORI, TemNEI, function(A, B) {
#     if(class(A) == "data.frame" || class(A) == "list") return(mergeData(A, B)) else return(B)
#   })
#   Out <- c(OutOri, OutI, OutNew)
#   class(Out) <- class(Ori)
#   return(Out)
# }

#' @title mergeData.data.frame
#' @description merge the data from Ori and New bei elments names for data.frame
#' @import purrr
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeData.data.frame <- function(Ori, New){

  INa <- intersect(names(Ori),names(New))
  DONa <- setdiff(names(Ori),names(New))
  DNNa <- setdiff(names(New),names(Ori))
  judgeOR <- as.data.frame(map(DONa, function(DNa, Da) DNa == names(Da), Ori))
  indexOR <- which(as.logical(rowSums(judgeOR)))
  OutOri <- Ori[indexOR]
  Out <- c(OutOri, New)
  Out <- as.data.frame(Out)
  return(Out)
}

#' @title leftjoinData
#' @description leftjoin the data from Left and Right bei elments names
#' @param Left the left data
#' @param Right the right data
#' @return a data leftjoind from Left and Right
#' @export
leftjoinData <- function(Left, Right) UseMethod("leftjoinData", Left)
#' @title leftjoinData.list
#' @description leftjoin the data from Left and Right bei elments names for list
#' @import purrr
#' @param Left the left data
#' @param Right the right data
#' @return a data leftjoind from Left and Right
#' @export
leftjoinData.list <- function(Left, Right){
  LeNa <- names(Left)
  RiNa <- names(Right)
  INa <- intersect(LeNa,RiNa)
  indexLI <- as.integer(map(INa, function(a, b)which(b %in% a), LeNa))
  indexRI <- as.integer(map(INa, function(a, b)which(b %in% a), RiNa))
  Left0 <- Left[indexLI]
  Right0 <- Right[indexRI]
  OutI <- map2(Left0, Right0, function(A, B) {
    if(class(A) == "data.frame" || class(A) == "list") return(leftjoinData(A, B)) else return(B)
  })
  Out <- mergeData(Left, OutI)
  return(Out)
}
#' @title leftjoinData.data.frame
#' @description leftjoin the data from Left and Right bei elments names for data.frame
#' @import purrr
#' @param Left the left data
#' @param Right the right data
#' @return a data leftjoind from Left and Right
#' @export
leftjoinData.data.frame <- function(Left, Right){
  LeNa <- names(Left)
  RiNa <- names(Right)
  INa <- intersect(LeNa,RiNa)
  indexRI <- as.integer(map(INa, function(a, b)which(b %in% a), RiNa))
  Right0 <- Right[indexRI]
  Out <- mergeData(Left, Right0)
  return(as.data.frame(Out))
}

#' @title interjoinData
#' @description innerjoin the data from Left and Right bei elments names
#' @param Left the left data
#' @param Right the right data
#' @return a data innerjoind from Left and Right
#' @export
interjoinData <- function(Left, Right) UseMethod("interjoinData", Left)
#' @title interjoinData.list
#' @description innerjoin the data from Left and Right bei elments names for list
#' @import purrr
#' @param Left the left data
#' @param Right the right data
#' @return a data innerjoind from Left and Right
#' @export
interjoinData.list <- function(Left, Right){
  LeNa <- names(Left)
  RiNa <- names(Right)
  INa <- intersect(LeNa,RiNa)
  indexLI <- which(LeNa %in% INa)
  indexRI <- which(RiNa %in% INa)
  Left <- Left[indexLI]
  Right <- Right[indexRI]
  Out <- map2(Left, Right, function(A, B) {
    if(class(A) == "data.frame" || class(A) == "list") return(interjoinData(A, B)) else return(B)
  })
  return(Out)
}
#' @title interjoinData.data.frame
#' @description innerjoin the data from Left and Right bei elments names for data.frame
#' @import purrr
#' @param Left the left data
#' @param Right the right data
#' @return a data innerjoind from Left and Right
#' @export
interjoinData.data.frame <- function(Left, Right){
  LeNa <- names(Left)
  RiNa <- names(Right)
  INa <- intersect(LeNa,RiNa)
  indexRI <- which(RiNa %in% INa)
  Out <- Right[indexRI]
  return(as.data.frame(Out))
}

#' @title deleteData
#' @description delete the data in Left, which ist(are) auch in Right bei elments names
#' @param Left the left data
#' @param Right the right data
#' @return a data deleted from Left
#' @export
deleteData <- function(Left, Right) UseMethod("deleteData", Left)
#' @title deleteData.list
#' @description delete the data in Left, which ist(are) auch in Right bei elments names for list
#' @import purrr
#' @param Left the left data
#' @param Right the right data
#' @return a data deleted from Left
#' @export
deleteData.list <- function(Left, Right){
  LeNa <- names(Left)
  RiNa <- names(Right)
  InNa <- intersect(LeNa,RiNa)
  DiNa <- setdiff(LeNa,RiNa)

  index <- as.integer(map(DiNa, function(a, b)which(b %in% a), LeNa))
  Left0 <- Left[index]

  indexLI <- as.integer(map(InNa, function(a, b)which(b %in% a), LeNa))
  indexRI <- as.integer(map(InNa, function(a, b)which(b %in% a), RiNa))
  Left <- Left[indexLI]
  Right <- Right[indexRI]

  Out0 <- map2(Left, Right, function(A, B) {
    if(class(A) == "data.frame" || class(A) == "list") return(deleteData(A, B))
  })
  Out <- c(Left0, Out0)
  Out[which(as.logical(map(Out, is.null)))] <- NULL
  return(Out)
}
#' @title deleteData.data.frame
#' @description delete the data in Left, which ist(are) auch in Right bei elments names for data.frame
#' @import purrr
#' @param Left the left data
#' @param Right the right data
#' @return a data deleted from Left
#' @export
deleteData.data.frame <- function(Left, Right){
  LeNa <- names(Left)
  RiNa <- names(Right)
  DiNa <- setdiff(LeNa,RiNa)
  if(length(DiNa) == 0) return()
  index <- which(LeNa %in% DiNa)
  Out <- Left[index]
  return(as.data.frame(Out))
}


#' put the atrribut
#' @param Data Data of list or data,frame
#' @param index index for the Data
#' @param AttrNa name of the attribut
#' @param AttrVa value of atrribut
#' @return data with atrribut
#' @export
putAttr <- function(Data, index, AttrNa, AttrVa){
  for(i in index) attr(Data[[i]], AttrNa) <- AttrVa
  return(Data)
}

#' put the unit atrribut
#' @param Data Data of list or data,frame
#' @param AttrVa value of unit
#' @return data with unit
#' @export
putUnit <- function(Data, AttrVa){
  for(i in 1: length(Data)) attr(Data[[i]], "Unit") <- AttrVa[i]
  return(Data)
}
