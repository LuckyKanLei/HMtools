#' @title subsetting for HM.Data
#' @description Extract the data
#' @importFrom  purrr map
#' @param Data array in class t_vari.array
#' @param i index
#' @return Locat whith the required data
#' @export
getI_t_vari.array <- function(Data, i){
  dimN <- length(dim(Data))
  return(eval(parse(text = paste0("Data[c(", paste(i, collapse = ","),")",
                                  strrep(",", dimN - 1), "]"))))
}
#' @title subsetting for HM.Data
#' @description Extract the data
#' @importFrom  purrr map
#' @param Data array in class t_vari.array
#' @param i index
#' @return Locat whith the required data
#' @export

'[[.t_vari.array' <- getI_t_vari.array

#' @title getI_sub_hm.list
#' @description Extract the data required by Locat in Data for list
#' @import purrr
#' @param Data the data, from it get the required data
#' @param i index
#' @return Locat whith the required data
#' @export
getI_sub_hm.list <- function(Data, i){
  Out <- map(Data, getI_t_vari.array, i)
  return(Out)
}

#' @title subsetting for hm.list
#' @description Extract the data
#' @importFrom  purrr map
#' @param Data the data, from it get the required data
#' @param i index
#' @return Locat whith the required data
#' @export
'[.t_vari.hm.list' <- function(Data, i){
  Out <- map(Data, getI_sub_hm.list, i)
  return(Out)
}

#' @title subsetting geben for HM.Data
#' @description Extract the data required by Locat in Data for list
#' @param Data the data, from it get the required data
#' @param i index
#' @param value value
#' @return new data
#' @export
putI_t_vari.array <- function(Data, i, value){
  dimN <- length(dim(Data))
  eval(parse(text = paste0("Data[", i, strrep(",", dimN - 1), "] <- value")))
  return(Data)
}
#' @title subsetting geben for HM.Data
#' @description Extract the data required by Locat in Data for list
#' @param Data the data, from it get the required data
#' @param i index
#' @param value value
#' @return new data
#' @export
'[[<-.t_vari.array' <- putI_t_vari.array

#' @title putList
#' @description put the data to a HM.Data
#' @param Data the data, from it get the required data
#' @param i index
#' @param value PutData
#' @return list
#' @export
putI_sub_hm.list <- function(Data, i, value){
  DaNa <- names(Data)
  PuNa <- names(value)
  INa <- intersect(DaNa,PuNa)
  for(j in INa){
    Data[[j]][[i]] <- value[[j]]
  }
  return(Data)
}
#' @title subsetting geben for HM.Data
#' @description Extract the data required by Locat in Data for list
#' @param Data the data, from it get the required data
#' @param i index
#' @param value value
#' @return new data
#' @export
'[<-.t_vari.hm.list' <- function(Data, i, value){
  DaNa <- names(Data)
  PuNa <- names(value)
  INa <- intersect(DaNa,PuNa)
  for (j in INa) {
    Data[[j]] <- putI_sub_hm.list(Data[[j]], i, value[[j]])
  }
  return(Data)
}

#' @title merge_sub_list
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
merge_sub_list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  for (i in NeNa) {
    Ori[[i]] <- New[[i]]
  }
  return(Ori)
}

#' @title mergeData
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
left_merge <- function(Ori, New) UseMethod("left_merge", Ori)

#' @title mergeData
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
left_merge.hm.list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  DNNa <- setdiff(NeNa, OrNa)
  for (i in DNNa) {
    Ori[[i]] <- New[[i]]
  }

  for(i in INa){
    Ori[[i]] <- merge_sub_list(Ori[[i]], New[[i]])
  }
  return(Ori)
}

#' @title join_sub_list
#' @description join the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data joind from Ori and New
#' @export
join_sub_list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  for (i in INa) {
    Ori[[i]] <- New[[i]]
  }
  return(Ori)
}

#' @title left_join
#' @description join the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data joind from Ori and New
#' @export
left_join <- function(Ori, New) UseMethod("left_join", Ori)

#' @title left_join
#' @description join the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data joind from Ori and New
#' @export
left_join.hm.list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  for(i in INa){
    Ori[[i]] <- join_sub_list(Ori[[i]], New[[i]])
  }
  return(Ori)
}

#' @title delete_sub_list
#' @description delete the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data deleted from Ori and New
#' @export
delete_sub_list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  for (i in INa) {
    Ori[[i]] <- NULL
  }
  return(Ori)
}

#' @title left_delete
#' @description delete the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data deleted from Ori and New
#' @export
left_delete <- function(Ori, New) UseMethod("left_delete", Ori)

#' @title left_delete
#' @description delete the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data deleted from Ori and New
#' @export
left_delete.hm.list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  for(i in INa){
    Ori[[i]] <- delete_sub_list(Ori[[i]], New[[i]])
  }
  return(Ori)
}


