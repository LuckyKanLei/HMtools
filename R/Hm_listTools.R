#' @title subsetting for HM.Data
#' @description Extract the data
#' @importFrom  purrr map
#' @param Data array in class t_vari.array
#' @param i index
#' @return Locat whith the required data
#' @export
getI_t_vari.array <- function(Data, i){
  dimN <- length(dim(Data))
  if(is.null(dimN) | dimN == 1 | dimN == 0) return(Data[i])
  else return(eval(parse(text = paste0("Data[c(", paste(i, collapse = ","),")",
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
  if(is.numeric(value)) {
    for (j in DaNa) {
      Data[[j]] <- map(Data[[j]], putI_t_vari.array, i, value)
    }
    return(Data)
  }
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


#' @title intersect_sub_list
#' @description intersect the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data intersectd from Ori and New
#' @export
intersect_sub_list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  if(length(INa) == 0) return(NULL)
  return(New[INa])
}

#' @title left_intersect
#' @description intersect the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data intersectd from Ori and New
#' @export
left_intersect <- function(Ori, New) UseMethod("left_intersect", Ori)

#' @title left_intersect
#' @description intersect the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data intersectd from Ori and New
#' @export
left_intersect.hm.list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  Out <- hm.list()
  for(i in INa){
    Out[[i]] <- Ori[[i]]
    Out[[i]] <- intersect_sub_list(Ori[[i]], New[[i]])
  }
  return(Out)
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
  if(length(Ori) == 0) return(NULL)
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

judge_sub_list <- function(Data, i, Judge){
  OrNa <- names(Data)
  NeNa <- names(Judge)
  INa <- intersect(OrNa, NeNa)
  for (j in INa) {
    if(i == 1){
      if(Data[[j]][[1]] == 0 && Data[[j]][[2]] == 0) Data[[j]][[1]] <- 1
      if(Data[[j]][[1]] == 0 && Data[[j]][[2]] == 1) Data[[j]][[3]] <- 1
    }
    else{
      Data[[j]][[2]] <- 1
    }

  }
  return(Data)
}

set_judge <- function(Data, i, Judge) UseMethod("set_judge", Data)

set_judge.hm.list <- function(Data, i, Judge){
  OrNa <- names(Data)
  NeNa <- names(Judge)
  INa <- intersect(OrNa, NeNa)
  for(j in INa){
    Data[[j]] <- judge_sub_list(Data[[j]], i, Judge[[j]])
  }
  return(Data)
}

#' @title na_check
#' @description check the NA in the hm.list data
#' @param Data the original data
#' @return a data deleted from Ori and New
#' @export
na_check <- function(Data) {
  Out <- map(Data, na_check_sub)
  return(Out)
}
na_check_sub <- function(Data){
  check_ <- map(Data, function(Data) any(is.na(Data)))
}




