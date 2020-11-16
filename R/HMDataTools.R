#' @title mergeData.list
#' @description merge the data from Ori and New bei elments names for list
#' @import purrr
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeSlots <- function(Ori, New){
  OrSl <- getSlots(Ori)
  OrNa <- names(OrSl)
  NeSl <- getSlots(New)
  NeNa <- names(NeSl)

  INa <- intersect(OrNa, NeNa)
  ISl <- rbind(OrSl[INa], NeSl[INa])
  DONa <- setdiff(OrNa, NeNa)
  DNNa <- setdiff(NeNa, OrNa)
  return(c(OrSl[DONa], NeSl))
}

#' @title mergeData.list
#' @description merge the data from Ori and New bei elments names for list
#' @import purrr
#' @param cName class name
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeClass <- function(cName, Ori, New){
  return(setClass(cName,
                         slots = mergeSlots(Ori,New)))
}

#' @title mergeData.list
#' @description merge the data from Ori and New bei elments names for list
#' @import purrr
#' @param cName class name
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeSubClass <- function(cName, Ori, New){
  OrSl <- getSlots(Ori)
  OrNa <- names(OrSl)
  NeSl <- getSlots(New)
  NeNa <- names(NeSl)

  INa <- intersect(OrNa, NeNa)
  ISl <- rbind(OrSl[INa], NeSl[INa])
  DONa <- setdiff(OrNa, NeNa)
  DNNa <- setdiff(NeNa, OrNa)
  DiffSl <- c(OrSl[DONa], NeSl[DNNa])

  for (i in 1:length(INa)) {
    eval(parse(text = paste0(INa[i], " <- mergeClass(ISl[1,i], ISl[2,i], INa[i])")))
  }
  # IntSl <- INa
  names(INa) <- INa
  return(setClass(cName,
                  slots = c(INa,DiffSl)))

}

#' @title mergeData.list
#' @description merge the data from Ori and New bei elments names for list
#' @import purrr
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
mergeData.HM.Data <- function(Ori, New){
  OrNa <- slot(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  DONa <- setdiff(OrNa, NeNa)
  DNNa <- setdiff(NeNa, OrNa)
  for (i in DNNa) {
    Ori[[i]] <- New[[i]]
  }
  for(i in INa){
    if(class(Ori[[i]]) == "data.frame" || class(Ori[[i]]) == "list") Ori[[i]] <- mergeData(Ori[[i]], New[[i]])
    else Ori[[i]] <- New[[i]]
  }
  return(Ori)
}
