#' @title merge_sub_list
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
left_merge.list <- function(Ori, New){
  NeNa <- names(New)
  for (i in NeNa) {
    Ori[[i]] <- New[[i]]
  }
  return(Ori)
}
#' @title merge_sub_list
#' @description merge the data from Ori and New bei elments names
#' @param Ori the original data
#' @param New the new data
#' @return a data merged from Ori and New
#' @export
left_join.list <- function(Ori, New){
  OrNa <- names(Ori)
  NeNa <- names(New)
  INa <- intersect(OrNa, NeNa)
  for (i in INa) {
    Ori[[i]] <- New[[i]]
  }
  return(Ori)
}








