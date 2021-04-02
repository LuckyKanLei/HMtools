
#' @title merge the Data for every Modules
#' @description merge the data
#' @param Data_Model.A the Data for Modul A
#' @param Data_Model.B the Data for Modul B
#' @return a data merged from Data_Model.A and Data_Model.A
#' @export
merge_mdl_data2 <- function(Data_Model.A, Data_Model.B){
  InData <- left_merge(Data_Model.A$InData, Data_Model.B$InData)
  Param <- left_merge(Data_Model.A$Param, Data_Model.B$Param)
  OutData <- left_merge(Data_Model.A$OutData, Data_Model.B$OutData)
  return(list(InData = InData, Param = Param, OutData = OutData))
}

#' @title merge the Data for every Modules
#' @description merge the data
#' @importFrom purrr reduce
#' @param ... the Data for Modul A B C and so on
#' @return a data merged from Data_Model.A B C and so on
#' @export
merge_Data_Modul <- function(...){
  return(reduce(list(...), merge_mdl_data2))
}
