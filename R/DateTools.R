#' caculate which day of this year
#' @param DateDay nur Date or vector of Date
#' @return a one-dimesion array
#' @examples
#' toNDayofYear(as.Date("2020-2-23"))
#' toNDayofYear(seq(as.Date("1996-2-23"),as.Date("2020-2-23"),1))
#' @export
toNDayofYear <- function(DateDay){
  dn <- length(DateDay)
  NDay <- array(-1, dim = c(dn))
  for (i in 1:dn) {
    NDay[i] <- as.integer(DateDay[i] - as.Date(paste(substr(DateDay[i],1,4),1,1,sep = "-"))) + 1
  }
  return(NDay)
}


#' caculate month in upper abb
#' @importFrom lubridate month
#' @param Date Date or date list
#' @return ABB month in upper abb
#' @examples
#' toMON("2020-4-1")
#' @export
toMON <- function(Date){
  return(toupper(month.abb[month(Date)]))
}
