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

#' Make the month Data to the period Data.
#' @param NMon the moth in number
#' @param MonData the data in month scaler(12 \* gridN) or (gridN \* 12)
#' @return the data in periodN
#' @export
fctMon2Period <- function(NMon, MonData){
  if(dim(MonData)[1] == 12){
    MonData <- t(MonData)
    warning("The Data is to month-grid translate, if there is error, please make sure the dim of Data is (12, gridN).")
  }
  if(dim(MonData)[2] != 12) stop("Please make sure that there are 12 elements for every grid.")
  periodN <- length(NMon)
  gridN <- dim(MonData)[1]
  PeriodData <- matrix(0.0, gridN, periodN)
  for (i in 1:12) {
    PeriodData[, which(NMon == i)] <- MonData[,i]
  }
  return(t(PeriodData))
}
