% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Station2GridIDW.R
\name{Station2GridIDW}
\alias{Station2GridIDW}
\title{Station-Data interpolation to Grid-Data with IDW}
\usage{
Station2GridIDW(
  StationLocation,
  GridLocation,
  StationData,
  cacuStationN = 5,
  wightCoefficient = 2
)
}
\arguments{
\item{StationLocation}{n* 3(ID, Latitude, Longitude) 2-array of station}

\item{GridLocation}{n * 3(ID, Latitude, Longitude) 2-array of Grid}

\item{StationData}{3-array(periodN, stationN, fieldN) of data from station}

\item{cacuStationN}{= 5 how many Station are relevant for a grid}

\item{wightCoefficient}{= 2 wight Coefficient k, 1-5}
}
\value{
Ntime * Nstation * Nfild 3-array of data for Grids
}
\description{
Station-Data interpolation to Grid-Data with IDW
}
\examples{
GridMetroData <- Station2GridIDW(SLC, GLC, SD)
}
