% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GeoTools.R
\name{fctAcculation}
\alias{fctAcculation}
\title{caculate acculation from DirGrid}
\usage{
fctAcculation(DirGrid, IDGrid)
}
\arguments{
\item{DirGrid}{RasterLayer,from raster::terrain(dem, opt = "flowdir")}

\item{IDGrid}{RasterLayer,same size with DirGrid}
}
\value{
RasterLayer, acculation
}
\description{
caculate acculation from DirGrid
}
