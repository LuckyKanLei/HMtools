#' translate raster(RasterLayer) to data.frame, inoder to use ggplot
#' @importClassesFrom raster RasterLayer
#' @importFrom raster rasterToPoints
#' @param raster RasterLayer
#' @return data.frame, inoder to use ggplot.
#' @export
raster.seq <- function(raster) {
  r_points = rasterToPoints(raster)
  r_df = data.frame(r_points)
  r_df$fill <- r_df[[3]]
  return(r_df)
}


#' new funtion for geom_ in ggplot2
#' @importClassesFrom raster RasterLayer
#' @importFrom ggplot2 geom_raster
#' @param raster RasterLayer
#' @param ... other paramters for geom_
#' @return ggploto like geo_raster
#' @export
geom_raster.seq <- function(raster, ...) geom_raster(data = raster.seq(raster), aes(x = x, y = y, fill = fill), ...)

#' @rdname raster.seq
#' @param breaks breakslikes in cut
#' @importFrom  utils head
#' @export
raster.div <- function(raster, breaks) {
  r_points = rasterToPoints(r)
  r_df = data.frame(r_points)
  head(r_df) #breaks will be set to column "layer"
  r_df$fill=cut(r_df[[3]], breaks = breaks) #set breaks
  return(r_df)
}

#' @rdname geom_raster.seq
#' @param breaks breakslikes in cut
#' @export
geom_raster.div <- function(raster, breaks, ...) geom_raster(data = raster.div(raster, breaks), aes(x = x, y = y, fill = fill), ...)

#' @rdname raster.seq
#' @export
raster.qual <- function(raster) {
  r_points = rasterToPoints(raster)
  r_df = data.frame(r_points)
  r_df$fill <- as.factor(r_df[[3]])
  return(r_df)
}
#' @rdname geom_raster.seq
#' @export
geom_raster.qual <- function(raster, ...) geom_raster(data = raster.qual(raster), aes(x = x, y = y, fill = fill), ...)


#' translate raster(RasterLayer) to data.frame, inoder to use ggplot
#' @importClassesFrom raster RasterLayer
#' @importFrom raster rasterToPoints
#' @param DirGrid RasterLayer, dirction from terrn()
#' @return data.frame, inoder to use ggplot.
#' @export
raster.dir <- function(DirGrid) {
  IDGrid <- DirGrid
  IDGrid[!is.na(IDGrid)] <- 1:length(IDGrid[!is.na(IDGrid)])

  r_points = rasterToPoints(IDGrid)
  r_df = data.frame(r_points)
  names(r_df) <- c("xend", "yend", "id")
  NextGrid <- fctNextGrid(IDGrid, DirGrid)
  r_points1 = rasterToPoints(NextGrid)
  r_df1 = data.frame(r_points1)
  names(r_df1) <- c("x", "y", "id")
  r_df0 <- join(r_df1, r_df)
  return(r_df0)
}
#' new funtion for geom_raster in ggplot2
#' @importClassesFrom raster RasterLayer
#' @importFrom ggplot2 geom_segment
#' @param DirGrid RasterLayer,same size with DirGrid
#' @param dirColour colour in geom_segment
#' @param dirArrow arrow in geom_segment
#' @param ... other paramters for geom_
#' @return ggploto like geo_raster
#' @export
geom_dir <- function(DirGrid, dirColour = "#084594",
                     dirArrow = arrow(length = unit(0.2,"cm")),
                     ...) geom_segment(data = raster.dir(DirGrid),
                                    aes(x = x, y = y, xend = xend, yend = yend),
                                    colour = dirColour,
                                    arrow = dirArrow,
                                    ...)


#' new funtion for geom_raster in ggplot2
#' @importClassesFrom raster RasterLayer
#' @import ggplot2
#' @importFrom grDevices terrain.colors
#' @param DEM RasterLayer, dem
#' @return ggplot like ggplot()
#' @export
ggplot_dem <- function(DEM) {
  ggplot() +
    geom_raster(data = raster.seq(DEM), aes(x = x, y = y, fill = fill)) +
    scale_fill_gradientn("DEM/m", colours = terrain.colors(6000)) +
    coord_equal() +
    theme_bw() +
    xlab("Longitude") + ylab("Latitude")
}

#' translate raster(RasterLayer) to data.frame, inoder to use ggplot
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @importFrom plyr join
#' @param sppath SpatialPolygonsDataFrame
#' @return data.frame, inoder to use ggplot.
#' @export
sp.path <- function(sppath) {
  sppath@data$id = rownames(sppath@data)
  sppath_points = fortify(sppath, region="id")
  sppath_df = join(sppath_points, sppath@data, by="id")
  sppath@data$id = rownames(sppath@data)
  sppath_points = fortify(sppath, region="id")
  sppath_df = join(sppath_points, sppath@data, by="id")
}

#' translate raster(RasterLayer) to data.frame, inoder to use ggplot
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @importFrom ggplot2 geom_path
#' @param sp_basin SpatialPolygonsDataFrame, basin
#' @param Color color for linie in geom_path
#' @param Size size for linie in geom_path
#' @param ... other parameters for geom_path
#' @return data.frame, inoder to use ggplot.
#' @export
geom_basin <- function(sp_basin,
                       Color = "#e31a1c",
                       Size = 1.3, ...) geom_path(data = sp.path(sp_basin),
                                                                            aes(long,lat, group = group),
                                                                            color = Color, size = Size,
                                                                            ...)
#' @rdname geom_basin
#' @param sp_river SpatialPolygonsDataFrame, river
#' @export
geom_river <- function(sp_river,
                       Color = "#6baed6",
                       Size = 1, ...) geom_path(data = sp.path(sp_river),
                                                                                 aes(long,lat, group = group),
                                                                                 color = Color, size = Size,
                                                                                 ...)

