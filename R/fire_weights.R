#' Fire probability weights for point locations
#'
#' Compute fire probability weights for point locations.
#'
#' This function applies an inverse distance weighting to grid cells within a radial buffer of a center grid cell.
#' The function is used for extracting values within a buffer around a given point from ALFRESCO fire scar output geotiffs.
#' Points outside the boundary defined by \code{buffer} are excluded and have zero
#' influence on estimated probability of fire at the center point.
#' Fire scars that interset the radial buffer zone contribute more to the estimated likelihood of fire
#' for the point location the closer the fire is to the point.
#'
#' @param file input file, an ALFRESCO fire scar output geotiff.
#' @param xy a single location, in a 2-column data frame or matrix of longitude and latitude.
#' @param buffer radius of buffer around \code{xy}.
#' @param weight inverse distance weighting, \code{"linear"} or \code{"quadratic"}.
#' @param lonlat_to_akalbers if \code{xy} is unprojected (lon/lat), reproject to NAD83 Alaska Albers equal area conic projection.
#'
#' @return a data frame containing a logical column indicating burn or no burn,
#' a column of distances from point location, and a column of weights.
#' @export
#'
#' @examples
#' \dontrun{
#' coords <- data.frame(lon = -147.7164, lat = 64.8378)
#' fire_weights("someFile.tif", coords)
#' }
fire_weights <- function(file, xy, buffer = 20000, weight = "linear",
                         lonlat_to_akalbers = TRUE){
  r <- raster::readAll(raster::raster(file, band = 2))
  xy <- wgs2ak(coords)
  cell <- raster::cellFromXY(r, fbks)
  rd <- r
  rd[] <- NA
  rd[cell] <- 1
  rd <- raster::distance(rd)
  x <- raster::extract(raster::stack(r, rd), fbks, buffer = buffer)[[1]] %>%
    dplyr::as_data_frame()
  names(x) <- c("burn", "distance")
  weight_fun <- function(x){
    x <- (max(x) - x) / max(x)
    if(type == "quadratic") x <- x^2
    x
  }
  x <- dplyr::mutate(x, burn = as.logical(.data[["burn"]])) %>%
    dplyr::mutate(burn = ifelse(is.na(.data[["burn"]]), FALSE, .data[["burn"]])) %>%
    dplyr::mutate(weight = weight_fun(.data[["distance"]], type = weight))
  x
}
