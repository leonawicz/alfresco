#' Convert from Alaska Albers projection to WGS84
#'
#' Simple wrapper around \code{spTransform} to convert from Alaska Albers projection to WGS84.
#'
#' @param xy
#'
#' @return data frame with new attached coordinates.
#' @export
#'
#' @examples
wgs2ak <- function(xy){
  akalbers <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0" # nolint
  if(class(xy) == "matrix") xy <- tibble::data_frame(xy)
  names(xy) <- c("x", "y")
  coordinates(xy) <- names(xy)
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  xy <- coordinates(spTransform(xy, CRS = CRS(akalbers)))
  xy
}
