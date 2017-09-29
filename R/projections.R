#' Convert from WGS84 to Alaska Albers projection
#'
#' Simple wrapper around \code{spTransform} to convert from WGS84 to Alaska Albers projection.
#'
#' @param xy a 2-column matrix or data frame of lon/lat coordinates.
#'
#' @return data frame with new attached coordinates.
#' @export
#'
#' @examples
#' # not run
wgs2ak <- function(xy){
  akalbers <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0" # nolint
  if("matrix" %in% class(xy)) xy <- tibble::as_data_frame(xy)
  names(xy) <- c("x", "y")
  sp::coordinates(xy) <- names(xy)
  sp::proj4string(xy) <- sp::CRS("+proj=longlat +datum=WGS84")
  xy <- sp::coordinates(sp::spTransform(xy, CRS = sp::CRS(akalbers)))
  xy
}
