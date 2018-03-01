#' Obtain Alaska fire perimeter data
#'
#' Obtain Alaska Fire Service historical Alaska fire perimeter data.
#'
#' The most current fire perimeter data available through this function is up through 2017.
#' The data is pulled from SNAP via Amazon Web Services. It is downloaded and imported from a ~21 MB rds file as a \code{SpatialPolygonsDataFrame}.
#' For slow connections, please allow time for download. Subsetting years does not affect this because the full file is imported and any subsetting is done subsequently.
#'
#' @param years numeric vector, defaults to all available years, 1940 - 2017.
#'
#' @return a SpatialPolygonsDataFrame
#' @export
#'
#' @examples
#' \dontrun{x <- fire_perimiters()}
fire_perimeters <- function(years = 1940:2017){
  if(any(years < 1940 | years > 2017))
    stop("Valid year range is 1940:2017 as of most recent data update.")
  path <- "https://s3.amazonaws.com/leonawicz/fire/historical_fire_perimeters/afs_fire_history.rds"
  x <- readRDS(url(path))
  if(any(!1940:2017 %in% years)) x <- x[x[["FireYear"]] %in% years, ]
  x
}
