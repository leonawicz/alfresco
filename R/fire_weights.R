#' Fire probability weights for point locations
#'
#' Compute fire probability weights for point locations.
#'
#' This function applies an inverse distance weighting to grid cells within a radial buffer of a center grid cell.
#' The function is used for extracting values within a buffer around a given point from ALFRESCO fire scar output geotiffs.
#' Points outside the boundary defined by \code{buffer} are excluded and have zero
#' influence on estimated probability of fire at the center point.
#' Fire scars that intersect the radial buffer zone contribute more to the estimated likelihood of fire
#' for the point location the closer the fire is to the point.
#'
#' It is recommended to not use \code{age} because ALFRESCO has a longstanding bug involving geogrpahic areas of uninitialized ages,
#' resulting in extreme negative age values that ruin analyses unless the user first implements their own fix to any age geotiffs before using them.
#'
#' @param r raster layer or path to file, an ALFRESCO fire scar output geotiff.
#' @param xy a single location, in a 2-column data frame or matrix of longitude and latitude.
#' @param buffer radius of buffer around \code{xy}.
#' @param weight inverse distance weighting, \code{"linear"} or \code{"quadratic"}.
#' @param lonlat_to_akalbers if \code{xy} is unprojected (lon/lat), reproject to NAD83 Alaska Albers equal area conic projection.
#' @param veg same as\code{r} but for accompanying vegetation class ALFRESCO output.
#' @param age same as\code{r} but for accompanying vegetation age ALFRESCO output. See details.
#'
#' @return a data frame containing a logical column indicating burn or no burn,
#' a column of distances from point location, and a column of weights.
#' @export
#'
#' @examples
#' \dontrun{
#' scar <- "FireScar_0_2004.tif"
#' veg <- "Veg_0_2004.tif"
#' age <- "Age_0_2004.tif"
#' coords <- data.frame(lon = -147.7164, lat = 64.8378)
#' fire_weights(r = scar, xy = coords, veg = veg, age = age)
#' }
fire_weights <- function(r, xy, buffer = 20000, weight = "linear",
                         lonlat_to_akalbers = TRUE, veg = NULL, age = NULL){
  if(inherits(r, "character"))
    r <- raster::readAll(raster::raster(r, band = 2))
  if(lonlat_to_akalbers) xy <- wgs2ak(xy)
  nam <- c("burn", "distance")
  cell <- raster::cellFromXY(r, xy)
  rd <- r
  rd[] <- NA
  rd[cell] <- 1
  rd <- raster::distance(rd)
  s <- raster::stack(r, rd)

  if(!is.null(veg)) {
    if(inherits(veg, "character")) veg <- raster::readAll(raster::raster(veg))
    s <- raster::addLayer(s, veg)
    nam <- c(nam, "veg")
  }
  if(!is.null(age)) {
    if(inherits(age, "character")) age <- raster::readAll(raster::raster(age))
    s <- raster::addLayer(s, age)
    nam <- c(nam, "age")
  }
  x <- raster::extract(s, xy, buffer = buffer)[[1]] %>%
    dplyr::as_data_frame()
  names(x) <- nam
  weight_fun <- function(x, type){
    x <- (max(x) - x) / max(x)
    if(type == "quadratic") x <- x^2
    x
  }
  dplyr::mutate(x, burn = as.logical(.data[["burn"]])) %>%
    dplyr::mutate(burn = ifelse(is.na(.data[["burn"]]), FALSE, .data[["burn"]])) %>%
    dplyr::mutate(weight = weight_fun(.data[["distance"]], type = weight))
}

#' Fire point probabilities
#'
#' Estimate fire point probability from fire weights for point locations.
#'
#' This function estimates point probability of fire for spatial point locations from fire weights output by \link{fire_weights}.
#' It generates a summary data frame that includes a column \code{prop} of a simple proportion of area burned and a column \code{prob}
#' of estimated fire point probabilities based on the weights in \code{data}.
#'
#' \code{covariates = TRUE} returns additional variables only if at least the \code{veg} column is present in \code{data}. If not, there will be
#' nothing additonal to return.
#'
#' This function returns a one-column data frame. It is called on a single point location just like \code{fire_weights}.
#' It is generally used in a context where it is invoked multiple times applied to several point locations.
#' See \link{point_probs} for the generic wrapper used to estimate fire point probability for multiple locations that returns a multi-row data frame.
#'
#' @param data a data frame output by \code{fire_weights}.
#' @param veg_labels character, vector of vegetation labels. If missing, they are assumed from \code{get_veg_labels("ak1km")}.
#' @param covariates logical, if \code{TRUE} (default), then a wide data frame is returned that includes additional spatially aggregated covariates. See details.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' scar <- "FireScar_0_2004.tif"
#' veg <- "Veg_0_2004.tif"
#' age <- "Age_0_2004.tif"
#' coords <- data.frame(lon = -147.7164, lat = 64.8378)
#' fire_weights(r = scar, xy = coords, veg = veg, age = age)
#' fire_probs(x)
#' }
fire_probs <- function(data, veg_labels, covariates = TRUE){
  if(missing(veg_labels)) veg_labels <- get_veg_labels("ak1km")
  veg <- if("veg" %in% names(data) & covariates) TRUE else FALSE
  age <- if("age" %in% names(data)) TRUE else FALSE
  if(veg) x <- .prep_agg(data, veg_labels, veg, age) %>% .agg_wide(veg_labels)
  data <- dplyr::summarise(data,
                           prop = mean(.data[["burn"]]),
                           prob = mean(.data[["burn"]] * .data[["weight"]]))
  if(veg) data <- dplyr::bind_cols(data, x)
  data
}

.prep_agg <- function(data, veg_labels, veg, age){
  vegid = seq_along(veg_labels) - 1
  v <- as.numeric(NA)
  if(veg & age){
    y <- tibble::data_frame(veg = vegid, distance = v, age = v, area = v)
    x <- dplyr::group_by(data, .data[["veg"]]) %>% dplyr::summarise(
      distance = mean(.data[["distance"]]),
      age = mean(.data[["age"]]),
      area = n())
    x <- dplyr::left_join(y, x, by = c("veg"), suffix = c(".x", "")) %>%
      dplyr::select(c(1, 5:7)) %>%
      tidyr::gather(key, vegid, -.data[["distance"]], -.data[["age"]], -.data[["area"]])
  } else if(veg){
    y <- tibble::data_frame(veg = vegid, distance = v, area = v)
    x <- dplyr::group_by(data, .data[["veg"]]) %>% dplyr::summarise(
      distance = mean(.data[["distance"]]),
      area = n())
    x <- dplyr::left_join(y, x, by = c("veg"), suffix = c(".x", "")) %>%
      dplyr::select(c(1, 4:5)) %>%
      tidyr::gather(key, vegid, -.data[["distance"]], -.data[["area"]])
  } else {
    stop("`veg` should always be TRUE.")
  }
  x
}

.agg_wide <- function(x, veg_labels){
  distance <- tidyr::spread(dplyr::select(x, -.data[["age"]], -.data[["area"]]),
                            .data[["vegid"]], .data[["distance"]]) %>%
    dplyr::select(-1)
  names(distance) <- paste(veg_labels, "distance")
  area <- tidyr::spread(dplyr::select(x, -.data[["distance"]], -.data[["age"]]),
                        .data[["vegid"]], .data[["area"]]) %>%
    dplyr::select(-1)
  names(area) <- paste(veg_labels, "area")
  age <- tidyr::spread(dplyr::select(x, -.data[["distance"]], -.data[["area"]]),
                       .data[["vegid"]], .data[["age"]]) %>%
    dplyr::select(-1)
  names(age) <- paste(veg_labels, "age")
  dplyr::bind_cols(distance, area, age)
}

#' Fire probabilities for point locations
#'
#' Compute fire probability weights for point locations.
#'
#' This function is a wrapper around \link{fire_weights} and \link{fire_probs}.
#' It uses parallel processing via \code{parallel::mclapply} to apply these functions to multiple point locations.
#' See these functions for further details.
#'
#' It is recommended to not use \code{age} because ALFRESCO has a longstanding bug involving geogrpahic areas of uninitialized ages,
#' resulting in extreme negative age values that ruin analyses unless the user first implements their own fix to any age geotiffs before using them.
#'
#' @param r raster layer or path to file, an ALFRESCO fire scar output geotiff.
#' @param xy a 2-column data frame or matrix of longitude and latitude.
#' @param id a vector of IDs or names associated with the locations in the rows of \code{xy}.
#' @param buffer radius of buffer around \code{xy}.
#' @param weight inverse distance weighting, \code{"linear"} or \code{"quadratic"}.
#' @param lonlat_to_akalbers if \code{xy} is unprojected (lon/lat), reproject to NAD83 Alaska Albers equal area conic projection.
#' @param veg_labels character, vector of vegetation labels. If missing, they are assumed from \code{get_veg_labels("ak1km")}.
#' @param covariates logical, if \code{TRUE} (default), then a wide data frame is returned that includes additional spatially aggregated covariates. See details.
#' @param veg same as\code{r} but for accompanying vegetation class ALFRESCO output.
#' @param age same as\code{r} but for accompanying vegetation age ALFRESCO output. See details.
#' @param max.cores integer, maximum number of processors, defaults to 32 for Atlas compute node.
#'
#' @return a data frame containing a logical column indicating burn or no burn,
#' a column of distances from point location, and a column of weights.
#' @export
#'
#' @examples
#' \dontrun{
#' scar <- "FireScar_0_2004.tif"
#' veg <- "Veg_0_2004.tif"
#' age <- "Age_0_2004.tif"
#' coords <- data.frame(lon = c(-146, -150), lat = c(64, 67))
#' point_probs(r = scar, xy = coords, id = c("point1", "point2"), veg = veg, age = age)
#' }
point_probs <- function(r, xy, id, buffer = 20000, weight = "linear", lonlat_to_akalbers = TRUE,
                        veg_labels, covariates = TRUE, veg = NULL, age = NULL, max.cores = 32){
  if(missing(veg_labels)) veg_labels <- get_veg_labels("ak1km")
  mc.cores <- min(nrow(xy), max.cores)
  if(inherits(r, "character")) r <- raster::readAll(raster::raster(r))
  if(inherits(veg, "character")) veg <- raster::readAll(raster::raster(veg))
  if(inherits(age, "character")) age <- raster::readAll(raster::raster(age))
  .fun <- function(i){
    x <- fire_weights(r, xy[i, ], buffer, weight, lonlat_to_akalbers, veg, age)
    x <- fire_probs(x, veg_labels, covariates)
    dplyr::bind_cols(tibble::data_frame(id = id[i]), x)
  }
  parallel::mclapply(1:nrow(xy), .fun, mc.cores = mc.cores) %>% dplyr::bind_rows()
}
