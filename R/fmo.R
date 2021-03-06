#' Save FMO ratios maps
#'
#' Save fire management options ratios maps.
#'
#' This functions save a geotiff of the FMO ratios raster layer and an accompanying png graphic.
#' If \code{type = "both"}, the same pair of files is saved for ALFRESCO ignition factor input geotiff suppression and
#' ALFRESCO fire sensitivity input geotiff suppression.
#' It uses \code{swfmo} from the \code{snapgrid} package. The \code{snapgrid} package also includes \code{swratios},
#' the same FMO ratios map that this function creates using default arguments.
#'
#' @param type character, \code{"sensitivity"}, \code{"ignition"} or \code{"both"}.
#' @param out_dir output directory.
#' @param unmanaged numeric, ratio value to set for unmanaged grid cells.
#' @param limited numeric, ratio value to set for limited fire management grid cells.
#' @param modified numeric, ratio value to set for modified fire management grid cells.
#' @param full numeric, ratio value to set for full fire management grid cells.
#' @param critical numeric, ratio value to set for critical fire management grid cells.
#' @param other numeric, ratio value to set for any other grid cells.
#' @param width numeric, png width.
#' @param height numeric, png height.
#'
#' @export
#'
#' @examples
#' \dontrun{save_fmo_ratios()}
save_fmo_ratios <- function(type = "both", out_dir = ".", unmanaged = 1, limited = 1,
                            modified = 1, full = 1.25, critical = 2, other = 1, width = 1000, height = 1000){
  if(!type %in% c("both", "sensitivity", "ignition"))
    stop("`type` must be 'sensitivity', 'ignition' or 'both'.")
  r0 <- snapgrid::swfmo
  r <- r0 < 5
  r[r0 == 0] <- unmanaged
  r[r0 == 1] <- limited
  r[r0 == 2] <- modified
  r[r0 == 3] <- critical
  r[r0 == 4] <- full
  r[r0 > 4] <- other
  if(type == "both"){
    file <- paste0(out_dir, "/fmo_standard_", c("ig", "fs"), ".tif") # nolint
    purrr::walk(file, ~raster::writeRaster(r, .x[1], overwrite = TRUE, datatype = "FLT4S"))
  } else {
    suffix <- switch(type, "sensitivity" = "fs", "ignition" = "ig")
    file <- paste0(out_dir, "/fmo_standard_", suffix, ".tif") # nolint
    raster::writeRaster(r, file, overwrite = TRUE, datatype = "FLT4S")
  }
  r <- raster::ratify(r)
  lev <- raster::levels(r)[[1]]
  clrs <- c("#eeeeee", rev(RColorBrewer::brewer.pal(9, "Spectral")[1:(length(lev$ID) - 1)]))
  suffix <- if(type != "sensitivity") "ignition" else "sensitivity"
  suffix2 <- if(type != "sensitivity") "ignition factor" else "fire sensitivity"
  file <- paste0(out_dir, "/fmo_ratios_", suffix, ".png") # nolint
  Cairo::CairoPNG(file, width = width, height = height)
  print(rasterVis::levelplot(
    r, att = "ID", col.regions = clrs,
    maxpixels = 1e6, main = paste("Fire suppression effort ratios:", suffix2),
    xlab = NULL, ylab = NULL, scales = list(draw = FALSE), colorkey = list(space = "bottom")))
  grDevices::dev.off()
  if(type == "both"){
    file <- paste0(out_dir, "/fmo_ratios_sensitivity.png") # nolint
    Cairo::CairoPNG(file, width = width, height = height)
    print(rasterVis::levelplot(
      r, att = "ID", col.regions = clrs,
      maxpixels = 1e6, main = "Fire suppression effort ratios: fire sensitivity",
      xlab = NULL, ylab = NULL, scales = list(draw = FALSE), colorkey = list(space = "bottom")))
    grDevices::dev.off()
  }
  invisible()
}

#' Save FMO panel plot
#'
#' Save fire management options panel plot showing each FMO layer.
#'
#' This functions save a png graphic of the standard FMO management IDs raster layer from the \code{snapgrid} package.
#'
#' @param out_dir output directory.
#' @param width numeric, png width.
#' @param height numeric, png height.
#'
#' @export
#'
#' @examples
#' \dontrun{save_fmo_panel()}
save_fmo_panel <- function(out_dir = ".", width = 1200, height = 800){
  r <- snapgrid::swfmo
  r[r > 4] <- 0
  r <- raster::ratify(r)
  classes <- c("Other", "Limited", "Modified", "Full", "Critical")
  suppressWarnings(levels(r) <- data.frame(ID = c(0:2, 4:3), class = factor(classes, levels = classes)))
  slice_ratified <- function(x, id){
    r <- raster::mask(x == id, x != id, maskvalue = 0, updatevalue = id)
    if(is.factor(x)) levels(r) <- raster::levels(x)[[1]]
    r
  }
  Cairo::CairoPNG(file.path(out_dir, "fmo.png"), width = width, height = height)
  print(rasterVis::levelplot(
    r, att = "class", col.regions = c("black", "#eeeeee", "dodgerblue", "orange", "firebrick"),
    maxpixels = 1e6, main = "FMO zones: Standard",
    xlab = NULL, ylab = NULL, scales = list(draw = FALSE),
    colorkey = list(space = "bottom", height = 1, labels = list(cex = 1.5))))
  grDevices::dev.off()
  invisible()
}

#' FMO cumulative burn reduction table
#'
#' Generate a data frame of reductions in cumulative burn based on fire management options treatment levels.
#'
#' @param data a data frame resulting from \link{fsdf}.
#' @param pretty_names logical, use more meaningful column names, better for printing. For programming, original names may be more convenient.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- fsdf("historical_fsv.rds")
#' fmo_cb_reduction(x)
#' }
fmo_cb_reduction <- function(data, pretty_names = TRUE){
  veg <- "Vegetation" %in% names(data)
  rcp <- "Scenario" %in% names(data)
  mod <- "Model" %in% names(data)
  if(veg) data <- dplyr::group_by(data, .data[["Vegetation"]])
  data <- dplyr::group_by(data, .data[["Scenario"]], .data[["Model"]],
                          .data[["FMO"]], .data[["Replicate"]], add = TRUE)
  data <- dplyr::summarise(data, BA = sum(.data[["FS"]]))
  sq <- dplyr::filter(data, .data[["FMO"]] == "fmo00s00i") %>%
           dplyr::summarise(BA = mean(.data[["BA"]]))
  pct_form <- function(x, id, y){
    veg <- "Vegetation" %in% names(x)
    rcp <- "Scenario" %in% names(x)
    mod <- "Model" %in% names(x)
    if(veg) v <- unique(x[["Vegetation"]])
    if(rcp) s <- unique(x[["Scenario"]])
    if(mod) m <- unique(x[["Model"]])
    x <- x[[id]]
    if(veg) y <- dplyr::filter(y, .data[["Vegetation"]] == v)
    if(rcp) y <- dplyr::filter(y, .data[["Scenario"]] == s)
    if(mod) y <- dplyr::filter(y, .data[["Model"]] == m)
    round(100 * (x / y$BA - 1), 1)
  }
  data <- dplyr::summarise(data,
                           Min = min(.data[["BA"]]),
                           Mean = round(mean(.data[["BA"]])),
                           Max = max(.data[["BA"]])) %>%
    dplyr::mutate(
      PctSQLB = pct_form(.data, "Min", sq),
      PctSQMean = pct_form(.data, "Mean", sq),
      PctSQUB = pct_form(.data, "Max", sq),
      PctFsSupp = as.numeric(substr(.data[["FMO"]], 4, 5)),
      PctIgSupp = as.numeric(substr(.data[["FMO"]], 7, 8))
    ) %>% dplyr::ungroup()
  idx <- c(8:9, 5:7, 2:4)
  if(veg) idx <- c(1, idx + 1)
  if(rcp) idx <- c(1, idx + 1)
  if(mod) idx <- c(1, idx + 1)
  data <- dplyr::select(data, idx)
  if(pretty_names){
    pnames <- c("% Sensitivity", "% Ignition",
                paste(c("Lower", "Mean", "Upper"), "% Change"),
                paste(c("Min", "Mean", "Max"), "Area"))
    names(data)[which(!names(data) %in% c("Vegetation", "Scenario", "Model"))] <- pnames
  }
  data
}

#' Burn area by FMO zone
#'
#' Generate a data frame of mean (across replicates) annual burn area by fire management options zones.
#'
#' Options for Alaska ecoregion and Alaska fire management zone masking include \code{"Arctic Tundra"}, \code{"Bering Taiga"}, \code{"Bering Tundra"} and \code{"Intermontane Boreal"} and
#' eight FMZs ("DAS", "FAS", "GAD", "MID", "SWS", "TAD", "TAS", "UYD"), respectively.
#' The limited sets represent their spatial intersections with the JFSP ALFRESCO spatial domain.
#'
#' @param in_dir input directory, a \code{Maps} directory for ALFRESCO run output.
#' @param years integer.
#' @param id integer, 0 through 5 available, pertaining to ID codes for FMO zones.
#' @param labels character, labels for \code{id} values.
#' @param fmo_layer raster layer, optional. If \code{NULL}, the standard FMO base map from the snapgrid package is used.
#' @param mask character, optional mask: \code{NULL}, \code{"ecoreg"} for \code{snappoly::ecoreg} ecoregions, or \code{"fmz"} for \code{snappoly::fmz} Fire Management Zones. See details.
#' @param mask_value character, ecoregion or FMZ name to mask by, not applicable if \code{mask = NULL}, in which case the full statewide Alaska JFSP ALFRESCO domain. See details.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' ba_fmo(".", 1950:2013, 0:5)
#' }
ba_fmo <- function(in_dir, years, id = 0:5,
                   labels = c("Unmanaged", "Limited", "Modified", "Critical", "Full", "Other"),
                   fmo_layer = NULL, mask = NULL, mask_value = NULL){
  if(is.null(fmo_layer)) fmo_layer <- snapgrid::swfmo
  if(!is.null(mask) & !is.null(mask_value)){
    if(mask == "ecoreg") x <- snappoly::ecoreg[snappoly::ecoreg[["LEVEL_2"]] == mask_value, ]
    if(mask == "fmz") x <- snappoly::fmz[snappoly::fmz[["REGION"]] == mask_value, ]
    cells <- raster::extract(fmo_layer, x, cellnumbers = TRUE) %>% purrr::map(~.x[, 1]) %>% unlist() %>% sort() # nolint
    idx <- purrr::map(id, ~which(fmo_layer[] == .x & seq_along(fmo_layer) %in% cells))
  } else {
    idx <- purrr::map(id, ~which(fmo_layer[] == .x))
  }
  f <- function(year, id){
    files <- list.files(file.path(in_dir, year), pattern = "^FireScar", full.names = TRUE)
    s <- raster::readAll(raster::stack(files, bands = 2)) # nolint
    x <- purrr::map_dbl(idx, ~length(which(!is.na(as.numeric(raster::extract(s, .x))))) / raster::nlayers(s))
    x <- c(year, x)
    names(x) <- c("Year", labels[id + 1])
    do.call(tibble::data_frame, as.list(x))
  }
  purrr::map(years, ~f(.x, id)) %>% dplyr::bind_rows()
}

#' Add buffer to FMO zone base map
#'
#' Add a buffer around any FMO zone in the FMO zone raster base map.
#'
#' This function allows for adding a buffer of different radii (in meters) around any of the unique FMO zones.
#'
#' @param out_dir output directory.
#' @param file output filename.
#' @param unmanaged numeric.
#' @param limited numeric.
#' @param modified numeric.
#' @param full numeric.
#' @param critical numeric.
#' @param other numeric.
#'
#' @return nothing is returned but a file is written to disk.
#' @export
#'
#' @examples
#' \dontrun{fmo_add_buff(full = 5000)}
fmo_add_buffer <- function(out_dir = ".", file = "fmo_buffer.tif", unmanaged = NULL, limited = NULL,
                           modified = NULL, full = NULL, critical = NULL, other = NULL){
  x <- as.list(raster::layerize(snapgrid::swfmo, falseNA = TRUE))
  y <- list(unmanaged, limited, modified, critical, full, other) # nolint
  x <- purrr::map2(x, seq_along(x), ~({
    .x[.x == 1] <- .y - 1
    .x
  }))
  x <- purrr::map(seq_along(x), ~({
    if(is.null(y[[.x]])) return(x[[.x]])
    r <- raster::buffer(x[[.x]], width = y[[.x]], doEdge = TRUE)
    r[r == 1] <- .x - 1
    r
  }))
  x <- do.call(raster::merge, x[c(4, 5, 3, 2, 1, 6)])
  raster::writeRaster(x, file.path(out_dir, file), overwrite = TRUE, datatype = "FLT4S")
  invisible()
}
