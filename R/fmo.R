#' Save FMO ratios maps
#'
#' Save fire management options ratios maps.
#'
#' This functions save a geotiff of the FMO ratios raster layer and an accompanying png graphic.
#' If \code{type = "both"}, the same pair of files is saved for ALFRESCO ignition factor input geotiff suppression and
#' ALFRESCO fire sensitivity input geotiff suppression.
#' It uses \code{swfmoBuffer} from the \code{snapgrid} package. The \code{snapgrid} package also includes \code{swfmoRatios},
#' the same FMO ratios map that this function creates using default arguments.
#'
#' @param type character, \code{"sensitivity"}, \code{"ignition"} or \code{"both"}.
#' @param out_dir output directory.
#' @param unmanaged numeric, ratio value to set for unmanaged grid cells.
#' @param limited numeric, ratio value to set for limited fire management grid cells.
#' @param modified numeric, ratio value to set for modified fire management grid cells.
#' @param full numeric, ratio value to set for full fire management grid cells.
#' @param critical numeric, ratio value to set for critical fire management grid cells.
#' @param width numeric, png width.
#' @param height numeric, png height.
#'
#' @export
#'
#' @examples
#' \dontrun{save_fmo_ratios()}
save_fmo_ratios <- function(type = "both", out_dir = ".", unmanaged = 1, limited = 1,
                            modified = 1.25, full = 1.5, critical = 1.75, width = 1000, height = 1000){
  if(!isNamespaceLoaded("raster")) attachNamespace("raster")
  if(!type %in% c("both", "sensitivity", "ignition"))
    stop("`type` must be 'sensitivity', 'ignition' or 'both'.")
  r0 <- snapgrid::swfmoBuffer
  r <- r0 < 5
  r[r0 == 0] <- unmanaged
  r[r0 == 1] <- limited
  r[r0 == 2] <- modified
  r[r0 == 3] <- critical
  r[r0 == 4] <- full
  if(type == "both"){
    file <- paste0(out_dir, "/fmo_2017_buffered_", c("ig", "fs"), ".tif") # nolint
    purrr::walk(file, ~raster::writeRaster(r, .x[1], overwrite = TRUE, datatype = "FLT4S"))
  } else {
    suffix <- switch(type, "sensitivity" = "fs", "ignition" = "ig")
    file <- paste0(out_dir, "/fmo_2017_buffered_", suffix, ".tif") # nolint
    raster::writeRaster(r, file, overwrite = TRUE, datatype = "FLT4S")
  }
  r <- raster::ratify(r)
  lev <- levels(r)[[1]]
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
#' This functions save a png graphic of the 15-km buffered FMO management IDs raster layer from the \code{snapgrid} package.
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
  if(!isNamespaceLoaded("raster")) attachNamespace("raster")
  r <- raster::ratify(snapgrid::swfmoBuffer)
  classes <- c("Domain", "Limited", "Modified", "Critical", "Full")
  suppressWarnings(levels(r) <- data.frame(ID = 0:4, class = factor(classes, levels = classes)))
  slice_ratified <- function(x, id){
    r <- raster::mask(x == id, x != id, maskvalue = 0, updatevalue = id)
    if(is.factor(x)) levels(r) <- levels(x)[[1]]
    r
  }
  s <- raster::stack(c(purrr::map(1:4, ~slice_ratified(r, .x)), r))
  names(s) <- c(classes[-1], "Stacked")
  Cairo::CairoPNG(file.path(out_dir, "fmo.png"), width = width, height = height)
  print(rasterVis::levelplot(
    s, att = "class", col.regions = c("#eeeeee", RColorBrewer::brewer.pal(6, "Set2")[c(6, 2, 4, 1)]),
    maxpixels = 1e6, main = "15-km buffered FMO: individual and stacked overlapping layers",
    xlab = NULL, ylab = NULL, scales = list(draw = FALSE), colorkey = list(space = "bottom")))
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
