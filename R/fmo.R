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
#' @param base_path output directory.
#' @param unmanaged numeric, ratio value to set for unmanaged grid cells.
#' @param limited numeric, ratio value to set for limited fire management grid cells.
#' @param modified numeric, ratio value to set for modified fire management grid cells.
#' @param full numeric, ratio value to set for full fire management grid cells.
#' @param critical numeric, ratio value to set for crtical fire management grid cells.
#' @param width numeric, png width.
#' @param height numeric, png height.
#'
#' @export
#'
#' @examples
#' \dontrun{save_fmo_ratios()}
save_fmo_ratios <- function(type = "both", base_path = ".", unmanaged = 1, limited = 1,
                            modified = 1.25, full = 1.5, critical = 1.75, width = 1000, height = 1000){
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
    file <- paste0(base_path, "/fmo_2017_buffered_", c("ig", "fs"), ".tif") # nolint
    purrr::walk(file, ~raster::writeRaster(r, .x[1], overwrite = TRUE, datatype = "FLT4S"))
  } else {
    suffix <- switch(type, "sensitivity" = "fs", "ignition" = "ig")
    file <- paste0(base_path, "/fmo_2017_buffered_", suffix, ".tif") # nolint
    raster::writeRaster(r, file, overwrite = TRUE, datatype = "FLT4S")
  }
  r <- raster::ratify(r)
  lev <- levels(r)[[1]]
  clrs <- c("#eeeeee", rev(RColorBrewer::brewer.pal(9, "Spectral")[1:(length(lev$ID) - 1)]))
  suffix <- if(type != "sensitivity") "ignition" else "sensitivity"
  suffix2 <- if(type != "sensitivity") "ignition factor" else "fire sensitivity"
  file <- paste0(base_path, "/fmo_ratios_", suffix, ".png") # nolint
  Cairo::CairoPNG(file, width = width, height = height)
  rasterVis::levelplot(
    r, att = "ID", col.regions = clrs,
    maxpixels = 1e6, main = paste("Fire suppression effort ratios:", suffix2),
    xlab = NULL, ylab = NULL, scales = list(draw = FALSE), colorkey = list(space = "bottom"))
  grDevices::dev.off()
  if(type == "both"){
    file <- paste0(base_path, "/fmo_ratios_sensitivity.png") # nolint
    Cairo::CairoPNG(file, width = width, height = height)
    rasterVis::levelplot(
      r, att = "ID", col.regions = clrs,
      maxpixels = 1e6, main = "Fire suppression effort ratios: fire sensitivity",
      xlab = NULL, ylab = NULL, scales = list(draw = FALSE), colorkey = list(space = "bottom"))
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
#' save_fmo_panel()
save_fmo_panel <- function(out_dir = ".", width = 1200, height = 800){
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
  rasterVis::levelplot(
    s, att = "class", col.regions = c("#eeeeee", RColorBrewer::brewer.pal(6, "Set2")[c(6, 2, 4, 1)]),
    maxpixels = 1e6, main = "15-km buffered FMO: individual and stacked overlapping layers",
    xlab = NULL, ylab = NULL, scales = list(draw=FALSE), colorkey = list(space = "bottom"))
  grDevices::dev.off()
  invisible()
}
