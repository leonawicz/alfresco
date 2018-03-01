.buffered_data <- function(x, xy, bfrs, convert = FALSE){
  xy <- dplyr::slice(xy, rep(1, length(bfrs)))
  if(convert) xy <- wgs2ak(xy)
  x[snapgrid::swveg == 0] <- NA
  e <- raster::extract(x, xy, fun = function(x, ...) mean(x > 0, ...), buffer = bfrs)
  e[is.na(e)] <- 0
  tibble::as_data_frame(e) %>% dplyr::mutate(buffer = bfrs / 1000) %>% dplyr::mutate(run = names(x)) %>%
    dplyr::mutate(period = ifelse(substr(.data[["run"]], 1, 3) %in%
                                    c("his", "cru", "obs"), "Historical", "Projected"))
}

#' Probability of fire
#'
#' Compute the probability of fire within some radius of a point.
#'
#' This function computes the proportion of cells within a radial buffer zone of a sampled grid cell that has burned in ALFRESCO fire scar geotiff outputs.
#' It does this for each of a number of sampled points around a centroid point of interest. There are radius arguments for both the size of the buffer around the center from
#' which to draw random grid cell samples for bootstrapping a more robust estimate around the centroid grid cell as well as the buffer around each sampled point within
#' which to compute the likelihood of fire. The latter buffer argument may be a vector. This function sums fire occurrences across replicates and years.
#'
#' @param rep integer, ALFRESCO replicate.
#' @param base_path input directory to ALFRESCO fire scar geotiffs.
#' @param label character, name to attach to raster layer sum for input fire scar geotiffs.
#' @param center numeric, coordinate pair for centroid around which buffered extractions are taken.
#' @param sample_size integer, number of grid cells to sample within \code{sample_buffer} of \code{center}.
#' @param sample_buffer numeric, buffer radius in meters around \code{center} to sample grid cells.
#' @param extraction_buffers numeric, vector of buffer radii in meters from each sampled point to compute the proportion of cells where fire has occurred.
#' @param years numeric, vector of years for which to compute the probability of fire.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' # not run
point_fire <- function(rep, base_path = ".", label, center, sample_size = 100,
                     sample_buffer = 25000, extraction_buffers = seq(1000, 30000, length.out = 100), years){
  pat <- if(missing(rep)) "^FireScar" else paste0("^FireScar_", rep - 1)
  f <- function(files) raster::calc(raster::stack(files, bands = 3), mean, na.rm = TRUE)
  files <- list.files(base_path, pattern = pat, recursive = TRUE, full.names = TRUE)
  yrs <- basename(dirname(files))
  files <- files[yrs %in% years]
  if(!missing(rep)){
    x <- f(files)
  } else {
    files <- split(files, yrs)
    x <- raster::calc(raster::stack(parallel::mclapply(files, f, mc.cores = 32), quick = TRUE), mean, na.rm = TRUE)
  }
  names(x) <- label
  x <- mask(x, snapgrid::swflam)

  cells <- raster::extract(subset(x, 1), center, buffer = sample_buffer, cellnumbers = TRUE)[[1]][, 1]
  cell_sample <- raster::xyFromCell(raster::subset(x, 1), cells)
  cell_sample <- tibble::as_data_frame(cell_sample[sample(1:nrow(cell_sample), sample_size), ])
  purrr::map(1:nrow(cell_sample), ~.buffered_data(x, cell_sample[.x, ], extraction_buffers) %>%
               dplyr::mutate(location = .x)) %>% dplyr::bind_rows() %>% dplyr::mutate(rep = rep)
}

#' Plot map composition for radial buffer around a point
#'
#' Plot the map composition of a raster layer in a radial neighborhood of a point.
#'
#' This function saves a png to disk of a radial buffer around a point. It is used for zooming in spatially around points of interest in raster layers such as those output by ALFRESCO.
#'
#' @param file character, output file.
#' @param x raster layer. Should be discretely valued/categorical.
#' @param cells cell numbers, typically defining a radial clump, but will accept any valid cells numbers.
#' @param radius in meters, catering to NAD83 Alaska Albers projected ALFRESCO map output.
#' @param classes character, the names of the classes assigned to the unique values in \code{x}.
#' @param col color vector the same length as \code{lasses} and the number of unique non-\code{NA} values in \code{x}.
#' @param title character, plot title.
#' @param class_order integer vector, optional reordering of classes when the ordered values in \code{x} are not the desired order.
#' @param base_path character, defaults to working directory.
#' @param width numeric, plot width in pixels.
#' @param height numeric, plot height in pixels.
#'
#' @return nothing is returned, but a file is saved to disk.
#' @export
#'
#' @examples
#' # not run
plot_map_zoom <- function(file, x, cells, radius, classes, col, title = NULL,
                          class_order = seq_along(classes), base_path = ".", width = 1000, height = 1000){
  if(is.null(title)) title <- paste0("Map composition within ", radius / 1000, " km of center")
  x[1:ncell(x)][-cells] <- NA
  x <- raster::ratify(raster::trim(x))
  rat <- raster::levels(x)[[1]]
  rat$class <- factor(classes[class_order], levels = classes)
  levels(x) <- rat[class_order, ]
  Cairo::CairoPNG(file.path(base_path, file), width = width, height = height)
  print(rasterVis::levelplot(
    x, att = "class", maxpixels = 1e6, main = title, col.regions = col,
    xlab = NULL, ylab = NULL, scales = list(draw = FALSE), colorkey = list(space = "bottom", height = 1)))
  grDevices::dev.off()
  invisible()
}
