.buffered_data <- function(x, xy, bfrs, convert = FALSE){
  xy <- dplyr::slice(xy, rep(1, length(bfrs)))
  if(convert) xy <- wgs2ak(xy)
  x[snapgrid::swveg == 0] <- NA
  e <- raster::extract(x, xy, fun = function(x, ...) mean(x > 0, ...), buffer = bfrs)
  e[is.na(e)] <- 0
  tibble::as_data_frame(e) %>% dplyr::mutate(buffer = bfrs / 1000) %>% dplyr::mutate(run = names(x)) %>%
    dplyr::mutate(period = ifelse(substr(run, 1, 3) %in% c("his", "cru", "obs"), "Historical", "Projected"))
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
