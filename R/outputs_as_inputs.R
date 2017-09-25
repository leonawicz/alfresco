#' Copy ALFRESCO output maps
#'
#' Copy ALFRESCO output maps to new location for use as inputs to subsequent model runs.
#'
#' This function copies ALFRESCO output maps from a single, "final" year where \code{year} is chosen to be
#' one less than the starting year of the subsequent model run. For example, a run beginning in 1950
#' would use as inputs 1949 output maps from a prior model run.
#' \code{year} need not be the final year of the original run, but it is final
#' in the context that the following year will be part of a new, subsequent run.
#' The output directory is \code{<top_dir>/secondary_run_inputs/<domain>/<run_name>}.
#'
#' @param domain character, usually \code{"Statewide"} or \code{"Noatak"}.
#' @param run_name character.
#' @param year numeric, the "final" year of a run. See details.
#' @param owner character, an email address, defaults to \code{alfdef()$atlas_run_owner_dir}.
#' @param top_dir character, defaults to \code{alfdef()$atlas_shiny_dir}.
#' @param template_raster character, defaults to \code{alfdef()$age_spinups}.
#' @param suffix appended to output directory, defaults to \code{"runs"}.
#' @param mc.cores number of processors, defaults to 32.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' outputs_as_inputs("Statewide", "fmo00s00i_historical_CRU32", 1949)
#' outputs_as_inputs("Statewide", "fmo00s00i_historical_CRU32", 2013)
#' }
outputs_as_inputs <- function(domain, run_name, year, owner = alfdef()$atlas_run_owner_dir,
                              top_dir = alfdef()$atlas_shiny_dir, template_raster = alfdef()$age_spinups,
                              suffix = "runs", mc.cores = 32){
  in_dir <- paste0(top_dir, "/Runs_", domain, "/", owner, "/", run_name, "/Maps") # nolint
  dir.create(
    out_dir <- file.path(top_dir, "secondary_run_inputs", domain, run_name), # nolint
    showWarnings = FALSE, recursive = TRUE)
  files <- list.files(in_dir, pattern = paste0(year, "\\.tif"), full.names = TRUE)
  files <- files[-which(substr(basename(files), 1, 8) == "FireScar")]
  r <- raster::raster(template_raster)
  parallel::mclapply(seq_along(files), .write_outputs_as_inputs, files = files,
                     template = r, out_dir = out_dir, mc.cores = mc.cores)
  invisible()
}

.write_outputs_as_inputs <- function(i, files, template, out_dir){
  r <- raster::raster(files[i])
  r <- raster::extend(r, template)
  if(substr(basename(files[i]), 1, 3) == "Age")
    raster::writeRaster(r, file.path(out_dir, basename(files[i])), datatype = "INT4S", overwrite = TRUE)
  if(substr(basename(files[i]), 1, 3) != "Age")
    raster::writeRaster(r, file.path(out_dir, basename(files[i])), datatype = "INT1U", overwrite = TRUE)
  invisible()
}
