#' Compute fire size by vegetation class
#'
#' Compute and save to disk exact fire size by vegetation class from extracted ALFRESCO outputs.
#'
#' This function computes exact annual fire sizes by vegetation from extracted ALFRESCO
#' model runs. If discrete probability distributions of annual fire by vegetation class are required, see \link{alf_dist}.
#' This function uses the same inputs, the extraction .rds files created by \link{run_alf_extraction}.
#' It essentially does nothing more than row bind the data frames from the extraction files without computing anything other statistics.
#' This yields one file in a project directory that covers all extracted fire size by vegetation class data for a unique geographic region.
#'
#' @param in_dir input \code{fsv} directory where extracted \code{fsv} data are located in .rds files.
#' @param out_dir top level (project) output directory: the function will add a \code{fsv/[group]/[region]} subdirectory where the output file is saved.
#' @param group character, the region group, used for file selection in \code{in_dir}.
#' @param region character, the region, used for file selection in \code{in_dir}.
#' @param period character, \code{"historical"} or \code{"projected"}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' in_dir <- "/atlas_scratch/mfleonawicz/alfresco/JFSP/extractions/fsv"
#' out_dir <- "/workspace/UA/mfleonawicz/data/alfresco/JFSP"
#' fsv(in_dir, out_dir, "FMO", "MFC buffers", "historical")
#' }
fsv <- function(in_dir, out_dir, group, region, period){
  id <- "fsv"
  project <- basename(dirname(dirname(in_dir)))
  inputs <- alf_dist_inputs(project) %>%
    dplyr::filter(.data[["Var"]] == id & .data[["LocGroup"]] == group & .data[["Location"]] == region)
  fmo <- "FMO" %in% names(inputs)
  if(fmo) all_fmo <- unique(inputs$FMO)
  uloc <- unique(paste(inputs$LocGroup, inputs$Location, sep = "__"))
  uloc <- strsplit(uloc, "__")[[1]]
  pat <- paste0("^", id, ".*.", uloc[1], "__",
                gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", uloc[2])), "__.*.") # nolint
  pat <- if(period == "historical") paste0(pat, "historical.*.rds$") else paste0(pat, "rcp.*.rds$")
  files <- list.files(in_dir, full.names = TRUE, pattern = pat)
  d <- purrr::map(files, ~readRDS(.x) %>% dplyr::ungroup()) %>% dplyr::bind_rows()
  if(fmo) d <- dplyr::mutate(FMO = factor(.data[["FMO"]], levels = all_fmo))
  dir.create(out_dir <- file.path(out_dir, "fsv", uloc[1], uloc[2]),
             recursive = TRUE, showWarnings = FALSE)
  prefix <- if(d$Scenario[1] == "Historical") "historical" else "projected"
  saveRDS(d, file = file.path(out_dir, paste0(prefix, "_fsv.rds")))
  invisible()
}
