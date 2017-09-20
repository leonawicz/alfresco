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
  if(fmo) d <- dplyr::mutate(d, FMO = factor(.data[["FMO"]], levels = all_fmo))
  dir.create(out_dir <- file.path(out_dir, "fsv", uloc[1], uloc[2]),
             recursive = TRUE, showWarnings = FALSE)
  prefix <- if(d$Scenario[1] == "Historical") "historical" else "projected"
  saveRDS(d, file = file.path(out_dir, paste0(prefix, "_fsv.rds")))
  invisible()
}

#' Fire size and cumulative burn data frame
#'
#' Generate a data frame of exact fire sizes and cumulative burn for the union of all vegetation classes in a region.
#'
#' This function takes an exact-type fire size by vegetation class .rds file or the data frame it contains if already loaded into the R session.
#' The source file is one created by \link{fsv}.
#' Vegetation-specific fire sizes are aggregated by unique fire IDs per simulation replicate, year and,
#' if present, fire management options treatment levels. It also adds a column for cumulative burn area
#' alongside ascending fire sizes.
#'
#' @param x character or data frame. The file name of the or the data frame object from that file. See details.
#'
#' @return a data frame.
#' @export
#' @seealso fsv cbdf
#'
#' @examples
#' \dontrun{fsdf("historical_fsv.rds")}
fsdf <- function(x){
  x <- .avdf(x) %>% dplyr::select(-.data[["Year"]])
  fmo <- "FMO" %in% names(x)
  if(fmo){
    x <- dplyr::arrange(x, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                        .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                        .data[["FMO"]], .data[["Replicate"]], .data[["FS"]])
  } else {
    x <- dplyr::arrange(x, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                        .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                        .data[["Replicate"]], .data[["FS"]])
  }
  x <- dplyr::group_by(x, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                       .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                       .data[["Replicate"]])
  if(fmo) x <- dplyr::group_by(x, .data[["FMO"]], add = TRUE)
  dplyr::mutate(x, CAB = cumsum(.data[["FS"]])) %>% dplyr::ungroup()
}

#' Cumulative burn over time data frame
#'
#' Generate a data frame of cumulative burn over time for the union of all vegetation classes in a region.
#'
#' This function takes an exact-type fire size by vegetation class .rds file or the data frame it contains if already loaded into the R session.
#' The source file is one created by \link{fsv}.
#' Vegetation-specific fire sizes are aggregated by unique fire IDs per simulation replicate, year and,
#' if present, fire management options treatment levels. It then adds a column for cumulative burn area
#' alongside ascending years.
#'
#' @param x character or data frame. The file name of the or the data frame object from that file. See details.
#'
#' @return a data frame.
#' @export
#' @seealso fsv fsdf
#'
#' @examples
#' \dontrun{cbdf("historical_fsv.rds")}
cbdf <- function(x){
  x <- .avdf(x, by_year = TRUE)
  fmo <- "FMO" %in% names(x)
  if(fmo){
    x <- dplyr::arrange(x, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                        .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                        .data[["FMO"]], .data[["Replicate"]], .data[["Year"]], .data[["FS"]])
  } else {
    x <- dplyr::arrange(x, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                        .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                        .data[["Replicate"]], .data[["Year"]], .data[["FS"]])
  }
  x <- dplyr::group_by(x, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                       .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                       .data[["Replicate"]], .data[["Year"]])
  if(fmo) x <- dplyr::group_by(x, .data[["FMO"]], add = TRUE)
  dplyr::mutate(x, CAB = cumsum(.data[["FS"]])) %>% dplyr::ungroup()
}

.avdf <- function(x, by_year = FALSE){
  x <- if(inherits(x, "character")) readRDS(x) else x
  fmo <- "FMO" %in% names(x)
  x <- dplyr::group_by(x, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                       .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                       .data[["Replicate"]], .data[["Year"]])
  if(!by_year) x <- dplyr::group_by(x, .data[["FID"]], add = TRUE)
  if(fmo) x <- dplyr::group_by(x, .data[["FMO"]], add = TRUE)
  dplyr::summarise(x, FS = sum(.data[["Val"]])) %>% dplyr::ungroup() %>%
    dplyr::select(-.data[["FID"]])
}
