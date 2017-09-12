#' Obtain total cumulative burn area ratios
#'
#' Obtain ratios of modeled total CBA to that of observed data as well as to baseline model run output.
#'
#' The table of ratios show minimum, mean and maximums across ALFRESCO replicates for CBA and
#' the corresponding percent change vs. both the empirically observed CBA and the baseline/status quo (no fire management options)
#' model run over the same historical period.
#'
#' @param files vector of workspace files from ALFRESCO outputs Shiny app; basenames expected to begin with \code{fmo} formate, e.g., \code{fmo00s00i}.
#' @param years numeric, defaults to \code{1950:2013}.
#' @param domain character, \code{"Full"} or \code{"Masked"}, the full ALFRESCO run domain or the subregion masked to the extent of observed historical fire.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' indir <- "C:/github/Flammability/data/fmo/app_workspaces"
#' files <- list.files(indir, full.names = TRUE)
#' x <- ba_ratios(files)
#'
#' library(dplyr)
#' select(x, -PctIgSupp) %>% group_by(PctFsSupp) %>% summarise_all(function(x) round(mean(x), 3))
#' }
ba_ratios <- function(files, years = 1950:2013, domain = "Full"){
  load(files[1], envir = environment())
  fmo <- substr(basename(files[1]), 4, 9)
  d <- dplyr::filter(d.fs, .data[["Domain"]] == domain & .data[["Year"]] %in% years) %>%
    dplyr::group_by(.data[["Replicate"]]) %>% dplyr::summarise(BA = sum(.data[["FS"]]))
  if(fmo == "00s00i"){
    sq <- (dplyr::filter(d, .data[["Replicate"]] != "Observed") %>%
             dplyr::summarise(BA = mean(.data[["BA"]])))$BA
  } else {
    stop("baseline FMO workspace expected first in files list.")
  }
  purrr::map(
    files,
    ~({
      d.fs <- NULL
      load(.x, envir = environment())
      fmo <- substr(basename(.x), 4, 9)
      d <- dplyr::filter(d.fs, .data[["Domain"]] == "Full") %>%
        dplyr::group_by(.data[["Replicate"]]) %>% dplyr::summarise(BA = sum(.data[["FS"]]))
      obs <- dplyr::filter(d, .data[["Replicate"]] == "Observed")$BA
      dplyr::filter(d, .data[["Replicate"]] != "Observed") %>% dplyr::summarise(
        Min = min(.data[["BA"]]), Mean = mean(.data[["BA"]]), Max = max(.data[["BA"]])) %>%
        dplyr::mutate(
          PctObsLB = .data[["Min"]] / obs,
          PctObsMean = .data[["Mean"]] / obs,
          PctObsUB = .data[["Max"]] / obs,
          PctSQLB = .data[["Min"]] / sq,
          PctSQMean = .data[["Mean"]] / sq,
          PctSQUB = .data[["Max"]] / sq,
          PctFsSupp = as.numeric(substr(fmo, 1, 2)),
          PctIgSupp = as.numeric(substr(fmo, 4, 5))
        )
    })
  ) %>% dplyr::bind_rows()
}
