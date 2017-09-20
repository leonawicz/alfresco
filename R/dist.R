#' Compute probability distribution tables
#'
#' Compute and save to disk probability distributions of random variables from extracted ALFRESCO outputs.
#'
#' This function estimates discrete probability distributions of random variables from extracted ALFRESCO
#' model runs. The random variables available include fire size, burn area and fire frequency by vegetation class, as
#' well as cover area and age for a vegetation class.
#'
#' This function estimates probability distributions for outputs from \link{run_alf_extraction}.
#'
#' @param j integer, iterator, the sequence of unique geographic regions available among the extracted data.
#' @param in_dir input directory where extracted data are located in .rds files.
#' @param out_dir output directory where random variables distribution tables are saved as .rds files.
#' @param period character, \code{"historical"} or \code{"projected"}.
#' @param reps integer vector, simulation replicates included in data extraction, e.g., \code{1:200}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mclapply(1:n.regions, alf_dist, in_dir, out_dir, mc.cores = n.cores, period = period, reps = reps)
#' }
alf_dist <- function(j, in_dir, out_dir, period, reps){
  id <- basename(in_dir)
  project <- basename(dirname(dirname(in_dir)))
  inputs <- alf_dist_inputs(project) %>% dplyr::filter(.data[["Var"]] == id)
  fmo <- "FMO" %in% names(inputs)
  if(fmo) all_fmo <- unique(inputs$FMO)
  uloc <- unique(paste(inputs$LocGroup, inputs$Location, sep = "__"))[j]
  uloc <- strsplit(uloc, "__")[[1]]
  inputs <- dplyr::filter(inputs, .data[["LocGroup"]] == uloc[1] & .data[["Location"]] == uloc[2])
  pat <- paste0("^", id, ".*.", uloc[1], "__", uloc[2], "__.*.")
  pat <- if(period == "historical") paste0(pat, "historical.*.rds$") else paste0(pat, "rcp.*.rds$")
  files <- list.files(in_dir, full.names = TRUE, pattern = pat)
  dat <- vector("list", length(files))
  for(i in seq_along(files)){
    d <- readRDS(files[i]) %>% dplyr::ungroup() %>%
      dplyr::mutate(Vegetation = as.character(.data[["Vegetation"]]))
    if(fmo) d <- dplyr::mutate(d, FMO = factor(.data[["FMO"]], levels = all_fmo))
    if(id != "age"){
      reps.all <- if(is.null(reps)) sort(unique(d$Replicate)) else reps
    }
    if(id == "fsv"){
      fire_vars <- c("Fire Count", "Burn Area", "Fire Size")
      d2 <- dplyr::group_by(d, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                            .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                            .data[["Year"]], .data[["Replicate"]], .data[["FID"]])
      if(fmo) d2 <- dplyr::group_by(d2, .data[["FMO"]], add = TRUE)
      d2 <- dplyr::summarise(d2, Val = sum(.data[["Val"]])) %>% dplyr::ungroup() %>%
        dplyr::mutate(Vegetation = "All") # agg-veg FS
      d <- dplyr::bind_rows(d, d2) %>% dplyr::mutate( # individual and aggregate-veg fire sizes
        Vegetation = factor(.data[["Vegetation"]], levels = unique(.data[["Vegetation"]])))
      d <- dplyr::group_by(d, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]], .data[["LocGroup"]],
                           .data[["Location"]], .data[["Var"]], .data[["Vegetation"]], .data[["Year"]])
      if(fmo) d <- dplyr::group_by(d, .data[["FMO"]], add = TRUE)
      d2 <- dplyr::group_by(d, .data[["Replicate"]], add = TRUE) %>%
        dplyr::summarise(BA = sum(.data[["Val"]]), FC = length(.data[["Val"]])) # burn area and fire frequency
      if(fmo){
        d2 <- d2 %>% dplyr::do(
          .,
          Expanded = suppressMessages(dplyr::right_join( # expand to include reps w/ BA and FC zero
            ., tibble::data_frame(Replicate = as.integer(reps.all)))) %>%
            tidyr::complete(tidyr::nesting(Phase, Scenario, Model, LocGroup, Location, Var, Vegetation, Year,
                                           FMO),
                            fill = list(BA = 0L, FC = 0L)) %>%
            tidyr::fill(.data[["Phase"]], .data[["Scenario"]], .data[["Model"]], .data[["LocGroup"]],
                        .data[["Location"]], .data[["Var"]], .data[["Vegetation"]], .data[["Year"]],
                        .data[["FMO"]]))
      } else {
        d2 <- d2 %>% dplyr::do(
          .,
          Expanded = suppressMessages(dplyr::right_join( # expand to include reps w/ BA and FC zero
            ., tibble::data_frame(Replicate = as.integer(reps.all)))) %>%
            tidyr::complete(tidyr::nesting(Phase, Scenario, Model, LocGroup, Location, Var, Vegetation, Year),
                            fill = list(BA = 0L, FC = 0L)) %>%
            tidyr::fill(.data[["Phase"]], .data[["Scenario"]], .data[["Model"]], .data[["LocGroup"]],
                        .data[["Location"]], .data[["Var"]], .data[["Vegetation"]], .data[["Year"]]))
      }
     d2 <- dplyr::select(d2, .data[["Expanded"]]) %>% tidyr::unnest(.data[["Expanded"]]) %>%
        dplyr::ungroup()
      d <- dplyr::ungroup(d) %>% dplyr::select(-.data[["FID"]], -.data[["Replicate"]]) %>%
        rvtable::rvtable(discrete = TRUE)
      d2.ba <- dplyr::select(d2, -.data[["FC"]], -.data[["Replicate"]]) %>%
        dplyr::rename(Val = .data[["BA"]]) %>% rvtable::rvtable(discrete = TRUE) %>%
        dplyr::mutate(Var = fire_vars[2])
      d2.fc <- dplyr::select(d2, -.data[["BA"]], -.data[["Replicate"]]) %>%
        dplyr::rename(Val = .data[["FC"]]) %>% rvtable::rvtable(discrete = TRUE) %>%
        dplyr::mutate(Var = fire_vars[1])
      rm(d2)
      d <- dplyr::bind_rows(d, d2.ba, d2.fc) %>%
        dplyr::mutate(Var = factor(.data[["Var"]], levels = fire_vars))
      rm(d2.ba, d2.fc)
      gc()
    }
    if(id == "age"){
      skip_veg <- c("Wetland Tundra", "Barren lichen-moss", "Temperate Rainforest")
      d <- dplyr::filter(d, !(.data[["Vegetation"]] %in% skip_veg)) %>%
        rvtable::rvtable(Val = "Age", Prob = "Freq", discrete = TRUE)
    }
    if(id == "veg"){
      d <- dplyr::select(d, -.data[["Replicate"]]) %>%
        rvtable::rvtable(d, discrete = TRUE)
    }
    dat[[i]] <- d
    cat(paste("Location:", j, ". RV set:", id, ". File:", i, "\n"))
  }
  dir.create(dist_dir <- file.path(out_dir, "distributions", uloc[1], uloc[2]),
             recursive = TRUE, showWarnings = FALSE)
  dat <- dplyr::bind_rows(dat)
  prefix <- if(dat$Scenario[1] == "Historical") "historical" else "projected"
  if(id == "fsv"){
    d_alf_fs <- dplyr::filter(dat, .data[["Var"]] == "Fire Size") %>%
      rvtable::rvtable(discrete = TRUE)
    d_alf_ba <- dplyr::filter(dat, .data[["Var"]] == "Burn Area") %>%
      rvtable::rvtable(discrete = TRUE)
    d_alf_fc <- dplyr::filter(dat, .data[["Var"]] == "Fire Count") %>%
      rvtable::rvtable(discrete = TRUE)
    out_ids <- c("fc", "ba", "fs")
    out <- paste0(prefix, "_", out_ids, ".rds")
    cat(paste("Location:", j, ". Saving file:", out[1], "\n"))
    saveRDS(d_alf_fc, file = file.path(dist_dir, out[1]))
    cat(paste("Location:", j, ". Saving file:", out[2], "\n"))
    saveRDS(d_alf_ba, file = file.path(dist_dir, out[2]))
    cat(paste("Location:", j, ". Saving file:", out[2], "\n"))
    saveRDS(d_alf_fs, file = file.path(dist_dir, out[3]))
  } else {
    if(id == "veg") dat <- rvtable::rvtable(dat, discrete = TRUE)
    if(id == "age") dat <- rvtable::rvtable(dat, Val = "Age", Prob = "Freq", discrete = TRUE)
    file <- paste0(prefix, "_", id, ".rds")
    cat(paste("Location:", j, ". Saving file:", file, "\n"))
    saveRDS(dat, file = file.path(dist_dir, file))
  }
  invisible()
}

#' Table inputs from extracted data
#'
#' Create a data frame of inputs from extraction .rds files for probability distribution estimation.
#'
#' This function creates a data frame containing columns of metadata based on the .rds files
#' stored in the \code{fsv}, \code{veg} and \code{age} extraction subdirectories.
#' These files are output during ALFRESCO data extraction by \link{run_alf_extraction}.
#' The columns of the data frame describe the random variable set (\code{fsv}, \code{veg} or \code{age}),
#' location group (region set), specific location (region),
#' model (a GCM or CRU), RCP (including "historical"), and the fire management options (FMO) treatment.
#'
#' If \code{in_dir} is not provided, a default is taken from \code{alfdef()$alf_extract_dir} and \code{project}, so \code{project}
#' must be provided. \code{project} is ignored if the full project path is provided via \code{in_dir}.
#' Provide a directory or provide a project name and rely on the default extractions directory.
#'
#' @param project character, see details.
#' @param in_dir path to data extractions, the parent directory with the three subdirectories, \code{fsv}, \code{veg} and \code{age}. See details.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{alf_dist_inputs("JFSP")}
alf_dist_inputs <- function(project, in_dir){
  if(missing(project) & missing(in_dir))
    stop("`project` and `in_dir` cannot both be missing.")
  vars <- c("fsv", "veg", "age")
  ids <- c("Var", "LocGroup", "Location", "Model", "RCP", "FMO")
  if(missing(in_dir))
    in_dir <- file.path(alfdef()$alf_extract_dir, project, "extractions")
  in_dir <- file.path(in_dir, vars)
  x <- purrr::map(in_dir, ~do.call(rbind, strsplit(list.files(file.path(.x)), "__")) %>%
                    tibble::as_data_frame()) %>% dplyr::bind_rows()
  names(x) <- ids[1:ncol(x)]
  if(ncol(x) == length(ids)) x <- dplyr::mutate(x, FMO = gsub("\\.rds", "", .data[["FMO"]]))
  x
}
