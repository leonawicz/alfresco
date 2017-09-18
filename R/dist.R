#' Compute probability distribution tables
#'
#' Compute and save to disk probability distributions of random variables from extracted ALFRESCO outputs.
#'
#' This function estimates continuous probability distributions of random variables from extracted ALFRESCO
#' model runs. The random variables available include fire size, burn area and fire frequency by vegetation class, as
#' well as cover area and age for a vegetation class.
#'
#' This function estimates probability densities for outputs from \link{run_alf_extraction}.
#'
#' @param j integer, iterator, the sequence of unique geographic regions available among the extracted data.
#' @param in_dir input directory where extracted data are located in .rds files.
#' @param out_dir output directory where random variables distribution tables are saved as .rds files.
#' @param n_samples numeric, number of samples when distribution sampling is required.
#' @param period character, \code{"historical"} or \code{"projected"}.
#' @param reps integer vector, simulation replicates included in data extraction, e.g., \code{1:200}.
#' @param density.args arguments passed to \code{density} during estimation. Used by the \code{rvtable} package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mclapply(1:n.regions, alf_dist, in_dir, out_dir, mc.cores = n.cores, period = period, reps = reps)
#' }
alf_dist <- function(j, in_dir, out_dir, period, reps, n_samples = 10000,
                     density.args = list(n = 1000, adjust = 0.1, from = 0)){
  id <- basename(in_dir)
  files.hist <- list.files(in_dir, full.names = TRUE, pattern = paste0("^", id, "__.*.CRU.*.RData$"))
  files.all <- list.files(in_dir, full.names = TRUE, pattern=paste0("^", id, "__.*.RData$"))
  if(period == "historical"){
    files <- files.hist
  } else if(period == "projected") {
    files <- setdiff(files.all, files.hist)
  }
  files.locs <- sapply(strsplit(files, "__"), "[", 2)
  locs <- unique(files.locs)
  if(j > length(locs)) return()
  loc <- locs[j]
  files <- files[which(files.locs %in% loc)]
  dat <- stat <- vector("list", length(files))
  for(i in 1:length(files)){
    d <- readRDS(files[i])
    loc.grp <- d$LocGroup[1]
    loc <- d$Location[1]
    if(id != "age"){
      reps.all <- if(is.null(reps)) sort(unique(d$Replicate)) else reps
    }
    if(id == "fsv"){
      d2 <- dplyr::group_by(d, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                            .data[["Location"]], .data[["Var"]], .data[["Year"]],
                            .data[["Replicate"]], .data[["FID"]]) %>%
        dplyr::summarise(Val = sum(.data[["Val"]])) %>% dplyr::ungroup() %>%
        dplyr::mutate(Vegetation = "All") # agg-veg FS
      d <- dplyr::bind_rows(d, d2) %>% dplyr::mutate( # individual and aggregate-veg fire sizes
        Vegetation = factor(.data[["Vegetation"]], levels = unique(.data[["Vegetation"]]))) %>%
        dplyr::group_by(.data[["Phase"]], .data[["Scenario"]], .data[["Model"]], .data[["Location"]],
                        .data[["Var"]], .data[["Vegetation"]], .data[["Year"]])
      d2 <- dplyr::group_by(d, .data[["Replicate"]], add = TRUE) %>%
        dplyr::summarise(BA = sum(.data[["Val"]]), FC = length(.data[["Val"]])) # burn area and fire frequency
      d2 <- d2 %>% dplyr::do(
        .,
        Expanded = suppressMessages(dplyr::right_join( # expand to include reps w/ BA and FC zero
          ., tibble::data_frame(Replicate = as.integer(reps.all)))) %>%
          tidyr::complete(tidyr::nesting(.data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                                  .data[["Location"]], .data[["Var"]], .data[["Vegetation"]], .data[["Year"]]),
                          fill = list(BA = 0L, FC = 0L)) %>%
          tidyr::fill(.data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                      .data[["Location"]], .data[["Var"]], .data[["Vegetation"]], .data[["Year"]])
      ) %>% dplyr::select(.data[["Expanded"]]) %>% tidyr::unnest(.data[["Expanded"]]) %>%
        dplyr::group_by(.data[["Phase"]], .data[["Scenario"]], .data[["Model"]], .data[["Location"]],
                        .data[["Var"]], .data[["Vegetation"]], .data[["Year"]])
      d <- rvtable::rvtable(d, density.args = density.args)
      d2.ba <- dplyr::select(d2, -.data[["FC"]]) %>% dplyr::rename(Val = .data[["BA"]]) %>%
        rvtable::rvtable(density.args = density.args) %>% dplyr::ungroup() %>%
        dplyr::mutate(Var = "Burn Area")
      d2.fc <- dplyr::select(d2, -.data[["BA"]]) %>% dplyr::rename(Val = .data[["FC"]]) %>%
        rvtable::rvtable(density.args = density.args) %>% dplyr::ungroup() %>%
        dplyr::mutate(Var = "Fire Count")
      rm(d2)
      d <- dplyr::bind_rows(d, d2.ba, d2.fc)
      rm(d2.ba, d2.fc)
      gc()
    }
    d <- dplyr::group_by(d, .data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                         .data[["Location"]], .data[["Var"]], .data[["Vegetation"]],
                         .data[["Year"]])
    if(id == "age"){
      skip_veg <- c("Wetland Tundra", "Barren lichen-moss", "Temperate Rainforest")
      d <- dplyr::filter(
        d, !(.data[["Vegetation"]] %in% skip_veg) & sum(.data[["Freq"]]) > 30) # min 30 pixels across reps
      if(nrow(d) == 0) return("Insufficient data.") # skip if any null df for location occurs for age
      d <- dplyr::mutate(d, N = n()) %>% dplyr::group_by(.data[["Year"]], add = TRUE) # nolint
      d <- rvtable::rvtable(d, density.args = density.args)
    }
    if(id == "veg"){
      dx <- d %>% dplyr::do(
        .,
        Expanded = suppressMessages(dplyr::right_join(
          ., tibble::data_frame(Replicate = as.integer(reps.all)))) %>% # expand to include reps w/ age zero
          tidyr::complete(tidyr::nesting(.data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                                         .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                                         .data[["Vegetation"]], .data[["Year"]]),
                          fill = list(Val = 0)) %>%
          tidyr::fill(.data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                      .data[["LocGroup"]], .data[["Location"]], .data[["Var"]],
                      .data[["Vegetation"]], .data[["Year"]])
      ) %>% dplyr::select(.data[["Expanded"]]) %>% tidyr::unnest(.data[["Expanded"]]) %>% dplyr::ungroup() %>%
        dplyr::group_by(.data[["Phase"]], .data[["Scenario"]], .data[["Model"]],
                        .data[["Location"]], .data[["Var"]], .data[["Vegetation"]], .data[["Year"]])
      d <- rvtable::rvtable(d, density.args = density.args)
    }
    get_stats <- function(data){
      data <- rvtable::sample_rvtable(data, n = n_samples, density.args = density.args)
      dplyr::summarise(data, Mean = round(mean(.data[["Val"]])),
                       SD = round(stats::sd(.data[["Val"]]), 1),
                       Min = round(min(.data[["Val"]])),
                       Pct_05 = round(stats::quantile(.data[["Val"]], 0.05)),
                       Pct_10 = round(stats::quantile(.data[["Val"]], 0.10)),
                       Pct_25 = round(stats::quantile(.data[["Val"]], 0.25)),
                       Pct_50 = round(stats::quantile(.data[["Val"]], 0.50)),
                       Pct_75 = round(stats::quantile(.data[["Val"]], 0.75)),
                       Pct_90 = round(stats::quantile(.data[["Val"]], 0.90)),
                       Pct_95 = round(stats::quantile(.data[["Val"]], 0.95)),
                       Max = round(max(.data[["Val"]]))) %>%
        dplyr::group_by(.data[["Year"]], add = TRUE)
    }
    s <- get_stats(d)
    dat[[i]] <- d
    stat[[i]] <- s
    print(i)
  }
  dir.create(statsDir <- file.path(out_dir, "stats", loc.grp, loc),
             recursive = TRUE, showWarnings = FALSE)
  dir.create(samplesDir <- file.path(out_dir, "samples", loc.grp, loc),
             recursive = TRUE, showWarnings = FALSE)
  prefix <- if(stat[[1]]$Scenario[1] == "Historical") "historical" else "projected"
  if(id == "fsv"){
    dat <- dplyr::bind_rows(dat)
    d_alf_fs <- dplyr::filter(dat, .data[["Var"]] == "Fire Size")
    d_alf_ba <- dplyr::filter(dat, .data[["Var"]] == "Burn Area")
    d_alf_fc <- dplyr::filter(dat, .data[["Var"]] == "Fire Count")
    stats_alf_fire <- dplyr::bind_rows(stat)
    saveRDS(d_alf_fs, file = file.path(samplesDir, paste0(prefix, "_fs.rds")))
    saveRDS(d_alf_ba, file = file.path(samplesDir, paste0(prefix, "_ba.rds")))
    saveRDS(d_alf_fc, file = file.path(samplesDir, paste0(prefix, "_fc.rds")))
    saveRDS(stats_alf_fire, file = file.path(statsDir, paste0(prefix, "_fsv_stats.rds")))
  } else {
    dat <- dplyr::bind_rows(dat)
    stat <- dplyr::bind_rows(stat)
    file_dat <- paste0(prefix, "_", id, ".rds")
    file_stat <- paste0(prefix, "_", id, "_stats.rds")
    saveRDS(dat, file = file.path(samplesDir, file_dat))
    saveRDS(stat, file = file.path(statsDir, file_stat))
  }
  invisible()
}
