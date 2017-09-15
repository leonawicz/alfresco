#' ALFRESCO extraction stop error checks
#'
#' Helper function to check for missing objects and stop process if necessary.
#'
#' @return invisible, or an error.
#' @export
#'
#' @examples
#' \dontrun{extract_alf_stops}
extract_alf_stops <- function(){
  if(!exists("modelIndex", envir = .GlobalEnv))
    stop("Must provide a modelIndex 1 to 15, e.g., modelIndex=1")
  stopifnot(length(get("modelIndex", envir = .GlobalEnv)) == 1)
  if(!exists("domain", envir = .GlobalEnv))
    stop("Must provide domain, e.g., domain = 'akcan1km' or domain = 'ak1km'")
  if(!exists("years", envir = .GlobalEnv))
    stop("Must provide a year range, e.g., 1950:2013, 2008:2100, based on project.")
  if(!exists("reps", envir = .GlobalEnv))
    stop("Must provide replicates as integer(s) 1:200, e.g., reps = 1:25")
  if(!exists("project", envir = .GlobalEnv)) stop("Must provide a `project` name.")
  invisible()
}

#' Load cell index data frame for subregions
#'
#' Load data frame of raster grid cell indices defining groups of spatial subregions.
#'
#' @param domain character, the ALFRESCO run spatial domain, either \code{"akcan1km"} or \code{"ak1km"}.
#' @param cells character, file name of cell index table rds file. If missing, default path taken from \code{snapprep::snapdef()} based on \code{domain}.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{cells <- get_domain_cells()}
get_domain_cells <- function(domain = "akcan1km", cells){
  if(domain=="akcan1km"){
    if(missing(cells)) cells <- snapprep::snapdef()$cells_akcan1km2km
    cells <- readRDS(cells) %>% dplyr::filter(.data[["Source"]] == "akcan1km") %>%
      dplyr::ungroup() %>% dplyr::select(-.data[["Source"]]) %>%
      dplyr::group_by(.data[["LocGroup"]], .data[["Location"]])
  } else if(domain=="ak1km"){
    if(missing(cells)) cells <- snapprep::snapdef()$cells_ak1km
    cells <- readRDS(cells) %>% dplyr::filter(.data[["Source"]] == "ak1km") %>%
      dplyr::ungroup() %>% dplyr::select(-.data[["Source"]]) %>%
      dplyr::group_by(.data[["LocGroup"]], .data[["Location"]])
  }
  if(exists("locgroup", envir = .GlobalEnv)){
    locgroup <- gsub("_", " ", locgroup)
    cat("locgroup = "); cat(locgroup); cat("\n")
    if(is.character(locgroup))
      cells <- dplyr::filter(cells, .data[["LocGroup"]] %in% locgroup)
    if(is.numeric(locgroup))
      cells <- dplyr::filter(cells, .data[["LocGroup"]] %in% unique(cells$LocGroup)[locgroup])
    print(unique(cells$LocGroup))
    stopifnot(nrow(cells) > 0)
  }
  cells
}

#' Get vegetation labels
#'
#' Get vegetation labels based on ALFRESCO domain, which is suggestive of the vegetation base map.
#'
#' @param domain character, the ALFRESCO run spatial domain, either \code{"akcan1km"} or \code{"ak1km"}.
#'
#' @return a vector of vegetation labels.
#' @export
#'
#' @examples
#' get_veg_labels()
get_veg_labels <- function(domain = "akcan1km"){
  if(domain == "akcan1km"){
    c("Black Spruce", "White Spruce", "Deciduous", "Shrub Tundra", "Graminoid Tundra",
      "Wetland Tundra", "Barren lichen-moss", "Temperate Rainforest")
  } else if(domain == "ak1km"){
    c("Alpine Tundra", "Black Spruce", "White Spruce", "Deciduous", "Shrub Tundra",
      "Graminoid Tundra", "Wetland Tundra")
  } else {
    stop("`domain` must be 'akcan1km' or 'ak1km'.")
  }
}

#' Get ALFRESCO output directories
#'
#' Get ALFRESCO output directories for an ALFRESCO project.
#'
#' This function supports data extraction procedures for ALFRESCO model geotiff map outputs.
#' A project refers to a collection of output directories pertaining to the set of climate models and emissions scenarios/RCPs
#' that were used in project simulations. This function returns the full file path to each pertinent directory, given a valid \code{domain}
#' and \code{project}.
#' Valid projects for Alaska/western Canada include \code{"IEM"} and \code{"FMO_Calibrated"}. For Alaska "statewide", it is \code{"CMIP5_SW"}.
#'
#' @param domain character, the ALFRESCO run spatial domain, either \code{"akcan1km"} or \code{"ak1km"}.
#' @param project character, valid projects based on the domain. See details.
#' @param cru logical, whether data extraction is for historical years (ALFRESCO runs based on CRU data) or projected years (GCM data).
#' @param cru_id character, label for CRU data. Defaults to \code{"CRU 3.2"}.
#'
#' @return a vector of project climate model/emissions scenario ALFRESCO output directories.
#' @export
#'
#' @examples
#' \dontrun{domain = "ak1km", project = "CMIP5_SW", cru = TRUE}
get_out_dirs <- function(domain, project, cru, cru_id = "CRU 3.2"){
  if(!domain %in% c("akcan1km", "ak1km")) stop("`domain` must be 'akcan1km' or 'ak1km'.")
  if(!project %in% .valid_alf_projects(domain))
    stop(paste(project, "is not a valid project in the", domain, "domain"))
  .alf_project_dirs(project, cru, cru_id)
}

.valid_alf_projects <- function(domain){
  switch(domain,
         "akcan1km" = c("IEM", "FMO_Calibrated"),
         "ak1km" = "CMIP5_SW")
}

.alf_project_dirs <- function(project, cru, cru_id){
  if(cru){
    pat <- gsub("\\.| ", "", cru_id)
  } else {
    pat <- switch(project, "IEM" = ".*.sres.*.", "FMO_Calibrated" = ".*.rcp.*.",
                  "CMIP5_SW" = "^rcp.*.", "JFSP" = "^fmo.*.")
  }
  switch(project,
         "IEM" = list.files(
           "/atlas_scratch/mfleonawicz/alfresco/IEM/outputs/FinalCalib", # nolint
           pattern = pat, full.names = TRUE),
         "FMO_Calibrated" = list.files(
           "/atlas_scratch/apbennett/Calibration/HighCalib/FMO_Calibrated", # nolint
           pattern = pat, full.names = TRUE),
         "CMIP5_SW" = list.files(
           "/atlas_scratch/mfleonawicz/alfresco/CMIP5_Statewide/outputs/5m", # nolint
           pattern = pat, full.names = TRUE),
         "JFSP" = list.files(
           "/atlas_scratch/mfleonawicz/alfresco/JFSP/outputs", # nolint
           pattern = pat, full.names = TRUE))
}

swapModelName <- function(x){
  switch(x,
         cccma_cgcm3_1 = "CCCMAcgcm31", gfdl_cm2_1 = "GFDLcm21", miroc3_2_medres = "MIROC32m",
         mpi_echam5 = "MPIecham5", ukmo_hadcm3="ukmoHADcm3",
         CCSM4 = "CCSM4", "GFDL-CM3" = "GFDL-CM3", "GISS-E2-R" = "GISS-E2-R",
         "IPSL-CM5A-LR" = "IPSL-CM5A-LR", "MRI-CGCM3" = "MRI-CGCM3"
  )
}

swapScenarioName <- function(x){
  switch(x,
         sresb1 = "SRES B1", sresa1b = "SRES A1B", sresa2 = "SRES A2",
         rcp45 = "RCP 4.5", rcp60 = "RCP 6.0", rcp85 = "RCP 8.5"
  )
}

getPhase <- function(x){
  switch(x,
         sresb1 = "AR4", sresa1b = "AR4", sresa2 = "AR4",
         rcp45 = "AR5", rcp60 = "AR5", rcp85 = "AR5"
  )
}

#' Run ALFRESCO data extraction
#'
#' Extract data for different variables from ALFRESCO model outputs.
#'
#' The extracted data depends on \code{type}, which is \code{"fsv"} for fire size by vegetation class data or
#' \code{"av"} for vegetation age and vegetation cover area data.
#' A project refers to a collection of output directories pertaining to the set of climate models and emissions scenarios/RCPs
#' that were used in project simulations. This function returns the full file path to each pertinent directory, given a valid \code{domain}
#' and \code{project}.
#' Valid projects for Alaska/western Canada include \code{"IEM"} and \code{"FMO_Calibrated"}. For Alaska "statewide", it is \code{"CMIP5_SW"}.
#' \code{mc.cores} is used explicitly when \code{rmpi = FALSE} for \code{parallel::mclapply} instead of multi-node processing.
#'
#' @param domain character, the ALFRESCO run spatial domain, either \code{"akcan1km"} or \code{"ak1km"}.
#' @param type \code{"fsv"} or \code{"av"}. See details.
#' @param loop_by \code{"rep"} or \code{"year"} (default).
#' @param main_dir input directory.
#' @param out_dir output directory. If missing, a default is provided by \code{alfdef()$alf_extract_dir}.
#' @param project character, valid projects based on the domain. See details.
#' @param reps integer vector, simulation replicates included in data extraction, e.g., \code{1:200}.
#' @param years ALFRESCO model run years included in data extraction.
#' @param cells data frame of raster grid cell indices appropriate to \code{domain}. See \code{get_domain_cells}.
#' @param veg_labels vegetation labels appropriate to domain/vegetation input map for ALFRESCO runs. See \code{get_veg_labels}.
#' @param cru logical, whether data extraction is for historical years (ALFRESCO runs based on CRU data) or projected years (GCM data).
#' @param cru_id character, label for CRU data. Defaults to \code{"CRU 3.2"}.
#' @param itervar integer vector, iterator, defaults to \code{1:length(years)}.
#' @param mc.cores number of processors. See details.
#' @param rmpi logical, use Rmpi. Defaults to \code{TRUE}.
#'
#' @return invisible, writes files.
#' @export
#'
#' @examples
#' # Not run; decontextualized example.
#' \dontrun{run_alf_extraction(type = "fsv", main_dir = "Maps", project = "MyProject",
#'   reps = 1:200, years = 2014:2099, cells = cells, veg_labels = veg_labels)}
run_alf_extraction <- function(domain = "akcan1km", type, loop_by = "rep", main_dir, out_dir,
                               project, reps, years, cells, veg_labels,
                               cru = FALSE, cru_id = "CRU 3.2",
                               itervar = seq_along(years), mc.cores = 32, rmpi = TRUE){
  if(!domain %in% c("akcan1km", "ak1km"))
    stop("`domain` must be 'akcan1km' or 'ak1km'.")
  if(!type %in% c("fsv", "av"))
    stop("`type` must be 'fsv' or 'av'.")
  if(missing(out_dir)) out_dir <- alfdef()$alf_extract_dir
  scen.levels <- c("SRES B1", "SRES A1B", "SRES A2", "RCP 4.5", "RCP 6.0", "RCP 8.5")
  modname <- basename(dirname(main_dir))
  mod.scen <- unlist(strsplit(modname, "\\."))
  if(domain == "ak1km") mod.scen <- rev(mod.scen)

  if(type == "fsv"){
    cat(paste("Compiling", type, "statistics...\n"))
    cells <- dplyr::select(cells, -.data[["Cell_rmNA"]])
    if(rmpi){
      x <- Rmpi::mpi.remote.exec(
        extract_alf(i = itervar[rmpi_proc_id], type = type, loop_by = loop_by, main_dir = main_dir, reps = reps,
                    years = years, cells = cells, veg_labels = veg_labels) )
      x <- dplyr::bind_rows(x)
    } else {
      len <- length(itervar)
      if(len <= mc.cores){
        x <- parallel::mclapply(itervar, extract_alf, type = type, loop_by = loop_by,
                                  main_dir = main_dir, reps = reps, years = years,
                                  cells = cells, veg_labels = veg_labels, mc.cores = mc.cores)
        x <- dplyr::bind_rows(x)
      } else {
        serial_iters <- ceiling(len / mc.cores)
        mc.cores2 <- which(len / (1:mc.cores) < serial_iters)[1]
        x <- vector("list", serial_iters)
        for(j in 1:serial_iters){
          itervar_tmp <- 1:mc.cores2 + (j - 1) * mc.cores2
          itervar_tmp <- itervar_tmp[itervar_tmp <= max(itervar)]
          x_tmp <- parallel::mclapply(
            itervar_tmp, extract_alf, type = type, loop_by = loop_by, main_dir = main_dir,
            reps = reps, years = years, cells = cells, veg_labels = veg_labels, mc.cores = mc.cores)
          x[[j]] <- dplyr::bind_rows(x_tmp)
          rm(x_tmp)
          gc()
          print(paste("Replicate batch", j, "of", serial_iters, "complete."))
        }
        x <- dplyr::bind_rows(x)
      }
    }
    x <- .add_columns(x, mod.scen, scen.levels, cru, cru_id)
    .save_alf_rds(x, out_dir, project, "fsv", cru, cru_id, modname)
    return(invisible())
  } else {
    cat(paste("Compiling", type, "statistics...\n"))
    cells <- dplyr::select(cells, -.data[["Cell"]])
    if(rmpi){
      va <- Rmpi::mpi.remote.exec(
        extract_alf(i = itervar[rmpi_proc_id], type = type, loop_by = loop_by, main_dir = main_dir, reps = reps,
                    years = years, cells = cells, veg_labels = veg_labels) )
      d.area <- dplyr::bind_rows(purrr::map(va, ~.x$d.area))
      d.age <- dplyr::bind_rows(purrr::map(va, ~.x$d.age))
    } else {
      len <- length(itervar)
      if(len <= mc.cores){
        va <- parallel::mclapply(
          itervar, extract_alf, type = type, loop_by = loop_by, main_dir = main_dir, reps = reps,
          years = years, cells = cells, veg_labels = veg_labels, mc.cores = mc.cores)
        d.area <- dplyr::bind_rows(purrr::map(va, ~.x$d.area))
        d.age <- dplyr::bind_rows(purrr::map(va, ~.x$d.age))
      } else {
        serial_iters <- ceiling(len / mc.cores)
        mc.cores2 <- which(len / (1:mc.cores) < serial_iters)[1]
        d.age <- d.area <- vector("list", serial_iters)
        for(j in 1:serial_iters){
          itervar_tmp <- 1:mc.cores2 + (j - 1) * mc.cores2
          itervar_tmp <- itervar_tmp[itervar_tmp <= max(itervar)]
          va <- parallel::mclapply(
            itervar_tmp, extract_alf, type = type, loop_by = loop_by, main_dir = main_dir, reps = reps,
            years = years, cells = cells, veg_labels = veg_labels, mc.cores = mc.cores)
          d.area[[j]] <- dplyr::bind_rows(lapply(va, function(x) x$d.area))
          d.age[[j]] <- dplyr::bind_rows(lapply(va, function(x) x$d.age))
          rm(va)
          gc()
          print(paste("Replicate batch", j, "of", serial_iters, "complete."))
        }
        d.area <- dplyr::bind_rows(d.area)
        d.age <- dplyr::bind_rows(d.age)
      }
    }
    d.area <- .add_columns(d.area, mod.scen, scen.levels, cru, cru_id)
    .save_alf_rds(d.area, out_dir, project, "veg", cru, cru_id, modname)
    d.age <- .add_columns(d.age, mod.scen, scen.levels, cru, cru_id)
    .save_alf_rds(d.age, out_dir, project, "age", cru, cru_id, modname)
    invisible()
  }
}

.save_alf_rds <- function(d, out_dir, project, var_id, cru, cru_id, modname){
  txt <- switch(
    var_id,
    "fsv" = paste0(
      "Fire size by vegetation class completed.\n",
      "Saving fire size by vegetation class data frames by location to .RData file.\n"),
    "veg" = paste0(
      "Vegetation area completed.\n",
      "Saving vegetation area data tables by location to .RData files.\n"),
    "age" = paste0(
      "Vegetation area by age completed.\n",
      "Saving vegetation area by age data tables by location to .RData files.\n"))
  cat(txt)
  locs <- unique(d$Location)
  dir.create(var_dir <- file.path(out_dir, project, "extractions", var_id),
             recursive = TRUE, showWarnings = FALSE)
  for(j in seq_along(locs)){
    if(cru){
      filename_tmp <- paste0(var_id, "__", locs[j], "__", gsub("\\.| ", "", cru_id))
    } else {
      filename_tmp <- paste0(var_id, "__", locs[j], "__", modname)
    }
    d2 <- d[locs[j]]
    saveRDS(d2, file = paste0(var_dir, "/", filename_tmp, ".rds"))
    print(paste(filename_tmp, "object", j, "of", length(locs), "saved."))
  }
  invisible()
}

.add_columns <- function(d, mod.scen, scen.levels, cru, cru_id){
  names_ini <- names(d)
  if(cru){
    d <- dplyr::mutate(d, Phase = "Observed", Scenario = "Historical", Model = cru_id)
  } else {
    d <- dplyr::mutate(
      d,
      Phase = getPhase(mod.scen[2]),
      Scenario = factor(swapScenarioName(mod.scen[2]), levels=scen.levels),
      Model = swapModelName(mod.scen[1]))
  }
  d_names <- c("Phase", "Scenario", "Model", names_ini)
  dplyr::select_(d, .dots = d_names)
}

#' Extract ALFRESCO data
#'
#' Extract ALFRESCO data from simulation model run outputs.
#'
#' The ALFRESCO wildfire model outputs collections of geotiff files for each year and simulation replicate of a model run.
#' These functions assist with extracting data from these map layers for use in statistical analysis and other applications.
#'
#' \code{prep_alf_files} prepares ALFRESCO output map file lists for ingestion by the data extraction functions.
#'
#' @param i numeric, iterator.
#' @param type \code{"av"} or \code{"fsv"} for vegetation age and cover area data extraction or fire sizes by vegetation cover type data extraction, respectively.
#' @param loop_by character, loop over files by \code{"rep"} or by \code{"year"}.
#' @param main_dir main input directory.
#' @param age_dir age inputs directory.
#' @param reps integer vector, ALFRESCO simulation replicate indices for data extraction.
#' @param years numeric vector, years for data extraction. Defaults to \code{2008:2100}.
#' @param cells data frame containing cell indices for the appropriate ALFRESCO spatial domain.
#' @param ... additional arguments.
#'
#' @return a data frame is returned for further processing as well as there being a side effect of additional files being written directly to disk.
#' @export
#'
#' @examples
#' # not run
extract_alf <- function(i, type, loop_by, main_dir, age_dir = NULL, reps = NULL, years = NULL,
                         cells, ...){
  stopifnot(length(type) > 0 && type %in% c("av", "fsv"))
  if(type == "av")
    return(extract_av(i, loop_by, main_dir, age_dir, reps, years, cells, ...))
  if(type == "fsv")
    return(extract_fsv(i, loop_by, main_dir, reps, years, cells, ...))
}

#' @export
#' @rdname extract_alf
prep_alf_files <- function(i, loop_by, main_dir, reps, years){
  if(is.null(years)) years <- 2008:2100
  if(loop_by == "rep"){
    iter <- reps
    keep <- reps - 1
    id <- paste0("_", years[i], ".tif$")
    main_dir <- file.path(main_dir, years[i])
  } else if(loop_by == "year") {
    keep <- iter <- years
    id <- paste0("_", c(0:199)[i], "_.*.tif$")
    main_dir <- file.path(main_dir, reps[i])
  }
  p <- lapply(c("A", "V", "FireS"), function(p, id){
    gsub("expression", "", paste(bquote(expression("^", .(p), ".*.", .(id))), collapse = ""))
  } , id = id) # nolint
  files <- lapply(1:length(p), function(i, dir, p){
    list.files(dir, pattern = p[[i]], full.names = TRUE, recursive = TRUE)
  }, dir = main_dir, p = p) # nolint
  names(files) <- c("Age", "Veg", "FID")
  if(loop_by == "rep")
    files.idx <- as.numeric(gsub("FireScar_", "", gsub("_\\d+\\.tif", "", basename(files$FID)))) + 1
  if(loop_by == "year")
    files.idx <- as.numeric(gsub("FireScar_\\d+_", "", gsub(".tif", "", basename(files$FID))))
  ord <- order(files.idx)
  files <- lapply(files, function(x, ord) x[ord], ord = ord)
  if(loop_by == "rep") files <- lapply(files, function(x, idx) x[idx], idx=reps)
  if(loop_by == "year") files <- lapply(files, function(x, file.idx, keep) {
    k <- file.idx %in% keep
    if(any(k)) x[which(k)] else x
  }, file.idx = files.idx, keep = keep) # nolint
  files$iter <- if(is.null(iter)) files.idx[ord] else iter
  stopifnot(!any(unlist(sapply(files, is.na))))
  stopifnot(all(diff(unlist(sapply(files, length))) == 0))
  files
}

#' @export
#' @rdname extract_alf
extract_fsv <- function(i, loop_by, main_dir, reps=NULL, years=NULL, cells, ...){
  if(is.null(list(...)$veg.labels)) {
    veg.labels <- c("Black Spruce", "White Spruce", "Deciduous", "Shrub Tundra",
                    "Graminoid Tundra", "Wetland Tundra", "Barren lichen-moss", "Temperate Rainforest")
  } else veg.labels <- list(...)$veg.labels
  g <- c("LocGroup", "Location", "Var", "Vegetation", "Year", "Val", "FID", "Replicate")
  x <- prep_alf_files(i = i, loop_by = loop_by, main_dir = main_dir, reps = reps, years = years)
  cells <- dplyr::ungroup(cells) %>% dplyr::group_by(.data[[g[1]]], .data[[g[2]]])
  d.fs <- vector("list", length(x$iter))
  for(j in 1:length(x$iter)){ # nolint fire size by vegetation class
    v <- list(
      FID = raster::getValues(raster::raster(x$FID[j], band = 2)),
      Veg = raster::getValues(raster::raster(x$Veg[j]))
    )
    d <- dplyr::filter(cells, .data[["Cell"]] %in% which(!is.na(v$FID))) %>%
      dplyr::mutate(
        Vegetation = factor(veg.labels[v$Veg[.data[["Cell"]]]], levels = veg.labels),
        FID = v$FID[.data[["Cell"]]]) %>%
      dplyr::group_by(.data[[g[1]]], .data[[g[2]]], .data[[g[4]]], .data[[g[7]]]) %>%
      dplyr::summarise(Val = length(.data[["Cell"]]), Var = "Fire Size")
    if(loop_by == "rep"){
      d.fs[[j]] <- dplyr::mutate(d, Replicate = x$iter[j])
    } else {
      d.fs[[j]] <-  dplyr::mutate(d, Year = x$iter[j])
    }
    print(switch(loop_by,
                 "rep" = paste0("Year ", years[i], ": Replicate ", x$iter[j]),
                 "year" = paste0("Replicate ", i, ": Year ", years[x$iter[j]])))
  }
  if(loop_by == "rep"){
    d.fs <- dplyr::bind_rows(d.fs) %>% dplyr::mutate(Year = as.integer(years[i]))
  } else {
    d.fs <- dplyr::bind_rows(d.fs) %>% dplyr::mutate(Replicate = as.integer(i))
  }
  d.fs <- dplyr::select(
    d.fs, .data[[g[1]]], .data[[g[2]]], .data[[g[3]]], .data[[g[4]]],
    .data[[g[5]]], .data[[g[6]]], .data[[g[7]]], .data[[g[8]]]) %>%
    dplyr::group_by(.data[[g[1]]], .data[[g[2]]], .data[[g[3]]], .data[[g[4]]], .data[[g[5]]]) %>%
    dplyr::arrange(.data[[g[8]]], .data[[g[1]]], .data[[g[2]]], .data[[g[3]]], .data[[g[4]]],
                   .data[[g[5]]], .data[[g[6]]])
  print(paste("Returning fire size by vegetation class data table."))
  d.fs
}

#' @export
#' @rdname extract_alf
extract_av <- function(i, loop_by, main_dir, age_dir=NULL, reps=NULL, years=NULL, cells, ...){
  if(is.null(list(...)$veg.labels)) {
    veg.labels <- c("Black Spruce", "White Spruce", "Deciduous", "Shrub Tundra",
                    "Graminoid Tundra", "Wetland Tundra", "Barren lichen-moss", "Temperate Rainforest")
  } else veg.labels <- list(...)$veg.labels
  g <- c("LocGroup", "Location", "Var", "Vegetation", "Year", "Val", "FID", "Replicate")
  x <- prep_alf_files(i = i, loop_by = loop_by, main_dir = main_dir, reps = reps, years = years)
  cells <- dplyr::ungroup(cells) %>% dplyr::group_by(.data[[g[1]]], .data[[g[2]]])
  r <- raster::getValues(raster::raster(x$Age[1])) # use as a template
  idx <- which(!is.na(r))
  idx.rmNA <- which(idx %in% 1:length(r))
  d.age <- vector("list", length(x$iter))
  for(j in 1:length(x$iter)){
    v <- list(
      Age = raster::getValues(raster::raster(x$Age[j]))[idx],
      Veg = raster::getValues(raster::raster(x$Veg[j]))[idx]
    )
    v$Age[v$Age < 0] <- v$Age[ v$Age < 0] + 2147483647 # temporary hack around longstanding ALFRESCO bug
    d <- dplyr::filter(cells, .data[["Cell_rmNA"]] %in% idx.rmNA) %>%
      dplyr::mutate(
        Vegetation = factor(veg.labels[v$Veg[.data[["Cell_rmNA"]]]], levels = veg.labels),
        Age = v$Age[.data[["Cell_rmNA"]]]) %>%
      dplyr::group_by(.data[[g[1]]], .data[[g[2]]], .data[[g[4]]], .data[["Age"]]) %>%
      dplyr::summarise(Freq = length(.data[["Cell_rmNA"]]))
    if(loop_by=="rep"){
      d.age[[j]] <- dplyr::mutate(d, Replicate = x$iter[j])
    } else {
      d.age[[j]] <- dplyr::mutate(d, Year = x$iter[j])
    }
    print(switch(loop_by,
                 "rep" = paste0("Year ", years[i], ": Replicate ", x$iter[j]),
                 "year" = paste0("Replicate ", i, ": Year ", years[x$iter[j]])))
  }
  if(loop_by=="rep"){
    d.age <- dplyr::bind_rows(d.age) %>% dplyr::mutate(Year = as.integer(years[i]))
  } else {
    d.age <- dplyr::bind_rows(d.age) %>% dplyr::mutate(Replicate = as.integer(i))
  }
  d.age <- dplyr::group_by(d.age, .data[[g[1]]], .data[[g[2]]], .data[[g[5]]], .data[[g[4]]]) %>%
    dplyr::arrange(.data[[g[1]]], .data[[g[2]]], .data[[g[3]]], .data[[g[5]]],
                   .data[[g[4]]], .data[["Age"]], .data[["Freq"]])
  d.area <- dplyr::group_by(d.age, .data[[g[8]]], add = TRUE) %>%
    dplyr::summarise(Val = sum(.data[["Freq"]]))
  d.area <- dplyr::mutate(d.area, Var = "Vegetated Area") %>%
    dplyr::select(.data[[g[1]]], .data[[g[2]]], .data[[g[3]]], .data[[g[4]]],
                  .data[[g[5]]], .data[[g[6]]], .data[[g[8]]])
  locs <- unique(d.age$Location)
  if(loop_by == "rep"){
    d.age <- dplyr::group_by(d.age, .data[["Age"]], add = TRUE) %>%
      dplyr::summarise(Freq = sum(.data[["Freq"]])) %>% dplyr::mutate(d.age, Var = "Vegetation Age") %>%
      dplyr::select(d.age, .data[[g[1]]], .data[[g[2]]], .data[[g[3]]], .data[[g[4]]],
                    .data[[g[5]]], .data[["Age"]], .data[["Freq"]])
    return(list(d.area = d.area, d.age = d.age))
  } else {
    for(j in 1:length(locs)){
      obj_name_tmp <- paste0("age__", locs[j], "__rep", i)
      assign(obj_name_tmp, d.age[locs[j]])
      save(list = c("locs", obj_name_tmp), file = paste0(age_dir, "/", obj_name_tmp, ".RData"))
      print(paste(obj_name_tmp, "object", j, "of", length(locs), "saved."))
      rm(list = obj_name_tmp)
      gc()
    }
    rm(d.age)
    gc()
    return(list(d.area = d.area))
  }
}
