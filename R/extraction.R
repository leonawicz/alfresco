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
