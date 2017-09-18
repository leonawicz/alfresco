#' ALFRESCO post-processing of fire events
#'
#' These functions are used for post-processing of ALFRESCO fire events from map outputs.
#'
#' @param k integer, an iterator.
#' @param b a raster brick.
#' @param pts table of point locations.
#' @param locs location names
#' @param replicates character, simulation replicate labels.
#' @param source character, \code{"Observed"} or \code{"Modeled"}.
#' @param buffer_list list of point location buffers in meters.
#' @param buffer_labels labels for buffers in list.
#' @param veg vegetation raster layer.
#' @param yrs vector of years.
#' @param main_dir directory path.
#'
#' @return a list.
prep_fire_events <- function(k, pts, locs, replicates, source="Modeled", buffer_list = list(NULL),
                          buffer_labels = LETTERS[1:length(buffer_list)],
                          veg = NULL, main_dir){
  if(!is.null(veg)){
    veg <- raster::Which(veg > 0)
    burnable_cells <- raster::Which(veg == 1)
  } else {
    burnable_cells <- NULL
  }
  reps <- paste0("_", k - 1, "_") # nolint
  files <- list.files(main_dir, pattern =
                        gsub("expression", "",
                             paste(bquote(expression("^FireSc.*.", .(reps), ".*.tif$")), collapse="")),
                      full.names = TRUE)
  yrs <- as.numeric(gsub("FireScar_\\d+_", "", gsub(".tif", "", basename(files))))
  n <- length(yrs)
  ord <- order(yrs)
  files <- files[ord]
  yrs <- yrs[ord]
  d <- vector("list", n)
  for(i in 1:n){
    r <- raster::raster(files[i])
    r[is.na(r)] <- 0
    if(i == 1) r <- raster::mask(r, burnable_cells, maskvalue = 0)
    r[r > 0] <- 1
    if(i == 1){
      r.hold <- r
      cells <- vector("list", length(buffer_list))
      for(p in seq_along(buffer_list)){
        if(is.null(buffer_list[[p]])){
          tmp <- as.list(raster::extract(r, pts, cellnumbers = TRUE)[, 1])
        } else {
          tmp <- purrr::map(raster::extract(r, pts, buffer=buffer_list[[p]], cellnumbers = TRUE), ~.x[, 1])
        }
        tmp <- purrr::map(seq_along(tmp), ~tibble::data_frame(Location=locs[.x], Cell=tmp[[.x]])) %>%
          dplyr::bind_rows()
        cells[[p]] <- dplyr::mutate(tmp, Buffer_km = buffer_labels[p])
      }
      cells <- dplyr::bind_rows(cells)
    } else r.hold <- r.hold + r
    cells <- dplyr::mutate(cells, Value=r[.data[["Cell"]]])
    d[[i]] <- dplyr::group_by(cells, .data[["Buffer_km"]], .data[["Location"]]) %>%
      dplyr::summarise(Value = mean((.data[["Value"]]), na.rm=TRUE)) %>% dplyr::mutate(Year = yrs[i])
  }
  d <- dplyr::bind_rows(d) %>% dplyr::mutate(
    Source = factor(source, levels=c("Observed", "Modeled")),
    Replicate = factor(replicates[k], levels = unique(c("Observed", replicates)))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data[["Source"]], .data[["Replicate"]], .data[["Buffer_km"]],
                   .data[["Location"]], .data[["Year"]]) %>%
    dplyr::select(.data[["Source"]], .data[["Replicate"]], .data[["Buffer_km"]],
                  .data[["Location"]], .data[["Year"]], .data[["Value"]])
  list(rasters = r.hold, points = d, years = n, years.vec = yrs)
}

#' @export
#' @rdname prep_fire_events
prep_fire_events_emp <- function(b, pts, locs, replicates = "Observed", source = "Observed",
                                   buffer_list = list(NULL), buffer_labels = LETTERS[1:length(buffer_list)],
                                   veg = NULL, yrs){
  if(!is.null(veg)){
    veg <- raster::Which(veg > 0)
    burnable_cells <- raster::Which(veg == 1)
  } else {
    burnable_cells <- NULL
  }
  n <- raster::nlayers(b)
  d <- vector("list", n)
  for(i in 1:n){
    r <- subset(b, i)
    r[is.na(r)] <- 0
    if(i == 1) r <- raster::mask(r, burnable_cells, maskvalue = 0)
    r[r > 0] <- 1
    if(i == 1){
      r.hold <- r
      cells <- vector("list", length(buffer_list))
      for(p in seq_along(buffer_list)){
        if(is.null(buffer_list[[p]])){
          tmp <- as.list(raster::extract(r, pts, cellnumbers = TRUE)[, 1])
        } else {
          tmp <- purrr::map(raster::extract(
            r, pts, buffer = buffer_list[[p]], cellnumbers = TRUE), ~.x[, 1])
        }
        tmp <- purrr::map(
          seq_along(tmp), tibble::data_frame(Location = locs[.x], Cell = tmp[[.x]])) %>%
          dplyr::bind_rows()
        cells[[p]] <- dplyr::mutate(tmp, Buffer_km = buffer_labels[p])
      }
      cells <- dplyr::bind_rows(cells)
    } else r.hold <- r.hold + r
    cells <- dplyr::mutate(cells, Value = r[.data[["Cell"]]])
    d[[i]] <- dplyr::group_by(cells, .data[["Buffer_km"]], .data[["Location"]]) %>%
      dplyr::summarise(Value = mean((.data[["Value"]]), na.rm=TRUE)) %>% dplyr::mutate(Year = yrs[i])
  }
  d <- dplyr::bind_rows(d) %>% dplyr::mutate(
    Source = factor(source, levels=c("Observed", "Modeled")),
    Replicate = factor(replicates[1], levels=unique(c("Observed", replicates)))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data[["Source"]], .data[["Replicate"]], .data[["Buffer_km"]],
                   .data[["Location"]], .data[["Year"]]) %>%
    dplyr::select(.data[["Source"]], .data[["Replicate"]], .data[["Buffer_km"]],
                  .data[["Location"]], .data[["Year"]], .data[["Value"]])
  list(rasters = r.hold, points = d, years = n)
}

#' Make stock FRP maps
#'
#' Make stock unbuffered FRP maps for each Alfresco replicate.
#'
#' @param i integer, replicate iterator.
#' @param alf_data list of outputs from \code{fire_events}.
#' @param emp_data output from \code{fire_events_emp}.
#' @param shp masking shapefile.
#' @param fire_area_history shapefile of fire area history.
#' @param out output directory.
#' @param domain character, ALFRESCO domain, \code{Statewide} or \code{Noatak}.
#' @param emp_yrs empirical data years.
#' @param alf_yrs ALFRESCO run years.
#'
#' @return side effect of saving plots to disk.
#' @export
#'
#' @examples
#' # not run
frp_maps_no_buffer <- function(i, alf_data, emp_data, shp, fire_area_history, out, domain, alf_yrs, emp_yrs){
  alf_yrs <- alf_data[[1]][[4]]
  zlm <- c(0, alf_data[[1]][[3]])
  dir.create(out_dir <- file.path(out, "FRP/Maps_noBuffer/PNGs"),
             recursive = TRUE, showWarnings = FALSE)
  file <- paste0(out_dir, "/Maps_noBuffer/FRP_", domain, "_Rep", i - 1, ".tif") # nolint
  raster::writeRaster(alf_data[[i]][[1]], file, datatype="FLT4S", overwrite = TRUE)
  pngname <- paste0(out_dir, "/Maps_noBuffer/PNGs/FRP_", domain, "_Rep", i - 1, ".png") # nolint
  if(domain == "Noatak"){
    grDevices::png(pngname, width=1200, height=1370)
    graphics::layout(matrix(1:2, 2, 1))
  } else if(domain == "Statewide") {
    grDevices::png(pngname, width=2400, height=1370)
    graphics::layout(matrix(1:2, 1, 2))
  }
  graphics::plot(round(alf_data[[i]][[3]] / alf_data[[i]][[1]]), col = grDevices::heat.colors(20), zlim = zlm,
       main = paste0(domain, " ", alf_yrs[1], "-", utils::tail(alf_yrs, 1), " replicate ", i - 1, " FRP"))
  graphics::plot(shp, bg = "transparent", add = TRUE)
  graphics::plot(fire_area_history, bg = "transparent", add = TRUE)
  graphics::plot(round(length(emp_yrs) / emp_data), col = grDevices::heat.colors(20), zlim = zlm,
       main = paste0(domain, " ", emp_yrs[1], "-", utils::tail(emp_yrs, 1), " observed FRP"))
  graphics::plot(shp, bg = "transparent", add = TRUE)
  graphics::plot(fire_area_history, bg = "transparent", add = TRUE)
  grDevices::dev.off()
  if(i == 1) file.copy(pngname, file.path(dirname(out_dir), basename(pngname)))
}

#' Prepare data frame during ALFRESCO post-processing
#'
#' Prepare data frame during post-processing of ALFRESCO fire events from map outputs.
#'
#' @param x output from \code{fire_events} or \code{fire_events_emp}.
#' @param multiple_reps logical, typically \code{TRUE} for ALFRESCO model output and \code{FALSE} for empirical data.
#'
#' @return a tibble.
#' @export
#'
#' @examples
#' # not run
pp_df_prep <- function(x, multiple_reps = TRUE){
  if(!multiple_reps) x <- list(x)
  x <- purrr::map(x, ~.x[[2]])
  dplyr::bind_rows(x) %>% tibble::as_data_frame() %>%
    dplyr::group_by(.data[["Source"]], .data[["Replicate"]], .data[["Buffer_km"]],
                    .data[["Location"]], .data[["Year"]])
}

#' Prepare list of data frames during ALFRESCO post-processing
#'
#' Prepare list of data frames during post-processing of ALFRESCO fire events from map outputs.
#'
#' @param alf_data output from \code{fire_events}.
#' @param emp_data output from \code{fire_events_emp}.
#' @param domain character, ALFRESCO domain.
#' @param group_name character, ALFRESCO run group name.
#' @param run_name character, unique ALFRESCO run name.
#' @param emp_yrs empirical years.
#' @param out character, output directory.
#' @param send_to_app logical, send processed fire event outputs to Shiny app on Eris server. Defaults to \code{TRUE}.
#'
#' @return side effect of reading and writing files.
#' @export
#'
#' @examples
#' # not run
pp_fire_events <- function(alf_data, emp_data, domain, group_name, run_name, emp_yrs, out,
                           send_to_app = TRUE){
  alf_yrs <- alf_data[[1]][[4]]
  # Organize empirical and modeled data
  d_emp <- pp_df_prep(emp_data, FALSE)
  d <- pp_df_prep(alf_data)
  d2_emp <- dplyr::group_by(d_emp, .data[["Source"]], .data[["Replicate"]],
                            .data[["Buffer_km"]], .data[["Location"]]) %>%
    dplyr::summarise(FRP = length(.data[["Year"]]) / sum(.data[["Value"]]))

  d2 <- dplyr::group_by(d, .data[["Source"]], .data[["Replicate"]], .data[["Buffer_km"]],
                        .data[["Location"]]) %>%
    dplyr::summarise(FRP = length(.data[["Year"]]) / sum(.data[["Value"]]))
  # Additional objects to transport to app
  buffersize <- unique(d_emp$Buffer_km)
  obs.years.range <- range(d_emp$Year) # nolint
  mod.years.range <- range(alf_yrs) # nolint
  # Assemble final data frames
  rab.dat <- dplyr::bind_rows(d, d_emp) %>% tibble::as_data_frame() %>% dplyr::ungroup() %>%
    dplyr::arrange(.data[["Replicate"]], .data[["Buffer_km"]], .data[["Location"]])
  frp.dat <- dplyr::bind_rows(d2, d2_emp) %>% tibble::as_data_frame() %>% dplyr::ungroup() %>%
    dplyr::arrange(.data[["Replicate"]], .data[["Buffer_km"]], .data[["Location"]])
  rab.dat <- dplyr::mutate(Source = ifelse(Replicate == "Observed", "Observed", "Modeled"))
  frp.dat <- dplyr::mutate(Source = ifelse(Replicate == "Observed", "Observed", "Modeled"))
  # Make Fire Return Interval data frame
  # no fires = one FRI of period length; one fire = one FRI of time from fire to period end
  fri.dat <- dplyr::filter(rab.dat, .data[["Value"]] != 0) %>%
    dplyr::group_by(.data[["Source"]], .data[["Replicate"]], .data[["Buffer_km"]], .data[["Location"]]) %>%
    dplyr::mutate(FRI = c(NA, diff(.data[["Year"]])))
  fri.dat <- dplyr::left_join(rab.dat, fri.dat) %>%
    dplyr::group_by(.data[["Source"]], .data[["Replicate"]], .data[["Buffer_km"]], .data[["Location"]]) %>%
    dplyr::do(tibble::data_frame(FRI = censor(.[["FRI"]], .[["Value"]]))) %>% tibble::as_data_frame

  if(domain == "Noatak"){
    rab.dat <- .noatak_df(rab.dat)
    frp.dat <- .noatak_df(frp.dat)
    fri.dat <- .noatak_df(fri.dat)
  }

  # Load/save objects in a workspace file to be transported to app
  infile <- paste0(out, "/fsByVeg_df_", domain, ".RData") # nolint
  d.fs <- NULL # dummy variable, will be loaded next
  load(infile, envir = environment())
  out_dir <- file.path(out, "FRP")
  prefix <- ifelse(group_name == "none", "RAB_FRP", run_name)
  ws <- ifelse(group_name == "none",
               paste0(out_dir, "/", prefix, "_Emp_", emp_yrs[1], "_",
                      utils::tail(emp_yrs, 1), "_Alf_", alf_yrs[1], "_", utils::tail(alf_yrs, 1), ".RData"),
               paste0(out_dir, "/", prefix, ".RData"))
  save(d.fs, buffersize, obs.years.range, mod.years.range, rab.dat, frp.dat, fri.dat, file=ws) # nolint
  if(send_to_app) send_app_to_eris(group_name, ws)
  invisible()
}

censor <- function(x, y){
  if(all(y==0)) {
    length(x)
  } else if(all(is.na(x))) {
    as.integer(length(y) - which(y != 0))
  } else {
    as.integer(x[!is.na(x)])
  }
}

.noatak_df <- function(x){
  lev <- c("RedLake", "Raven", "Uchugrak", "Poktovik", "LittleIsac", "FoxLake")
  lev2 <- c("Origin", "Shrub", "Graminoid", "Fire")
  x <- dplyr::ungroup(x) %>% dplyr::mutate(LocGroup = "Origin") %>%
    dplyr::mutate(
      LocGroup = ifelse(substr(.data[["Location"]], 1, 5) == "Gram_", "Graminoid",
                        ifelse(substr(.data[["Location"]], 1, 5) == "Shrub", "Shrub",
                               ifelse(substr(.data[["Location"]], 1, 5) == "Fire_",
                                      "Fire", "Origin")))) %>%
    dplyr::mutate(LocGroup = factor(.data[["LocGroup"]], levels=lev2),
           Location = factor(
             gsub("Fire_", "", gsub("Shrub_", "", gsub("Gram_", "", .data[["Location"]]))),
             levels = lev))
}

send_app_to_eris <- function(group_name, ws, out){
  alf_dir <- alfdef()$eris_alf_dir
  template_dir <- alfdef()$eris_template_app_dir
  app_name <- ifelse(group_name == "none", "alf_results", paste0(group_name, "_alf_results"))
  app_dir <- file.path(alf_dir, app_name)

  system(paste("ssh eris.snap.uaf.edu mkdir -p", paste0(app_dir, "/www"))) # nolint start
  system(paste("ssh eris.snap.uaf.edu chmod 2775", app_dir))
  system(paste0("ssh eris.snap.uaf.edu cp ", template_dir, "/*.R ", app_dir, "/"))
  system(paste0("ssh eris.snap.uaf.edu cp ", template_dir, "/www/* ", app_dir, "/www/"))
  system(paste0("scp ", ws, " eris.snap.uaf.edu:", file.path(app_dir, basename(ws))))

  sink(file = file.path(out, "message.txt"), append = TRUE)
  cat(paste0("Alfresco output results:\n\n", "http://eris.snap.uaf.edu/shiny-apps/",
             basename(alf_dir), "/", app_name)) # nolint end
  sink()
  invisible()
}

prep_fire_scars <- function(year, x, y, years_avail, field = 1){
  if(year %in% years_avail) {
    x <- x[x$FireYear==year, ]
    x <- raster::rasterize(x, y, field = field)
  } else {
    x <- x[x$FireYear == years_avail[1], ]
    x <- raster::rasterize(x, y, field = 1)
    x[!is.na(x)] <- NA
  }
  x
}

#' Prepare fire scar bricks
#'
#' Prepare raster bricks of empirical fire scar data.
#'
#' This function loads relevant fire scar raster brick objects into the global environment from disk if they exist.
#' Otherwise it generates them from scratch, saves to disk for quicker use next time the function is called during a
#' future ALFRESCO run, and similarly pushes the objects to the global environment.
#'
#' @param fire_area_history shapefile of fire area history.
#' @param fah_yrs fire area history years.
#' @param emp_yrs empirical data years.
#' @param emp_fire_cause character, cause of fire: \code{"lightning"} or \code{"all"}.
#' @param domain character, ALFRESCO domain.
#' @param shp masking shapefile.
#' @param template_raster raster layer.
#' @param mc.cores number of processors.
#'
#' @return side effect of reading and/or writing files and pushing objects to global environment.
#' @export
#'
#' @examples
#' # not run
fire_scar_brick <- function(fire_area_history, fah_yrs, emp_yrs, emp_fire_cause, domain, shp, # nolint start
                            template_raster, mc.cores = 32){
  suffix <- paste0("_observed_", gsub("_", "", domain), "_", tolower(emp_fire_cause), "_",
                   min(emp_yrs), "_", max(emp_yrs), ".tif")
  b.fid.name <- paste0(alfdef()$atlas_shiny_dir, "/fireIDbrick_annual", suffix)
  result.name <- paste0(alfdef()$atlas_shiny_dir, "/firescarbrick_annual", suffix)
  result2.name <- paste0(alfdef()$atlas_shiny_dir, "/firescarlayer_total", suffix)
  if(file.exists(b.fid.name))
    assign("b.fid", raster::brick(b.fid.name), envir = .GlobalEnv)
  if(file.exists(result.name))
    assign("result", raster::brick(result.name), envir = .GlobalEnv)
  if(file.exists(result2.name))
    assign("result2", raster::raster(result2.name), envir = .GlobalEnv)


  if(!exists("b.fid", envir = .GlobalEnv)){
    b.fid <- parallel::mclapply(emp_yrs, prep_fire_scars, x = fire_area_history,
                                y = template_raster, years_avail = fah_yrs, field = "FIREID",
                                mc.cores = mc.cores)
    b.fid <- raster::brick(b.fid)
    if(domain == "Noatak") b.fid <- raster::mask(b.fid, shp)
    if(!file.exists(b.fid.name)){
      b <- raster::brick(b.fid, values=FALSE)
      b <- raster::writeStart(b, filename = b.fid.name, format = "GTiff", datatype = "FLT4S",
                              overwrite = TRUE)
      tr <- raster::blockSize(b)
      for (i in 1:tr$n) {
        v <- raster::getValuesBlock(b.fid, row = tr$row[i], nrows = tr$nrows[i])
        b <- raster::writeValues(b, v, tr$row[i])
      }
      b <- raster::writeStop(b)
    }
    assign("b.fid", b.fid, envir = .GlobalEnv)
  }
  if(!exists("result", envir = .GlobalEnv)){
    result <- parallel::mclapply(emp_yrs, prep_fire_scars, x = fire_area_history,
                       y = template_raster, years_avail = fah_yrs,
                       mc.cores = mc.cores)
    result <- lapply(result, function(x, r) {
      x[is.na(x) & !is.na(r) & r > 0] <- 0
      x
    }, r = template_raster)
    if(!exists("result2", envir = .GlobalEnv))
      result2 <- do.call("sum", c(result, na.rm = TRUE))
    result <- raster::brick(result)
    if(domain == "Noatak"){
      b.fid <- raster::mask(b.fid, shp)
      result <- raster::mask(result, shp)
      result2 <- raster::mask(result2, shp)
    }
    names(result) <- names(b.fid) <- emp_yrs
    names(result2) <- paste(emp_yrs[1], utils::tail(emp_yrs, 1), sep="_")
    if(!file.exists(result.name)){
      b <- raster::brick(result, values = FALSE)
      b <- raster::writeStart(b, filename = result.name, format="GTiff", datatype = "FLT4S",
                              overwrite = TRUE)
      tr <- raster::blockSize(b)
      for (i in 1:tr$n) {
        v <- raster::getValuesBlock(result, row = tr$row[i], nrows = tr$nrows[i])
        b <- raster::writeValues(b, v, tr$row[i])
      }
      b <- raster::writeStop(b)
    }
    if(!file.exists(result2.name)) {
      raster::writeRaster(result2, result2.name, datatype = "FLT4S", overwrite = TRUE)
    }
    assign("result", result, envir = .GlobalEnv)
    assign("result2", result2, envir = .GlobalEnv)
  }
  invisible()
} # nolint end

adjust_buffers <- function(buffers){
  buffers <- as.list(sort(unique(buffers)))
  null.ind <- which(buffers == 0)
  if(length(null.ind)) buffers[null.ind] <- list(NULL)
  buffers
}

pp_frp_stops <- function(){
  if(!exists("pts", envir = .GlobalEnv))
    stop("No coordinates file provided for relative area burned time series extraction.")
  if(!exists("buffers", envir = .GlobalEnv))
    stop("No buffer(s) provided for relative area burned time series extraction.")
  if(!exists("baseline.year", envir = .GlobalEnv))
    stop("baseline.year not found")

}

#' Prepare points matrix and locations vector
#'
#' Prepare points matrix and locations vector for buffered FRP extractions
#'
#' This function also makes adjustments for Noatak domain if applicable.
#'
#' @param pts a lon/lat matrix.
#' @param path file path.
#' @param domain ALFRESCO domain.
#'
#' @return a list.
#' @export
#'
#' @examples
#' # not run
prep_points <- function(pts, path, domain){
  pts <- utils::read.csv(file.path(path, pts))
  if(domain == "Noatak"){
    pts$ID <- factor(
      pts$ID, levels=c(paste0(rep(c("", "Shrub_", "Gram_"), each=4),
                              c("Raven", "Uchugrak", "Poktovik", "LittleIsac")),
                       "Fire_RedLake", "Fire_FoxLake"))
  }
  locs <- as.character(pts$ID)
  pts <- cbind(pts$Lon, pts$Lat)
  if(!is.matrix(pts)) stop("No coordinates matrix provided for relative area burned time series extraction")
  pts <- wgs2ak(pts)
  list(pts=pts, locs=locs)
}
