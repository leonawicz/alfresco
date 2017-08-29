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
#' @param buffer.list list of point location buffers in meters.
#' @param buffer.labels labels for buffers in list.
#' @param burnable_cells binary raster of possibly burnable cells.
#' @param yrs vector of years.
#' @param main_dir directory path.
#'
#' @return a list.
fireEventsFun <- function(k, pts, locs, replicates, source="Modeled", buffer.list = list(NULL),
                          buffer.labels = LETTERS[1:length(buffer.list)],
                          burnable_cells = NULL, main_dir){
  if(!is.null(burnable_cells))
    burnable.cells <- raster::Which(burnable_cells == 1)
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
    if(i == 1) r <- raster::mask(r, burnable.cells, maskvalue = 0)
    r[r > 0] <- 1
    if(i == 1){
      r.hold <- r
      cells <- vector("list", length(buffer.list))
      for(p in seq_along(buffer.list)){
        if(is.null(buffer.list[[p]])){
          tmp <- as.list(raster::extract(r, pts, cellnumbers = TRUE)[, 1])
        } else {
          tmp <- purrr::map(raster::extract(r, pts, buffer=buffer.list[[p]], cellnumbers = TRUE), ~.x[, 1])
        }
        tmp <- purrr::map(seq_along(tmp), ~tibble::data_frame(Location=locs[.x], Cell=tmp[[.x]])) %>%
          dplyr::bind_rows()
        cells[[p]] <- dplyr::mutate(tmp, Buffer_km = buffer.labels[p])
      }
      cells <- dplyr::bind_rows(cells)
    } else r.hold <- r.hold + r
    cells <- dplyr::mutate(cells, Value=r[(!!as.name("Cell"))])
    d[[i]] <- dplyr::group_by(cells, (!!as.name("Buffer_km")), (!!as.name("Location"))) %>%
      dplyr::summarise(Value = mean((!!as.name("Value")), na.rm=TRUE)) %>% dplyr::mutate(Year = yrs[i])
  }
  a <- c("Source", "Replicate", "Buffer_km", "Location", "Year", "Value")
  d <- dplyr::bind_rows(d) %>% dplyr::mutate(
    Source = factor(source, levels=c("Observed", "Modeled")),
    Replicate = factor(replicates[k], levels = unique(c("Observed", replicates)))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!as.name(a[1]), !!as.name(a[2]), !!as.name(a[3]),
                   !!as.name(a[4]), !!as.name(a[5])) %>%
    dplyr::select(!!as.name(a[1]), !!as.name(a[2]), !!as.name(a[3]),
                  !!as.name(a[4]), !!as.name(a[5]), !!as.name(a[6]))
  list(rasters = r.hold, points = d, years = n, years.vec = yrs)
}

#' @export
#' @rdname fireEventsFun
fireEventsFunEmpirical <- function(b, pts, locs, replicates = "Observed", source = "Observed",
                                   buffer.list = list(NULL), buffer.labels = LETTERS[1:length(buffer.list)],
                                   burnable_cells = NULL, yrs){
  if(!is.null(burnable_cells))
    burnable.cells <- raster::Which(burnable_cells == 1)
  n <- raster::nlayers(b)
  d <- vector("list", n)
  for(i in 1:n){
    r <- subset(b, i)
    r[is.na(r)] <- 0
    if(i == 1) r <- raster::mask(r, burnable.cells, maskvalue = 0)
    r[r > 0] <- 1
    if(i == 1){
      r.hold <- r
      cells <- vector("list", length(buffer.list))
      for(p in seq_along(buffer.list)){
        if(is.null(buffer.list[[p]])){
          tmp <- as.list(raster::extract(r, pts, cellnumbers = TRUE)[, 1])
        } else {
          tmp <- purrr::map(raster::extract(
            r, pts, buffer = buffer.list[[p]], cellnumbers = TRUE), ~.x[, 1])
        }
        tmp <- purrr::map(
          seq_along(tmp), tibble::data_frame(Location = locs[.x], Cell = tmp[[.x]])) %>%
          dplyr::bind_rows()
        cells[[p]] <- dplyr::mutate(tmp, Buffer_km = buffer.labels[p])
      }
      cells <- dplyr::bind_rows(cells)
    } else r.hold <- r.hold + r
    cells <- dplyr::mutate(cells, Value = r[(!!as.name("Cell"))])
    d[[i]] <- dplyr::group_by(cells, (!!as.name("Buffer_km")), (!!as.name("Location"))) %>%
      dplyr::summarise(Value = mean((!!as.name("Value")), na.rm=TRUE)) %>% dplyr::mutate(Year = yrs[i])
  }
  a <- c("Source", "Replicate", "Buffer_km", "Location", "Year", "Value")
  d <- dplyr::bind_rows(d) %>% dplyr::mutate(
    Source = factor(source, levels=c("Observed", "Modeled")),
    Replicate = factor(replicates[1], levels=unique(c("Observed", replicates)))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!as.name(a[1]), !!as.name(a[2]), !!as.name(a[3]),
                   !!as.name(a[4]), !!as.name(a[5])) %>%
    dplyr::select(!!as.name(a[1]), !!as.name(a[2]), !!as.name(a[3]),
                  !!as.name(a[4]), !!as.name(a[5]), !!as.name(a[6]))
  list(rasters = r.hold, points = d, years = n)
}
