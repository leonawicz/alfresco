#' Fire size by vegetation ALFRESCO post-processing
#'
#' Annual fire size by vegetation class ALFRESCO post-processing. \code{pp_fsv} works with model outputs.
#' The companion function, \code{pp_fsv_emp} handles empirical data.
#' \code{pp_fsv_save} is a wrapper function that calls both and also generates and saves plots and data to disk.
#'
#' @param d iterator, ALFRESCO replicates.
#' @param main_dir input directory.
#' @param veg vegetation raster layer.
#' @param shp masking shapefile.
#' @param years numeric, vector of years.
#' @param alf_yrs ALFRESCO run years.
#' @param emp_yrs empirical data years.
#' @param obs fire scar period totals raster layer.
#' @param i iterator, layer number of \code{b}.
#' @param b annual fire ID raster brick.
#' @param out character, output directory.
#' @param domain character, ALFRESCO domain, \code{Statewide} or \code{Noatak}.
#' @param mc.cores number of processors.
#'
#' @return a tibble.
#' @export
#'
#' @examples
#' # not run
pp_fsv <- function(d, main_dir, veg, shp, years, obs = NULL){
  if(!is.null(obs)) obs <- .fire_scars_totals_prep(obs)
  vid <- .pp_veg(veg, shp, type = "vid")
  veg <- .pp_veg(veg, shp)
  reps <- paste0("_", d - 1, "_")
  files <- list.files(main_dir, pattern=gsub(
    "expression", "", paste(bquote(expression("FireSc.*.", .(reps), ".*.tif$")), collapse="")),
    recursive = TRUE, full.names = TRUE)
  yrs <- as.numeric(gsub("FireScar_\\d+_", "", gsub(".tif", "", basename(files))))
  ord <- order(yrs)
  files <- files[ord]
  yrs <- yrs[ord]
  ind <- which(yrs %in% years)
  files <- files[ind]
  yrs <- yrs[ind]
  n <- length(yrs)
  dlist <- vector("list", n)
  for(k in 1:n){
    v.fid <- raster::getValues(raster::raster(files[k], band = 2))
    if(!all(is.na(v.fid))){
      dl <- purrr::map(vid, ~.pp_fsv(.x, veg, v.fid, obs))
      dlist[[k]] <- tibble::as_data_frame(dplyr::bind_rows(dl))
      dlist[[k]]$Year <- yrs[k]
    }
  }
  d <- dplyr::bind_rows(dlist)
  if(nrow(d) > 0){
    d <- dplyr::mutate(d, Source = "Modeled", Replicate = paste("Rep", gsub("_", "", reps)))
    s <- names(d)[c(3, 5, 6, 1, 4, 2)]
    d <- dplyr::select(d, .data[[s[1]]], .data[[s[2]]], .data[[s[3]]], # nolint
                       .data[[s[4]]], .data[[s[5]]], .data[[s[6]]]) # no lint
  } else d <- NULL
  d
}

#' @export
#' @rdname pp_fsv
pp_fsv_emp <- function(i, b, veg, shp, years, obs = NULL){
  if(!is.null(obs)) obs <- .fire_scars_totals_prep(obs)
  vid <- .pp_veg(veg, shp, type = "vid")
  veg <- .pp_veg(veg, shp)
  v.fid <- raster::getValues(subset(b, i))
  if(all(is.na(v.fid))) return()
  dl <- purrr::map(vid, ~.pp_fsv(.x, veg, v.fid, obs))
  d <- dplyr::bind_rows(dl) %>% dplyr::mutate(Year = years[i], Source = "Observed", Replicate = "Observed") %>%
    dplyr::ungroup()
  s <- names(d)[c(3, 5, 6, 1, 4, 2)]
  dplyr::select(d, .data[[s[1]]], .data[[s[2]]], .data[[s[3]]], .data[[s[4]]], .data[[s[5]]], .data[[s[6]]]) # nolint
}

.pp_fsv <- function(i, v, f, obs = NULL){
  v[v!=i] <- NA
  x <- f[!is.na(v) & !is.na(f)]
  d <- d2 <- NULL
  if(length(x)) d <- tibble::data_frame(
    Vegetation = i, FS = sort(as.numeric(tapply(x, x, length))),
    Domain = factor("Full", levels = c("Full", "Masked")))
  if(!is.null(obs)){
    x <- f[!is.na(v) & !is.na(f) & !is.na(obs)]
    if(length(x)) d2 <- tibble::data_frame(
      Vegetation = i, FS=sort(as.numeric(tapply(x, x, length))),
      Domain = factor("Masked", levels = c("Full", "Masked")))
    if(is.data.frame(d2)) d <- dplyr::bind_rows(list(d, d2))
  }
  d
}


#' @export
#' @rdname pp_fsv
pp_fsv_save <- function(d, main_dir, veg, shp, alf_yrs, emp_yrs, obs, i, b, out, domain, mc.cores = 32){
  totalba <- .pp_veg(veg, shp, type = "tba")
  empba <- raster::extract(obs, shp)[[1]]
  empba <- length(empba[!is.na(empba) & empba > 0])
  empba_totalba_ratio <- empba / totalba
  v.names <- c("Alpine", "Forest", "", "", "Shrub", "Graminoid", "Wetland")
  yr.start <- emp_yrs[1]
  yr.end <- utils::tail(emp_yrs, 1)

  fs.emp <- parallel::mclapply(i, pp_fsv_emp, b = b, veg = veg, shp = shp, years = emp_yrs, obs = obs,
                     mc.cores = mc.cores) %>% dplyr::bind_rows()
  fs.alf <- parallel::mclapply(d, pp_fsv, main_dir = main_dir, veg = veg, shp = shp, years = alf_yrs,
                     obs = obs, mc.cores = mc.cores) %>% dplyr::bind_rows()
  d.fs <- dplyr::bind_rows(fs.emp, fs.alf) %>% dplyr::mutate(Vegetation = v.names[.data[["Vegetation"]]]) # nolint
  d.fs <- tidyr::complete(d.fs, Domain,
                          tidyr::nesting(Source, Replicate),
                          Vegetation,
                          Year = tidyr::full_seq(.data[["Year"]], 1L), fill = list(FS = 0))

  save(d.fs, file = paste0(out, "/fsByVeg_df_", domain, ".RData")) # nolint

  cbpalette <- c("#000000", "gray", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  subjects <- sprintf("interaction(%s)", paste0(c("Replicate", "Vegetation"), collapse = ", "))
  file <- paste0(out, "/CABvsTimeByVeg_fullDomain_", yr.start, "_", yr.end, ".png") # nolint
  grDevices::png(file, width = 3200, height = 2400, res = 200)
  plot_rtab_time_veg(d.fs %>% dplyr::filter(.data[["Domain"]] == "Full"), c(yr.start, yr.end), cumulative = TRUE,
                     subject = subjects, grp="Vegetation", colpal = cbpalette, facet.by = "~ Vegetation",
                     facet.cols = 3, facet.scales = "free")
  grDevices::dev.off()
  file <- paste0(out, "/CABvsTimeByVeg_maskedToObsFire_", yr.start, "_", yr.end, ".png") # nolint
  grDevices::png(file, width = 3200, height = 2400, res = 200)
  plot_rtab_time_veg(d.fs %>% dplyr::filter(.data[["Domain"]] == "Masked"), c(yr.start, yr.end), cumulative = TRUE,
                     subject=subjects, grp="Vegetation", colpal = cbpalette, facet.by = "~ Vegetation",
                     facet.cols = 3, facet.scales = "free")
  grDevices::dev.off()

  veg.lev <- c("Combined Area", unique(d.fs$Vegetation))
  g <- c("Domain", "Source", "Replicate", "Vegetation", "Year")
  d.fs2 <- dplyr::filter(d.fs, .data[[Domain]] == "Full") %>%
    dplyr::mutate(FS = ifelse(.data[["Source"]] == "Modeled",
                              .data[["FS"]] * empba_totalba_ratio, .data[["FS"]])) %>%
    dplyr::group_by(.data[[g[1]]], .data[[g[2]]], .data[[g[3]]], .data[[g[5]]])
  d.fs2 <- d.fs2 %>% dplyr::bind_rows(
    d.fs2 %>% dplyr::summarise(FS = sum(.data[["FS"]]), Vegetation = veg.lev[1])) %>%
    dplyr::group_by(.data[[g[1]]], .data[[g[2]]], .data[[g[3]]], .data[[g[4]]]) %>%
    dplyr::arrange(.data[["FS"]]) %>% dplyr::mutate(CAB = cumsum(.data[["FS"]])) %>% dplyr::ungroup() %>%
    dplyr::mutate(Vegetation = factor(.data[["Vegetation"]], levels = veg.lev),
                  Source = factor(.data[["Source"]], levels = c("Observed", "Modeled")))

  file <- paste0(out, "/CABvsFSByVeg_ObsFire_to_AlfArea_Ratio_scaledDomain_", # nolint
                 yr.start, "_", yr.end, ".png")
  grDevices::png(file, width=3200, height=2400, res=200)
  ggplot2::ggplot(d.fs2, ggplot2::aes_string(x = "FS", y = "CAB", group = "Replicate", colour = "Source")) +
    ggplot2::geom_step(data = dplyr::filter(d.fs2, .data[["Source"]] == "Modeled"), colour = "gray") +
    ggplot2::geom_step(data = dplyr::filter(d.fs2, .data[["Source"]] == "Observed"),
                       ggplot2::aes_string(group = "Source", colour = "Source"), size = 1) +
    ggplot2::facet_wrap(stats::as.formula("~ Vegetation")) + ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = paste(yr.start, "-", yr.end,
                     "cumulative area burned vs. fire size by vegetation and combined area"),
         x = expression("Fire size "~(km^2)~""), y = expression("Cumulative burn area "~(km^2)~"")) +
    ggplot2::scale_colour_manual(values = cbpalette)
  grDevices::dev.off()

  file <- paste0(out, "/CABvsFSByVeg_ObsFire_to_AlfArea_Ratio_scaledDomain_", # nolint
                 yr.start, "_", yr.end, "_freeXY.png")
  grDevices::png(file, width=3200, height=2400, res=200)
  ggplot2::ggplot(d.fs2, ggplot2::aes_string(x = "FS", y = "CAB", group = "Replicate", colour = "Source")) +
    ggplot2::geom_step(data = dplyr::filter(d.fs2, .data[["Source"]] == "Modeled"), colour = "gray") +
    ggplot2::geom_step(data = dplyr::filter(d.fs2, .data[["Source"]] == "Observed"),
                       ggplot2::aes_string(group = "Source", colour = "Source"), size = 1) +
    ggplot2::facet_wrap(stats::as.formula("~ Vegetation"), scales = "free") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = paste(yr.start, "-", yr.end,
                                "cumulative area burned vs. fire size by vegetation and combined area"),
         x = expression("Fire size "~(km^2)~""), y = expression("Cumulative burn area "~(km^2)~"")) +
    ggplot2::scale_colour_manual(values = cbpalette)
  grDevices::dev.off()

  sink(file = file.path(out, "message.txt"), append = TRUE)
  cat("An R workspace containing fire sizes by vegetation class is attached.\n")
  sink()
  invisible()
}

plot_rtab_time_veg <- function(data, year.range, cumulative = F, subject, grp = "", colpal, fontsize = 16,
                               lgd.pos = "top", facet.by = NULL, facet.cols = 1, facet.scales = NULL, ...){
  a <- c("Domain", "Source", "Replicate", "Vegetation", "Year")
  d <- dplyr::filter(data, Year >= year.range[1] & Year <= year.range[2]) %>%
    dplyr::group_by(.data[[a[1]]], .data[[a[2]]], .data[[a[3]]], .data[[a[4]]], .data[[a[5]]]) # nolint
  given.veg <- "| Vegetation"
  xlb <- "Year"
  if(cumulative){
    d <- dplyr::summarise(d, Value = sum(.data[["FS"]])) %>%
      dplyr::mutate(Value = cumsum(.data[["Value"]]))
    maintitle <- paste(year.range[1], "-", year.range[2],
                       "Regional Cumulative Total Area Burned ~ Time", given.veg)
    ylb <- expression("CTAB ("~km^2~")")
  } else {
    d <- dplyr::summarise(d, Value = sum(.data[["FS"]])) # nolint
    maintitle <- paste(year.range[1], "-", year.range[2],
                       "Regional Total Area Burned ~ Time", given.veg)
    ylb <- expression("TAB ("~km^2~")")
  }
  g <- ggplot2::ggplot(d, ggplot2::aes_string(x = "Year", y = "Value", group = subject, colour="Source"))
  if(cumulative){
    if(grp != ""){
      g <- g + ggplot2::geom_step(data = dplyr::filter(
        d, .data[["Source"]] == "Modeled"), colour = "gray") # nolint
      g <- g + ggplot2::geom_step(ggplot2::aes_string(colour = grp),
                                  data = dplyr::filter(d, .data[["Source"]] == "Observed"), size = 1) +
        ggplot2::scale_color_manual(values = colpal[-c(1, 2)]) +
        ggplot2::scale_fill_manual(values = colpal[-c(1, 2)])
    } else g <- g + ggplot2::geom_step() +
        ggplot2::geom_step(data = dplyr::filter(d, .data[["Source"]] == "Observed"), size = 1) +
        ggplot2::scale_color_manual(values = colpal) + ggplot2::scale_fill_manual(values = colpal)
  } else {
    if(grp != ""){
      g <- g + ggplot2::geom_point(data = dplyr::filter(
        d, .data[["Source"]] == "Modeled"), colour = "gray") # nolint
      g <- g + ggplot2::geom_point(ggplot2::aes_string(colour = grp),
                                   data = dplyr::filter(
                                     d, .data[["Source"]] == "Observed"), size = 2.5) + # nolint
        ggplot2::scale_color_manual(values = colpal[-c(1, 2)]) +
        ggplot2::scale_fill_manual(values = colpal[-c(1, 2)])
    } else g <- g + ggplot2::geom_point() +
        ggplot2::geom_point(data = dplyr::filter(d, .data[["Source"]] == "Observed"), size = 2.5) +
        ggplot2::scale_color_manual(values = colpal) + ggplot2::scale_fill_manual(values = colpal)
  }
  ttl <- maintitle
  g <- g + ggplot2::theme_bw(base_size = fontsize) +
    ggplot2::theme(legend.position = tolower(lgd.pos)) +
    ggplot2::ggtitle(ttl) + ggplot2::xlab(xlb) + ggplot2::ylab(ylb)
  if(!is.null(facet.by)){
    string <- if(length(facet.by) == 1)
      paste("~", facet.by) else paste(facet.by[1], "~", facet.by[2])
    g <- g + ggplot2::facet_wrap(stats::as.formula(string), ncol = as.numeric(facet.cols), scales = facet.scales)
  }
  print(g)
}

.pp_veg <- function(veg, shp, type = "veg"){
  veg <- raster::getValues(raster::mask(veg, shp))
  if(type == "tba") return(length(veg[!is.na(veg) & veg > 0]))
  veg[veg == 3 | veg == 4] <- 2 # 3 and 4 combine to 2 (forest); 1, 5:7 unchanged
  if(type == "veg") return(veg)
  if(type == "vid") return(sort(unique(veg[!is.na(veg) & veg > 0])))
  stop("Invalid `type`.")
}

.fire_scars_totals_prep <- function(r){
  r[r == 0] <- NA
  r[r > 0] <- 1
  raster::getValues(r)
}
