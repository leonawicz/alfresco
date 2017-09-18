library(rgdal)
library(raster)
library(alfresco)
cargs <- (commandArgs(TRUE))
if(!length(cargs)) q("no") else for(i in 1:length(cargs)) eval(parse(text = cargs[[i]]))
extract_alf_stops()

if(!exists("rmpi")) rmpi <- TRUE
if(exists("repSample") && is.numeric(repSample)){
  set.seed(47)
  reps <- sort(sample(reps, min(repSample, length(reps))))
  cat("Sampled replicates:\n", reps, "\n")
}
if(!exists("cru")) cru <- FALSE

if(rmpi){
  library(Rmpi)
  mpi.spawn.Rslaves(needlog = TRUE) # remainder of setup performed by run_alf_extraction
  mpi.bcast.cmd(library(rgdal))
  mpi.bcast.cmd(library(raster))
  mpi.bcast.cmd(library(alfresco))
} else {
  library(parallel)
  n.cores <- 32
  tmp_dir <- paste0(alfdef()$raster_tmp_dir, "procX")
  rasterOptions(chunksize = 10e10, maxmemory = 10e11, tmpdir = tmp_dir)
}
cells <- get_domain_cells(domain)
veg_labels <- get_veg_labels(domain)
dirs <- get_out_dirs(domain, project, cru)
main_dirs <- rep(paste0(dirs, "/Maps")[modelIndex], each = length(years))
main_dir <- main_dirs[1] # all equal when single modelIndex

run_alf_extraction(domain, type = "fsv", main_dir = main_dir, project = project, reps = reps,
                   years = years, cells = cells, veg_labels = veg_labels, cru = cru, rmpi = rmpi)
run_alf_extraction(domain, type = "av", main_dir = main_dir, project = project, reps = reps,
                   years = years, cells = cells, veg_labels = veg_labels, cru = cru, rmpi = rmpi)

if(rmpi){
  mpi.close.Rslaves(dellog = TRUE)
  mpi.exit()
}
