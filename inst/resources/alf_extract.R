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
  mpi.spawn.Rslaves(needlog = TRUE)
  mpi.bcast.cmd( rmpi_proc_id <- mpi.comm.rank() )
  mpi.bcast.cmd( np <- mpi.comm.size() )
  mpi.bcast.cmd( host <- mpi.get.processor.name() )
} else {
  library(parallel)
  n.cores <- 32
}
cells <- get_domain_cells(domain)
veg_labels <- get_veg_labels(domain)
dirs <- get_out_dirs(domain, project, cru)
main_dirs <- rep(paste0(dirs, "/Maps")[modelIndex], each = length(years))
main_dir <- main_dirs[1] # all equal when single modelIndex

# Export objects to slaves
if(rmpi){
  mpi.bcast.Robj2slave(cells)
  mpi.bcast.Robj2slave(reps)
  mpi.bcast.Robj2slave(years)
  mpi.bcast.Robj2slave(main_dirs)
  mpi.bcast.Robj2slave(veg_labels)
  print("mpi.bcast.Robj2slave calls completed.")
}

# Issue commands to slaves
if(rmpi){
  mpi.bcast.cmd( library(rgdal) )
  mpi.bcast.cmd( library(raster) )
  mpi.bcast.cmd( library(alfresco) )
  mpi.bcast.cmd( dir.create(
    tmp_dir <- paste0(alfdef()$raster_tmp_dir, "/proc", rmpi_proc_id), showWarnings = FALSE) )
  mpi.bcast.cmd( rasterOptions(chunksize = 10e10, maxmemory = 10e11, tmpdir = tmp_dir) )
  print("mpi.bcast.cmd calls completed. Now running mpi.remote.exec...")
} else {
  tmp_dir <- paste0(alfdef()$raster_tmp_dir, "procX")
  rasterOptions(chunksize = 10e10, maxmemory = 10e11, tmpdir = tmp_dir)
}

run_alf_extraction(domain, type = "fsv", main_dir = main_dir, project = project, reps = reps,
                   years = years, cells = cells, veg_labels = veg_labels, cru = cru, rmpi = rmpi)
run_alf_extraction(domain, type = "av", main_dir = main_dir, project = project, reps = reps,
                   years = years, cells = cells, veg_labels = veg_labels, cru = cru, rmpi = rmpi)

if(rmpi){
  mpi.close.Rslaves(dellog = FALSE)
  mpi.exit()
}
