library(alfresco)
library(parallel)
cargs <- (commandArgs(TRUE))
if(!length(cargs)) q("no") else for(z in 1:length(cargs)) eval(parse(text = cargs[[z]]))
prep_alf_stops()

if(dirname(project) != "."){
  in_project <- strsplit(project, "/")[[1]]
  in_project <- file.path(in_project[1], "extractions", in_project[2])
} else {
  in_project <- file.path(project, "extractions")
}
if(!exists("in_dir")) in_dir <- file.path(alfdef()$alf_extract_dir, in_project)
if(!exists("out_dir")) out_dir <- file.path(snapprep::snapdef()$dist_dir, "alfresco", project)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
inputs <- alf_dist_inputs(project)
n <- purrr::map_int(c("fsv", "veg", "age"), ~{
  x <- dplyr::filter(inputs, Var == .x)
  length(unique(paste(x$LocGroup, x$Location)))
  })
mc.cores <- 32

mclapply(1:n[1], alf_dist, in_dir = file.path(in_dir, "fsv"), out_dir = out_dir,
         period = period, reps = reps, project = project, mc.cores = mc.cores)
mclapply(1:n[2], alf_dist, in_dir = file.path(in_dir, "veg"), out_dir = out_dir,
         period = period, reps = reps, project = project, mc.cores = mc.cores)
mclapply(1:n[3], alf_dist, in_dir = file.path(in_dir, "age"), out_dir = out_dir,
         period = period, reps = reps, project = project, mc.cores = mc.cores)
