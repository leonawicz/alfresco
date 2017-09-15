#' Generate bash script for copying ALFRESCO maps
#'
#' Generate bash script for copying ALFRESCO map outputs from one file system location to another.
#'
#' This function is used for generating a bash script that is used for making a copy of a subset of ALFRESCO geotiff map outputs for subsequent analyses.
#' The historical and projected periods (years subsets) are inferred from the choice of ALFRESCO spatial domain.
#' The file copy process adjusts the new file path structure, creates more consistency, and limits the number of geotiffs
#' to only the required ones. This means not only limiting to standard year ranges but also retaining only
#' fire scar, vegetation type and vegetation age maps. The bash script itself is run on SNAP's Atlas cluster.
#'
#' Note that this function is intended to be run on the Atlas cluster. If you make a bash script like this on Windows,
#' you may have to run a command line utility like \code{dos2unix} on the file.
#'
#' @param project character, a (new) name for the project, not required to match anything in source data directories.
#' @param in_dir input directory, the parent directory above model (GCM/RCP) run subdirectories.
#' @param out_dir output directory.
#' @param file name of shell script.
#' @param domain character, the ALFRESCO run spatial domain, either \code{"akcan1km"} or \code{"ak1km"}.
#' @param cru_id character, label for CRU data. Defaults to \code{"CRU 3.2"}.
#'
#' @return invisible, writes a file to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' copy_alf_outputs(
#'   "JFSP",
#'   "/big_scratch/shiny/Runs_Statewide/JFSP",
#'   domain = "ak1km")
#' }
copy_alf_outputs <- function(project, in_dir, out_dir = alfdef()$alf_extract_dir,
                             file = "copy_alf_outputs.sh", domain = "akcan1km", cru_id = "CRU32"){
  if(!domain %in% c("akcan1km", "ak1km"))
    stop("`domain` must be 'akcan1km' or 'ak1km'.")
  ak <- domain == "ak1km"
  if(ak){
    hist_year_limits <- c(1950, 2013)
    proj_year_limits <- c(2014, 2099)
  } else {
    hist_year_limits <- c(1900, 2007)
    proj_year_limits <- c(2008, 2099)
  }
  x <- "#! /bin/bash\n\n" # nolint start
  if(ak) x <- paste0(x, "rcp=$1 #e.g. rcp60\n")
  x <- paste0(x, "model=$2 # model directory\n",
    "period=$3 # historical or not (i.e., projected), if years below indicate CRU data\n")
  if(ak) x <- paste0(x, "fmo=$4 # e.g., fmo20s10i\n")
  x <- paste0(
    x, "\n",
    "if [ \"$period\" == \"historical\" ]\nthen\n",
    " modelOut=", cru_id, " # renaming destination model directory\n",
    " yr1=", hist_year_limits[1], "\n yr2=", hist_year_limits[2], "\nelse\n",
    " modelOut=$model # leaving destination directory same as source\n",
    " yr1=", proj_year_limits[1], "\n yr2=", proj_year_limits[2], "\nfi\n\n")

  if(ak){
    x <- paste0(
      x,
      "inDir=", in_dir, "/$fmo*$rcp*$model/Maps # source Maps directory\n",
      "outDir=", out_dir, "/", project, "/outputs/$fmo.$rcp.$modelOut/Maps # destination Maps directory\n")
  } else {
    x <- paste0(
      x,
      "inDir=", in_dir, "/$model/Maps # source Maps directory\n",
      "outDir=", out_dir, "/", project, "/outputs/runs/$modelOut/Maps # destination Maps directory\n")
  }
  x <- paste0(
    x, "mkdir -p $outDir\n\n",
    "for year in `seq $yr1 $yr2`;\ndo\n out=$outDir/$year\n")
  if(ak) x <- paste0(x, " in=$inDir\n") else x <- paste0(x, " in=$inDir/*$year\n")
  x <- paste0(
    x, " mkdir $out\n cp $in/Age*$year.tif $out/\n cp $in/FireScar*$year.tif $out/\n",
    " cp $in/Veg*$year.tif $out/\ndone\n") # nolint end
  sink(file = file)
  cat(x)
  sink()
  invisible()
}
