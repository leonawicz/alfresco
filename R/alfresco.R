globalVariables(c("rmpi_proc_id", ".", ".x", ".data", "n", "Value", "Year",
                  "Domain", "Phase", "Scenario", "Model", "Location", "Var",
                  "Replicate", "Source", "BUffer_km", "Vegetation", "FMO", "LocGroup", "key"))

#' alfresco: R functions for ALFRESCO wildfire model.
#'
#' The alfresco package provides functions for working with and analyzing ALFRESCO wildfire model inputs and outputs.
#' It is a member package in the functional sector of the SNAPverse.
#'
#' @docType package
#' @name alfresco
NULL

#' @importFrom magrittr %>%
NULL

#' Common default values
#'
#' Common default values in the context of ALFRESCO modeling.
#'
#' This function returns a list of default values like standard file paths on the SNAP Atlas cluster
#' for running the model and processing outputs and paths to standard input maps.
#'
#' @return a named list.
#' @export
#'
#' @examples
#' alfdef()$atlas_shiny_dir
alfdef <- function(){
  bsalf <- "/big_scratch/mfleonawicz/Alf_Files_20121129" # nolint start
  alfex <- "/atlas_scratch/mfleonawicz/alfresco"
  proj <- "/atlas_scratch/mfleonawicz/projects"
  list(
    atlas_shiny_dir = "/big_scratch/shiny",
    atlas_run_owner_dir = "mfleonawicz@alaska.edu",
    age_spinups = file.path(bsalf, "Spinup300Year_32Reps/Age_0_1900.tif"),
    fire_cause_lightning = "/big_scratch/mfleonawicz/FAH/FireAreaHistory_11182013.shp",
    fire_cause_all = "/big_scratch/mfleonawicz/FAH/Lightning_Fires_11182013.shp",
    eris_apps_dir = "/var/www/shiny-server/shiny-apps",
    eris_template_app_dir = "/var/www/shiny-server/shiny-apps/alfoutdev",
    eris_alf_dir = "/var/www/shiny-server/shiny-apps/alfresco",
    noa_veg = file.path(bsalf, "alf2005.cavm.merged.030212_Noatak.tif"),
    sw_veg = file.path(bsalf, "alf2005.cavm.merged.030212.tif"),
    noa_shp = file.path(bsalf, "noa_basin2/Noa_basin2.shp"),
    sw_shp = file.path(bsalf, "statewide_shape/Alaska_Albers_ESRI.shp"),
    alf_extract_dir = alfex,
    alf_slurm_dir = file.path(alfex, "slurm_jobs"),
    raster_tmp_dir = "/atlas_scratch/mfleonawicz/tmp" # nolint end
  )
}
