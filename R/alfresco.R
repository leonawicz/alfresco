globalVariables(c(".x", "BUffer_km"))

#' alfresco: R functions for ALFRESCO wildfire model.
#'
#' The alfresco package provides functions for working with and analyzing ALFRESCO wildfire model inputs and outputs.
#' It is a member package in the functional sector of the SNAPverse.
#'
#' @docType package
#' @name alfresco
NULL

#' @importFrom rlang !!
#' @importFrom magrittr %>%
NULL

# nolint start
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
  list(
    atlas_shiny_dir = "/big_scratch/shiny",
    atlas_run_owner_dir = "mfleonawicz@alaska.edu",
    age_spinups = "/big_scratch/mfleonawicz/Alf_Files_20121129/Spinup300Year_32Reps/Age_0_1900.tif",
    fire_cause_lightning="/big_scratch/mfleonawicz/FAH/FireAreaHistory_11182013.shp",
    fire_cause_all="/big_scratch/mfleonawicz/FAH/Lightning_Fires_11182013.shp"
  )
}
# nolint end
