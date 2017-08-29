#' Prepare command line arguments
#'
#' Prepare command line argument inputs in a matrix.
#'
#' @param comArgs output from \code{commandArgs(TRUE)}.
#'
#' @return a matrix.
#' @export
#'
#' @examples
#' # not run
prep_comArgs <- function(comArgs){
  x <- do.call("rbind", strsplit(comArgs, "="))
  options(warn = -1)
  arg.char <- which(is.na(as.numeric(x[, 2])))
  options(warn=0)
  if(length(arg.char) > 0)
    x[arg.char, 2] <- paste0("'", x[arg.char, 2], "'")
  x
}

#' Helper function for FRP processing
#'
#' Helper function for Fire Return Period processing.
#'
#' @param comArgs output from \code{commandArgs(TRUE)}.
#' @param out character, output directory.
#'
#' @return side effects of creating output directory, \code{sink}-ing message.
#' @export
#'
#' @examples
#' # not run
prep_alf_frp <- function(comArgs, out){
  cat(comArgs)
  dir.create(file.path(out, "FRP"), showWarnings = FALSE)
  sink(file = file.path(out, "message.txt"), append = TRUE)
  cat("Below is a link to a preliminary R Shiny Alfresco FRP/FRI results app.\n")
  sink()
}
