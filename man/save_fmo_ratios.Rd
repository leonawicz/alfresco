% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fmo.R
\name{save_fmo_ratios}
\alias{save_fmo_ratios}
\title{Save FMO ratios maps}
\usage{
save_fmo_ratios(type = "both", out_dir = ".", unmanaged = 1,
  limited = 1, modified = 1, full = 1.25, critical = 2,
  other = 1, width = 1000, height = 1000)
}
\arguments{
\item{type}{character, \code{"sensitivity"}, \code{"ignition"} or \code{"both"}.}

\item{out_dir}{output directory.}

\item{unmanaged}{numeric, ratio value to set for unmanaged grid cells.}

\item{limited}{numeric, ratio value to set for limited fire management grid cells.}

\item{modified}{numeric, ratio value to set for modified fire management grid cells.}

\item{full}{numeric, ratio value to set for full fire management grid cells.}

\item{critical}{numeric, ratio value to set for critical fire management grid cells.}

\item{other}{numeric, ratio value to set for any other grid cells.}

\item{width}{numeric, png width.}

\item{height}{numeric, png height.}
}
\description{
Save fire management options ratios maps.
}
\details{
This functions save a geotiff of the FMO ratios raster layer and an accompanying png graphic.
If \code{type = "both"}, the same pair of files is saved for ALFRESCO ignition factor input geotiff suppression and
ALFRESCO fire sensitivity input geotiff suppression.
It uses \code{swfmo} from the \code{snapgrid} package. The \code{snapgrid} package also includes \code{swratios},
the same FMO ratios map that this function creates using default arguments.
}
\examples{
\dontrun{save_fmo_ratios()}
}
