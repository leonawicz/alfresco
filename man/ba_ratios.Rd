% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postproc_app.R
\name{ba_ratios}
\alias{ba_ratios}
\title{Obtain total cumulative burn area ratios}
\usage{
ba_ratios(files, years = 1950:2013, domain = "Full")
}
\arguments{
\item{files}{vector of workspace files from ALFRESCO outputs Shiny app; run directory basename format expected to begin with \code{fmo}, e.g., \code{fmo00s00i}.}

\item{years}{numeric, defaults to \code{1950:2013}.}

\item{domain}{character, \code{"Full"} or \code{"Masked"}, the full ALFRESCO run domain or the subregion masked to the extent of observed historical fire.}
}
\value{
a data frame.
}
\description{
Obtain ratios of modeled total CBA to that of observed data as well as to baseline model run output.
}
\details{
The table of ratios show minimum, mean and maximums across ALFRESCO replicates for CBA and
the corresponding percent change vs. both the empirically observed CBA and the baseline/status quo (no fire management options)
model run over the same historical period.
}
\examples{
\dontrun{
indir <- "C:/github/Flammability/data/fmo/app_workspaces"
files <- list.files(indir, full.names = TRUE)
x <- ba_ratios(files)

library(dplyr)
select(x, -PctIgSupp) \%>\% group_by(PctFsSupp) \%>\% summarise_all(function(x) round(mean(x), 3))
}
}
