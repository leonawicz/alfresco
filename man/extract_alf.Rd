% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{extract_alf}
\alias{extract_alf}
\alias{prep_alf_files}
\alias{extract_fsv}
\alias{extract_av}
\title{Extract ALFRESCO data}
\usage{
extract_alf(i, type, loop_by, main_dir, age_dir = NULL, reps, years,
  cells, ...)

prep_alf_files(i, loop_by, main_dir, reps, years)

extract_fsv(i, loop_by, main_dir, reps = NULL, years = NULL, cells,
  ...)

extract_av(i, loop_by, main_dir, age_dir = NULL, reps = NULL,
  years = NULL, cells, ...)
}
\arguments{
\item{i}{numeric, iterator.}

\item{type}{\code{"av"} or \code{"fsv"} for vegetation age and cover area data extraction or fire sizes by vegetation cover type data extraction, respectively.}

\item{loop_by}{character, loop over files by \code{"rep"} or by \code{"year"}.}

\item{main_dir}{main input directory.}

\item{age_dir}{age inputs directory.}

\item{reps}{integer vector, ALFRESCO simulation replicate indices for data extraction.}

\item{years}{numeric vector, years for data extraction.}

\item{cells}{data frame containing cell indices for the appropriate ALFRESCO spatial domain.}

\item{...}{additional arguments.}
}
\value{
a data frame is returned for further processing as well as there being a side effect of additional files being written directly to disk.
}
\description{
Extract ALFRESCO data from simulation model run outputs.
}
\details{
The ALFRESCO wildfire model outputs collections of geotiff files for each year and simulation replicate of a model run.
These functions assist with extracting data from these map layers for use in statistical analysis and other applications.

\code{prep_alf_files} prepares ALFRESCO output map file lists for ingestion by the data extraction functions.
}
\examples{
# not run
}
