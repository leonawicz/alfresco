% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postproc_frp.R
\name{frp_maps_no_buffer}
\alias{frp_maps_no_buffer}
\title{Make stock FRP maps}
\usage{
frp_maps_no_buffer(i, alf_data, emp_data, shp, fire_area_history, out,
  domain, alf_yrs, emp_yrs)
}
\arguments{
\item{i}{integer, replicate iterator.}

\item{alf_data}{list of outputs from \code{fire_events}.}

\item{emp_data}{output from \code{fire_events_emp}.}

\item{shp}{masking shapefile.}

\item{fire_area_history}{shapefile of fire area history.}

\item{out}{output directory.}

\item{domain}{character, ALFRESCO domain, \code{Statewide} or \code{Noatak}.}

\item{alf_yrs}{ALFRESCO run years.}

\item{emp_yrs}{empirical data years.}
}
\value{
side effect of saving plots to disk.
}
\description{
Make stock unbuffered FRP maps for each Alfresco replicate.
}
\examples{
# not run
}
