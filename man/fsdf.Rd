% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsv.R
\name{fsdf}
\alias{fsdf}
\title{Fire size and cumulative burn data frame}
\usage{
fsdf(x, by_veg = FALSE)
}
\arguments{
\item{x}{character or data frame. The file name of the or the data frame object from that file. See details.}

\item{by_veg}{logical, if \code{TRUE}, do not aggregate vegetation classes for full regional totals.}
}
\value{
a data frame.
}
\description{
Generate a data frame of exact fire sizes and cumulative burn for each vegetation class in a region or their spatial union.
}
\details{
This function takes an exact-type fire size by vegetation class .rds file or the data frame it contains if already loaded into the R session.
The source file is one created by \link{fsv}.
Vegetation-specific fire sizes are aggregated by unique fire IDs by default(\code{by_veg = FALSE}) per simulation replicate, year and,
if present, fire management options treatment levels. In either case the function adds a column for cumulative burn area
alongside ascending fire sizes.
}
\examples{
\dontrun{fsdf("historical_fsv.rds")}
}
\seealso{
fsv cbdf
}
