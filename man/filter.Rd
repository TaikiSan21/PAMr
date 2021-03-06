% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter.AcousticStudy}
\alias{filter.AcousticStudy}
\title{Filter an AcousticStudy or AcousticEvent Object}
\usage{
\method{filter}{AcousticStudy}(.data, ..., .preserve = FALSE)
}
\arguments{
\item{.data}{AcousticStudy or AcousticEvent to filter}

\item{\dots}{Logical expressions, syntax is identical to \link[dplyr]{filter}.
There is a special case if \code{.data} is an AcousticStudy object where a
logical expression using \code{species} or \code{Species} will filter by the
species present in the \code{$id} of the \code{species} slot within each
AcousticEvent}

\item{.preserve}{not used}
}
\value{
The original \code{.data} object, filtered by the given logical expressions
}
\description{
Apply dplyr-like filtering to the detecitons of an
  AcousticStudy or AcousticEvent object, with a special case for
  filtering by species for an AcousticStudy
}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
