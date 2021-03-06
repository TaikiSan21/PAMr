% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getBinaryData.R
\name{getBinaryData}
\alias{getBinaryData}
\title{Get Binary Data for Detections from an Acoustic Event}
\usage{
getBinaryData(x, UID, quiet = FALSE, ...)
}
\arguments{
\item{x}{a \linkS4class{AcousticStudy} object, a list of \linkS4class{AcousticEvent}
objects, or a single \linkS4class{AcousticEvent} object}

\item{UID}{the UID(s) of the individual detections to fetch the binary
data for}

\item{quiet}{logical flag to quiet some warnings, used internally}

\item{\dots}{additional arguments to pass to
\code{\link[PamBinaries]{loadPamguardBinaryFile}}}
}
\value{
a list of \code{PamBinary} objects for each \code{UID}
}
\description{
Fetches matching binary data from a single or multiple
  detections in an \linkS4class{AcousticEvent} object
}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
