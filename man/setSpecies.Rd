% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setSpecies.R
\name{setSpecies}
\alias{setSpecies}
\title{Set the Species Classification of an Acoustic Event}
\usage{
setSpecies(x, type = "id", method = c("pamguard", "manual", "reassign"), value)
}
\arguments{
\item{x}{a \linkS4class{AcousticStudy} object, a list of \linkS4class{AcousticEvent}
objects, or a single \linkS4class{AcousticEvent} object}

\item{type}{the type of classification to set, this is just a label within
the \code{species} slot}

\item{method}{the method for assigning species to an event. Currently supports
\code{pamguard}, which will use the 'eventType' or 'Text_Annotation' column
to assign species, \code{manual} which will use \code{value} to assign
species manually, or \code{reassign} which will use \code{value} to
reassign an old species label to a new one}

\item{value}{optional argument required if \code{method} is set to manual or reassign.
For \code{'manual'}, can either be a single value to assign to all events, or if assigning to
a list a vector with length equal to the list. Can also be a dataframe
with columns \code{event} and \code{species}, in which case species will
be matched to corresponding event names instead of just relying on the
order. If using this, please note the prefix OE or DGL present on most
event numbers (see the \code{id} slot of your events).
For \code{'reassign'}, \code{value} must be a data frame with columns
\code{old} and \code{new}. Any events with species id in the \code{old} column
of the dataframe will get reassigned to the corresponding id in the
\code{new} column.}
}
\value{
the same object as \code{x}, with species identifications assigned
  as an item named \code{type} in the \code{species} slot
}
\description{
Sets the \code{species} slot of an \linkS4class{AcousticEvent}
  object or list of objects
}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
