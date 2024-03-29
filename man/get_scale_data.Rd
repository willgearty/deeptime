% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_scale_data.R
\name{get_scale_data}
\alias{get_scale_data}
\title{Get geological timescale data}
\usage{
get_scale_data(name, true_colors = TRUE)
}
\arguments{
\item{name}{The name of the desired timescale.}

\item{true_colors}{Return original international time scale colors? (as
opposed to custom Macrostrat plotting colors)}
}
\value{
A \code{data.frame} with the following columns: \item{name}{the names of
the time intervals.}
\item{max_age}{the oldest boundaries of the time intervals, in millions of
years.}
\item{min_age}{the youngest boundaries of the time intervals, in millions
of years.}
\item{abbr}{either traditional abbreviations of the names of the time
intervals (if they exist) or custom abbreviations created with R.}
\item{color}{hex color codes associated with the time intervals (if
applicable).}
}
\description{
This function takes a name of a geological timescale and returns data for the
timescale. Valid names include those of built-in \code{data.frames} (\code{\link[=periods]{periods()}},
\code{\link[=epochs]{epochs()}}, \code{\link[=stages]{stages()}}, \code{\link[=eons]{eons()}}, or \code{\link[=eras]{eras()}}), partial matches of those
names (e.g., "per" or "stage"), and exact matches to those hosted by
Macrostrat (see full list here:
\url{https://macrostrat.org/api/defs/timescales?all}). Note that the colors in
the built-in \code{data.frames} are according to the Commission for the Geological
Map of the World. If you would like to obtain custom Macrostrat colors that
are better for mapping, you should specify the full name of a timescale
(e.g., "international periods") and set \code{true_colors} to \code{FALSE}. Note that
these colors only vary for the Precambrian.
}
