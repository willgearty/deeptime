% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/points_range.R
\name{geom_points_range}
\alias{geom_points_range}
\alias{stat_points_range}
\title{Display points and their range}
\usage{
geom_points_range(
  mapping = NULL,
  data = NULL,
  stat = "points_range",
  position = "identity",
  ...,
  na.rm = FALSE,
  orientation = NA,
  background_line = NULL,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_points_range(
  mapping = NULL,
  data = NULL,
  geom = "points_range",
  position = "identity",
  ...,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this layer.
When using a \verb{geom_*()} function to construct a layer, the \code{stat}
argument can be used the override the default coupling between geoms and
stats. The \code{stat} argument accepts the following:
\itemize{
\item A \code{Stat} ggproto subclass, for example \code{StatCount}.
\item A string naming the stat. To give the stat as a string, strip the
function name of the \code{stat_} prefix. For example, to use \code{stat_count()},
give the stat as \code{"count"}.
}}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
}}

\item{...}{Arguments passed on to both \code{\link[ggplot2:geom_linerange]{ggplot2::geom_linerange()}} and
\code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{orientation}{The orientation of the layer. The default (\code{NA})
automatically determines the orientation from the aesthetic mapping. In the
rare event that this fails it can be given explicitly by setting \code{orientation}
to either \code{"x"} or \code{"y"}. See the \emph{Orientation} section for more detail.}

\item{background_line}{A named list of aesthetic values to use for plotted
line segments that span the entire \code{y} or \code{x} range for each \code{x} or \code{y}
category. The default aesthetics will be used for any aesthetics that are
not specified in the list. This can be useful if the plotted groups of
points don't overlap but you want a continuous line connecting all points
for a given \code{x} or \code{y} category. If NULL (the default), no line segments
will be plotted.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display. To include legend keys for all levels, even
when no data exists, use \code{TRUE}.  If \code{NA}, all levels are shown in legend,
but unobserved levels are omitted.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{geom}{The geometric object to use to display the data for this layer.
When using a \verb{stat_*()} function to construct a layer, the \code{geom} argument
can be used to override the default coupling between stats and geoms. The
\code{geom} argument accepts the following:
\itemize{
\item A \code{Geom} ggproto subclass, for example \code{GeomPoint}.
\item A string naming the geom. To give the geom as a string, strip the
function name of the \code{geom_} prefix. For example, to use \code{geom_point()},
give the geom as \code{"point"}.
}}
}
\description{
This geom is like \code{\link[ggplot2:geom_linerange]{ggplot2::geom_pointrange()}} in that it draws points and
lines. However, unlike \code{\link[ggplot2:geom_linerange]{ggplot2::geom_pointrange()}}, this geom takes in sets
of x-y points and calculates the ranges/intervals based on those. It then
plots both the original points and the ranges using
\code{\link[ggplot2:geom_linerange]{ggplot2::geom_linerange()}}. In cases where not all points are connected
(because of grouping due to aesthetics), the \code{background_line} argument can
be used to add lines that span the entire point range for each \code{x} or \code{y}
category.
}
\section{Aesthetics}{
 \code{geom_points_range()} understands the following
aesthetics (required aesthetics are in bold):
\itemize{
\item \strong{x}
\item \strong{y}
\item size
\item color/colour
\item fill
\item shape
\item alpha
\item group
\item linetype
\item linewidth
}
}

\section{Computed variables}{
 These are calculated by the 'stat' part of
layers and can be accessed with \link[ggplot2:aes_eval]{delayed evaluation}.
\code{stat_points_range()} provides the following variables, some of which
depend on the orientation:
\itemize{
\item \code{after_stat(ymin)} \emph{or} \code{after_stat(xmin)}\cr
the minimum extent of the point range
\item \code{after_stat(ymax)} \emph{or} \code{after_stat(xmax)}\cr
the maximum extent of the point range
}
}

\section{Orientation}{


This geom treats each axis differently and, thus, can thus have two orientations. Often the orientation is easy to deduce from a combination of the given mappings and the types of positional scales in use. Thus, ggplot2 will by default try to guess which orientation the layer should have. Under rare circumstances, the orientation is ambiguous and guessing may fail. In that case the orientation can be specified directly using the \code{orientation} parameter, which can be either \code{"x"} or \code{"y"}. The value gives the axis that the geom should run along, \code{"x"} being the default orientation you would expect for the geom.

}

\examples{
library(ggplot2)
\dontshow{if (require(palaeoverse)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(palaeoverse)
data(tetrapods)
tetrapod_names <- tetrapods$accepted_name[1:50]
beds_sampled <- sample.int(n = 10, size = 50, replace = TRUE)
occdf <- data.frame(taxon = tetrapod_names, bed = beds_sampled)
ggplot(occdf, aes(y = reorder(taxon, bed, min), x = bed)) +
  geom_points_range()
\dontshow{\}) # examplesIf}
}
