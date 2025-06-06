% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facet_color.R
\docType{data}
\name{facet_nested_wrap_color}
\alias{facet_nested_wrap_color}
\alias{facet_nested_wrap_geo}
\alias{FacetNestedWrapColor}
\title{Ribbon of panels with nested colored strips}
\usage{
facet_nested_wrap_color(
  facets,
  colors,
  nrow = NULL,
  ncol = NULL,
  scales = "fixed",
  axes = "margins",
  remove_labels = "none",
  shrink = TRUE,
  labeller = "label_value",
  lab_colors = "auto",
  as.table = TRUE,
  drop = TRUE,
  dir = "h",
  strip.position = "top",
  nest_line = element_line(inherit.blank = TRUE),
  solo_line = FALSE,
  resect = unit(0, "mm"),
  trim_blank = TRUE,
  strip = strip_nested(),
  bleed = NULL
)

facet_nested_wrap_geo(
  facets,
  colors = rbind(periods, stages),
  nrow = NULL,
  ncol = NULL,
  scales = "fixed",
  axes = "margins",
  remove_labels = "none",
  shrink = TRUE,
  labeller = "label_value",
  lab_colors = "auto",
  as.table = TRUE,
  drop = TRUE,
  dir = "h",
  strip.position = "top",
  nest_line = element_line(inherit.blank = TRUE),
  solo_line = FALSE,
  resect = unit(0, "mm"),
  trim_blank = TRUE,
  strip = strip_nested(),
  bleed = NULL
)
}
\arguments{
\item{facets}{A set of variables or expressions quoted by \code{\link[ggplot2:vars]{vars()}}
and defining faceting groups on the rows or columns dimension.
The variables can be named (the names are passed to \code{labeller}).

For compatibility with the classic interface, can also be a
formula or character vector. Use either a one sided formula, \code{~a + b},
or a character vector, \code{c("a", "b")}.}

\item{colors}{Specifies which colors to use to replace the strip backgrounds.
Either A) a function that returns a color for a given strip label, B) the
character name of a function that does the same, C) a named character
vector with names matching strip labels and values indicating the desired
colors, or D) a data.frame representing a lookup table with columns named
"name" (matching strip labels) and "color" (indicating desired colors). If
the function returns \code{NA}, the default background color will be used.}

\item{nrow, ncol}{Number of rows and columns.}

\item{scales}{A \code{character(1)} or \code{logical(1)} whether scales are shared
across facets or allowed to vary. One of the following:
\describe{
\item{\code{"fixed"} or \code{FALSE}}{Scales are shared across all facets
(default).}
\item{\code{"free_x"}}{x-scales are allowed to vary.}
\item{\code{"free_y"}}{y-scales are allowed to vary.}
\item{\code{"free"} or \code{TRUE}}{Both scales can vary}
}}

\item{axes}{A \code{character(1)} or \code{logical(1)} where axes should be drawn. One
of the following:
\describe{
\item{\code{"margins"} or \code{FALSE}}{Only draw axes at the outer margins
(default).}
\item{\code{"x"}}{Draw axes at the outer margins and all inner x-axes too.}
\item{\code{"y"}}{Draw axes at the outer margins and all inner y-axes too.}
\item{\code{"all"} or \code{TRUE}}{Draw the axes for every panel.}
}}

\item{remove_labels}{A \code{character(1)} or \code{logical(1)} determining whether
axis text is displayed at inner panels. One of the following:
\describe{
\item{\code{"none"} or \code{FALSE}}{Display axis text at all axes (default).}
\item{\code{"x"}}{Display axis text at outer margins and all inner y-axes.}
\item{\code{"y"}}{Display axis text at outer margins and all inner x-axes.}
\item{\code{"all"} or \code{TRUE}}{Only display axis text at the outer margins.}
}}

\item{shrink}{If \code{TRUE}, will shrink scales to fit output of
statistics, not raw data. If \code{FALSE}, will be range of raw data
before statistical summary.}

\item{labeller}{A function that takes one data frame of labels and
returns a list or data frame of character vectors. Each input
column corresponds to one factor. Thus there will be more than
one with \code{vars(cyl, am)}. Each output
column gets displayed as one separate line in the strip
label. This function should inherit from the "labeller" S3 class
for compatibility with \code{\link[ggplot2:labeller]{labeller()}}. You can use different labeling
functions for different kind of labels, for example use \code{\link[ggplot2:label_parsed]{label_parsed()}} for
formatting facet labels. \code{\link[ggplot2:label_value]{label_value()}} is used by default,
check it for more details and pointers to other options.}

\item{lab_colors}{Specifies which colors to use for the strip labels. Either
A) a function that returns a color for a given strip label, B) the
character name of a function that does the same, C) a named character
vector with names matching strip labels and values indicating the desired
colors, D) a data.frame representing a lookup table with columns named
"name" (matching strip labels) and "lab_color" (indicating desired colors),
or E) "auto" (the default), which set the labels to black or white,
whichever has better contrast with the background color, based on
\href{https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en}{recommendations by the International Telecommunication Union}.
If the function returns \code{NA}, the default label color will be used.}

\item{as.table}{If \code{TRUE}, the default, the facets are laid out like
a table with highest values at the bottom-right. If \code{FALSE}, the
facets are laid out like a plot with the highest value at the top-right.}

\item{drop}{If \code{TRUE}, the default, all factor levels not used in the
data will automatically be dropped. If \code{FALSE}, all factor levels
will be shown, regardless of whether or not they appear in the data.}

\item{dir}{Direction: either \code{"h"} for horizontal, the default, or \code{"v"},
for vertical.}

\item{strip.position}{By default, the labels are displayed on the top of
the plot. Using \code{strip.position} it is possible to place the labels on
either of the four sides by setting \code{strip.position = c("top",
  "bottom", "left", "right")}}

\item{nest_line}{a theme element, either \code{element_blank()} or inheriting
from \code{\link[ggplot2:element]{ggplot2::element_line()}}. Lines are drawn between layers of strips
indicating hierarchy. The element inherits from the
\code{\link[ggh4x:theme_extensions]{ggh4x.facet.nestline}} element in the theme.}

\item{solo_line}{A \code{logical(1)} indicating whether parent strips with a
single child should be drawn with a \code{nest_line} (\code{TRUE}) or the line only
applies to parents with multiple children (\code{FALSE}, default). Only relevant
when \code{nest_line} is drawn.}

\item{resect}{a \code{unit} vector of length 1, indicating how much the
nesting line should be shortened.}

\item{trim_blank}{A \code{logical(1)}. When \code{TRUE} (default), does not draw rows
and columns containing no panels. When \code{FALSE}, the \code{nrow} and \code{ncol}
arguments are taken literally, even when there are more than needed to
fit all panels.}

\item{strip}{An object created by a call to a strip function, such as
\code{\link[ggh4x:strip_nested]{strip_nested()}}.}

\item{bleed}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} the \code{bleed} argument has
moved to the \code{strip_nested()} function.}
}
\description{
\code{facet_nested_wrap_color} behaves similarly to \code{\link[ggh4x:facet_nested_wrap]{ggh4x::facet_nested_wrap()}}
in that it wraps a sequence of panels onto a two-dimensional layout, and
nests grouped facets where possible.. The main difference is that it also
allows the user to specify the background and label colors of the individual
facet strips using the \code{colors} and \code{lab_colors} arguments.
}
\details{
\code{facet_nested_wrap_geo(...)} is an alias of \code{facet_nested_wrap_color()} with the
default of \code{colors} set to \code{rbind(periods, stages)}.
}
\examples{
library(ggplot2)
df <- data.frame(x = 1:10, y = 1:10,
                 period = factor(c("Permian", "Triassic", "Jurassic",
                                   "Cretaceous", "Paleogene"),
                                 levels = c("Permian", "Triassic",
                                            "Jurassic", "Cretaceous",
                                            "Paleogene")),
                 era = factor(c("Paleozoic", "Mesozoic", "Mesozoic",
                                "Mesozoic", "Cenozoic"),
                                levels = c("Paleozoic", "Mesozoic",
                                           "Cenozoic")))
ggplot(df, aes(x, y)) +
  geom_point() +
  facet_nested_wrap_color(~ era + period, colors = rbind(periods, eras))
}
\seealso{
Other faceting functions: 
\code{\link{facet_grid_color}()},
\code{\link{facet_nested_color}()},
\code{\link{facet_wrap_color}()}
}
\concept{faceting functions}
\keyword{datasets}
