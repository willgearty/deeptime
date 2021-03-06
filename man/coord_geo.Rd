% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coord_geo.R
\docType{data}
\name{coord_geo}
\alias{coord_geo}
\alias{CoordGeo}
\title{Transformed coordinate system with geological timescale}
\usage{
coord_geo(
  pos = "bottom",
  dat = "periods",
  xlim = NULL,
  ylim = NULL,
  xtrans = identity_trans(),
  ytrans = identity_trans(),
  clip = "on",
  expand = FALSE,
  fill = NULL,
  color = "black",
  alpha = 1,
  height = unit(2, "line"),
  lab = TRUE,
  rot = 0,
  abbrv = TRUE,
  skip = c("Quaternary", "Holocene", "Late Pleistocene"),
  size = 5,
  lwd = 0.25,
  neg = FALSE,
  bord = c("left", "right", "top", "bottom"),
  center_end_labels = FALSE
)
}
\arguments{
\item{pos}{Which side to add the scale to (left, right, top, or bottom). First letter may also be used.}

\item{dat}{Either A) a string indicating a built-in dataframe with interval data from the ICS ("periods", "epochs", "stages", "eons", or "eras"),
B) a string indicating a timescale from macrostrat (see list here: \url{https://macrostrat.org/api/defs/timescales?all}),
or C) a custom dataframe of time interval boundaries (see Details).}

\item{xlim, ylim}{Limits for the x and y axes.}

\item{xtrans, ytrans}{transformers for y axis. For more information see \code{\link[ggplot2]{coord_trans}}.}

\item{expand}{If `TRUE`, the default, adds a small expansion factor to
the limits to ensure that data and axes don't overlap. If `FALSE`,
limits are taken exactly from the data or `xlim`/`ylim`.}

\item{fill}{The fill color of the boxes. The default is to use the colors included in \code{dat}.
If a custom dataset is provided with \code{dat} without color and without fill, a greyscale will be used.
Custom fill colors can be provided with this option and will be recycled if/as necessary.}

\item{color}{The outline color of the interval boxes.}

\item{alpha}{The transparency of the fill colors.}

\item{height}{The height (or width if \code{pos} is \code{left} or \code{right}) of the scale.}

\item{lab}{Whether to include labels.}

\item{rot}{The amount of counter-clockwise rotation to add to the labels (in degrees).}

\item{abbrv}{If including labels, whether to use abbreviations instead of full interval names.}

\item{skip}{A vector of interval names indicating which intervals should not be labelled.}

\item{size}{Label size.}

\item{lwd}{Line width.}

\item{neg}{Set this to true if your x-axis is using negative values.}

\item{bord}{A vector specifying on Which sides of the scale to add borders (same options as \code{pos}).}

\item{center_end_labels}{Should labels be centered within the visible range of intervals at the ends of the axis?}
}
\description{
\code{coord_geo} behaves similarly to \code{\link[ggplot2]{coord_trans}} in that it occurs after
statistical transformation and will affect the visual appearance of geoms. The main difference
is that it also adds a geological timescale to the specified side of the plot.
}
\details{
Transforming the side with the scale is not currently implemented.
If custom data is provided (with \code{dat}), it should consist of at least 3 columns of data. See \code{data(periods)} for an example.
  The \code{name} column lists the names of each time interval. These will be used as labels if no abbreviations are provided.
  The \code{max_age} column lists the oldest boundary of each time interval.
  The \code{min_age} column lists the youngest boundary of each time interval.
  The \code{abbr} column is optional and lists abbreviations that may be used as labels.
  The \code{color} column is also optional and lists a hex color code (which can be obtained with \code{rgb()}) for each time interval.

\code{pos} may also be a list of sides (including duplicates) if multiple time scales should be added to the plot.
In this case, \code{dat}, \code{fill}, \code{color}, \code{alpha}, \code{height}, \code{lab},
\code{rot}, \code{abbrv}, \code{skip}, \code{size}, \code{lwd}, \code{neg}, and \code{bord} can also be lists.
If these lists are not as long as \code{pos}, the elements will be recycled.
}
\examples{
library(ggplot2)
#single scale on bottom
ggplot() +
  geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 1000))) +
  scale_x_reverse() +
  coord_geo(xlim = c(1000, 0), ylim = c(0,8)) +
  theme_classic()

#stack multiple scales
ggplot() +
  geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 100))) +
  scale_x_reverse() +
  coord_geo(xlim = c(100, 0), ylim = c(0,8), pos = as.list(rep("bottom", 3)),
  dat = list("stages", "epochs", "periods"),
  height = list(unit(4, "lines"), unit(4, "lines"), unit(2, "line")),
  rot = list(90, 90, 0), size = list(2.5, 2.5, 5), abbrv = FALSE) +
  theme_classic()
}
\keyword{datasets}
