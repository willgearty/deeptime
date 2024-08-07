% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patterns.R
\name{grid.pattern_geo}
\alias{grid.pattern_geo}
\title{Plot an individual FGDC pattern using grid}
\usage{
grid.pattern_geo(params, boundary_df, aspect_ratio, legend = FALSE)
}
\arguments{
\item{params}{A list of pattern parameters to customize the plotted pattern
(see "Details").}

\item{boundary_df}{A \code{data.frame} consisting of three columns: "x"
(x-coordinates), "y" (y-coordinates), and "id" (polygon group ID). This
\code{data.frame} defines the boundary (as a closed polygon) of the plotted
pattern.}

\item{aspect_ratio}{Unused.}

\item{legend}{Unused.}
}
\description{
This function can be used to plot a single geologic pattern as defined in the
\href{https://ngmdb.usgs.gov/fgdc_gds/geolsymstd.php}{FGDC Digital Cartographic Standard for Geologic Map Symbolization} by the \href{https://www.usgs.gov/}{U.S. Geological Survey} and the \href{https://ngmdb.usgs.gov/fgdc_gds/index.php}{Geologic Data Subcommittee (GDS)} of the \href{https://www.fgdc.gov/}{Federal Geographic Data Committee (FGDC)}. The pattern is plotted on the
existing canvas (i.e., use \code{\link[grid:grid.newpage]{grid::grid.newpage()}} to make a new canvas).
}
\details{
The following \code{params} are accepted:
\describe{
\item{\strong{\code{pattern_alpha}}}{ Alpha transparency for pattern. default: 1}
\item{\strong{\code{pattern_colour}}}{ Color used for strokes and points in
the pattern. default: 'black'}
\item{\strong{\code{pattern_fill}}}{ Color used to fill various closed shapes
(e.g., circles) in the pattern. default: \code{NA}}
\item{\strong{\code{pattern_scale}}}{ Scale. default: 2}
\item{\strong{\code{pattern_type}}}{ Code for the FGDC pattern to use. See
\code{\link[=geo_pattern]{geo_pattern()}} for more details. default: "101" }
\item{\strong{\code{fill}}}{ Color used for the background. default: "white" }
}
}
\section{Warning}{
 Pattern fills are not supported on all graphics devices.
Not all devices are under active development, and such devices are unlikely
to add support for new features (such as pattern fills). The new features
have only been implemented on a subset of graphics devices so far:
\code{\link[=cairo_pdf]{cairo_pdf()}}, \code{\link[=cairo_ps]{cairo_ps()}}, \code{\link[=x11]{x11(type="cairo")}},
\code{\link[=png]{png(type="cairo")}},
\code{\link[=jpeg]{jpeg(type="cairo")}},
\code{\link[=tiff]{tiff(type="cairo")}}, \code{\link[=svg]{svg()}}, and \code{\link[=pdf]{pdf()}}. Although
there is no support yet for \code{\link[=quartz]{quartz()}} or \code{\link[=windows]{windows()}}, almost all of the
graphics devices above will work on all major platforms. Further, the
\href{https://ragg.r-lib.org/}{ragg} and
\href{https://svglite.r-lib.org/index.html}{svglite} packages contain graphics
devices that support patterns. When using a graphics device where patterns
are not supported, closed shapes will be rendered with a transparent fill.
Note that, at least on Windows machines, the default device in RStudio and
in the knitr package is \code{\link[=png]{png()}}, which does not support patterns. In
RStudio, you can go to ‘Tools > Global Options > General > Graphics’ and
choose the ‘Cairo PNG’ device from the dropdown menu to display patterns.
Similar issues may arise when using RStudio on other operating systems.
}

\examples{
# use the function directly to make a hexagon with the pattern
library(grid)
x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
grid.newpage()
grid.pattern_geo(params = list(pattern_type = "633", pattern_scale = 4),
                 boundary_df = data.frame(x, y, id = 1))
\dontshow{if (require(ggpattern)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# use the function via ggpattern by specifying `pattern = 'geo'`
library(ggplot2)
library(ggpattern)
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df, aes(trt, outcome)) +
  geom_col_pattern(aes(color = trt, pattern_type = trt), pattern = 'geo',
  pattern_color = "black", fill = "white", pattern_fill = "white") +
  scale_pattern_type_manual(values = c("101", "313", "634")) +
  scale_color_viridis_d() +
  theme(legend.key.size = unit(1.5, 'cm'))
\dontshow{\}) # examplesIf}
}
\seealso{
FGDC patterns:
\code{\link{geo_pattern}()},
\code{\link{scale_fill_geopattern}()}
}
\concept{patterns}
