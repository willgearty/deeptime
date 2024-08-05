#' Get a FGDC geologic plotting pattern
#'
#' Retrieve a single geologic pattern as defined in the [FGDC Digital
#' Cartographic Standard for Geologic Map
#' Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd.php) by the [U.S.
#' Geological Survey](https://www.usgs.gov/) and the [Geologic Data Subcommittee
#' (GDS)](https://ngmdb.usgs.gov/fgdc_gds/index.php) of the [Federal Geographic
#' Data Committee (FGDC)](https://www.fgdc.gov/).
#'
#' @details For specific codes, see the "pattern numbers" in the [full pattern
#'   chart](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd/fgdc-geolsym-patternchart.pdf)
#'   for valid `code` values. Daven Quinn has also assembled more accessible
#'   documentation of the [map
#'   patterns/codes](https://davenquinn.com/projects/geologic-patterns/#pattern-reference)
#'   and [lithology
#'   patterns/codes](https://davenquinn.com/projects/geologic-patterns/#series-600).
#'   [rmacrostrat::def_lithologies()] can also be used to look up pattern codes
#'   for various lithologies (see the "fill" column). Note that codes associated
#'   with color variants (e.g., "101-M") are supported but will result in the
#'   default color variant instead (usually black and white, e.g., "101-K").
#'
#'   These patterns were originally processed and optimized by Daven Quinn and
#'   are hosted on [GitHub](https://github.com/davenquinn/geologic-patterns/).
#'
#' @param code The number corresponding to the pattern to return. Strings and
#'   numbers are permitted.
#' @param scale The visual scale of the pattern (higher values mean the pattern
#'   is more zoomed in).
#' @param col The color to use for the lines of the pattern.
#' @param fill The color used to fill various closed shapes (e.g., circles) in
#'   the pattern.
#' @param alpha The transparency to use for the fill of the pattern.
#' @param bg The background color to use for the pattern.
#'
#' @return `geo_grob()` returns a [grob][grid::grid.grob] object with a single
#'   instance of the desired pattern. `geo_pattern()` returns a
#'   [GridPattern][grid::patterns] object with a repeated instance of the
#'   desired pattern.
#' @importFrom grid viewport pattern unit
#' @export
#'
#' @examples
#' library(grid)
#' # Get a generic igneous pattern
#' pattern1 <- geo_pattern(code = "313-K")
#' # Get the pattern for a sandstone
#' pattern2 <- geo_pattern(code = "607")
#'
#' # plot the two patterns
#' grid.newpage()
#' grid.draw(rectGrob(gp = gpar(fill = pattern1)))
#' grid.newpage()
#' grid.draw(rectGrob(gp = gpar(fill = pattern2)))
#' @family patterns
geo_pattern <- function(code, scale = 2,
                        col = NULL, fill = NULL, alpha = NULL, bg = "white") {
  img_grob <- geo_grob(code, col = col, fill = fill, alpha = alpha, bg = bg)

  # need to account for grobs that aren't square
  scale_vp <- img_grob$children[[1]]$vp[[2]]
  ar <- abs(diff(scale_vp$xscale)) / abs(diff(scale_vp$yscale))

  if (!is.null(scale) && !is.na(scale)) scale <- 2

  # convert to pattern
  img_grob$vp <- viewport(width = unit(ar * scale, "cm"),
                          height = unit(scale, "cm"))
  pattern(img_grob, width = unit(ar * scale, "cm"), height = unit(scale, "cm"),
          extend = "repeat")
}

#' @export
#' @rdname geo_pattern
#' @importFrom grid editGrob rectGrob gList gTree gPath gpar
geo_grob <- function(code,
                     col = NULL, fill = NULL, alpha = NULL, bg = "white") {
  code <- as.character(code)
  # get the grob for the given code
  code <- gsub("(-.*)?", "", code)
  if (code %in% names(geo_grobs)) {
    img_grob <- geo_grobs[[code]]
  } else {
    cli::cli_abort(paste0("`code` \"", code, "\" does not match an FGDC code."))
  }

  # recolor the grob (this modifies all children)
  gp <- list()
  if (!is.null(col) && !is.na(col)) gp$col <- col
  if (!is.null(fill) && !is.na(fill)) gp$fill <- fill
  if (!is.null(alpha) && !is.na(alpha)) gp$alpha <- alpha
  img_grob <- editGrob(img_grob, gp = do.call(gpar, gp),
                       gPath("*"), grep = TRUE, global = TRUE)

  # now add the background with a transparent outline
  rect_grob <- rectGrob(gp = gpar(fill = bg, col = "transparent"))
  rect_grob$vp <- img_grob$vp
  img_grob <- gList(rect_grob, img_grob)

  # needed to make it work for ggplot stuff
  gTree(children = img_grob)
}

#' Geologic pattern fill scale
#'
#' Fill scale using the [FGDC Digital Cartographic Standard for Geologic Map
#' Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd.php). Fill values
#' should correspond to specific pattern codes (see "Details"). Note that
#' `ggplot2` >= 3.5.0 is required to use this function.
#'
#' @inherit geo_pattern details
#' @inheritDotParams ggplot2::discrete_scale -palette -aesthetics -super -position -expand
#' @param na.value The aesthetic value to use for missing (NA) values. May be
#'   either a color or a [GridPattern][grid::patterns] object (such as that
#'   returned by [geo_pattern()]).
#' @section Warning: Pattern fills are not supported on all graphics devices.
#'   Not all devices are under active development, and such devices are unlikely
#'   to add support for new features (such as pattern fills). The new features
#'   have only been implemented on a subset of graphics devices so far:
#'   [cairo_pdf()], [cairo_ps()], \code{\link[=x11]{x11(type="cairo")}},
#'   \code{\link[=png]{png(type="cairo")}},
#'   \code{\link[=jpeg]{jpeg(type="cairo")}},
#'   \code{\link[=tiff]{tiff(type="cairo")}}, [svg()], and [pdf()]. Although
#'   there is no support yet for [quartz()] or [windows()], almost all of the
#'   graphics devices above will work on all major platforms. Further, the
#'   [ragg](https://ragg.r-lib.org/) and
#'   [svglite](https://svglite.r-lib.org/index.html) packages contain graphics
#'   devices that support patterns. When using a graphics device where patterns
#'   are not supported, closed shapes will be rendered with a transparent fill.
#'   Note that, at least on Windows machines, the default device in RStudio and
#'   in the knitr package is [png()], which does not support patterns. In
#'   RStudio, you can go to ‘Tools > Global Options > General > Graphics’ and
#'   choose the ‘Cairo PNG’ device from the dropdown menu to display patterns.
#'   Similar issues may arise when using RStudio on other operating systems.
#' @export
#' @importFrom ggplot2 discrete_scale
#' @importFrom methods is
#' @examplesIf packageVersion("ggplot2") >= "3.5.0"
#' library(ggplot2)
#' vals <- c("101", "313", "603", "733")
#' ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
#'   geom_bar() +
#'   scale_fill_geopattern(name = NULL)
#' @family patterns
scale_fill_geopattern <- function(na.value = "grey50", ...) {
  rlang::check_installed("ggplot2", version = "3.5.0",
                         reason = "to use `scale_fill_geopattern()`")
  discrete_scale(
    "fill", palette = NULL, ..., na.value = na.value,
    super = ScaleDiscreteGeoPattern
  )
}
# TODO: is there a way to support colors and scaling?

#' @importFrom ggplot2 ggproto ScaleDiscrete
#' @keywords internal
ScaleDiscreteGeoPattern <- ggproto("ScaleDiscreteGeoPattern", ScaleDiscrete,
  map = function(self, x, limits = self$get_limits()) {
    na_value <- if (self$na.translate) self$na.value else NA
    # clean limits
    limits <- limits[!is.na(limits)]
    # if no limits, just return a bunch of NA values
    if (length(limits) < 1) {
      return(rep(list(na_value), length(x)))
    }

    # convert to character
    if (is.factor(x)) {
      x <- as.character(x)
    }

    # filter to limits
    x[is.na(match(x, limits))] <- NA

    # get patterns
    sapply(x, function(code) {
      if (is.na(code)) {
        na_value
      } else {
        geo_pattern(code)
      }
    },
    simplify = FALSE)
  }
)

#' Plot an individual FGDC pattern using grid
#'
#' This function can be used to plot a single geologic pattern as defined in the
#' [FGDC Digital Cartographic Standard for Geologic Map
#' Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd.php) by the [U.S.
#' Geological Survey](https://www.usgs.gov/) and the [Geologic Data Subcommittee
#' (GDS)](https://ngmdb.usgs.gov/fgdc_gds/index.php) of the [Federal Geographic
#' Data Committee (FGDC)](https://www.fgdc.gov/). The pattern is plotted on the
#' existing canvas (i.e., use [grid::grid.newpage()] to make a new canvas).
#'
#' @details The following `params` are accepted:
#'   \describe{
#'     \item{\strong{`pattern_alpha`}}{ Alpha transparency for pattern. default: 1}
#'     \item{\strong{`pattern_colour`}}{ Color used for strokes and points in
#'       the pattern. default: 'black'}
#'     \item{\strong{`pattern_fill`}}{ Color used to fill various closed shapes
#'       (e.g., circles) in the pattern. default: `NA`}
#'     \item{\strong{`pattern_scale`}}{ Scale. default: 2}
#'     \item{\strong{`pattern_type`}}{ Code for the FGDC pattern to use. See
#'       [geo_pattern()] for more details. default: "101" }
#'     \item{\strong{`fill`}}{ Color used for the background. default: "white" }
#'   }
#' @param params A list of pattern parameters to customize the plotted pattern
#'   (see "Details").
#' @param boundary_df A `data.frame` consisting of three columns: "x"
#'   (x-coordinates), "y" (y-coordinates), and "id" (polygon group ID). This
#'   `data.frame` defines the boundary (as a closed polygon) of the plotted
#'   pattern.
#' @param aspect_ratio Unused.
#' @param legend Unused.
#' @inheritSection scale_fill_geopattern Warning
#'
#' @importFrom grid viewport pattern unit grid.polygon
#' @family patterns
#' @export
#' @examples
#' # use the function directly to make a hexagon with the pattern
#' library(grid)
#' x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' grid.newpage()
#' grid.pattern_geo(params = list(pattern_type = "633", pattern_scale = 4),
#'                  boundary_df = data.frame(x, y, id = 1))
#' @examplesIf require(ggpattern)
#' # use the function via ggpattern by specifying `pattern = 'geo'`
#' library(ggplot2)
#' library(ggpattern)
#' df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#' ggplot(df, aes(trt, outcome)) +
#'   geom_col_pattern(aes(color = trt, pattern_type = trt), pattern = 'geo',
#'   pattern_color = "black", fill = "white", pattern_fill = "white") +
#'   scale_pattern_type_manual(values = c("101", "313", "634")) +
#'   scale_color_viridis_d() +
#'   theme(legend.key.size = unit(1.5, 'cm'))
grid.pattern_geo <- function(params, boundary_df, aspect_ratio,
                             legend = FALSE) {
  # not sure why (probably viewport-related), but we can't just use geo_pattern
  grob <- geo_grob(params$pattern_type %||% "101",
                   col = params$pattern_colour %||% "black",
                   fill = params$pattern_fill %||% NA,
                   alpha = params$pattern_alpha %||% 1,
                   bg = params$fill %||% "white")

  # need to account for grobs that aren't square
  scale_vp <- grob$children[[1]]$vp[[2]]
  ar <- abs(diff(scale_vp$xscale)) / abs(diff(scale_vp$yscale))

  scale <- params$pattern_scale %||% 2
  grob$vp <- viewport(width = unit(ar * scale, "cm"),
                      height = unit(scale, "cm"))
  patt <- pattern(grob, extend = "repeat",
                  width = unit(ar * scale, "cm"), height = unit(scale, "cm"))
  grid.polygon(x = boundary_df$x, y = boundary_df$y, id = boundary_df$id,
               gp = gpar(fill = patt))
}
