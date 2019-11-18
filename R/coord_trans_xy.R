#' Transformed XY Cartesian coordinate system
#'
#' \code{coord_trans_xy} behaves similarly to \code{\link[ggplot2]{coord_trans}} in that it occurs after
#' statistical transformation and will affect the visual appearance of geoms. The main difference
#' is that it takes a single transforer that is applied to the x and y axes simultaneously. Any
#' transformers produced by \code{\link[ggforce]{linear_trans}} that have x and y arguments should work,
#' but any other transformers produced using \code{\link[scales]{trans_new}} that take x and y arguments
#' should also work. Axis limits will be adjusted to account for transformation unless limits are
#' specified with `xlim` or `ylim`.
#'
#' @param trans Transformer for x and y axes.
#' @param xlim,ylim Limits for the x and y axes.
#' @param expand If `TRUE`, the default, adds a small expansion factor to
#'   the limits to ensure that data and axes don't overlap. If `FALSE`,
#'   limits are taken exactly from the data or `xlim`/`ylim`.
#' @param default Is this the default coordinate system? If `FALSE` (the default),
#'   then replacing this coordinate system with another one creates a message alerting
#'   the user that the coordinate system is being replaced. If `TRUE`, that warning
#'   is suppressed.
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of `"on"` (the default) means yes, and a setting of `"off"`
#'   means no. In most cases, the default of `"on"` should not be changed,
#'   as setting `clip = "off"` can cause unexpected results. It allows
#'   drawing of data points anywhere on the plot, including in the plot margins. If
#'   limits are set via `xlim` and `ylim` and some data points fall outside those
#'   limits, then those data points may show up in places such as the axes, the
#'   legend, the plot title, or the plot margins.
#' @importFrom ggplot2 ggproto CoordCartesian ggproto_parent
#' @export
#' @examples
#' #make transformer
#' library(ggforce)
#' trans <- linear_trans(shear(2, 0), rotate(-pi / 3))
#'
#' #set up data to be plotted
#' square <- data.frame(x = c(0, 0, 4, 4), y = c(0, 1, 1, 0))
#' points <- data.frame(x = runif(100, 0, 4), y = runif(100, 0, 1))
#'
#' #plot data normally
#' library(ggplot2)
#' ggplot(data = points, aes(x = x, y = y)) +
#'   geom_polygon(data = square, fill = NA, color = "black") +
#'   geom_point(color = 'black') +
#'   coord_cartesian(expand = FALSE) +
#'   theme_classic()
#'
#' #plot data with transformation
#' ggplot(data = points, aes(x = x, y = y)) +
#'   geom_polygon(data = square, fill = NA, color = "black") +
#'   geom_point(color = 'black') +
#'   coord_trans_xy(trans = trans, expand = FALSE) +
#'   theme_classic()
coord_trans_xy <- function(trans = NULL, xlim = NULL, ylim = NULL, expand = TRUE,
                           default = FALSE, clip = "on") {
  ggproto(NULL, CoordTransXY,
          trans = trans,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          default = default,
          clip = clip
  )
}

#' @rdname coord_trans_xy
#' @format NULL
#' @usage NULL
#' @export
CoordTransXY <- ggproto("CoordTransXY", CoordCartesian,
                        setup_panel_params = function(self, scale_x, scale_y, params = list()) {
                          if (is.null(self$limits$x) | is.null(self$limits$y)){
                            lims <- expand.grid(x = scale_x$get_limits(), y = scale_y$get_limits())
                            new_lims <- self$trans$transform(lims$x, lims$y)
                            if (is.null(self$limits$x)) self$limits$x <- range(new_lims$x)
                            if (is.null(self$limits$y)) self$limits$y <- range(new_lims$y)
                          }
                          parent <- ggproto_parent(CoordCartesian, self)
                          panel_params <- parent$setup_panel_params(scale_x, scale_y, params)
                          panel_params$trans <- self$trans
                          panel_params
                        },
                        transform = function(data, panel_params) {
                          new_data <- data
                          if (!is.null(panel_params$trans)) {
                            new_data[, c("x","y")] <- panel_params$trans$transform(data$x, data$y)
                          }
                          CoordCartesian$transform(new_data, panel_params)
                        }
)
