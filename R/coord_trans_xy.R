#' Transformed XY Cartesian coordinate system
#'
#' `coord_trans_xy` behaves similarly to [ggplot2::coord_trans()] in that it
#' occurs after statistical transformation and will affect the visual appearance
#' of geoms. The main difference is that it takes a single transformer that is
#' applied to the x and y axes simultaneously. Any transformers produced by
#' [ggforce::linear_trans()] that have x and y arguments should work, but any
#' other transformers produced using [scales::trans_new()] that take x and y
#' arguments should also work. Axis limits will be adjusted to account for
#' transformation unless limits are specified with `xlim` or `ylim`.
#'
#' @details This coordinate system only works with geoms where all points are
#'   defined with x and y coordinates (e.g., [ggplot2::geom_point()],
#'   [ggplot2::geom_polygon()]). This does not currently work with geoms where
#'   point coordinates are extrapolated (e.g., [ggplot2::geom_rect()]).
#'   Furthermore, when used with ggplot2 3.5.0 and later, some transformation
#'   edge cases may cause problems with rendering axis lines. This includes not
#'   currently support "capping" axes. I hope to support all of these geoms,
#'   edge cases, and features in the future.
#'
#' @param trans Transformer for x and y axes.
#' @importFrom ggplot2 ggproto
#' @importFrom ggforce linear_trans
#' @importFrom cli cli_abort
#' @inheritParams ggplot2::coord_cartesian
#' @export
#' @examples
#' # make transformer
#' library(ggforce)
#' trans <- linear_trans(shear(2, 0), rotate(-pi / 3))
#'
#' # set up data to be plotted
#' square <- data.frame(x = c(0, 0, 4, 4), y = c(0, 1, 1, 0))
#' points <- data.frame(x = runif(100, 0, 4), y = runif(100, 0, 1))
#'
#' # plot data normally
#' library(ggplot2)
#' ggplot(data = points, aes(x = x, y = y)) +
#'   geom_polygon(data = square, fill = NA, color = "black") +
#'   geom_point(color = "black") +
#'   coord_cartesian(expand = FALSE) +
#'   theme_classic()
#'
#' # plot data with transformation
#' ggplot(data = points, aes(x = x, y = y)) +
#'   geom_polygon(data = square, fill = NA, color = "black") +
#'   geom_point(color = "black") +
#'   coord_trans_xy(trans = trans, expand = FALSE) +
#'   theme_classic()
coord_trans_xy <- function(trans = NULL, xlim = NULL, ylim = NULL,
                           expand = FALSE, default = FALSE, clip = "on") {
  if (is.null(trans)) trans <- linear_trans(translate(0, 0))
  ggproto(NULL, CoordTransXY,
    trans = trans,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default,
    clip = clip
  )
}

default_expansion <- function(...) {
  asNamespace("ggplot2")$default_expansion(...)
}

expand_range4 <- function(...) {
  asNamespace("ggplot2")$expand_range4(...)
}

dist_euclidean <- function(...) {
  asNamespace("ggplot2")$dist_euclidean(...)
}

#' @rdname coord_trans_xy
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto CoordCartesian CoordTrans ggproto_parent
#' @importFrom ggplot2 transform_position
#' @importFrom scales rescale squish_infinite
#' @export
CoordTransXY <- ggproto("CoordTransXY", CoordTrans,
  distance = function(self, x, y, panel_params) {
    max_dist <- dist_euclidean(panel_params$x.range, panel_params$y.range)
    points_trans <- self$trans$transform(x, y)
    dist_euclidean(points_trans$x, points_trans$y) / max_dist
  },
  backtransform_range = function(self, panel_params) {
    ranges <- self$trans$inverse(panel_params$x.range, panel_params$y.range)
    list(
      x = ranges$x,
      y = ranges$y
    )
  },
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    # TODO: handle discrete scales?
    expansion_x <- default_expansion(scale_x, expand = self$expand)
    expansion_y <- default_expansion(scale_y, expand = self$expand)
    limits <- data.frame(
      x = if (is.null(self$limits$x)) scale_x$get_limits() else self$limits$x,
      y = if (is.null(self$limits$y)) scale_y$get_limits() else self$limits$y
    )
    lims <- expand.grid(x = limits$x, y = limits$y)
    lims_trans <- self$trans$transform(lims$x, lims$y)
    limits_trans <- data.frame(
      x = if (scale_x$trans$name == "reverse") rev(range(lims_trans$x))
            else range(lims_trans$x),
      y = if (scale_y$trans$name == "reverse") rev(range(lims_trans$y))
            else range(lims_trans$y)
    )
    # range expansion expects values in increasing order, which may not be true
    # for reciprocal/reverse transformations
    if (all(is.finite(limits_trans$x)) && diff(limits_trans$x) < 0) {
      range_x_coord <- rev(expand_range4(rev(limits_trans$x), expansion_x))
    } else {
      range_x_coord <- expand_range4(limits_trans$x, expansion_x)
    }
    if (all(is.finite(limits_trans$y)) && diff(limits_trans$y) < 0) {
      range_y_coord <- rev(expand_range4(rev(limits_trans$y), expansion_y))
    } else {
      range_y_coord <- expand_range4(limits_trans$y, expansion_y)
    }
    expand_range <- expand.grid(x = range_x_coord, y = range_y_coord)
    final_scale_limits <- self$trans$inverse(expand_range$x, expand_range$y)

    scale_range_x <- final_scale_limits$x[1:2]
    scale_range_y <- final_scale_limits$y[c(1, 3)]
    scale_range_x_sec <- final_scale_limits$x[3:4]
    scale_range_y_sec <- final_scale_limits$y[c(2, 4)]

    # calculate break information
    out_x <- scale_x$break_info(scale_range_x)
    out_y <- scale_y$break_info(scale_range_y)

    # secondary axis breaks are potentially different
    out_x_sec <- scale_x$break_info(scale_range_x_sec)
    out_y_sec <- scale_x$break_info(scale_range_y_sec)

    # range in coord space
    out_x$range <- range(range_x_coord)
    out_y$range <- range(range_y_coord)

    c(
      list(
        x = view_scale_primary(scale_x, continuous_range = scale_range_x),
        x.sec = view_scale_secondary(scale_x,
                                     continuous_range = scale_range_x_sec),
        x.range = out_x$range,
        x.range.coord = range_x_coord,
        x.full.range = final_scale_limits$x[c(1, 4)],
        x.labels = out_x$labels,
        x.major = rescale(out_x$major_source, 0:1, scale_range_x),
        x.minor = rescale(out_x$minor_source, 0:1, scale_range_x),
        x.sec.labels = out_x_sec$sec.labels,
        x.sec.major = rescale(out_x_sec$sec.major_source,
                              0:1, scale_range_x_sec),
        x.sec.minor = rescale(out_x_sec$sec.minor_source,
                              0:1, scale_range_x_sec)
      ),
      list(
        y = view_scale_primary(scale_y, continuous_range = scale_range_y),
        y.sec = view_scale_secondary(scale_y,
                                     continuous_range = scale_range_y_sec),
        y.range = out_y$range,
        y.range.coord = range_y_coord,
        y.full.range = final_scale_limits$y[c(1, 4)],
        y.labels = out_y$labels,
        y.major = rescale(out_y$major_source, 0:1, scale_range_y),
        y.minor = rescale(out_y$minor_source, 0:1, scale_range_y),
        y.sec.labels = out_y_sec$sec.labels,
        y.sec.major = rescale(out_y_sec$sec.major_source,
                              0:1, scale_range_y_sec),
        y.sec.minor = rescale(out_y_sec$sec.minor_source,
                              0:1, scale_range_y_sec)
      )
    )
  },
  transform = function(self, data, panel_params) {
    new_data <- data
    # transform x and y coordinates
    if ("x" %in% colnames(new_data)) {
      # hacks for axis lines
      if (all(data$y == panel_params$y.full.range[1])) {
        new_data$x <- rescale(data$x, 0:1, range(data$x))
      } else if (all(data$y == panel_params$y.full.range[2])) {
        new_data$x <- rescale(data$x, 0:1, range(data$x))
      } else if (all(data$x == panel_params$x.full.range[1])) {
        new_data$y <- rescale(data$y, 0:1, range(data$y))
      } else if (all(data$x == panel_params$x.full.range[2])) {
        new_data$y <- rescale(data$y, 0:1, range(data$y))
      }
      # hacks for axis tick labels
      else if (all(new_data$x == -Inf)) {
        new_data$y <- rescale(data$y, 0:1, panel_params$y$continuous_range)
      } else if (all(data$x == Inf)) {
        new_data$y <- rescale(data$y, 0:1, panel_params$y.sec$continuous_range)
      } else if (all(data$y == -Inf)) {
        new_data$x <- rescale(data$x, 0:1, panel_params$x$continuous_range)
      } else if (all(data$y == Inf)) {
        new_data$x <- rescale(data$x, 0:1, panel_params$x.sec$continuous_range)
      } else {
        temp_data <- self$trans$transform(data$x, data$y)
        new_data$x <- rescale(temp_data$x, 0:1, panel_params$x.range.coord)
        new_data$y <- rescale(temp_data$y, 0:1, panel_params$y.range.coord)
      }
    }
    # transform end points for segments
    if ("xend" %in% colnames(new_data)) {
      temp_data <- self$trans$transform(new_data$xend, data$yend)
      new_data$xend <- rescale(temp_data$x, 0:1, panel_params$x.range.coord)
      new_data$yend <- rescale(temp_data$y, 0:1, panel_params$y.range.coord)
    }
    # TODO: transform corners for geom_rect?
    transform_position(new_data, squish_infinite, squish_infinite)
  }
)

view_scale_primary <- function(...) {
  asNamespace("ggplot2")$view_scale_primary(...)
}

view_scale_secondary <- function(...) {
  asNamespace("ggplot2")$view_scale_secondary(...)
}
