#' Polar coordinate system with geological timescale
#'
#' \code{coord_geo_polar} behaves similarly to \code{\link[ggplot2]{coord_polar}} in that it occurs after
#' statistical transformation and will affect the visual appearance of geoms. The main difference
#' is that it also adds a geological timescale to the background of the plot.
#'
#' If a custom data.frame is provided (with \code{dat}), it should consist of at least 3 columns of data. See \code{data(periods)} for an example.
#' \itemize{
#'   \item The \code{max_age} column lists the oldest boundary of each time interval.
#'   \item The \code{min_age} column lists the youngest boundary of each time interval.
#'   \item The \code{color} column is optional and lists a \link[ggplot2]{color} for the background for each time interval.
#' }
#'
#' \code{dat} may also be a list of values and/or dataframes if multiple time scales should be added to the background.
#' Scales will be added sequentially starting at \code{start} and going in the specified \code{direction}.
#' By default the scales will all be equal in circular/rotational proportion, but this can be overriden with \code{prop}.
#' If \code{dat} is a list, \code{fill}, \code{alpha}, \code{lwd}, \code{lty}, \code{color}, \code{neg}, and \code{prop} can also be lists.
#' If these lists are not as long as \code{dat}, the elements will be recycled.
#' If individual values (or vectors) are used for these parameters, they will be applied to all time scales (and recycled as necessary).
#'
#' If the sum of the \code{prop} values is greater than 1, the proportions will be scaled such that they sum to 1.
#' However, the \code{prop} values may sum to less than 1 if the user would like blank space in the background.
#'
#' The \code{axis.line.r}, \code{axis.text.r}, \code{axis.ticks.r}, and \code{axis.ticks.length.r}
#' ggplot2 \link[ggplot2:theme]{theme elements} can be modified just like their x and y counterparts
#' to change the appearance of the radius axis. The default settings work well for a horizontal axis
#' pointing towards the right, but these theme settings will need to be modified for other orientations.
#' The default value for \code{axis.line.r} is \code{element_line(color = "grey50")}.
#' The default value for \code{axis.text.r} is \code{element_text(size = 3.5, vjust = -2, hjust = NA)}.
#' The default value for \code{axis.ticks.r} is \code{element_line()}.
#' The default value for \code{axis.ticks.length.r} is \code{unit(1.5, "points")}. However, note that the units
#' for this element are meaningless and only the numeric value will be used (but a \code{unit} must still be used).
#'
#' @param dat Either A) a string indicating a built-in dataframe with interval data from the ICS ("periods", "epochs", "stages", "eons", or "eras"),
#'   B) a string indicating a timescale from macrostrat (see list here: \url{https://macrostrat.org/api/defs/timescales?all}),
#'   or C) a custom data.frame of time interval boundaries (see Details).
#' @param fill The fill color of the background. The default is to use the \code{color} column included in \code{dat}.
#'   If a custom dataset is provided with \code{dat} without a \code{color} column and without fill, a greyscale will be used.
#'   Custom fill colors can be provided with this option (overriding the \code{color} column) and will be recycled if/as necessary.
#' @param alpha The transparency of the fill colors.
#' @param lwd Line width for lines between intervals. Set to \code{NULL} to remove lines.
#' @param lty Line type for lines between intervals.
#' @param color The color of the lines between intervals.
#' @param neg Set this to true if your theta-axis is using negative values. This is often true if you are using \code{ggtree}.
#' @param prop This is the rotational proportion of the background that the scale takes up.
#' @inheritParams ggplot2::coord_polar
#' @importFrom ggplot2 ggproto
#' @importFrom rlang arg_match0
#' @export
#' @examples
#' library(ggplot2)
#' @examplesIf require(ggtree)
#' library(ggtree)
#' set.seed(1)
#' tree <- rtree(100)
#' # single scale
#' revts(ggtree(tree)) +
#'   coord_geo_polar(dat = "stages")
#'
#' # multiple scales
#' revts(ggtree(tree)) +
#'   coord_geo_polar(dat = list("stages", "periods"), alpha = .5,
#'                   prop = list(0.75, .25), start = pi/4, lty = "dashed") +
#'   scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
#'   theme(axis.text.r = element_text(size = 3.5, hjust = .75, vjust = .75))
#' @examplesIf require(ggtree) && require(paleotree)
#' library(ggplot2)
#' library(paleotree)
#' data(RaiaCopesRule)
#' ggtree(ceratopsianTreeRaia, position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
#'   coord_geo_polar(dat = "stages")
coord_geo_polar <- function(dat = "periods", theta = "y",
                            start = -pi/2, direction = -1, clip = "off",
                            fill = NULL, alpha = 1,
                            lwd = .25, color = "grey80", lty = "solid",
                            neg = TRUE, prop = 1) {

  dat <- make_list(dat)
  n_scales <- length(dat)

  theta <- arg_match0(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"

  ggproto(NULL, CoordGeoPolar,
          theta = theta, r = r,
          start = start, direction = sign(direction), clip = clip,
          dat = dat,
          fill = rep(make_list(fill), length.out = n_scales),
          alpha = rep(make_list(alpha), length.out = n_scales),
          lwd = rep(make_list(lwd), length.out = n_scales),
          lty = rep(make_list(lty), length.out = n_scales),
          color = rep(make_list(color), length.out = n_scales),
          neg = rep(make_list(neg), length.out = n_scales),
          prop = rep(make_list(prop), length.out = n_scales)
  )
}

rename_data <- utils::getFromNamespace("rename_data", "ggplot2")
ggname <- utils::getFromNamespace("ggname", "ggplot2")

#' @rdname coord_geo_polar
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto CoordPolar ggproto_parent coord_polar theme_void geom_vline
#' @importFrom ggplot2 geom_rect geom_segment scale_x_continuous scale_fill_manual calc_element
#' @importFrom grid addGrob reorderGrob grid.ls
#' @importFrom rlang %||%
CoordGeoPolar <- ggproto("CoordGeoPolar", CoordPolar,
  render_bg = function(self, panel_params, theme) {
    panel_params <- rename_data(self, panel_params)
    # do the new coord_geo_polar background stuff
    r_lims <- panel_params$r.range

    # convert, subset, and adjust data
    clean_dat <- function(dat, fill, neg) {
      if (is(dat, "data.frame")) {
        #just use the supplied data
      } else {
        dat <- getScaleData(dat)
      }

      if (neg) {
        dat$max_age <- -1 * (dat$max_age)
        dat$min_age <- -1 * (dat$min_age)
      }

      if (!is.null(fill)) {
        dat$color <- rep(fill, length.out = nrow(dat))
      } else if (!("color" %in% colnames(dat))) {
        dat$color <- rep(c("grey60","grey80"), length.out = nrow(dat))
      }

      if (neg) {
        dat$max_age[(dat$max_age < min(r_lims) & dat$min_age < min(r_lims)) |
                      (dat$max_age < min(r_lims) & dat$min_age > min(r_lims))] <- min(r_lims)
        dat$min_age[(dat$max_age > max(r_lims) & dat$min_age < max(r_lims)) |
                      (dat$max_age < max(r_lims) & dat$min_age > max(r_lims))] <- max(r_lims)
      } else {
        dat$max_age[(dat$max_age > max(r_lims) & dat$min_age < max(r_lims)) |
                      (dat$max_age < max(r_lims) & dat$min_age > max(r_lims))] <- max(r_lims)
        dat$min_age[(dat$max_age > min(r_lims) & dat$min_age < min(r_lims)) |
                      (dat$max_age < min(r_lims) & dat$min_age > min(r_lims))] <- min(r_lims)
      }
      subset(dat, max_age <= max(r_lims) & min_age >= min(r_lims))
    }

    dat_list <- mapply(clean_dat,
                       dat = self$dat,
                       fill = self$fill,
                       neg = self$neg,
                       SIMPLIFY = FALSE)

    prop_sum <- do.call(sum, self$prop)
    if (prop_sum > 1) {
      prop_list <- lapply(self$prop, function(prop) prop/prop_sum)
    } else {
      prop_list <- self$prop
    }
    xmins <- cumsum(c(0, prop_list))

    geo_scale <- ggplot()
    for (ind in seq_along(dat_list)) {
      geo_scale <- geo_scale +
        geom_rect(data = dat_list[[ind]],
                  aes(ymin = min_age, ymax = max_age, fill = color),
                  xmin = xmins[ind], xmax = xmins[ind + 1], alpha = self$alpha[[ind]],
                  show.legend = FALSE, inherit.aes = FALSE)
      # add lines if requested
      if (!is.null(self$lwd[[ind]])) {
        geo_scale <- geo_scale +
          geom_segment(data = dat_list[[ind]],
                       aes(y = min_age, yend = min_age),
                       x = xmins[ind], xend = xmins[ind + 1],
                       color = self$color[[ind]], lwd = self$lwd[[ind]],
                       lty = self$lty[[ind]]) +
          geom_segment(data = dat_list[[ind]],
                       aes(y = max_age, yend = max_age),
                       x = xmins[ind], xend = xmins[ind + 1],
                       color = self$color[[ind]], lwd = self$lwd[[ind]],
                       lty = self$lty[[ind]])
      }
    }

    # add an axis
    axis_line <- calc_element('axis.line.r', theme)
    axis_text <- calc_element('axis.text.r', theme)
    axis_ticks <- calc_element('axis.ticks.r', theme)
    axis_ticks_length <- calc_element('axis.ticks.length.r', theme)
    if (!is(axis_line, "element_blank")) {
      geo_scale <- geo_scale +
        geom_vline(xintercept = 0, color = axis_line$colour %||% NA,
                   size = axis_line$size %||% NA,
                   linetype = axis_line$linetype %||% NA)
    }
    if (!is(axis_text, "element_blank")) {
      geo_scale <- geo_scale +
        annotate(geom = "text", label = panel_params$r.labels,
                 x = 0, y = panel_params$r.major,
                 color = axis_text$colour %||% NA,
                 size = axis_text$size %||% NA,
                 family = axis_text$family %||% NA,
                 fontface = axis_text$face %||% "plain",
                 angle = axis_text$angle %||% 0,
                 lineheight = axis_text$lineheight %||% NA,
                 hjust = -axis_text$hjust %||% NA,
                 vjust = -axis_text$vjust %||% NA)
    }
    if (!is(axis_ticks, "element_blank")) {
      tick_length <- as.numeric(axis_ticks_length %||% unit(0, "points")) / (90 / abs(diff(r_lims)))
      rs <- sapply(panel_params$r.major, function(r) sqrt((r - min(r_lims)) ^ 2 + tick_length ^ 2))
      thetas <- sapply(rs, function(r) asin(tick_length/r))
      geo_scale <- geo_scale +
        annotate(geom = "segment", x = 1 - thetas / (2 * pi), xend = 1,
                 y = min(r_lims) + rs, yend = panel_params$r.major,
                 color = axis_ticks$colour %||% NA,
                 size = axis_ticks$size %||% NA,
                 linetype = axis_ticks$linetype %||% NA,
                 lineend = axis_ticks$lineend %||% NA)
    }
    # should there be an axis label?

    colors <- do.call(c, lapply(dat_list, function(dat) dat$color))
    geo_scale <- geo_scale +
      coord_polar(start = self$start, direction = self$direction) +
      scale_fill_manual(values = setNames(colors, colors)) +
      scale_x_continuous(limits = c(0, 1)) +
      theme_void()

    # do the normal coord_polar background stuff
    parent <- ggproto_parent(CoordPolar, self)
    bg <- parent$render_bg(panel_params, theme)

    # insert the geo_scale into the gTree, then reorder
    bg <- addGrob(bg, ggname("geo_scale", ggplotGrob(geo_scale)))
    reorderGrob(bg, order = c(1, length(grid.ls(bg, print = FALSE)$name) - 1))
  }
)
