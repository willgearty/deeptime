#' Enhanced polar coordinate system with geological timescale
#'
#' `coord_geo_radial` behaves similarly to [ggplot2::coord_radial()] in that it
#' occurs after statistical transformation and will affect the visual appearance
#' of geoms. The main difference is that it also adds a geological timescale to
#' the background of the plot. `coord_geo_radial` is similar to
#' [coord_geo_polar()] but has more options related to the polar coordinate
#' plotting that are inherited from [ggplot2::coord_radial()] (e.g., `end`,
#' `r_axis_inside`, `inner.radius`). Furthermore, unlike `coord_geo_polar`,
#' `coord_geo_radial` uses the ggplot2 internals to draw the `r` and `theta`
#' axes, gridlines, etc. This means that users can tweak the
#' [guide][ggplot2::guides] and [theme][ggplot2::theme] settings for these
#' features (see examples).
#'
#' If a custom data.frame is provided (with `dat`), it should consist of at
#' least 2 columns of data. See `data(periods)` for an example.
#' \itemize{
#'   \item The `max_age` column lists the oldest boundary of each time interval.
#'   \item The `min_age` column lists the youngest boundary of each time
#'      interval.
#'   \item The `abbr` column is optional and lists abbreviations that may be
#'     used as labels.
#'   \item The `color` column is optional and lists a [color][ggplot2::color]
#'      for the background for each time interval.
#' }
#'
#' `dat` may also be a list of values and/or dataframes if multiple time scales
#' should be added to the background. Scales will be added sequentially starting
#' at `start` and going in the specified `direction`. By default the scales will
#' all be equal in circular/rotational proportion, but this can be overridden
#' with `prop`. If `dat` is a list, `fill`, `alpha`, `lwd`, `color`, `lty`,
#' `lab`, `abbrv`, `skip`, `neg`, `prop`, and `textpath_args` can also be lists
#' (N.B. `textpath_args` would be a list of lists). If these lists are not as
#' long as `dat`, the elements will be recycled. If individual values (or
#' vectors) are used for these parameters, they will be applied to all time
#' scales (and recycled as necessary).
#'
#' If the sum of the `prop` values is greater than 1, the proportions will be
#' scaled such that they sum to 1. However, the `prop` values may sum to less
#' than 1 if the user would like blank space in the background.
#'
#' Care must be taken when adding labels to plots, as they are very likely to
#' overlap with the plot under the default settings. The `textpath_args`
#' argument can be used to adjust the settings for the plotting of the labels.
#' See [geomtextpath::geom_textpath()] for details about the available
#' arguments. Also note that the curvature of the labels may vary based on the
#' distance from the origin. This is why `abbrv` is set to `TRUE` by default.
#'
#' @inheritParams ggplot2::coord_radial
#' @inheritParams coord_geo_polar
#' @importFrom ggplot2 ggproto
#' @importFrom rlang arg_match0 %||%
#' @export
#' @examples
#' library(ggplot2)
#' @examplesIf require(ggtree)
#' library(ggtree)
#' set.seed(1)
#' tree <- rtree(100)
#' # single scale
#' revts(ggtree(tree)) +
#'   coord_geo_radial(dat = "stages") +
#'   scale_y_continuous(guide = "none", breaks = NULL) +
#'   theme_gray()
#'
#' # multiple scales
#' revts(ggtree(tree)) +
#'   coord_geo_radial(
#'     dat = list("stages", "periods"), alpha = .5,
#'     prop = list(0.75, .25), start = pi / 4, end = 2 * pi, lty = "dashed"
#'   ) +
#'   scale_y_continuous(expand = expansion(mult = c(0.02, 0.02)),
#'                      guide = "none", breaks = NULL) +
#'   theme_gray()
#' @examplesIf require(ggtree) && require(paleotree)
#' library(ggplot2)
#' library(paleotree)
#' data(RaiaCopesRule)
#' ggtree(ceratopsianTreeRaia,
#'        position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
#'   coord_geo_radial(dat = "stages") +
#'   scale_y_continuous(guide = "none", breaks = NULL) +
#'   theme_classic()
coord_geo_radial <- function(dat = "periods",
                             theta = "y", start = -0.5 * pi, end = 1.25 * pi,
                             expand = TRUE, direction = 1, reverse = "none",
                             r_axis_inside = NULL, inner.radius = 0.05,
                             fill = NULL, alpha = 1,
                             lwd = .25, color = "grey80", lty = "solid",
                             lab = FALSE, abbrv = TRUE,
                             skip = c("Quaternary", "Holocene",
                                      "Late Pleistocene"),
                             neg = TRUE, prop = 1, textpath_args = list(),
                             clip = "off", rotate_angle = FALSE) {
  dat <- make_list(dat)
  n_scales <- length(dat)

  # check global (non-list) arguments
  theta <- arg_match0(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  check_number_decimal(start, allow_infinite = FALSE)
  check_number_decimal(end, allow_infinite = FALSE, allow_null = TRUE)
  check_bool(expand)
  if (!direction %in% c(-1, 1)) {
    cli::cli_abort(paste0("`direction` must be either -1 or 1, not ",
                          direction, "."))
  }
  check_bool(r_axis_inside, allow_null = TRUE)
  check_number_decimal(inner.radius, min = 0, max = 1, allow_infinite = FALSE)
  clip <- arg_match0(clip, c("off", "on"))
  check_bool(rotate_angle)

  end <- end %||% (start + 2 * pi)
  if (start > end) {
    n_rotate <- ((start - end) %/% (2 * pi)) + 1
    start <- start - n_rotate * 2 * pi
  }
  r_axis_inside <- r_axis_inside %||% !(abs(end - start) >= 1.999 * pi)

  ggproto(NULL, CoordGeoRadial,
          theta = theta,
          r = r,
          start = start,
          end = end,
          arc = c(start, end),
          expand = expand,
          direction = sign(direction),
          reverse = reverse,
          r_axis_inside = r_axis_inside,
          rotate_angle = rotate_angle,
          inner.radius = inner.radius,
          inner_radius = c(inner.radius, 1) * 0.4,
          clip = clip,
          dat = dat,
          fill = rep(make_list(fill), length.out = n_scales),
          alpha = rep(make_list(alpha), length.out = n_scales),
          lwd = rep(make_list(lwd), length.out = n_scales),
          lty = rep(make_list(lty), length.out = n_scales),
          color = rep(make_list(color), length.out = n_scales),
          lab = rep(make_list(lab), length.out = n_scales),
          skip = rep(make_list(skip), length.out = n_scales),
          abbrv = rep(make_list(abbrv), length.out = n_scales),
          neg = rep(make_list(neg), length.out = n_scales),
          prop = rep(make_list(prop), length.out = n_scales),
          textpath_args = rep(list(textpath_args), length.out = n_scales)
  )
}

#' @rdname coord_geo_radial
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto ggproto_parent coord_radial theme_void
#' @importFrom ggplot2 geom_vline geom_rect geom_segment CoordRadial
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_fill_manual
#' @importFrom ggplot2 last_plot set_last_plot calc_element
#' @importFrom grid addGrob reorderGrob grid.ls
#' @importFrom rlang %||% exec
#' @importFrom utils packageVersion
CoordGeoRadial <- ggproto("CoordGeoRadial", CoordRadial,
  render_bg = function(self, panel_params, theme) {
    panel_params <- rename_data(self, panel_params)
    # do the new coord_geo_radial background stuff
    r_lims <- panel_params$r.range

    # convert, subset, and adjust data
    dat_list <- mapply(clean_dat,
                       dat = self$dat,
                       fill = self$fill,
                       neg = self$neg,
                       MoreArgs = list(r_lims = r_lims),
                       SIMPLIFY = FALSE
    )

    prop_sum <- do.call(sum, self$prop)
    if (prop_sum > 1) {
      prop_list <- lapply(self$prop, function(prop) prop / prop_sum)
    } else {
      prop_list <- self$prop
    }
    xmins <- cumsum(c(0, prop_list))

    # do this so ggsave gets the whole plot
    old_plot <- last_plot()
    on.exit(set_last_plot(old_plot))

    # assemble the timescale background as a ggplot
    geo_scale <- ggplot()
    for (ind in seq_along(dat_list)) {
      # check timescale-specific arguments
      check_number_decimal(self$alpha[[ind]], min = 0, max = 1, arg = "alpha")
      check_number_decimal(self$lwd[[ind]], arg = "lwd")
      check_bool(self$lab[[ind]], arg = "lab")
      check_bool(self$abbrv[[ind]], arg = "abbrv")
      check_character(self$skip[[ind]], arg = "skip", allow_null = TRUE)
      check_number_decimal(self$prop[[ind]], min = 0, max = 1, arg = "prop")
      if (!is.list(self$textpath_args[[ind]])) {
        cli::cli_abort("`textpath_args` must be a `list` of arguments.")
      }
      dat_ind <- dat_list[[ind]]
      geo_scale <- geo_scale +
        geom_rect(
          data = dat_ind,
          aes(ymin = min_age, ymax = max_age, fill = color),
          xmin = xmins[ind], xmax = xmins[ind + 1], alpha = self$alpha[[ind]],
          show.legend = FALSE, inherit.aes = FALSE
        )
      # add lines if requested
      if (!is.null(self$lwd[[ind]])) {
        geo_scale <- geo_scale +
          geom_segment(
            data = dat_ind,
            aes(y = min_age, yend = min_age),
            x = xmins[ind], xend = xmins[ind + 1],
            color = self$color[[ind]], linewidth = self$lwd[[ind]],
            lty = self$lty[[ind]]
          ) +
          geom_segment(
            data = dat_ind,
            aes(y = max_age, yend = max_age),
            x = xmins[ind], xend = xmins[ind + 1],
            color = self$color[[ind]], linewidth = self$lwd[[ind]],
            lty = self$lty[[ind]]
          )
      }
      # add labels if requested
      if (self$lab[[ind]]) { # nocov start
        rlang::check_installed("geomtextpath",
                               reason = paste0("to add labels with ",
                                               "`coord_geo_radial()`"))
        if (self$abbrv[[ind]] && "abbr" %in% colnames(dat_ind)) {
          dat_ind$name <- dat_ind$abbr
        }
        dat_temp <- dat_ind[rep(seq_len(nrow(dat_ind)), each = 2), ]
        geo_scale <- geo_scale +
          exec(geomtextpath::geom_textpath, data = dat_temp,
               aes(y = (min_age + max_age) / 2, label = name),
               x = rep(c(xmins[ind], xmins[ind + 1]), nrow(dat_ind)),
               text_only = TRUE, !!!self$textpath_args[[ind]])
      } # nocov end
    }

    colors <- do.call(c, lapply(dat_list, function(dat) dat$color))
    radial_args <- list(
      start = self$start, end = self$end,
      expand = FALSE, clip = self$clip, inner.radius = self$inner.radius
    )
    if (packageVersion("ggplot2") > "3.5.2") {
      radial_args$reverse <- ifelse(self$reverse == "none" &&
                                      self$direction == -1,
                                    "theta", self$reverse)
    } else {
      radial_args$direction <- self$direction
    }
    geo_scale <- geo_scale + do.call(coord_radial, radial_args)

    geo_scale <- geo_scale +
      scale_fill_manual(values = setNames(colors, colors)) +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = r_lims) +
      theme_void()

    # do the normal coord_radial background stuff
    parent <- ggproto_parent(CoordRadial, self)
    bg <- parent$render_bg(panel_params, theme)

    geo_scale_grob <- ggplotGrob(geo_scale)
    # insert the geo_scale into the gTree, then reorder
    bg <- addGrob(bg, ggname("geo_scale", geo_scale_grob))
    reorderGrob(bg, order = c(1, length(grid.ls(bg, print = FALSE)$name) - 1))
  }
)
