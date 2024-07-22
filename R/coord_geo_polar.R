#' Polar coordinate system with geological timescale
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' `coord_geo_polar` behaves similarly to [ggplot2::coord_polar()] in that it
#' occurs after statistical transformation and will affect the visual appearance
#' of geoms. The main difference is that it also adds a geological timescale to
#' the background of the plot.
#'
#' @details
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
#' `coord_geo_polar` manually generates the `r` axis, meaning it does not
#' support changing the guide features of ggplot v. 2.5.0 or later. However, the
#' `deeptime.axis.line.r`, `deeptime.axis.text.r`, `deeptime.axis.ticks.r`, and
#' `deeptime.axis.ticks.length.r` ggplot2 [theme elements][ggplot2::theme] can
#' be modified just like their x and y counterparts to change the appearance of
#' the radius axis. The default settings work well for a horizontal axis
#' pointing towards the right, but these theme settings will need to be modified
#' for other orientations. The default value for `deeptime.axis.line.r` is
#' `element_line()`. The default value for `deeptime.axis.text.r` is
#' `element_text(size = 3.5, vjust = -2, hjust = NA)`. The default value for
#' `deeptime.axis.ticks.r` is `element_line()`. The default value for
#' `deeptime.axis.ticks.length.r` is `unit(1.5, "points")`. However, note that
#' the units for this element are meaningless and only the numeric value will be
#' used (but a `unit` must still be used).
#'
#' Care must be taken when adding labels to plots, as they are very likely to
#' overlap with the plot under the default settings. The `textpath_args`
#' argument can be used to adjust the settings for the plotting of the labels.
#' See [geomtextpath::geom_textpath()] for details about the available
#' arguments. Also note that the curvature of the labels may vary based on the
#' distance from the origin. This is why `abbrv` is set to `TRUE` by default.
#'
#' @section Life cycle: This function is soft-deprecated in favor of
#'   [coord_geo_radial()] as of **deeptime** version 1.1.0. There is currently
#'   no plan to remove this function, but users are strongly encouraged to
#'   migrate to the new function for enhanced polar functionality. Note that
#'   [coord_geo_radial()] requires ggplot2 version 3.5.0 or later.
#'
#' @param dat Either A) a string indicating a built-in dataframe with interval
#'   data from the ICS ("periods", "epochs", "stages", "eons", or "eras"), B) a
#'   string indicating a timescale from macrostrat (see list here:
#'   <https://macrostrat.org/api/defs/timescales?all>), or C) a custom
#'   data.frame of time interval boundaries (see Details).
#' @param fill The fill color of the background. The default is to use the
#'   `color` column included in `dat`. If a custom dataset is provided with
#'   `dat` without a `color` column and without fill, a greyscale will be used.
#'   Custom fill colors can be provided with this option (overriding the `color`
#'   column) and will be recycled if/as necessary.
#' @param alpha The transparency of the fill colors.
#' @param lwd Line width for lines between intervals. Set to `NULL` to remove
#'   lines.
#' @param lty Line type for lines between intervals.
#' @param color The color of the lines between intervals.
#' @param lab Whether to include labels. Requires the `geomtextpath` package.
#' @param abbrv If including labels, whether to use abbreviations instead of
#'   full interval names.
#' @param skip A vector of interval names indicating which intervals should not
#'   be labeled. If `abbrv` is `TRUE`, this can also include interval
#'   abbreviations.
#' @param neg Set this to true if your theta-axis is using negative values. This
#'   is usually true if you are using `ggtree`.
#' @param prop This is the rotational proportion of the background that the
#'   scale takes up.
#' @param textpath_args A list of named arguments to provide to
#'   [geomtextpath::geom_textpath()]. Only used if `lab` is set to `TRUE`.
#'   Useful arguments include `color` (font color), `family` (font family),
#'   `fontface`, `hjust` (radial adjustment), and `size` (font size).
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
#'   coord_geo_polar(
#'     dat = list("stages", "periods"), alpha = .5,
#'     prop = list(0.75, .25), start = pi / 4, lty = "dashed"
#'   ) +
#'   scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
#'   theme(deeptime.axis.text.r = element_text(size = 3.5, hjust = .75,
#'                                             vjust = .75))
#' @examplesIf require(ggtree) && require(paleotree)
#' library(ggplot2)
#' library(paleotree)
#' data(RaiaCopesRule)
#' ggtree(ceratopsianTreeRaia,
#'        position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
#'   coord_geo_polar(dat = "stages")
coord_geo_polar <- function(dat = "periods", theta = "y",
                            start = -pi / 2, direction = -1, clip = "on",
                            fill = NULL, alpha = 1,
                            lwd = .25, color = "grey80", lty = "solid",
                            lab = FALSE, abbrv = TRUE,
                            skip = c("Quaternary", "Holocene",
                                     "Late Pleistocene"),
                            neg = TRUE, prop = 1, textpath_args = list()) {
  lifecycle::deprecate_soft("1.1.0", "coord_geo_polar()", "coord_geo_radial()")
  dat <- make_list(dat)
  n_scales <- length(dat)

  # check global (non-list) arguments
  theta <- arg_match0(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  check_number_decimal(start, allow_infinite = FALSE)
  if (!direction %in% c(-1, 1)) {
    cli::cli_abort(paste0("`direction` must be either -1 or 1, not ",
                          direction, "."))
  }
  clip <- arg_match0(clip, c("off", "on"))

  ggproto(NULL, CoordGeoPolar,
    theta = theta, r = r,
    start = start, direction = sign(direction), clip = clip,
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

rename <- function(x, replace) {
  current_names <- names(x)
  old_names <- names(replace)
  missing_names <- setdiff(old_names, current_names)
  if (length(missing_names) > 0) {
    replace <- replace[!old_names %in% missing_names]
    old_names <- names(replace)
  }
  names(x)[match(old_names, current_names)] <- as.vector(replace)
  x
}

rename_data <- function(coord, data) {
  if (coord$theta == "y") {
    rename(data, c("y" = "theta", "x" = "r"))
  } else {
    rename(data, c("y" = "r", "x" = "theta"))
  }
}

#' @importFrom grid grobName
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

clean_dat <- function(dat, fill, neg, r_lims) {
  # check arguments
  check_bool(neg)
  if (is(dat, "data.frame")) {
    # just use the supplied data
  } else if (is.character(dat)) {
    dat <- get_scale_data(dat)
  } else {
    cli::cli_abort("`dat` must be either a dataframe or a string.")
  }

  if (neg) {
    dat$max_age <- -1 * (dat$max_age)
    dat$min_age <- -1 * (dat$min_age)
  }

  if (!is.null(fill)) {
    dat$color <- rep(fill, length.out = nrow(dat))
  } else if (!("color" %in% colnames(dat))) {
    dat$color <- rep(c("grey60", "grey80"), length.out = nrow(dat))
  }

  if (neg) {
    dat$max_age[
      (dat$max_age < min(r_lims) & dat$min_age < min(r_lims)) |
        (dat$max_age < min(r_lims) & dat$min_age > min(r_lims))
    ] <- min(r_lims)
    dat$min_age[
      (dat$max_age > max(r_lims) & dat$min_age < max(r_lims)) |
        (dat$max_age < max(r_lims) & dat$min_age > max(r_lims))
    ] <- max(r_lims)
  } else {
    dat$max_age[
      (dat$max_age > max(r_lims) & dat$min_age < max(r_lims)) |
        (dat$max_age < max(r_lims) & dat$min_age > max(r_lims))
    ] <- max(r_lims)
    dat$min_age[
      (dat$max_age > min(r_lims) & dat$min_age < min(r_lims)) |
        (dat$max_age < min(r_lims) & dat$min_age > min(r_lims))
    ] <- min(r_lims)
  }
  subset(dat, max_age <= max(r_lims) & min_age >= min(r_lims))
}

#' @rdname coord_geo_polar
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto CoordPolar ggproto_parent coord_polar theme_void
#' @importFrom ggplot2 geom_vline geom_rect geom_segment
#' @importFrom ggplot2 scale_x_continuous scale_fill_manual calc_element
#' @importFrom ggplot2 last_plot set_last_plot
#' @importFrom grid addGrob reorderGrob grid.ls
#' @importFrom rlang %||% exec
CoordGeoPolar <- ggproto("CoordGeoPolar", CoordPolar,
  render_bg = function(self, panel_params, theme) {
    panel_params <- rename_data(self, panel_params)
    # do the new coord_geo_polar background stuff
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
                                               "`coord_geo_polar()`"))
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

    # add an axis
    axis_line <- calc_element("deeptime.axis.line.r", theme)
    axis_text <- calc_element("deeptime.axis.text.r", theme)
    axis_ticks <- calc_element("deeptime.axis.ticks.r", theme)
    axis_ticks_length <- calc_element("deeptime.axis.ticks.length.r", theme)
    if (!is(axis_line, "element_blank")) {
      geo_scale <- geo_scale +
        geom_vline(
          xintercept = 0, color = axis_line$colour %||% NA,
          linewidth = axis_line$linewidth %||% NA,
          linetype = axis_line$linetype %||% NA
        )
    }
    if (!is(axis_text, "element_blank")) {
      geo_scale <- geo_scale +
        annotate(
          geom = "text", label = panel_params$r.labels,
          x = 0, y = panel_params$r.major,
          color = axis_text$colour %||% NA,
          size = axis_text$size %||% NA,
          family = axis_text$family %||% NA,
          fontface = axis_text$face %||% "plain",
          angle = axis_text$angle %||% 0,
          lineheight = axis_text$lineheight %||% NA,
          hjust = -axis_text$hjust %||% NA,
          vjust = -axis_text$vjust %||% NA
        )
    }
    if (!is(axis_ticks, "element_blank")) {
      tick_length <- as.numeric(axis_ticks_length %||%
                                  unit(0, "points")) / (90 / abs(diff(r_lims)))
      rs <- sapply(panel_params$r.major,
                   function(r) sqrt((r - min(r_lims))^2 + tick_length^2))
      thetas <- sapply(rs, function(r) asin(tick_length / r))
      geo_scale <- geo_scale +
        annotate(
          geom = "segment", x = 1 - thetas / (2 * pi), xend = 1,
          y = min(r_lims) + rs, yend = panel_params$r.major,
          color = axis_ticks$colour %||% NA,
          linewidth = axis_ticks$linewidth %||% NA,
          linetype = axis_ticks$linetype %||% NA,
          lineend = axis_ticks$lineend %||% NA
        )
    }
    # should there be an axis label?

    colors <- do.call(c, lapply(dat_list, function(dat) dat$color))

    geo_scale <- geo_scale +
      coord_polar(start = self$start, direction = self$direction,
                  clip = self$clip) +
      scale_fill_manual(values = setNames(colors, colors)) +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = r_lims) +
      theme_void()

    # do the normal coord_polar background stuff
    parent <- ggproto_parent(CoordPolar, self)
    bg <- parent$render_bg(panel_params, theme)

    # if the axis ends at zero, the tick mark is clipped, but that warning would
    # probably be confusing to users
    suppressWarnings({
      geo_scale_grob <- ggplotGrob(geo_scale)
    })
    # insert the geo_scale into the gTree, then reorder
    bg <- addGrob(bg, ggname("geo_scale", geo_scale_grob))
    reorderGrob(bg, order = c(1, length(grid.ls(bg, print = FALSE)$name) - 1))
  }
)
