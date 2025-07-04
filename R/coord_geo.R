## declare variables that are used within aes() to prevent
## R CMD check from complaining
utils::globalVariables(c("min_age", "max_age", "mid_age", "label",
                         "name", "translate", "stages", "periods"))

coord_trans_deeptime <- function(...) {
  if (packageVersion("ggplot2") > "3.5.2") {
    asNamespace("ggplot2")$coord_transform(...)
  } else {
    asNamespace("ggplot2")$coord_trans(...)
  }
}

#' Transformed coordinate system with geological timescale
#'
#' `coord_geo` behaves similarly to [ggplot2::coord_trans()] in that it occurs
#' after statistical transformation and will affect the visual appearance of
#' geoms. The main difference is that it also adds a geological timescale to the
#' specified side(s) of the plot.
#'
#' Transforming the side with the scale is not currently implemented.
#' If a custom data.frame is provided (with `dat`), it should consist of at
#' least 3 columns of data. See `data(periods)` for an example.
#' \itemize{
#'   \item The `name` column lists the names of each time interval. These will
#'     be used as labels if no abbreviations are provided.
#'   \item The `max_age` column lists the oldest boundary of each time interval.
#'   \item The `min_age` column lists the youngest boundary of each time
#'     interval.
#'   \item The `abbr` column is optional and lists abbreviations that may be
#'     used as labels.
#'   \item The `color` column is also optional and lists a
#'     [color][ggplot2::color] for the background for each time interval.
#'   \item The `lab_color` column is also optional and lists a
#'     [color][ggplot2::color] for the label for each time interval.
#' }
#'
#' If the axis of the time scale is discrete, `max_age` and `min_age` will
#' automatically be converted to the discrete scale. In this case, the
#' categories of the discrete axis should match the values in the `name` column.
#' If the ages within `dat` are already discretized, you can set
#' `dat_is_discrete` to `TRUE` to prevent this automatic conversion. This can be
#' useful for adding a time scale where categories and time intervals are not
#' 1:1.
#'
#' `pos` may also be a `list` of sides (including duplicates) if multiple time
#' scales should be added to the plot. In this case, `dat`, `fill`, `alpha`,
#' `height`, `bord`, `lwd`, `color`, `lab`, `lab_color`, `rot`, `family`,
#' `fontface`, `size`, `skip`, `abbrv`, `neg`, `center_end_labels`, and
#' `dat_is_discrete` can also be `list`s. If these `list`s are not as long as
#' `pos`, the elements will be recycled.
#' If individual values (or vectors) are used for these parameters, they will be
#' applied to all time scales (and recycled as necessary).
#' @param pos Which side to add the scale to (left, right, top, or bottom).
#'   First letter may also be used.
#' @param dat Either A) a string indicating a built-in dataframe with interval
#'   data from the ICS ("periods", "epochs", "stages", "eons", or "eras"),
#'   B) a string indicating a timescale from macrostrat (see list here:
#'   <https://macrostrat.org/api/defs/timescales?all>), or C) a custom
#'   data.frame of time interval boundaries (see Details).
#' @param xtrans,ytrans Transformers for the x and y axes. For more information
#'   see [ggplot2::coord_trans()].
#' @param expand If `FALSE`, the default, limits are taken exactly from the data
#'   or `xlim`/`ylim`. If `TRUE`, adds a small expansion factor to the limits to
#'   ensure that data and axes don't overlap.
#' @param fill The fill color of the boxes. The default is to use the `color`
#'   column included in `dat`. If a custom dataset is provided with `dat`
#'   without a `color` column and without fill, a greyscale will be used.
#'   Custom fill colors can be provided with this option (overriding the `color`
#'   column) and will be recycled if/as necessary.
#' @param color The outline color of the interval boxes.
#' @param alpha The transparency of the fill colors.
#' @param height The height (or width if `pos` is `left` or `right`) of the
#'   scale.
#' @param lab Whether to include labels.
#' @param lab_color The color of the labels. The default is to use the
#'   `lab_color` column included in `dat`. If a custom dataset is provided with
#'   `dat` without a `lab_color` column and without fill, all labels will be
#'   black. Custom label colors can be provided with this option (overriding the
#'   `lab_color` column) and will be recycled if/as necessary.
#' @param rot The amount of counter-clockwise rotation to add to the labels
#'   (in degrees).
#' @param abbrv If including labels, should the labels be abbreviated? If
#'   `TRUE`, the `abbr` column will be used for the labels. If `FALSE`, the
#'   `name` column will be used for the labels. If `"auto"`, the [abbreviate()]
#'   function will be used to abbreviate the values in the `name` column. Note
#'   that the built-in data and data retrieved via [get_scale_data()] already
#'   have built-in abbreviations. However, using the `"auto"` option here will
#'   create new unique abbreviations based on only the intervals that are being
#'   plotted. In many cases, this may result in abbreviations that are shorter
#'   in length because there are fewer similar interval names to abbreviate.
#' @param skip A vector of interval names indicating which intervals should not
#'   be labeled. If `abbrv` is `TRUE`, this can also include interval
#'   abbreviations.
#' @param size Label size. Either a number as you would specify in
#'   [ggplot2::geom_text()] or `"auto"` to use [ggfittext::geom_fit_text()].
#' @param family The font family to use for the labels. There are only three
#'   fonts that are guaranteed to work everywhere: “sans” (the default),
#'   “serif”, or “mono”.
#' @param fontface The font face to use for the labels. The standard options are
#'   "plain" (default), "bold", "italic", and "bold.italic".
#' @param lwd Line width.
#' @param neg Set this to `TRUE` if your x-axis is using negative values.
#' @param bord A vector specifying on which sides of the scale to add borders
#'   (same options as `pos`).
#' @param center_end_labels Should labels be centered within the visible range
#'   of intervals at the ends of the axis?
#' @param dat_is_discrete Are the ages in `dat` already converted for a discrete
#'   scale?
#' @param fittext_args A list of named arguments to provide to
#'   [ggfittext::geom_fit_text()]. Only used if `size` is set to `"auto"`.
#' @inheritParams ggplot2::coord_trans
#' @importFrom ggplot2 ggproto
#' @export
#' @examples
#' library(ggplot2)
#' # single scale on bottom
#' ggplot() +
#'   geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 1000))) +
#'   scale_x_reverse() +
#'   coord_geo(xlim = c(1000, 0), ylim = c(0, 8)) +
#'   theme_classic()
#'
#' # stack multiple scales
#' ggplot() +
#'   geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 100))) +
#'   scale_x_reverse() +
#'   coord_geo(
#'     xlim = c(100, 0), ylim = c(0, 8), pos = as.list(rep("bottom", 3)),
#'     dat = list("stages", "epochs", "periods"),
#'     height = list(unit(4, "lines"), unit(4, "lines"), unit(2, "line")),
#'     rot = list(90, 90, 0), size = list(2.5, 2.5, 5), abbrv = FALSE
#'   ) +
#'   theme_classic()
coord_geo <- function(pos = "bottom", dat = "periods", xlim = NULL, ylim = NULL,
                      xtrans = identity_trans(), ytrans = identity_trans(),
                      clip = "on", expand = FALSE,
                      fill = NULL, alpha = 1, height = unit(2, "line"),
                      bord = c("left", "right", "top", "bottom"),
                      lwd = .25, color = "black",
                      lab = TRUE, lab_color = NULL, rot = 0,
                      family = "sans", fontface = "plain", size = 5,
                      skip = c("Quaternary", "Holocene", "Late Pleistocene"),
                      abbrv = TRUE, neg = FALSE,
                      center_end_labels = FALSE, dat_is_discrete = FALSE,
                      fittext_args = list()) {
  # resolve transformers
  if (is.character(xtrans)) xtrans <- as.trans(xtrans)
  if (!is.trans(xtrans)) {
    cli::cli_abort("`xtrans` must be a transformer function or a string.")
  }
  if (is.character(ytrans)) ytrans <- as.trans(ytrans)
  if (!is.trans(ytrans)) {
    cli::cli_abort("`ytrans` must be a transformer function or a string.")
  }

  # check global (non-list) arguments
  clip <- arg_match0(clip, c("off", "on"))
  check_bool(expand)

  if (any(!pos %in% c("bottom", "top", "left", "right", "b", "t", "l", "r"))) {
    cli::cli_abort('`pos` must be a one of "left", "right", "top", or
                   "bottom" (first letter may also be used).')
  }
  pos <- as.list(pos)
  n_scales <- length(pos)

  ggproto(NULL, CoordGeo,
    trans = list(x = xtrans, y = ytrans),
    limits = list(x = xlim, y = ylim),
    expand = expand, clip = clip,
    pos = pos, dat = rep(make_list(dat), length.out = n_scales),
    fill = rep(make_list(fill), length.out = n_scales),
    color = rep(make_list(color), length.out = n_scales),
    alpha = rep(make_list(alpha), length.out = n_scales),
    height = rep(make_list(height), length.out = n_scales),
    lab = rep(make_list(lab), length.out = n_scales),
    lab_color = rep(make_list(lab_color), length.out = n_scales),
    rot = rep(make_list(rot), length.out = n_scales),
    abbrv = rep(make_list(abbrv), length.out = n_scales),
    family = rep(make_list(family), length.out = n_scales),
    fontface = rep(make_list(fontface), length.out = n_scales),
    skip = rep(make_list(skip), length.out = n_scales),
    size = rep(make_list(size), length.out = n_scales),
    lwd = rep(make_list(lwd), length.out = n_scales),
    neg = rep(make_list(neg), length.out = n_scales),
    bord = rep(make_list(bord), length.out = n_scales),
    center_end_labels = rep(make_list(center_end_labels),
                            length.out = n_scales),
    dat_is_discrete = rep(make_list(dat_is_discrete), length.out = n_scales),
    fittext_args = fittext_args
  )
}

#' @rdname coord_geo
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto CoordTrans ggproto_parent
#' @importFrom rlang %||%
CoordGeo <- ggproto("CoordGeo", CoordTrans,
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    parent <- ggproto_parent(CoordTrans, self)
    panel_params <- parent$setup_panel_params(scale_x, scale_y, params)
    panel_params$scale_x <- scale_x
    panel_params$scale_y <- scale_y
    panel_params
  },
  render_axis_h = function(self, panel_params, theme) {
    arrange <- panel_params$x.arrange %||% c("secondary", "primary")
    axes <- CoordTrans$render_axis_h(panel_params, theme)
    if (any(self$pos %in% c("top", "t"))) {
      axes$top <- render_geo_scale(self, panel_params, theme, "top")
    }
    if (any(self$pos %in% c("bottom", "b"))) {
      axes$bottom <- render_geo_scale(self, panel_params, theme, "bottom")
    }

    axes
  },
  render_axis_v = function(self, panel_params, theme) {
    arrange <- panel_params$y.arrange %||% c("primary", "secondary")
    axes <- CoordTrans$render_axis_v(panel_params, theme)
    if (any(self$pos %in% c("left", "l"))) {
      axes$left <- render_geo_scale(self, panel_params, theme, "left")
    }
    if (any(self$pos %in% c("right", "r"))) {
      axes$right <- render_geo_scale(self, panel_params, theme, "right")
    }

    axes
  }
)

#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid unit unit.c viewport grobWidth grobHeight gList gTree
#' @importFrom grid is.unit
#' @importFrom gtable gtable_col gtable_row gtable_height gtable_width
render_geo_scale <- function(self, panel_params, theme, position) {
  one <- unit(1, "npc")
  ind <- self$pos %in% c(position, substring(position, 1, 1))

  geo_scales <- mapply(make_geo_scale,
    dat = self$dat[ind],
    fill = self$fill[ind],
    color = self$color[ind],
    alpha = self$alpha[ind],
    lab = self$lab[ind],
    lab_color = self$lab_color[ind],
    rot = self$rot[ind],
    abbrv = self$abbrv[ind],
    family = self$family[ind],
    fontface = self$fontface[ind],
    skip = self$skip[ind],
    size = self$size[ind],
    lwd = self$lwd[ind],
    neg = self$neg[ind],
    bord = self$bord[ind],
    center_end_labels = self$center_end_labels[ind],
    dat_is_discrete = self$dat_is_discrete[ind],
    MoreArgs = list(
      self = self, pos = position, panel_params = panel_params,
      theme = theme, fittext_args = self$fittext_args
    ),
    SIMPLIFY = FALSE
  )
  geo_grobs <- lapply(geo_scales, ggplotGrob)
  if (!all(sapply(self$height[ind], is.unit))) {
    cli::cli_abort("`height` must be a unit object such as that produced by
                   `grid::unit()`.")
  }

  if (position == "bottom") {
    axis <- CoordTrans$render_axis_h(panel_params, theme)$bottom
    gt <- gtable_col("axis",
      grobs = c(geo_grobs, list(axis)),
      width = one, heights = unit.c(do.call(unit.c, self$height[ind]),
                                    grobHeight(axis))
    )
    justvp <- viewport(y = 1, just = "top", height = gtable_height(gt))
  } else if (position == "top") {
    axis <- CoordTrans$render_axis_h(panel_params, theme)$top
    gt <- gtable_col("axis",
      grobs = c(list(axis), geo_grobs),
      width = one, heights = unit.c(grobHeight(axis),
                                    do.call(unit.c, self$height[ind]))
    )
    justvp <- viewport(y = 0, just = "bottom", height = gtable_height(gt))
  } else if (position == "left") {
    axis <- CoordTrans$render_axis_v(panel_params, theme)$left
    gt <- gtable_row("axis",
      grobs = c(list(axis), geo_grobs),
      height = one, widths = unit.c(grobWidth(axis),
                                    do.call(unit.c, self$height[ind]))
    )
    justvp <- viewport(x = 1, just = "right", width = gtable_width(gt))
  } else if (position == "right") {
    axis <- CoordTrans$render_axis_v(panel_params, theme)$right
    gt <- gtable_row("axis",
      grobs = c(geo_grobs, list(axis)),
      height = one, widths = unit.c(do.call(unit.c, self$height[ind]),
                                    grobWidth(axis))
    )
    justvp <- viewport(x = 0, just = "left", width = gtable_width(gt))
  }

  gTree(
    children = gList(gt),
    width = gtable_width(gt), height = gtable_height(gt),
    vp = justvp, cl = "absoluteGrob"
  )
}

#' @importFrom ggplot2 ggplot geom_rect geom_segment geom_text annotate aes
#' @importFrom ggplot2 scale_fill_identity scale_color_identity theme_void
#' @importFrom ggplot2 scale_x_reverse
#' @importFrom ggplot2 last_plot set_last_plot
#' @importFrom ggfittext geom_fit_text
#' @importFrom rlang exec
#' @importFrom utils packageVersion
make_geo_scale <- function(self, dat, fill, color, alpha, pos,
                           lab, lab_color, rot, abbrv, family, fontface, skip,
                           size, lwd, neg, bord,
                           center_end_labels, dat_is_discrete,
                           panel_params, theme, fittext_args) {
  # check timescale-specific arguments
  if (is(dat, "data.frame")) {
    # just use the supplied data
  } else if (is.character(dat)) {
    dat <- get_scale_data(dat)
  } else {
    cli::cli_abort("`dat` must be either a dataframe or a string.")
  }
  check_number_decimal(alpha, min = 0, max = 1)
  check_bool(lab)
  check_number_decimal(rot)
  if (!is.logical(abbrv) && abbrv != "auto") {
    cli::cli_abort('`abbrv` must be either a boolean or "auto".')
  }
  check_string(family)
  check_string(fontface)
  check_character(skip, allow_null = TRUE)
  if (!is.numeric(size) && size != "auto") {
    cli::cli_abort('`size` must be either a number or "auto".')
  }
  check_number_decimal(lwd, min = 0)
  check_bool(neg)
  check_character(bord, allow_null = TRUE)
  check_bool(center_end_labels)
  check_bool(dat_is_discrete)
  if (!is.list(fittext_args)) {
    cli::cli_abort("`fittext_args` must be a `list` of arguments.")
  }

  # if the axis is discrete, adjust the data accordingly
  if (pos %in% c("bottom", "top")) {
    discrete <- panel_params$scale_x$is_discrete()
    limits <- panel_params$scale_x$limits
  } else {
    discrete <- panel_params$scale_y$is_discrete()
    limits <- panel_params$scale_y$limits
  }

  if (discrete && !dat_is_discrete) {
    dat <- subset(dat, name %in% limits)
    dat$min_age <- -0.5 + match(limits, dat$name)
    dat$max_age <- 0.5 + match(limits, dat$name)
  }

  if (neg) {
    dat$max_age <- -1 * (dat$max_age)
    dat$min_age <- -1 * (dat$min_age)
  }
  dat$mid_age <- (dat$max_age + dat$min_age) / 2
  if (!is.null(fill)) {
    dat$color <- rep(fill, length.out = nrow(dat))
  } else if (!("color" %in% colnames(dat))) {
    dat$color <- rep(c("grey60", "grey80"), length.out = nrow(dat))
  }

  # do this so ggsave gets the whole plot
  old_plot <- last_plot()
  on.exit(set_last_plot(old_plot))

  # make ggplot of scale
  gg_scale <- ggplot() +
    geom_rect(
      data = dat, aes(xmin = min_age, xmax = max_age, fill = color),
      ymin = 0, ymax = 1, color = NA, alpha = alpha,
      show.legend = FALSE, inherit.aes = FALSE
    ) +
    scale_fill_identity()
  gg_scale <- gg_scale +
    geom_segment(
      data = dat, aes(x = min_age, xend = min_age), y = 0, yend = 1,
      color = color, linewidth = lwd
    ) +
    geom_segment(
      data = dat, aes(x = max_age, xend = max_age), y = 0, yend = 1,
      color = color, linewidth = lwd
    ) +
    theme_void()

  rev_axis <- FALSE
  # if left or right, rotate accordingly using coord_trans_flip,
  # otherwise, just use coord_trans
  if (pos %in% c("bottom", "top", "b", "t")) {
    if (discrete) {
      lims <- panel_params$x.range
    } else {
      rev_axis <- panel_params$scale_x$trans$name == "reverse"
      lims <- panel_params$x.range * c(1, -1)[rev_axis + 1]
    }
    gg_scale <- gg_scale +
      coord_trans_deeptime(x = self$trans$x, xlim = lims, ylim = c(0, 1),
                           expand = FALSE, clip = self$clip)
  } else if (pos %in% c("left", "right", "l", "r")) {
    if (discrete) {
      lims <- panel_params$y.range
    } else {
      rev_axis <- panel_params$scale_y$trans$name == "reverse"
      lims <- panel_params$y.range * c(1, -1)[rev_axis + 1]
    }
    gg_scale <- gg_scale +
      coord_trans_flip(x = self$trans$y, xlim = lims, ylim = c(0, 1),
                       expand = FALSE, clip = self$clip)
  }

  # Filter data to only those that are within the plot limits
  if (neg) {
    dat <- subset(dat, min_age < max(lims) & min_age > min(lims) |
                       max_age < max(lims) & max_age > min(lims))
  } else {
    dat <- subset(dat, min_age > min(lims) & min_age < max(lims) |
                       max_age > min(lims) & max_age < max(lims))
  }

  # Add labels
  if (lab) {
    if (!is.null(lab_color)) {
      dat$lab_color <- rep(lab_color, length.out = nrow(dat))
    } else if (!("lab_color" %in% colnames(dat))) {
      dat$lab_color <- "black"
    }

    if (center_end_labels) {
      # center the labels for the time periods at the ends of the axis
      # find which intervals overlap with the ends of the axis
      max_end_pos <- (dat$max_age > max(lims) & dat$min_age < max(lims))
      max_end_neg <- (dat$max_age < max(lims) & dat$min_age > max(lims))
      min_end_pos <- (dat$max_age > min(lims) & dat$min_age < min(lims))
      min_end_neg <- (dat$max_age < min(lims) & dat$min_age > min(lims))
      # replace the max/min ages with the scale limits
      dat$max_age[max_end_pos] <- max(lims)
      dat$min_age[max_end_neg] <- max(lims)
      dat$min_age[min_end_pos] <- min(lims)
      dat$max_age[min_end_neg] <- min(lims)
      # recalculate the mid ages
      dat$mid_age <- (dat$max_age + dat$min_age) / 2
    }
    if (abbrv == "auto") {
      dat$label <- abbreviate(dat$name, minlength = 1,
                              use.classes = TRUE, named = FALSE)
    } else if (abbrv && "abbr" %in% colnames(dat)) {
      dat$label <- dat$abbr
      dat$label[dat$abbr %in% skip] <- ""
    } else {
      dat$label <- dat$name
    }

    dat$label[dat$name %in% skip] <- ""

    if (size == "auto") {
      gg_scale <- gg_scale +
        exec(geom_fit_text,
          data = dat, aes(
            x = mid_age, label = label,
            ymin = 0, ymax = 1,
            xmin = min_age, xmax = max_age,
            color = lab_color
          ), angle = rot, family = family, fontface = fontface,
          show.legend = FALSE, inherit.aes = FALSE, !!!fittext_args
        )
    } else {
      gg_scale <- gg_scale +
        geom_text(
          data = dat, aes(x = mid_age, label = label, color = lab_color),
          y = .5,
          vjust = "middle", hjust = "middle", size = size, angle = rot,
          family = family, fontface = fontface,
          show.legend = FALSE, inherit.aes = FALSE
        )
    }
    gg_scale <- gg_scale +
      scale_color_identity()
  }

  # Add border
  if (discrete) {
    bord_lims <- c(min(dat[, c("min_age", "max_age")]),
                   max(dat[, c("min_age", "max_age")]))
  } else {
    bord_lims <- lims
    bord_lims[(if (neg) bord_lims > 0 else bord_lims < 0)] <- 0
  }

  if ("left" %in% bord || "l" %in% bord) {
    gg_scale <- gg_scale +
      annotate("segment",
        x = bord_lims[1], xend = bord_lims[1], y = 0, yend = 1,
        color = color,
        linewidth = if (bord_lims[1] == lims[1]) lwd * 2 else lwd
      )
  }
  if ("right" %in% bord || "r" %in% bord) {
    gg_scale <- gg_scale +
      annotate("segment",
        x = bord_lims[2], xend = bord_lims[2], y = 0, yend = 1,
        color = color,
        linewidth = if (bord_lims[2] == lims[2]) lwd * 2 else lwd
      )
  }
  if ("top" %in% bord || "t" %in% bord) {
    gg_scale <- gg_scale +
      annotate("segment",
        x = bord_lims[1], xend = bord_lims[2], y = 1, yend = 1,
        color = color, linewidth = lwd * 2
      )
  }
  if ("bottom" %in% bord || "b" %in% bord) {
    gg_scale <- gg_scale +
      annotate("segment",
        x = bord_lims[1], xend = bord_lims[2], y = 0, yend = 0,
        color = color, linewidth = lwd * 2
      )
  }

  # reverse axis if necessary
  if (rev_axis) {
    gg_scale <- gg_scale + scale_x_reverse()
  }

  gg_scale
}

make_list <- function(x) {
  if (is.list(x) && !is(x, "data.frame")) x else list(x)
}
