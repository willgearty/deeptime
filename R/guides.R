#' Geological timescale axis guide
#'
#' `guide_geo` behaves similarly to [ggplot2::guide_axis()] in that it modifies
#' the visual appearance of the axis. The main difference is that it adds a
#' geological timescale instead of an axis.
#'
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
#' Since this guide only plots the timescale and not ticks or an axis line, a
#' call to this function should almost always be combined with a call to
#' [ggplot2::guide_axis()] within a call to [ggplot2::guide_axis_stack()] (see
#' Examples). Note that in most cases this has the same end result as a single
#' call to [coord_geo()]; however, there are some use cases in which this may be
#' more useful or allow for more customization. For example, users may wish to
#' combine this guide in unique ways with other guides. Further, since
#' [coord_geo()] doesn't work with radial/fan phylogenies (and
#' [coord_geo_radial()] is quite different visually), this guide can be used to
#' achieve the look of [coord_geo()] on a radial/fan phylogeny.
#'
#' @param end_labels How should labels for intervals at the ends of the guide be
#'   treated? "center", the default, centers the labels within the visible part
#'   of the label. "clip" removes the labels if their midpoint is beyond the
#'   axis limits. "keep" plots the labels in the midpoint of the full interval.
#' @inheritParams ggplot2::guide_axis
#' @inheritParams coord_geo
#' @importFrom ggplot2 waiver new_guide
#' @export
#' @examples
#' library(ggplot2)
#' # reproduce the coord_geo() appearance
#' ggplot() +
#'   geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 1000))) +
#'   scale_x_reverse(guide = guide_axis_stack(guide_geo(), "axis",
#'                   spacing = unit(0, "npc"))) +
#'   coord_cartesian(xlim = c(1000, 0), ylim = c(0, 8)) +
#'   theme_classic()
#' @examplesIf require(ggtree)
#' # the coord_geo() look on a radial phylogeny
#' library(ggtree)
#' library(paleotree)
#' data(RaiaCopesRule)
#' ggtree(ceratopsianTreeRaia,
#'        position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
#'   coord_geo_radial(dat = "stages", fill = c("grey80", "grey95"),
#'                    end = 1.25 * pi) +
#'   guides(
#'     r = guide_axis_stack(guide_geo(rot = -90, neg = TRUE,
#'                                    height = unit(0.5, "line")),
#'                          "axis", spacing = unit(0, "npc"))
#'   ) +
#'   scale_y_continuous(guide = "none", breaks = NULL) +
#'   theme_classic()
guide_geo <- function(dat = "periods",
                      fill = NULL, alpha = 1, height = unit(2, "line"),
                      bord = c("left", "right", "top", "bottom"),
                      lwd = .25, color = "black",
                      lab = TRUE, lab_color = NULL,
                      rot = 0, family = "sans", fontface = "plain", size = 5,
                      skip = c("Quaternary", "Holocene", "Late Pleistocene"),
                      abbrv = TRUE, neg = FALSE,
                      end_labels = "center", dat_is_discrete = FALSE,
                      fittext_args = list(),
                      # Standard guide arguments
                      theme = NULL, title = waiver(), order = 0,
                      position = waiver()
) {
  # check timescale-specific arguments
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
  check_string(end_labels)
  if (!end_labels %in% c("center", "clip", "keep")) {
    cli::cli_abort('`end_labels` must be one of "center", "clip", or "keep".')
  }
  check_bool(dat_is_discrete)
  if (!is.list(fittext_args)) {
    cli::cli_abort("`fittext_args` must be a `list` of arguments.")
  }
  new_guide(
    # Arguments passed on to the GuideGeo$params field
    dat = dat, fill = fill, alpha = alpha, height = height,
    bord = bord, lwd = lwd, color = color, lab = lab, lab_color = lab_color,
    rot = rot, family = family, fontface = fontface, size = size, skip = skip,
    abbrv = abbrv, neg = neg, end_labels = end_labels,
    dat_is_discrete = dat_is_discrete, fittext_args = fittext_args,
    theme = theme, title = title, order = order, position = position,
    # Declare which aesthetics are supported
    available_aes = c("x", "y", "r"),
    # Set the guide class
    super = GuideGeo
  )
}

#' @rdname guide_geo
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 GuideAxis ggproto ggproto_parent zeroGrob last_plot
#' @importFrom ggplot2 scale_fill_identity scale_color_identity ggplotGrob
#' @importFrom ggplot2 annotate aes theme_void geom_rect geom_segment geom_text
#' @importFrom ggplot2 xlim
#' @importFrom grid unit viewport
#' @importFrom gtable gtable gtable_add_grob gtable_width gtable_height
#' @importFrom rlang := exec
#' @importFrom scales transform_identity
GuideGeo <- ggproto("GuideGeo", GuideAxis,
  params = c(GuideAxis$params,
             list(dat = "periods",
                  fill = NULL, alpha = 1, height = unit(2, "line"),
                  bord = c("left", "right", "top", "bottom"),
                  lwd = .25, color = "black",
                  lab = TRUE, lab_color = NULL,
                  rot = 0, family = "sans", fontface = "plain", size = 5,
                  skip = c("Quaternary", "Holocene", "Late Pleistocene"),
                  abbrv = TRUE, neg = FALSE,
                  end_labels = "center", dat_is_discrete = FALSE,
                  fittext_args = list())),
  transform = function(self, params, coord, panel_params) {
    self$panel_params <- panel_params
    ggproto_parent(GuideAxis, self)$transform(params, coord, panel_params)
  },

  # The decor in the axis guide is the axis line
  build_decor = function(self, decor, grobs, elements, params) {
    # TODO: a lot of this is duplicated from make_geo_scale()
    # return an empty grob if the decor df is empty (e.g., secondary axes)
    if (empty(decor)) {
      return(zeroGrob())
    }

    # extract some commonly used params
    dat <- params$dat
    panel_params <- self$panel_params
    position <- params$position
    lwd <- params$lwd
    color <- params$color
    bord <- params$bord
    n_labels <- nrow(params$key)

    # get data
    if (is(dat, "data.frame")) {
      # just use the supplied data
    } else if (is.character(dat)) {
      dat <- get_scale_data(dat)
    } else {
      cli::cli_abort("`dat` must be either a dataframe or a string.")
    }

    # get axis limits
    rev_axis <- FALSE
    trans <- transform_identity()
    if ("r.range" %in% names(panel_params)) {
      trans <- panel_params$r$scale$trans
      discrete <- FALSE
      rev_axis <- trans$name == "reverse"
      lims <- panel_params$r.range * c(1, -1)[rev_axis + 1]
    } else if (position %in% c("bottom", "top")) {
      discrete <- panel_params$x$scale_is_discrete
      if (discrete) {
        limits <- panel_params$x$limits
        lims <- panel_params$x.range
      } else {
        trans <- panel_params$x$scale$trans
        rev_axis <- trans$name == "reverse"
        lims <- panel_params$x.range * c(1, -1)[rev_axis + 1]
      }
    } else if (position %in% c("left", "right")) {
      discrete <- panel_params$y$scale_is_discrete
      if (discrete) {
        limits <- panel_params$y$limits
        lims <- panel_params$y.range
      } else {
        trans <- panel_params$y$scale$trans
        rev_axis <- trans$name == "reverse"
        lims <- panel_params$y.range * c(1, -1)[rev_axis + 1]
      }
    }

    if (discrete && !params$dat_is_discrete) {
      dat <- subset(dat, name %in% limits)
      dat$min_age <- -0.5 + match(limits, dat$name)
      dat$max_age <- 0.5 + match(limits, dat$name)
    }

    # Filter data to only those that are within the plot limits
    if (params$neg) {
      dat$max_age <- -1 * (dat$max_age)
      dat$min_age <- -1 * (dat$min_age)
      dat <- subset(dat, min_age < max(lims) & min_age > min(lims) |
                      max_age < max(lims) & max_age > min(lims))
    } else {
      dat <- subset(dat, min_age > min(lims) & min_age < max(lims) |
                      max_age > min(lims) & max_age < max(lims))
    }
    # calculate midpoints for labels
    dat$mid_age <- (dat$max_age + dat$min_age) / 2

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

    # replace fill colors if requested
    if (!is.null(params$fill)) {
      dat$color <- rep(params$fill, length.out = nrow(dat))
    } else if (!("color" %in% colnames(dat))) {
      dat$color <- rep(c("grey60", "grey80"), length.out = nrow(dat))
    }

    # do this so ggsave gets the whole plot
    old_plot <- last_plot()
    on.exit(set_last_plot(old_plot))

    # Add interval rectangles
    gg_scale <- ggplot() +
      geom_rect(
        data = dat, aes(xmin = min_age, xmax = max_age, fill = color),
        ymin = 0, ymax = 1, color = NA, alpha = params$alpha,
        show.legend = FALSE, inherit.aes = FALSE
      ) +
      scale_fill_identity() +
      geom_segment(
        data = dat, aes(x = min_age, xend = min_age), y = 0, yend = 1,
        color = color, linewidth = lwd
      ) +
      geom_segment(
        data = dat, aes(x = max_age, xend = max_age), y = 0, yend = 1,
        color = color, linewidth = lwd
      ) +
      theme_void()

    # Add labels
    if (params$lab) {
      if (params$end_labels == "center") {
        # recalculate the mid ages
        dat$mid_age <- (dat$max_age + dat$min_age) / 2
      }
      if (params$abbrv == "auto") {
        dat$label <- abbreviate(dat$name, minlength = 1,
                                use.classes = TRUE, named = FALSE)
      } else if (params$abbrv && "abbr" %in% colnames(dat)) {
        dat$label <- dat$abbr
        dat$label[dat$abbr %in% params$skip] <- ""
      } else {
        dat$label <- dat$name
      }
      dat$label[dat$name %in% params$skip] <- ""

      if (!is.null(params$lab_color)) {
        dat$lab_color <- rep(params$lab_color, length.out = nrow(dat))
      } else if (!("lab_color" %in% colnames(dat))) {
        dat$lab_color <- rep("black", length.out = nrow(dat))
      }
      if (params$size == "auto") {
        gg_scale <- gg_scale +
          exec(geom_fit_text,
               data = dat, aes(
                 x = mid_age, label = label,
                 ymin = 0, ymax = 1,
                 xmin = min_age, xmax = max_age,
                 color = lab_color
               ), angle = params$rot,
               family = params$family, fontface = params$fontface,
               show.legend = FALSE, inherit.aes = FALSE, !!!params$fittext_args
          )
      } else {
        gg_scale <- gg_scale +
          geom_text(
            data = dat, aes(x = mid_age, label = label, color = lab_color),
            y = .5,
            vjust = "middle", hjust = "middle",
            size = params$size, angle = params$rot,
            family = params$family, fontface = params$fontface,
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
      bord_lims[(if (params$neg) bord_lims > 0 else bord_lims < 0)] <- 0
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

    # if left or right, rotate accordingly using coord_trans_flip,
    # otherwise, just use coord_trans
    if (position %in% c("bottom", "top")) {
      if (packageVersion("ggplot2") > "3.5.2") {
        gg_scale <- gg_scale +
          ggplot2::coord_transform(x = trans, xlim = lims, ylim = c(0, 1),
                                   expand = FALSE, clip = "off")
      } else {
        gg_scale <- gg_scale +
          ggplot2::coord_trans(x = trans, xlim = lims, ylim = c(0, 1),
                               expand = FALSE, clip = "off")
      }
    } else if (position %in% c("left", "right")) {
      gg_scale <- gg_scale +
        coord_trans_flip(x = trans, xlim = lims, ylim = c(0, 1),
                         expand = FALSE, clip = "off")
    }
    if (params$end_labels == "clip") {
      gg_scale <- gg_scale + xlim(lims)
    }

    ggplotGrob(gg_scale)
  },
  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
    axis_geo <- grobs$decor

    if ("r.range" %in% names(self$panel_params)) {
      decor <- params$decor
      n_decor <- nrow(decor)
      size <- unit(abs(diff(decor$y[c(1, n_decor)])), "npc")
    } else {
      size <- unit(1, "npc")
    }
    gt <- exec(
      gtable,
      !!params$orth_sizes := params$height,
      !!params$para_sizes := size,
      name = "geo_axis"
    )
    gt <- gtable_add_grob(gt, axis_geo, 1, 1, clip = "off")

    justvp <- exec(
      viewport,
      !!params$orth_aes := unit(params$orth_side, "npc"),
      !!params$orth_size := params$measure_gtable(gt),
      just = params$opposite
    )
    # center properly if this is an r axis
    if ("r.range" %in% names(self$panel_params)) {
      justvp$y <- unit(mean(decor$y), "npc")
    }

    gTree(
      children = gList(gt),
      width = gtable_width(gt), height = gtable_height(gt),
      vp = justvp, cl = "absoluteGrob"
    )
  },
)

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}
