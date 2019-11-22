#' Transformed coordinate system with geological timescale
#'
#' \code{coord_geo} behaves similarly to \code{\link[ggplot2]{coord_trans}} in that it occurs after
#' statistical transformation and will affect the visual appearance of geoms. The main difference
#' is that it also adds a geological timescale to the specified side of the plot.
#'
#' Transforming the side with the scale is not currently implemented.
#' If custom data is provided (with \code{dat}), it should consist of at least 3 columns of data. See \code{data(periods)} for an example.
#'   The \code{name} column lists the names of each time interval. These will be used as labels if no abbreviations are provided.
#'   The \code{max_age} column lists the oldest boundary of each time interval.
#'   The \code{min_age} column lists the youngest boundary of each time interval.
#'   The \code{abbr} column is optional and lists abbreviations that may be used as labels.
#'   The \code{color} column is also optional and lists a hex color code (which can be obtained with \code{rgb()}) for each time interval.
#' @param xlim,ylim Limits for the x and y axes.
#' @param xtrans,ytrans transformers for y axis. For more information see \code{\link[ggplot2]{coord_trans}}.
#' @param expand If `TRUE`, the default, adds a small expansion factor to
#'   the limits to ensure that data and axes don't overlap. If `FALSE`,
#'   limits are taken exactly from the data or `xlim`/`ylim`.
#' @param dat Either A) a string indicating a built-in dataframe with interval data from the ICS ("periods", "epochs", "stages", "eons", or "eras"),
#'   B) a string indicating a timescale from macrostrat (see list here: \url{https://macrostrat.org/api/defs/timescales?all}),
#'   or C) a custom dataframe of time interval boundaries (see Details).
#' @param fill The fill color of the boxes. The default is to use the colors included in \code{dat}.
#'   If a custom dataset is provided with \code{dat} without color and without fill, a greyscale will be used.
#'   Custom fill colors can be provided with this option and will be recycled if/as necessary.
#' @param color The outline color of the interval boxes.
#' @param alpha The transparency of the fill colors.
#' @param height The height (or width if \code{pos} is \code{left} or \code{right}) of the scale.
#' @param pos Which side to add the scale to (left, right, top, or bottom). First letter may also be used.
#' @param lab Whether to include labels.
#' @param rot The amount of counter-clockwise rotation to add to the labels (in degrees).
#' @param abbrv If including labels, whether to use abbreviations instead of full interval names.
#' @param skip A vector of interval names indicating which intervals should not be labelled.
#' @param size Label size.
#' @param lwd Line width.
#' @param margin The width of the margin around the returned object (can be a vector of length 4).
#' @param neg Set this to true if your x-axis is using negative values.
#' @param bord A vector specifying on Which sides of the scale to add bords (same options as \code{pos}).
#' @importFrom ggplot2 ggproto
#' @importFrom scales as.trans
#' @export
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   geom_point(aes(y = runif(1000, .1, 100), x = runif(1000, 0, 1000))) +
#'   scale_x_reverse() +
#'   coord_geo(xlim = c(0, 1000), ylim = c(.1,100), expand = FALSE, pos = "bottom", lwd = 1) +
#'   theme_classic()
coord_geo <- function(xtrans = "identity", ytrans = "identity", xlim = NULL, ylim = NULL, clip = "on", expand = TRUE,
                            dat = "periods", fill = NULL, color = "black", alpha = 1, height = unit(2, "line"), pos = "bottom",
                            lab = TRUE, rot = 0, abbrv = TRUE, skip = c("Quaternary", "Holocene", "Late Pleistocene"), size = 5,
                            lwd = .25, neg = FALSE, bord = c("left", "right", "top", "bottom")) {

  # resolve transformers
  if (is.character(xtrans)) xtrans <- as.trans(xtrans)
  if (is.character(ytrans)) ytrans <- as.trans(ytrans)

  ggproto(NULL, CoordGeo,
          trans = list(x = xtrans, y = ytrans),
          limits = list(x = xlim, y = ylim),
          expand = expand, clip = clip,
          dat = dat, fill = fill, color = color, alpha = alpha,
          height = height, pos = pos, lab = lab, rot = rot, abbrv = abbrv,
          skip = skip, size = size, lwd = lwd, neg = neg, bord = bord
  )
}

#' @rdname coord_geo
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto CoordTrans
#' @importFrom rlang %||%
CoordGeo <- ggproto("CoordGeo", CoordTrans,
  render_axis_h = function(self, panel_params, theme) {
    arrange <- panel_params$x.arrange %||% c("secondary", "primary")
    if (self$pos %in% c("top", "t")){
      top = render_geo_scale(self, panel_params, theme)
    } else {
      top = ggplot2:::render_axis(panel_params, arrange[1], "x", "top", theme)
    }
    if (self$pos %in% c("bottom", "b")){
      bottom = render_geo_scale(self, panel_params, theme)
    } else {
      bottom = ggplot2:::render_axis(panel_params, arrange[2], "x", "bottom", theme)
    }

    list(
      top = top,
      bottom = bottom
    )
  },

  render_axis_v = function(self, panel_params, theme) {
    arrange <- panel_params$y.arrange %||% c("primary", "secondary")
    if (self$pos %in% c("left", "l")){
      left = render_geo_scale(self, panel_params, theme)
    } else {
      left = ggplot2:::render_axis(panel_params, arrange[1], "y", "left", theme)
    }
    if (self$pos %in% c("right", "r")){
      right = render_geo_scale(self, panel_params, theme)
    } else {
      right = ggplot2:::render_axis(panel_params, arrange[2], "y", "right", theme)
    }

    list(
      left = left,
      right = right
    )
  }
)

#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid unit unit.c viewport grobWidth grobHeight gList
#' @importFrom gtable gtable_col gtable_row gtable_height gtable_width
render_geo_scale <- function(self, panel_params, theme){
  one <- unit(1, "npc")

  geo_scale <- ggplotGrob(make_geo_scale(self, panel_params, theme))

  if(self$pos == "bottom"){
    axis <- ggplot2:::render_axis(panel_params, "primary", "x", "bottom", theme)
    gt <- gtable_col("axis", grobs = list(geo_scale, axis),
                     width = one, heights = unit.c(self$height, grobHeight(axis)))
    justvp <- viewport(y = 1, just = "top", height = gtable_height(gt))
  }else if(self$pos == "top"){
    axis <- ggplot2:::render_axis(panel_params, "primary", "x", "top", theme)
    gt <- gtable_col("axis", grobs = list(axis, geo_scale),
                     width = one, heights = unit.c(grobHeight(axis), self$height))
    justvp <-  viewport(y = 0, just = "bottom", height = gtable_height(gt))
  }else if(self$pos == "left"){
    axis <- ggplot2:::render_axis(panel_params, "primary", "y", "left", theme)
    gt <- gtable_row("axis", grobs = list(axis, geo_scale),
                     height = one, widths = unit.c(grobWidth(axis), self$height))
    justvp <-  viewport(x = 1, just = "right", width = gtable_width(gt))
  }else if(self$pos == "right"){
    axis <- ggplot2:::render_axis(panel_params, "primary", "y", "right", theme)
    gt <- gtable_row("axis", grobs = list(geo_scale, axis),
                     height = one, widths = unit.c(self$height, grobWidth(axis)))
    justvp <-  viewport(x = 0, just = "left", width = gtable_width(gt))
  }

  ggplot2:::absoluteGrob(gList(gt), width = gtable_width(gt),
                         height = gtable_height(gt), vp = justvp)
}

#' @importFrom ggplot2 ggplot geom_rect geom_segment geom_text annotate aes scale_fill_manual theme_void theme coord_cartesian coord_flip scale_x_reverse
make_geo_scale <- function(self, panel_params, theme){
  dat = self$dat
  fill = self$fill
  color = self$color
  alpha = self$alpha
  height = self$height
  pos = self$pos
  lab = self$lab
  rot = self$rot
  abbrv = self$abbrv
  skip = self$skip
  size = self$size
  lwd = self$lwd
  margin = self$margin
  neg = self$neg
  bord = self$bord

  if(is(dat, "data.frame")){
    #just use the supplied data
  }else{
    dat <- getScaleData(dat)
  }
  if(neg){
    dat$max_age <- -1 * (dat$max_age)
    dat$min_age <- -1 * (dat$min_age)
  }
  dat$mid_age <- (dat$max_age + dat$min_age)/2
  if(!is.null(fill)){
    dat$color <- rep(fill, length.out = nrow(dat))
  }else if(!("color" %in% colnames(dat))){
    dat$color <- rep(c("grey60","grey80"), length.out = nrow(dat))
  }
  if(abbrv & "abbr" %in% colnames(dat)){
    dat$label <- dat$abbr
  }else{
    dat$label <- dat$name
  }
  dat$label[dat$name %in% skip] <- ""

  #make ggplot of scale
  gg_scale <- ggplot() +
    geom_rect(data = dat, aes(xmin = min_age, xmax = max_age, fill = color),
              ymin = 0, ymax = 1, color = NA, alpha = alpha,
              show.legend = FALSE, inherit.aes = FALSE) +
    geom_segment(data = dat, aes(x = min_age, xend = min_age), y = 0, yend = 1,
                 color = color, size = lwd) +
    geom_segment(data = dat, aes(x = max_age, xend = max_age), y = 0, yend = 1,
                 color = color, size = lwd) +
    scale_fill_manual(values = setNames(dat$color, dat$color)) +
    geom_text(data = dat, aes(x = mid_age, label = label), y = .5,
              vjust = "middle", hjust = "middle", size = size, angle = rot,
              inherit.aes = FALSE) +
    theme_void()

  rev_axis <- FALSE
  #if left or right, rotate accordingly, otherwise, just use coord_cartesian
  if(pos %in% c("bottom", "top", "b", "t")){
    rev_axis <- panel_params$x.major[1] > panel_params$x.major[2]
    lims <- panel_params$x.range * c(1, -1)[rev_axis + 1]
    gg_scale <- gg_scale +
      coord_cartesian(xlim = lims, ylim = c(0,1), expand = FALSE)
  }else if(pos %in% c("left", "right","l","r")){
    rev_axis <- panel_params$y.major[1] > panel_params$y.major[2]
    lims <- panel_params$y.range * c(1, -1)[rev_axis + 1]
    gg_scale <- gg_scale +
      coord_flip(xlim = lims, ylim = c(0,1), expand = FALSE)
  }

  #Add border
  bord_lims <- lims
  bord_lims[(if(neg) bord_lims > 0 else bord_lims < 0)] <- 0

  if("left" %in% bord | "l" %in% bord){
    gg_scale <- gg_scale +
      annotate("segment", x = bord_lims[1], xend = bord_lims[1], y = 0, yend = 1,
               color = color, size = if(bord_lims[1] == lims[1]) lwd * 2 else lwd)
  }
  if("right" %in% bord | "r" %in% bord){
    gg_scale <- gg_scale +
      annotate("segment", x = bord_lims[2], xend = bord_lims[2], y = 0, yend = 1,
               color = color, size = if(bord_lims[2] == lims[2]) lwd * 2 else lwd)
  }
  if("top" %in% bord | "t" %in% bord){
    gg_scale <- gg_scale +
      annotate("segment", x = bord_lims[1], xend = bord_lims[2], y = 1, yend = 1,
               color = color, size = lwd * 2)
  }
  if("bottom" %in% bord | "b" %in% bord){
    gg_scale <- gg_scale +
      annotate("segment", x = bord_lims[1], xend = bord_lims[2], y = 0, yend = 0,
               color = color, size = lwd * 2)
  }

  #reverse axis if necessary
  if(rev_axis){
    gg_scale <- gg_scale + scale_x_reverse()
  }

  gg_scale
}
