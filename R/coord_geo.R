## declare variables that are used within aes() to prevent R CMD check from complaining
utils::globalVariables(c("min_age", "max_age", "mid_age", "label", "name"))

#' Transformed coordinate system with geological timescale
#'
#' \code{coord_geo} behaves similarly to \code{\link[ggplot2]{coord_trans}} in that it occurs after
#' statistical transformation and will affect the visual appearance of geoms. The main difference
#' is that it also adds a geological timescale to the specified side of the plot.
#'
#' Transforming the side with the scale is not currently implemented.
#' If a custom data.frame is provided (with \code{dat}), it should consist of at least 3 columns of data. See \code{data(periods)} for an example.
#' \itemize{
#'   \item The \code{name} column lists the names of each time interval. These will be used as labels if no abbreviations are provided.
#'   \item The \code{max_age} column lists the oldest boundary of each time interval.
#'   \item The \code{min_age} column lists the youngest boundary of each time interval.
#'   \item The \code{abbr} column is optional and lists abbreviations that may be used as labels.
#'   \item The \code{color} column is also optional and lists a hex color code (which can be obtained with \code{rgb()}) for each time interval.
#' }
#'
#' If the axis of the time scale is discrete, \code{max_age} and \code{min_age} will
#' automatically be converted to the discrete scale. In this case, the categories
#' of the discrete axis should match the values in the \code{name} column. If the ages
#' within \code{dat} are already discretized, you can set \code{dat_is_discrete} to
#' \code{TRUE} to prevent this automatic conversion. This can be useful for adding a
#' time scale where categories and time intervals are not 1:1.
#'
#' \code{pos} may also be a list of sides (including duplicates) if multiple time scales should be added to the plot.
#' In this case, \code{dat}, \code{fill}, \code{color}, \code{alpha}, \code{height}, \code{lab},
#' \code{rot}, \code{abbrv}, \code{skip}, \code{size}, \code{lwd}, \code{neg}, \code{bord},
#' \code{center_end_labels}, and \code{dat_is_discrete} can also be lists.
#' If these lists are not as long as \code{pos}, the elements will be recycled.
#' If individual values are used for these parameters, they will be applied to all time scales.
#' @param pos Which side to add the scale to (left, right, top, or bottom). First letter may also be used.
#' @param dat Either A) a string indicating a built-in dataframe with interval data from the ICS ("periods", "epochs", "stages", "eons", or "eras"),
#'   B) a string indicating a timescale from macrostrat (see list here: \url{https://macrostrat.org/api/defs/timescales?all}),
#'   or C) a custom data.frame of time interval boundaries (see Details).
#' @param xlim,ylim Limits for the x and y axes.
#' @param xtrans,ytrans Transformers for the x and y axes. For more information see \code{\link[ggplot2]{coord_trans}}.
#' @param clip Should drawing be clipped to the extent of the plot panel? For more information see \code{\link[ggplot2]{coord_trans}}.
#' @param expand If `FALSE`, the default, limits are taken exactly from the data or `xlim`/`ylim`.
#'   If `TRUE`, adds a small expansion factor to the limits to ensure that data and axes don't overlap.
#' @param fill The fill color of the boxes. The default is to use the colors included in \code{dat}.
#'   If a custom dataset is provided with \code{dat} without color and without fill, a greyscale will be used.
#'   Custom fill colors can be provided with this option and will be recycled if/as necessary.
#' @param color The outline color of the interval boxes.
#' @param alpha The transparency of the fill colors.
#' @param height The height (or width if \code{pos} is \code{left} or \code{right}) of the scale.
#' @param lab Whether to include labels.
#' @param rot The amount of counter-clockwise rotation to add to the labels (in degrees).
#' @param abbrv If including labels, whether to use abbreviations instead of full interval names.
#' @param skip A vector of interval names indicating which intervals should not be labeled. If \code{abbrv} is \code{TRUE}, this can also include interval abbreviations.
#' @param size Label size. Either a number as you would specify in \code{\link[ggplot2]{geom_text}} or \code{"auto"} to use \code{\link[ggfittext]{geom_fit_text}}.
#' @param lwd Line width.
#' @param neg Set this to true if your x-axis is using negative values.
#' @param bord A vector specifying on Which sides of the scale to add borders (same options as \code{pos}).
#' @param center_end_labels Should labels be centered within the visible range of intervals at the ends of the axis?
#' @param dat_is_discrete Are the ages in \code{dat} already converted for a discrete scale?
#' @param fittext_args A list of named arguments to provide to \code{\link[ggfittext]{geom_fit_text}}. Only used if \code{size} is set to \code{"auto"}.
#' @importFrom ggplot2 ggproto
#' @import scales
#' @export
#' @examples
#' library(ggplot2)
#' #single scale on bottom
#' ggplot() +
#'   geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 1000))) +
#'   scale_x_reverse() +
#'   coord_geo(xlim = c(1000, 0), ylim = c(0,8)) +
#'   theme_classic()
#'
#' #stack multiple scales
#' ggplot() +
#'   geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 100))) +
#'   scale_x_reverse() +
#'   coord_geo(xlim = c(100, 0), ylim = c(0,8), pos = as.list(rep("bottom", 3)),
#'   dat = list("stages", "epochs", "periods"),
#'   height = list(unit(4, "lines"), unit(4, "lines"), unit(2, "line")),
#'   rot = list(90, 90, 0), size = list(2.5, 2.5, 5), abbrv = FALSE) +
#'   theme_classic()
coord_geo <- function(pos = "bottom", dat = "periods", xlim = NULL, ylim = NULL, xtrans = identity_trans(), ytrans = identity_trans(),
                      clip = "on", expand = FALSE, fill = NULL, color = "black", alpha = 1, height = unit(2, "line"),
                      lab = TRUE, rot = 0, abbrv = TRUE, skip = c("Quaternary", "Holocene", "Late Pleistocene"), size = 5,
                      lwd = .25, neg = FALSE, bord = c("left", "right", "top", "bottom"),
                      center_end_labels = FALSE, dat_is_discrete = FALSE, fittext_args = list()) {

  # resolve transformers
  if (is.character(xtrans)) xtrans <- as.trans(xtrans)
  if (is.character(ytrans)) ytrans <- as.trans(ytrans)

  pos = as.list(pos)
  n_scales = length(pos)

  ggproto(NULL, CoordGeo,
          trans = list(x = xtrans, y = ytrans),
          limits = list(x = xlim, y = ylim),
          expand = expand, clip = clip,
          pos = pos, dat = rep(make_list(dat), length.out = n_scales), fill = rep(make_list(fill), length.out = n_scales),
          color = rep(make_list(color), length.out = n_scales), alpha = rep(make_list(alpha), length.out = n_scales),
          height = rep(make_list(height), length.out = n_scales), lab = rep(as.list(lab), length.out = n_scales),
          rot = rep(make_list(rot), length.out = n_scales), abbrv = rep(make_list(abbrv), length.out = n_scales),
          skip = rep(make_list(skip), length.out = n_scales), size = rep(make_list(size), length.out = n_scales),
          lwd = rep(make_list(lwd), length.out = n_scales), neg = rep(make_list(neg), length.out = n_scales),
          bord = rep(make_list(bord), length.out = n_scales),
          center_end_labels = rep(make_list(center_end_labels), length.out = n_scales),
          dat_is_discrete = rep(make_list(dat_is_discrete), length.out = n_scales),
          fittext_args = fittext_args
  )
}

render_axis <- utils::getFromNamespace("render_axis", "ggplot2")

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
    if (any(self$pos %in% c("top", "t"))){
      top = render_geo_scale(self, panel_params, theme, "top")
    } else {
      top = render_axis(panel_params, arrange[1], "x", "top", theme)
    }
    if (any(self$pos %in% c("bottom", "b"))){
      bottom = render_geo_scale(self, panel_params, theme, "bottom")
    } else {
      bottom = render_axis(panel_params, arrange[2], "x", "bottom", theme)
    }

    list(
      top = top,
      bottom = bottom
    )
  },

  render_axis_v = function(self, panel_params, theme) {
    arrange <- panel_params$y.arrange %||% c("primary", "secondary")
    if (any(self$pos %in% c("left", "l"))){
      left = render_geo_scale(self, panel_params, theme, "left")
    } else {
      left = render_axis(panel_params, arrange[1], "y", "left", theme)
    }
    if (any(self$pos %in% c("right", "r"))){
      right = render_geo_scale(self, panel_params, theme, "right")
    } else {
      right = render_axis(panel_params, arrange[2], "y", "right", theme)
    }

    list(
      left = left,
      right = right
    )
  }
)

#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid unit unit.c viewport grobWidth grobHeight gList gTree
#' @importFrom gtable gtable_col gtable_row gtable_height gtable_width
render_geo_scale <- function(self, panel_params, theme, position){
  one <- unit(1, "npc")
  ind <- self$pos %in% c(position, substring(position, 1, 1))

  geo_scales <- mapply(make_geo_scale,
                       dat = self$dat[ind],
                       fill = self$fill[ind],
                       color = self$color[ind],
                       alpha = self$alpha[ind],
                       pos = self$pos[ind],
                       lab = self$lab[ind],
                       rot = self$rot[ind],
                       abbrv = self$abbrv[ind],
                       skip = self$skip[ind],
                       size = self$size[ind],
                       lwd = self$lwd[ind],
                       neg = self$neg[ind],
                       bord = self$bord[ind],
                       center_end_labels = self$center_end_labels[ind],
                       dat_is_discrete = self$dat_is_discrete[ind],
                       MoreArgs = list(self = self, panel_params = panel_params,
                                       theme = theme, fittext_args = self$fittext_args),
                       SIMPLIFY = FALSE)
  geo_grobs <- lapply(geo_scales, ggplotGrob)

  if(position == "bottom"){
    axis <- render_axis(panel_params, "primary", "x", "bottom", theme)
    gt <- gtable_col("axis", grobs = c(geo_grobs, list(axis)),
                     width = one, heights = unit.c(do.call(unit.c, self$height[ind]), grobHeight(axis)))
    justvp <- viewport(y = 1, just = "top", height = gtable_height(gt))
  }else if(position == "top"){
    axis <- render_axis(panel_params, "primary", "x", "top", theme)
    gt <- gtable_col("axis", grobs = c(list(axis), geo_grobs),
                     width = one, heights = unit.c(grobHeight(axis), do.call(unit.c, self$height[ind])))
    justvp <-  viewport(y = 0, just = "bottom", height = gtable_height(gt))
  }else if(position == "left"){
    axis <- render_axis(panel_params, "primary", "y", "left", theme)
    gt <- gtable_row("axis", grobs = c(list(axis), geo_grobs),
                     height = one, widths = unit.c(grobWidth(axis), do.call(unit.c, self$height[ind])))
    justvp <-  viewport(x = 1, just = "right", width = gtable_width(gt))
  }else if(position == "right"){
    axis <- render_axis(panel_params, "primary", "y", "right", theme)
    gt <- gtable_row("axis", grobs = c(geo_grobs, list(axis)),
                     height = one, widths = unit.c(do.call(unit.c, self$height[ind]), grobWidth(axis)))
    justvp <-  viewport(x = 0, just = "left", width = gtable_width(gt))
  }

  gTree(children = gList(gt),
        width = gtable_width(gt), height = gtable_height(gt),
        vp = justvp, cl = "absoluteGrob"
  )
}

#' @importFrom ggplot2 ggplot geom_rect geom_segment geom_text annotate aes scale_fill_manual theme_void theme coord_cartesian coord_flip scale_x_reverse coord_trans
#' @importFrom ggfittext geom_fit_text
#' @importFrom rlang exec
make_geo_scale <- function(self, dat, fill, color, alpha, pos, lab, rot, abbrv, skip,
                           size, lwd, neg, bord, center_end_labels, dat_is_discrete,
                           panel_params, theme, fittext_args){
  if(is(dat, "data.frame")){
    #just use the supplied data
  }else{
    dat <- getScaleData(dat)
  }

  #if the axis is discrete, adjust the data accordingly
  if(pos %in% c("bottom", "top", "b", "t")){
    discrete <- panel_params$scale_x$is_discrete()
    limits <- panel_params$scale_x$limits
  }else if(pos %in% c("left", "right","l","r")){
    discrete <- panel_params$scale_y$is_discrete()
    limits <- panel_params$scale_y$limits
  }

  if(discrete && !dat_is_discrete){
    dat <- subset(dat, name %in% limits)
    dat$min_age <- -0.5 + match(limits, dat$name)
    dat$max_age <- 0.5 + match(limits, dat$name)
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
    dat$label[dat$abbr %in% skip] <- ""
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
    theme_void()

  rev_axis <- FALSE
  #if left or right, rotate accordingly using coord_trans_flip,
  #otherwise, just use coord_trans
  if(pos %in% c("bottom", "top", "b", "t")){
    if(discrete){
      lims <- panel_params$x.range
    } else {
      rev_axis <- panel_params$scale_x$trans$name == "reverse"
      lims <- panel_params$x.range * c(1, -1)[rev_axis + 1]
    }
    gg_scale <- gg_scale +
      coord_trans(x = self$trans$x, xlim = lims, ylim = c(0,1), expand = FALSE)
  }else if(pos %in% c("left", "right","l","r")){
    if(discrete){
      lims <- panel_params$y.range
    } else {
      rev_axis <- panel_params$scale_y$trans$name == "reverse"
      lims <- panel_params$y.range * c(1, -1)[rev_axis + 1]
    }
    gg_scale <- gg_scale +
      coord_trans_flip(x = self$trans$y, xlim = lims, ylim = c(0,1), expand = FALSE)
  }

  #Add labels
  if (lab) {
    if (center_end_labels) {
      #center the labels for the time periods at the ends of the axis
      max_end <- (dat$max_age > max(lims) & dat$min_age < max(lims)) | (dat$max_age < max(lims) & dat$min_age > max(lims))
      min_end <- (dat$max_age > min(lims) & dat$min_age < min(lims)) | (dat$max_age < min(lims) & dat$min_age > min(lims))
      if (any(max_end)){
        ends <- dat[max_end,c("min_age","max_age")]
        dat$mid_age[max_end] <- (ends[ends < max(lims) & ends > min(lims)] + max(lims))/2
      }
      if (any(min_end)){
        ends <- dat[min_end,c("min_age","max_age")]
        dat$mid_age[min_end] <- (ends[ends < max(lims) & ends > min(lims)] + min(lims))/2
      }
    }
    if(size == "auto"){
      gg_scale <- gg_scale +
        exec(geom_fit_text, data = dat, aes(x = mid_age, label = label,
                                            ymin = 0, ymax = 1,
                                            xmin = min_age, xmax = max_age),
                            angle = rot, inherit.aes = FALSE, !!!fittext_args)
    } else {
      gg_scale <- gg_scale +
        geom_text(data = dat, aes(x = mid_age, label = label), y = .5,
                  vjust = "middle", hjust = "middle", size = size, angle = rot,
                  inherit.aes = FALSE)
    }
  }

  #Add border
  if(discrete){
    bord_lims <- c(min(dat$min_age), max(dat$max_age))
  } else {
    bord_lims <- lims
    bord_lims[(if(neg) bord_lims > 0 else bord_lims < 0)] <- 0
  }

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

make_list <- function(x) {
  if (is.list(x) & !is(x, 'data.frame')) x else list(x)
}
