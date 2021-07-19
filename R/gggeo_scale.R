#' @export
#' @rdname gggeo_scale
#' @param obj An object of class ggplot, gtable, or geo_scale (as produced by this function).
gggeo_scale <- function(obj, ...) {
  UseMethod("gggeo_scale", obj)
}

#' Add a geologic scale to ggplots
#'
#' This function takes a ggplot object and adds a geologic time scale at the specified side.
#'
#' If custom data is provided (with \code{dat}), it should consist of at least 3 columns of data. See \code{data(periods)} for an example.
#'   The \code{name} column lists the names of each time interval. These will be used as labels if no abbreviations are provided.
#'   The \code{max_age} column lists the oldest boundary of each time interval.
#'   The \code{min_age} column lists the youngest boundary of each time interval.
#'   The \code{abbr} column is optional and lists abbreviations that may be used as labels.
#'   The \code{color} column is also optional and lists a hex color code (which can be obtained with \code{rgb()}) for each time interval.
#' @param obj A gtable object.
#' @param lims The limits of the axis of the desired side of the plot. Only required if using a gtable object not created by this function.
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
#' @param bord A vector specifying on Which sides of the scale to add borders (same options as \code{pos}).
#' @param center_end_labels Should labels be centered within the visible range of intervals at the ends of the axis?
#' @return A geo_scale object. Basically a gtable object but with the axis limits included.
#' @importFrom gtable gtable_add_grob gtable_add_cols gtable_add_rows gtable_add_padding
#' @importFrom grid unit
#' @importFrom ggplot2 ggplot geom_rect geom_segment geom_text annotate aes scale_fill_manual theme_void theme coord_cartesian coord_flip scale_x_reverse
#' @importFrom methods is
#' @export
#' @aliases gggeo_scale
#' @rdname gggeo_scale
#' @examples
#' library(ggplot2)
#' # bottom scale by default
#' p <- ggplot() +
#'   geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 1000))) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(1000, 0), ylim = c(0, 8), expand = FALSE) +
#'   theme_classic()
#' gggeo_scale(p)
#'
#' # can specify any side of the plot
#' p <- ggplot() +
#'  geom_point(aes(x = runif(1000, 0, 8), y = runif(1000, 0, 1000))) +
#'  scale_y_reverse() +
#'  coord_cartesian(xlim = c(0, 8), ylim = c(1000, 0), expand = FALSE) +
#'  theme_classic()
#' gggeo_scale(p, pos = "left", rot = 90)
#'
#' # can add multiple scales
#' p <- ggplot() +
#'   geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 100))) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(100, 0), ylim = c(0, 8), expand = FALSE) +
#'   theme_classic()
#' p <- gggeo_scale(p, abbrv = FALSE)
#' p <- gggeo_scale(p, dat = "epochs", height = unit(4, "lines"), rot = 90, size = 2.5, abbrv = FALSE)
#' gggeo_scale(p, dat = "stages", height = unit(4, "lines"), rot = 90, size = 2.5, abbrv = FALSE)
#'
#' # intervals on both sides for different timescales (ICS stages vs North American Land Mammal Ages)
#' p <- ggplot() +
#'   geom_point(aes(x = runif(1000, 0, 10), y = runif(1000, 0, 65))) +
#'   scale_y_reverse() +
#'   coord_cartesian(xlim = c(0, 10), ylim = c(65, 0), expand = FALSE) +
#'   theme_classic()
#' p <- gggeo_scale(p, dat = "stages", pos = "left", height = unit(4, "lines"), size = 2.5,
#'                  abbrv = FALSE)
#' gggeo_scale(p, dat = "North American Land Mammal Ages", pos = "right", height = unit(4, "lines"),
#'             size = 2.5, abbrv = FALSE)
#'
#' #can add scales to a faceted plot
#' #use gggeo_scale_old() if you have more than one column
#' df <- data.frame(x = runif(1000, 0, 541), y = runif(1000, 0, 8), z = sample(c(1, 2, 3, 4), 1000, TRUE))
#' p <- ggplot(df) +
#'   geom_point(aes(x, y)) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(541, 0), ylim = c(0, 8), expand = FALSE) +
#'   theme_classic() +
#'   facet_wrap(~z, ncol = 1)
#' gggeo_scale(p)
#'
#' #can even add a scale to a phylogeny (using ggtree)
#' \dontrun{
#' library(phytools)
#' library(ggtree)
#' tree <- pbtree(b = .03, d = .01,  n=100)
#' p <- ggtree(tree) +
#'   coord_cartesian(xlim = c(-500, 0), ylim = c(-2, Ntip(tree)), expand = FALSE) +
#'   scale_x_continuous(breaks=seq(-500, 0, 100), labels=abs(seq(-500, 0, 100))) +
#'   theme_tree2()
#' p <- revts(p)
#' gggeo_scale(p, neg = TRUE)
#' }
gggeo_scale.gtable <- function(gt, lims, dat = "periods", fill = NULL, color = "black", alpha = 1,
                               height = unit(2, "line"), pos = "bottom", lab = TRUE, rot = 0,
                               abbrv = TRUE, skip = c("Quaternary", "Holocene", "Late Pleistocene"),
                               size = 5, lwd = .25, margin = NULL, neg = FALSE,
                               bord = c("left", "right", "top", "bottom"), center_end_labels = FALSE) {
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
    theme_void()

  rev_axis <- FALSE
  #if left or right, rotate accordingly, otherwise, just use coord_cartesian
  if(pos %in% c("bottom", "top", "b", "t")){
    if(is.list(lims)){
      rev_axis <- lims$x$break_positions()[1] > lims$x$break_positions()[2]
      lims <- lims$x.range * c(1, -1)[rev_axis + 1]
    }else{
      rev_axis <- lims[1] > lims[2]
    }
    gg_scale <- gg_scale +
      coord_cartesian(xlim = lims, ylim = c(0,1), expand = FALSE)
  }else if(pos %in% c("left", "right","l","r")){
    if(is.list(lims)){
      rev_axis <- lims$y$break_positions()[1] > lims$y$break_positions()[2]
      lims <- lims$y.range * c(1, -1)[rev_axis + 1]
    }else{
      rev_axis <- lims[1] > lims[2]
    }
    gg_scale <- gg_scale +
      coord_flip(xlim = lims, ylim = c(0,1), expand = FALSE)
  }

  #Add labels
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
  gg_scale <- gg_scale +
    geom_text(data = dat, aes(x = mid_age, label = label), y = .5,
              vjust = "middle", hjust = "middle", size = size, angle = rot,
              inherit.aes = FALSE)

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

  #convert to grob
  grob_scale <- ggplotGrob(gg_scale)

  #find panels
  panels <- obj$layout[grepl("panel", obj$layout[["name"]]), 1:4]

  #add a row or column in the proper spot
  #then put the scale grob in the proper spot
  if(pos %in% c("top","t")){
    for(i in unique(panels$t)){
      obj <- gtable_add_rows(obj, heights = height, pos = i - 1)
    }
    for(i in 1:nrow(panels)){
      obj <- gtable_add_grob(obj, grob_scale, t = panels$t[i], l = panels$l[i], r = panels$r[i], name = "axis-scale")
    }
  }else if(pos %in% c("bottom","b")){
    for(i in unique(panels$b)){
      obj <- gtable_add_rows(obj, heights = height, pos = i)
    }
    for(i in 1:nrow(panels)){
      obj <- gtable_add_grob(obj, grob_scale, t = panels$b[i] + 1, l = panels$l[i], r = panels$r[i], name = "axis-scale")
    }
  }else if(pos %in% c("left", "l")){
    for(i in unique(panels$l)){
      obj <- gtable_add_cols(obj, widths = height, pos = i - 1)
    }
    for(i in 1:nrow(panels)){
      obj <- gtable_add_grob(obj, grob_scale, t = panels$t[i], l = panels$l[i], b = panels$b[i], name = "axis-scale")
    }
  }else if(pos %in% c("right","r")){
    for(i in unique(panels$r)){
      obj <- gtable_add_cols(obj, widths = height, pos = i)
    }
    for(i in 1:nrow(panels)){
      obj <- gtable_add_grob(obj, grob_scale, t = panels$t[i], l = panels$r[i] + 1, b = panels$b[i], name = "axis-scale")
    }
  }
  obj <- gtable_add_padding(obj, margin)
  obj$lims <- lims
  class(obj) <- c("geo_scale", class(obj))
  obj
}

#' @param gg A ggplot object.
#' @importFrom ggplot2 ggplot_build
#' @export
#' @rdname gggeo_scale
gggeo_scale.ggplot <- function(obj, dat = "periods", fill = NULL, color = "black", alpha = 1,
                               height = unit(2, "line"), pos = "bottom", lab = TRUE, rot = 0,
                               abbrv = TRUE, skip = c("Quaternary", "Holocene", "Late Pleistocene"),
                               size = 5, lwd = .25, margin = NULL, neg = FALSE,
                               bord = c("left", "right", "top", "bottom"), center_end_labels = FALSE){
  lims <- ggplot_build(obj)$layout$panel_params[[1]]
  #convert input to grob and gtable layout
  grob_gg <- ggplotGrob(obj)
  gt <- gtable_frame2(grob_gg)
  gggeo_scale.gtable(gt, lims = lims, dat = dat, fill = fill, color = color, alpha = alpha, height = height,
                     pos = pos, lab = lab, rot = rot, abbrv = abbrv, skip = skip, size = size, lwd = lwd,
                     margin = margin, neg = neg, bord = bord, center_end_labels = center_end_labels)
}

#' @param geo A geo_scale object output by \code{gggeo_scale()}.
#' @export
#' @rdname gggeo_scale
gggeo_scale.geo_scale <- function(obj, dat = "periods", fill = NULL, color = "black", alpha = 1,
                                  height = unit(2, "line"), pos = "bottom", lab = TRUE, rot = 0,
                                  abbrv = TRUE, skip = c("Quaternary", "Holocene", "Late Pleistocene"),
                                  size = 5, lwd = .25, margin = NULL, neg = FALSE,
                                  bord = c("left", "right", "top", "bottom"), center_end_labels = FALSE){
  lims <- obj$lims
  gggeo_scale.gtable(obj, lims = lims, dat = dat, fill = fill, color = color, alpha = alpha, height = height,
                     pos = pos, lab = lab, rot = rot, abbrv = abbrv, skip = skip, size = size, lwd = lwd,
                     margin = margin, neg = neg, bord = bord, center_end_labels = center_end_labels)
}

#' @param x An object of class geo_scale.
#' @param ... further arguments passed to \code{grid.draw}.
#' @export
#' @rdname gggeo_scale
#' @importFrom grid grid.newpage grid.draw
print.geo_scale <- function(x, ...) {
  grid.newpage()
  grid.draw(x, ...)
}
