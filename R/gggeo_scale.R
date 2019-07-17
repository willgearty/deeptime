#' @export
#' @rdname gggeo_scale
#' @param x Either a ggplot object or a gtable object.
gggeo_scale <- function(x, ...) {
  UseMethod("gggeo_scale", x)
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
#' @param gt A gtable object.
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
#' @param margin The width of the margin around the returned object (can be a vector of length 4).
#' @param neg Set this to true if your x-axis is using negative values.
#' @return A geo_scale object. Basically a gtable object but with the axis limits included.
#' @importFrom gtable gtable_add_grob gtable_add_cols gtable_add_rows gtable_add_padding
#' @importFrom grid unit
#' @importFrom ggplot2 ggplot geom_rect geom_text aes scale_fill_manual theme_void theme element_rect coord_cartesian coord_flip scale_x_reverse
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
#'   coord_cartesian(xlim = c(0, 1000), ylim = c(0,8), expand = FALSE) +
#'   theme_classic()
#' gggeo_scale(p)
#'
#' # can specify any side of the plot
#' p <- ggplot() +
#'  geom_point(aes(x = runif(1000, 0, 8), y = runif(1000, 0, 1000))) +
#'  scale_y_reverse() +
#'  coord_cartesian(xlim = c(0, 8), ylim = c(0,1000), expand = FALSE) +
#'  theme_classic()
#' gggeo_scale(p, pos = "left", rot = 90)
#'
#' # can add multiple scales
#' p <- ggplot() +
#'   geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 100))) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(0, 100), ylim = c(0,8), expand = FALSE) +
#'   theme_classic()
#' p <- gggeo_scale(p, abbrv = FALSE)
#' p <- gggeo_scale(p, dat = "epochs", height = unit(4, "lines"), rot = 90, size = 2.5, abbrv = FALSE)
#' gggeo_scale(p, dat = "stages", height = unit(4, "lines"), rot = 90, size = 2.5, abbrv = FALSE)
#'
#' # intervals on both sides for different timescales (ICS stages vs North American Land Mammal Ages)
#' p <- ggplot() +
#'   geom_point(aes(x = runif(1000, 0, 10), y = runif(1000, 0, 65))) +
#'   scale_y_reverse() +
#'   coord_cartesian(xlim = c(0, 10), ylim = c(0,65), expand = FALSE) +
#'   theme_classic()
#' p <- gggeo_scale(p, dat = "stages", pos = "left", height = unit(4, "lines"), size = 2.5,
#'                  abbrv = FALSE)
#' gggeo_scale(p, dat = "North American Land Mammal Ages", pos = "right", height = unit(4, "lines"),
#'             size = 2.5, abbrv = FALSE)
#'
#' #can add scales to a faceted plot
#' #use gggeo_scale_old() if you have more than one column
#' df <- data.frame(x = runif(1000, 0, 541), y = runif(1000, 0, 8), z = sample(c(1,2,3,4), 1000, TRUE))
#' p <- ggplot(df) +
#'   geom_point(aes(x, y)) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(0, 541), ylim = c(0,8), expand = FALSE) +
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
#'   coord_cartesian(xlim = c(0,-500), ylim = c(-2,Ntip(tree)), expand = FALSE) +
#'   scale_x_continuous(breaks=seq(-500,0,100), labels=abs(seq(-500,0,100))) +
#'   theme_tree2()
#' p <- revts(p)
#' gggeo_scale(p, neg = TRUE)
#' }
gggeo_scale.gtable <- function(gt, lims, dat = "periods", fill = NULL, color = "black", alpha = 1, height = unit(2, "line"), pos = "bottom", lab = TRUE, rot = 0, abbrv = TRUE, skip = c("Quaternary", "Holocene", "Late Pleistocene"), size = 5, margin = unit(0.5, "line"), neg = FALSE) {
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
                       ymin = 0, ymax = 1, color = color, alpha = alpha, show.legend = FALSE,
                       inherit.aes = FALSE) +
    scale_fill_manual(values = setNames(dat$color, dat$color)) +
    geom_text(data = dat, aes(x = mid_age, label = label), y = .5,
                       vjust = "middle", hjust = "middle", size = size, angle = rot,
                       inherit.aes = FALSE) +
    theme_void() +
    theme(panel.border = element_rect(color = color, fill = NA))

  rev_axis <- FALSE
  #if left or right, rotate accordingly, otherwise, just use coord_cartesian
  if(pos %in% c("bottom", "top", "b", "t")){
    if(is.list(lims)){
      rev_axis <- lims$x.major[1] > lims$x.major[2]
      gg_scale <- gg_scale +
        coord_cartesian(xlim = lims$x.range * c(1, -1)[rev_axis + 1], expand = FALSE)
    }else{
      rev_axis <- lims[1] > lims[2]
      gg_scale <- gg_scale +
        coord_cartesian(xlim = lims, expand = FALSE)
    }
  }else if(pos %in% c("left", "right","l","r")){
    if(is.list(lims)){
      rev_axis <- lims$y.major[1] > lims$y.major[2]
      gg_scale <- gg_scale +
        coord_flip(xlim = lims$y.range * c(1, -1)[rev_axis + 1], expand = FALSE)
    }else{
      rev_axis <- lims[1] > lims[2]
      gg_scale <- gg_scale +
        coord_flip(xlim = lims, expand = FALSE)
    }
  }

  #reverse axis if necessary
  if(rev_axis){
    gg_scale <- gg_scale + scale_x_reverse()
  }

  #convert to grob
  grob_scale <- ggplotGrob(gg_scale)

  #find panels
  panels <- gt$layout[grepl("panel", gt$layout[["name"]]), 1:4]

  #add a row or column in the proper spot
  #then put the scale grob in the proper spot
  if(pos %in% c("top","t")){
    for(i in unique(panels$t)){
      gt <- gtable_add_rows(gt, heights = height, pos = i - 1)
    }
    for(i in 1:nrow(panels)){
      gt <- gtable_add_grob(gt, grob_scale, t = panels$t[i], l = panels$l[i], r = panels$r[i], name = "scale")
    }
  }else if(pos %in% c("bottom","b")){
    for(i in unique(panels$b)){
      gt <- gtable_add_rows(gt, heights = height, pos = i)
    }
    for(i in 1:nrow(panels)){
      gt <- gtable_add_grob(gt, grob_scale, t = panels$b[i] + 1, l = panels$l[i], r = panels$r[i], name = "scale")
    }
  }else if(pos %in% c("left", "l")){
    for(i in unique(panels$l)){
      gt <- gtable_add_cols(gt, widths = height, pos = i - 1)
    }
    for(i in 1:nrow(panels)){
      gt <- gtable_add_grob(gt, grob_scale, t = panels$t[i], l = panels$l[i], b = panels$b[i], name = "scale")
    }
  }else if(pos %in% c("right","r")){
    for(i in unique(panels$r)){
      gt <- gtable_add_cols(gt, widths = height, pos = i)
    }
    for(i in 1:nrow(panels)){
      gt <- gtable_add_grob(gt, grob_scale, t = panels$t[i], l = panels$r[i] + 1, b = panels$b[i], name = "scale")
    }
  }
  gt <- gtable_add_padding(gt, margin)
  gt$lims <- lims
  class(gt) <- c("geo_scale", class(gt))
  gt
}

#' @param gg A ggplot object.
#' @importFrom ggplot2 ggplot_build
#' @export
#' @rdname gggeo_scale
gggeo_scale.ggplot <- function(gg, dat = "periods", fill = NULL, color = "black", alpha = 1, height = unit(2, "line"), pos = "bottom", lab = TRUE, rot = 0, abbrv = TRUE, skip = c("Quaternary", "Holocene", "Late Pleistocene"), size = 5, margin = unit(0.5, "line"), neg = FALSE){
  lims <- ggplot_build(gg)$layout$panel_params[[1]]
  #convert input to grob and gtable layout
  grob_gg <- ggplotGrob(gg)
  gt <- gtable_frame2(grob_gg)
  gggeo_scale.gtable(gt, lims = lims, dat = dat, fill = fill, color = color, alpha = alpha, height = height,
                     pos = pos, lab = lab, rot = rot, abbrv = abbrv, skip = skip, size = size, margin = margin, neg = neg)
}

#' @param geo A geo_scale object output by \code{gggeo_scale()}.
#' @export
#' @rdname gggeo_scale
gggeo_scale.geo_scale <- function(geo, dat = "periods", fill = NULL, color = "black", alpha = 1, height = unit(2, "line"), pos = "bottom", lab = TRUE, rot = 0, abbrv = TRUE, skip = c("Quaternary", "Holocene", "Late Pleistocene"), size = 5, margin = unit(0.5, "line"), neg = FALSE){
  lims <- geo$lims
  gggeo_scale.gtable(geo, lims = lims, dat = dat, fill = fill, color = color, alpha = alpha, height = height,
                     pos = pos, lab = lab, rot = rot, abbrv = abbrv, skip = skip, size = size, margin = margin, neg = neg)
}

#' @param ... further arguments passed to \code{grid.draw}.
#' @export
#' @rdname gggeo_scale
#' @importFrom grid grid.newpage grid.draw
print.geo_scale <- function(geo, ...) {
  grid.newpage()
  grid.draw(geo, ...)
}
