#' Add a geologic scale to ggplots (old version)
#'
#' This function takes a ggplot object and adds a geologic time scale at the specified side.
#'
#' If custom data is provided (with \code{dat}), it should consist of at least 3 columns of data. See \code{data(periods)} for an example.
#'   The \code{name} column lists the names of each time interval. These will be used as labels if no abbreviations are provided.
#'   The \code{max_age} column lists the oldest boundary of each time interval.
#'   The \code{min_age} column lists the youngest boundary of each time interval.
#'   The \code{abbr} column is optional and lists abbreviations that may be used as labels.
#'   The \code{color} column is also optional and lists a hex color code (which can be obtained with \code{rgb()}) for each time interval.
#' @param gg The ggplot object.
#' @param dat Either A) a string indicating a built-in dataframe with interval data from the ICS ("periods", "epochs", "stages", "eons", or "eras"),
#'   B) a string indicating a timescale from macrostrat (see list here: \url{https://macrostrat.org/api/defs/timescales?all}),
#'   or C) a custom dataframe of time interval boundaries (see Details).
#' @param fill The fill color of the boxes. The default is to use the colors included in \code{dat}.
#'   If a custom dataset is provided with \code{dat} without color and without fill, a greyscale will be used.
#'   Custom fill colors can be provided with this option and will be recycled if/as necessary.
#' @param color The outline color of the interval boxes.
#' @param alpha The transparency of the fill colors.
#' @param height The proportional height (or width if \code{pos} is \code{left} or \code{right}) of the entire plot to use for the scale.
#' @param gap The proportional height (or width) of the entire plot to use as a gap between the axis and the scale.
#' @param pos Which side to add the scale to (left, right, top, or bottom). First letter may also be used.
#' @param lab Whether to include labels.
#' @param rot The amount of counter-clockwise rotation to add to the labels (in degrees).
#' @param abbrv If including labels, whether to use abbreviations instead of full interval names.
#' @param skip A vector of interval names indicating which intervals should not be labelled.
#' @param size Label size.
#' @param neg Set this to true if your x-axis is using negative values.
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot geom_rect geom_text aes scale_fill_manual ggplot_build
#' @importFrom ggnewscale new_scale_fill
#' @importFrom stats setNames
#' @examples
#' library(ggplot2)
#' # bottom scale by default
#' p <- ggplot() +
#'   geom_point(aes(y = runif(1000, .5, 8), x = runif(1000, 0, 1000))) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(0, 1000), ylim = c(0,8), expand = FALSE) +
#'   theme_classic()
#' gggeo_scale_old(p)
#'
#' # can specify any side of the plot
#' p <- ggplot() +
#'  geom_point(aes(x = runif(1000, .5, 8), y = runif(1000, 0, 1000))) +
#'  scale_y_reverse() +
#'  coord_cartesian(xlim = c(0, 8), ylim = c(0,1000), expand = FALSE) +
#'  theme_classic()
#' gggeo_scale_old(p, pos = "left", rot = 90)
#'
#' # can add multiple scales
#' p <- ggplot() +
#'   geom_point(aes(y = runif(1000, 1, 8), x = runif(1000, 0, 1000))) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(0, 100), ylim = c(0,8), expand = FALSE) +
#'   theme_classic()
#' p <- gggeo_scale_old(p, height = .03, abbrv = FALSE)
#' p <- gggeo_scale_old(p, dat = "epochs", gap = .03, height = .1, rot = 90, size = 2.5, abbrv = FALSE)
#'
#' # intervals on both sides for different timescales (ICS stages vs North American Land Mammal Ages)
#' p <- ggplot() +
#'   geom_point(aes(x = runif(1000, 1, 9), y = runif(1000, 0, 65))) +
#'   scale_y_reverse() +
#'   coord_cartesian(xlim = c(0, 10), ylim = c(0,65), expand = FALSE) +
#'   theme_classic()
#' p <- gggeo_scale_old(p, dat = "stages", pos = "left", height = .1, size = 2.5, abbrv = FALSE)
#' gggeo_scale_old(p, dat = "North American Land Mammal Ages", pos = "right", height = .1, size = 2.5,
#'                 abbrv = FALSE)
#'
#' #can add scales to a faceted plot
#' df <- data.frame(x = runif(1000,0,541), y = runif(1000,.5,8), z = sample(c(1,2,3,4), 1000, TRUE))
#' p <- ggplot(df) +
#'   geom_point(aes(x, y)) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(0, 541), ylim = c(0,8), expand = FALSE) +
#'   theme_classic() +
#'   facet_wrap(~z, nrow = 2)
#' gggeo_scale_old(p)
#'
#' #can even add a scale to a phylogeny (using ggtree)
#' library(phytools)
#' library(ggtree)
#' tree <- pbtree(b = .03, d = .01,  n=100)
#' p <- ggtree(tree) +
#'  coord_cartesian(xlim = c(0,-500), ylim = c(-10,Ntip(tree)), expand = FALSE) +
#'  scale_x_continuous(breaks=seq(-500,0,100), labels=abs(seq(-500,0,100))) +
#'  theme_tree2()
#' p <- revts(p)
#' gggeo_scale_old(p, neg = TRUE)
gggeo_scale_old <- function(gg, dat = "periods", fill = NULL, color = "black", alpha = 1, height = .05, gap = 0, pos = "bottom", lab = TRUE, rot = 0, abbrv = TRUE, skip = c("Quaternary", "Holocene", "Late Pleistocene"), size = 5, neg = FALSE) {
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
  lims <- ggplot_build(gg)$layout$panel_params[[1]]
  if(abbrv & "abbr" %in% colnames(dat)){
    dat$label <- dat$abbr
  }else{
    dat$label <- dat$name
  }
  dat$label[dat$name %in% skip] <- ""
  gg <- gg + new_scale_fill()
  if(pos %in% c("bottom", "top", "b", "t")){
    y.range <- max(lims$y.range) - min(lims$y.range)
    if(pos %in% c("top","t")){
      ymax <- max(lims$y.range) - gap * y.range
      ymin <- max(lims$y.range) - (height + gap) * y.range
    }else{
      ymin <- min(lims$y.range) + gap * y.range
      ymax <- min(lims$y.range) + (height + gap) * y.range
    }
    gg <- gg +
      geom_rect(data = dat, aes(xmin = min_age, xmax = max_age, fill = color),
                         ymin = ymin, ymax = ymax, color = color, alpha = alpha, show.legend = FALSE,
                         inherit.aes = FALSE) +
      scale_fill_manual(values = setNames(dat$color, dat$color))
    if(lab){
      gg <- gg +
        geom_text(data = dat, aes(x = mid_age, label = label), y = (ymin+ymax)/2,
                           vjust = "middle", hjust = "middle", size = size, angle = rot,
                           inherit.aes = FALSE)
    }
  }else if(pos %in% c("left", "right","l","r")){
    x.range <- max(lims$x.range) - min(lims$x.range)
    if(pos %in% c("right","r")){
      xmax <- max(lims$x.range) - gap * x.range
      xmin <- max(lims$x.range) - (height + gap) * x.range
    }else{
      xmin <- min(lims$x.range) + gap * x.range
      xmax <- min(lims$x.range) + (height + gap) * x.range
    }
    gg <- gg +
      geom_rect(data = dat, aes(ymin = min_age, ymax = max_age, fill = color),
                         xmin = xmin, xmax = xmax, color = color, alpha = alpha, show.legend = FALSE,
                         inherit.aes = FALSE) +
      scale_fill_manual(values = setNames(dat$color, dat$color))
    if(lab){
      gg <- gg +
        geom_text(data = dat, aes(y = mid_age, label = label), x = (xmin+xmax)/2,
                           vjust = "middle", hjust = "middle", size = size, angle = rot,
                           inherit.aes = FALSE)
    }
  }
  gg
}
