#' Add a geologic scale to ggplots
#'
#' This function takes a ggplot object and adds a geologic time scale at the specified side.
#'
#' If custom data is provided (with \code{dat}), it should consist of at least 3 columns of data. See \code{data(periods)} for an example.
#'   The \code{name} column lists the names of each time interval. These will be used as labels if no abbreviations are provided.
#'   The \code{max_age} column lists the oldest boundary of each time interval.
#'   The \code{min_age} column lists the youngest boundary of each time interval.
#'   The \code{abbr} column is optonal and lists abbreviations that may be used as labels.
#'   The \code{color} column is also optional and lists a hex color code (which can be obtained with \code{rgb()}) for each time interval.
#' @param gg The ggplot object.
#' @param dat Either A) a string indicating a built-in dataframe with interval data (periods, epochs, or stages)
#'   or B) a custom dataframe of time interval boundaries (see Details).
#' @param fill The fill color of the boxes. The default is to use the colors included in \code{dat}.
#'   If a custom dataset is provided with dat without color and without fill, a greyscale will be used
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
#' @examples
#' library(ggplot2)
#' # bottom scale by default
#' p <- ggplot() +
#'   geom_point(aes(y = runif(1000, .5, 8), x = runif(1000, 0, 1000))) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(0, 1000), ylim = c(0,8), expand = FALSE) +
#'   theme_classic()
#' gggeo_scale(p)
#'
#' # can specify any side of the plot
#' p <- ggplot() +
#'  geom_point(aes(x = runif(1000, .5, 8), y = runif(1000, 0, 1000))) +
#'  scale_y_reverse() +
#'  coord_cartesian(xlim = c(0, 8), ylim = c(0,1000), expand = FALSE) +
#'  theme_classic()
#' gggeo_scale(p, pos = "left")
#'
#' # can add multiple scales
#' p <- ggplot() +
#'   geom_point(aes(y = runif(1000, 1, 8), x = runif(1000, 0, 1000))) +
#'   scale_x_reverse() +
#'   coord_cartesian(xlim = c(0, 1000), ylim = c(0,8), expand = FALSE) +
#'   theme_classic()
#' p <- gggeo_scale(p, height = .03)
#' p <- gggeo_scale(p, gap = .03, height = .03, dat = "epochs")
#' gggeo_scale(p, gap = .06, height = .03, dat = "stages")
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
#' gggeo_scale(p, neg = TRUE)
gggeo_scale <- function(gg, dat = "periods", fill = NULL, color = "black", alpha = 1, height = .05, gap = 0, pos = "bottom", lab = TRUE, rot = 0, abbrv = TRUE, skip = c("Quaternary", "Holocene"), size = 5, neg = FALSE) {
  if(dat == "periods"){
    dat <- periods
  }else if(dat == "epochs"){
    dat <- epochs
  }else if(dat == "stages"){
    dat <- stages
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
  lims <- ggplot2::ggplot_build(gg)$layout$panel_params[[1]]
  if(abbrv & "abbr" %in% colnames(dat)){
    dat$label <- dat$abbr
  }else{
    dat$label <- dat$name
  }
  dat$label[dat$name %in% skip] <- ""
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
      ggplot2::annotate("rect", xmin = dat$min_age, xmax = dat$max_age, ymin = ymin, ymax = ymax,
                        fill = dat$color, color = color, alpha = alpha)
    if(lab){
      gg <- gg + ggplot2::annotate("text", x = dat$mid_age, label = dat$label, y = (ymin+ymax)/2,
                                   vjust = "middle", hjust = "middle", size = size, angle = rot)
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
      ggplot2::annotate("rect", ymin = dat$min_age, ymax = dat$max_age, xmin = xmin, xmax = xmax,
                        fill = dat$color, color = color, alpha = alpha)
    if(lab){
      gg <- gg + ggplot2::annotate("text", y = dat$mid_age, label = dat$label, x = (xmin+xmax)/2,
                                   vjust = "middle", hjust = "middle", size = size, angle = 90 + rot)
    }
  }
  gg
}
