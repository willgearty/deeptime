#' Add a geologic scale to ggplots
#'
#' This function takes a ggplot object and adds a geologic time scale at the specified side.
#'
#' If custom data is provided (with \code{dat}), it should consist of at least 3 columns of data. See \code{data(dat)} for an example.
#'   The \code{name} column lists the names of each time interval. These will be used as labels if no abbreviations are provided.
#'   The \code{max_age} column lists the oldest boundary of each time interval.
#'   The \code{min_age} column lists the youngest boundary of each time interval.
#'   The \code{abbr} column is optonal and lists abbreviations that may be used as labels.
#'   The \code{color} column is also optional and lists a hex color code (which can be obtained with \code{rgb()}) for each time interval.
#' @param gg The ggplot object.
#' @param fill The fill color of the boxes. The default is to use the colors included in \code{dat}.
#'   If a custom dataset is provided with dat without color and without fill, a greyscale will be used
#'   Custom fill colors can be provided with this option and will be recycled if/as necessary.
#' @param color The outline color of the interval boxes.
#' @param alpha The transparency of the fill colors.
#' @param height The proportional height (or width if \code{pos} is \code{left} or \code{right}) of the entire plot to use for the scale.
#' @param size Label size.
#' @param quat Specifies whether the Quaternary should be labelled (if using the default data).
#' @param pos Which side to add the scale to (left, right, top, or bottom). First letter may also be used.
#' @param abbrv Whether to use abbreviations instead of full interval names.
#' @param dat A custom data set of time interval boundaries (see Details).
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
#' #can even add a scale to a phylogeny (using ggtree)
#' library(phytools)
#' library(ggtree)
#' tree <- pbtree(b = .03, d = .01,  n=100)
#' p <- ggtree(tree) +
#'  coord_cartesian(xlim = c(0,-500), ylim = c(-10,Ntip(tree)), expand = FALSE) +
#'  scale_x_continuous(breaks=seq(-300,0,100), labels=abs(seq(-300,0,100))) +
#'  theme_tree2()
#' p <- revts(p)
#' gggeo_scale(p, neg = TRUE)
gggeo_scale <- function(gg, fill = NULL, color = "black", alpha = 1, height = .05, size = 5, quat = FALSE, pos = "bottom", abbrv = TRUE, dat = NULL, neg = FALSE) {
  if(is.null(dat)){
    dat <- periods
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
  if(!quat){
    dat$label[dat$abbr=="Q"] <- ""
  }
  if(pos %in% c("bottom", "top", "b", "t")){
    if(pos %in% c("top","t")){
      ymax <- max(lims$y.range)
      ymin <- max(lims$y.range) - height * (max(lims$y.range) - min(lims$y.range))
    }else{
      ymin <- min(lims$y.range)
      ymax <- min(lims$y.range) + height * (max(lims$y.range) - min(lims$y.range))
    }
    gg <- gg +
      ggplot2::annotate("rect", xmin = dat$min_age, xmax = dat$max_age, ymin = ymin, ymax = ymax,
               fill = dat$color, color = color, alpha = alpha) +
      ggplot2::annotate("text", x = dat$mid_age, label = dat$label, y = (ymin+ymax)/2,
               vjust = "middle", hjust = "middle", size = size)
  }else if(pos %in% c("left", "right","l","r")){
    if(pos %in% c("right","r")){
      xmax <- max(lims$x.range)
      xmin <- max(lims$x.range) - height * (max(lims$x.range) - min(lims$x.range))
    }else{
      xmin <- min(lims$x.range)
      xmax <- min(lims$x.range) + height * (max(lims$x.range) - min(lims$x.range))
    }
    gg <- gg +
      ggplot2::annotate("rect", ymin = dat$min_age, ymax = dat$max_age, xmin = xmin, xmax = xmax,
               fill = dat$color, color = color, alpha = alpha) +
      ggplot2::annotate("text", y = dat$mid_age, label = dat$label, x = (xmin+xmax)/2,
               vjust = "middle", hjust = "middle", size = size, angle = 90)
  }
  gg
}
