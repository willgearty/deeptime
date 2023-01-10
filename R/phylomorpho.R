#' Plot a 2-D phylomorphospace in ggplot2
#'
#' This behaves similar to \code{\link[phytools]{phylomorphospace}}, but is
#' for plotting a 2-D phylomorphospace with \code{\link[ggplot2]{ggplot}}.
#' This function works like any other \code{ggplot2} geom; it can be
#' combined with other geoms (see the example below), and the output can be
#' modified using scales, themes, etc.
#'
#' The ancestral states are estimated using \code{\link[phytools]{fastAnc}}.
#' The nodes are connected using \code{\link[ggplot2]{geom_segment}},
#' while the tips are indicated using \code{\link[ggplot2]{geom_point}}.
#'
#' The default expectation is that the order of the data is the same order as
#' the tip labels of the tree (\code{tree$tip.label}). However, if this is not
#' the case, you can map the optional \code{label} aesthetic to a column in the
#' data that contains the tip names (see example below).
#'
#' @param tree An object of class "phylo".
#' @param seg_args A list of arguments passed only to
#'   \code{\link[ggplot2]{geom_segment}}.
#' @param point_args A list of arguments passed only to
#'   \code{\link[ggplot2]{geom_point}}.
#' @param ... Other arguments passed on to both
#'   \code{\link[ggplot2]{geom_segment}} and \code{\link[ggplot2]{geom_point}}.
#' @importFrom ggplot2 layer
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @export
#' @examples
#' library(ggplot2)
#' @examplesIf require(ape)
#' library(ape)
#' tr <- rtree(10)
#' dat <- data.frame(x = runif(10), y = runif(10), label = tr$tip.label,
#'                   row.names = tr$tip.label)
#' ggplot(dat, aes(x = x, y = y, label = label)) +
#'   geom_phylomorpho(tr) +
#'   geom_label(size = 5)
geom_phylomorpho <- function(tree, mapping = NULL, data = NULL,
                             position = "identity", ...,
                             seg_args = list(), point_args = list(),
                             arrow = NULL, arrow.fill = NULL,
                             lineend = "butt", linejoin = "round",
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE) {
  mapping2 <- mapping
  mapping2$label <- NULL
  list(
    layer(
      data = data,
      mapping = mapping,
      stat = StatPhylomorpho,
      geom = "segment",
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = c(list(
        tree = tree,
        arrow = arrow,
        arrow.fill = arrow.fill,
        lineend = lineend,
        linejoin = linejoin,
        na.rm = na.rm,
        ...), seg_args
      )
    ),
    layer(
      data = data,
      mapping = mapping2,
      stat = "identity",
      geom = "point",
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = c(list(
        na.rm = na.rm,
        ...), point_args
      )
    )
  )
}

#' @importFrom ggplot2 ggproto Stat
#' @importFrom phytools fastAnc
StatPhylomorpho <- ggproto("StatPhylomorpho", Stat,
  required_aes = c("x", "y"),
  optional_aes = c("label"),
  compute_group = function(data, params) {
    data
  },
  compute_panel = function(self, data, scales, params, tree) {
    if(nrow(data) != length(tree$tip.label)) stop("X must contain the same
                                                  number of rows as species in
                                                  tree.")
    if ("label" %in% colnames(data)) {
      rownames(data) <- data$label
      data$label <- NULL
    } else {
      rownames(data) <- tree$tip.label
    }
    # copied from phytools
    A <- apply(data, 2, fastAnc, tree = tree)
    aa <- setNames(c(data[tree$tip.label, "x"], A[, 1]),
                   c(1:length(tree$tip.label), rownames(A)))
    bb <- setNames(c(data[tree$tip.label, "y"], A[, 2]),
                   c(1:length(tree$tip.label), rownames(A)))
    XX <- matrix(aa[as.character(tree$edge)], nrow(tree$edge), 2)
    YY <- matrix(bb[as.character(tree$edge)], nrow(tree$edge), 2)
    xx <- setNames(c(XX[1, 1], XX[, 2]), c(tree$edge[1, 1], tree$edge[, 2]))
    xx <- xx[order(as.numeric(names(xx)))]
    yy <- setNames(c(YY[1, 1],YY[, 2]), c(tree$edge[1, 1], tree$edge[, 2]))
    yy <- yy[order(as.numeric(names(yy)))]
    df <- data.frame(x = xx[tree$edge[, 1]], xend = xx[tree$edge[, 2]],
                     y = yy[tree$edge[, 1]], yend = yy[tree$edge[, 2]])
    return(df)
  }
)
