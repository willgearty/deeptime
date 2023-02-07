calculateAxisComponents <- function(...) {
  asNamespace("lattice")$calculateAxisComponents(...)
}

#' Combined wireframe and cloud panel
#'
#' Plots the provided data on 2-D surfaces within a 3-D framework. See
#' [disparity_through_time()].
#' @param x,y,z,groups,subscripts,... Same as for [lattice::panel.cloud()]
#' @return No return value, plots the results of both [lattice::panel.cloud()]
#'   and [lattice::panel.wireframe()].
#' @importFrom lattice panel.wireframe panel.cloud
#' @export
panel.disparity <- function(x, y, z, groups, subscripts, ...) {
  args <- list(...)
  xlabelinfo <- calculateAxisComponents(args$xlim,
    at = args$scales.3d$x.scales$at,
    num.limit = NULL,
    labels = args$scales.3d$x.scales$labels,
    logsc = args$scales.3d$x.scales$log,
    abbreviate = args$scales.3d$x.scales$abbreviate,
    minlength = args$scales.3d$x.scales$minlength,
    format.posixt = args$scales.3d$x.scales$format,
    n = args$scales.3d$x.scales$tick.number
  )
  ylabelinfo <- calculateAxisComponents(args$ylim,
    at = args$scales.3d$y.scales$at,
    num.limit = NULL,
    labels = args$scales.3d$y.scales$labels,
    logsc = args$scales.3d$y.scales$log,
    abbreviate = args$scales.3d$y.scales$abbreviate,
    minlength = args$scales.3d$y.scales$minlength,
    format.posixt = args$scales.3d$y.scales$format,
    n = args$scales.3d$y.scales$tick.number
  )
  zlabelinfo <- calculateAxisComponents(args$zlim,
    at = args$scales.3d$z.scales$at,
    num.limit = NULL,
    labels = args$scales.3d$z.scales$labels,
    logsc = args$scales.3d$z.scales$log,
    abbreviate = args$scales.3d$z.scales$abbreviate,
    minlength = args$scales.3d$z.scales$minlength,
    format.posixt = args$scales.3d$z.scales$format,
    n = args$scales.3d$z.scales$tick.number
  )
  tmp <- expand.grid(x = sort(union(xlabelinfo$at, xlabelinfo$num.limit)),
                     y = sort(union(ylabelinfo$at, ylabelinfo$num.limit)),
                     gr = zlabelinfo$at)
  panel.wireframe(x = tmp$x, y = tmp$y, z = tmp$gr, groups = tmp$gr,
                  subscripts = seq_len(nrow(tmp)), ...)
  panel.cloud(x = x, y = y, z = z, groups = groups,
              subscripts = subscripts, par.box = list("col" = NA), ...)
}

#' Disparity through time plot using lattice
#'
#' Plots points on 2-D surfaces within a a 3-D framework. See
#' [lattice::wireframe()] and [lattice::panel.cloud()] for customization
#' options.
#'
#' @param x a formula (most likely of the form `z ~ x * y`)
#' @param data a data frame in which variables in the formula are to be
#'   evaluated
#' @param groups a variable in `data` to be used as a grouping variable (this is
#'   probably the z variable)
#' @param scales a list specifying how the axes are drawn (see
#'   [lattice::xyplot()] for details)
#' @param pch the point type
#' @param col.point color(s) for points on surfaces
#' @param drape logical, whether the surfaces should be colored based on
#'   `col.regions` and `alpha.regions`
#' @param col.regions color(s) for surfaces
#' @param alpha.regions alpha value(s) for surfaces
#' @param colorkey logical, should a legend be drawn (or a list describing the
#'   legend; see [lattice::levelplot()] for details)
#' @param screen a list of the rotations that should be applied to each axis
#' @param R.mat a transformational matrix that is applied to the orientation of
#'   the axes
#' @param aspect a numeric vector of length 2, giving the relative aspects of
#'   the y-size/x-size and z-size/x-size of the enclosing cube
#' @param perspective logical, whether to plot a perspective view
#' @param par.settings plotting settings (see [lattice::trellis.par.set()])
#' @param lattice.options lattice settings (see [lattice::lattice.options()])
#' @param ... Other arguments passed to [lattice::wireframe()]
#' @return An object of class `"trellis"`, as output by [lattice::wireframe()].
#' @importFrom lattice wireframe
#' @export
#' @examples
#' g <- data.frame(
#'   x = runif(100, 0, 60), y = runif(100, 0, 10),
#'   z = factor(rep(periods$name[1:5], each = 20),
#'     levels = periods$name[1:5]
#'   )
#' )
#' disparity_through_time(z ~ x * y,
#'   data = g, groups = z, aspect = c(1.5, 2),
#'   xlim = c(0, 60), ylim = c(0, 10), col.regions = "lightgreen",
#'   col.point = c("red", "blue")
#' )
disparity_through_time <-
  function(x, data, groups, pch = 16, col.point = c("blue"),
           scales = list(
             arrows = FALSE, distance = 1, col = "black",
             z = list(rot = 90)
           ),
           colorkey = FALSE,
           screen = list(z = 90, x = 70, y = 180), aspect = c(1.5, 4),
           drape = TRUE, col.regions = c("white"), alpha.regions = c(1),
           perspective = FALSE, R.mat = matrix(
             c(
               1, 1, 0, 0,
               0, 1, 0, 0,
               0, 0, 1, 0,
               0, 0, 0, 1
             ),
             4, 4
           ),
           par.settings = list(
             axis.line = list(col = "transparent"),
             layout.heights = list(
               top.padding = 0,
               main.key.padding = 0,
               key.axis.padding = 0,
               axis.xlab.padding = 0,
               xlab.key.padding = 0,
               key.sub.padding = 0,
               bottom.padding = 0
             ),
             layout.widths = list(
               left.padding = 0,
               key.ylab.padding = 0,
               ylab.axis.padding = 0,
               axis.key.padding = 0,
               right.padding = 0
             )
           ),
           lattice.options = list(axis.padding = list(factor = 0)), ...) {
  eval(substitute(wireframe(
    x = x, data = data, groups = groups, pch = pch, col.point = col.point,
    scales = scales, colorkey = colorkey, screen = screen,
    panel = panel.disparity, aspect = aspect, drape = drape,
    col.regions = col.regions, alpha.regions = alpha.regions,
    perspective = perspective, R.mat = R.mat,
    par.settings = par.settings, lattice.options = lattice.options, ...
  )))
}
