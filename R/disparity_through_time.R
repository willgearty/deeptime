## Copied from the lattice package
calculateAxisComponents <- function(x, ...,
           ## ignored, but needs to be caught:
           packet.number, packet.list,
           abbreviate = NULL, minlength = 4) {
  ## This aims to be a general function which given a general
  ## 'range' x and optional at, generates the locations of tick
  ## marks and corresponding labels.

  ## x is guaranteed to be given (possibly NA). Possible cases
  ## correspond to factors (character/expression), shingle (see
  ## below), "POSIXt", "date" and usual numeric. The last case will
  ## be default, and will be changed later if necessary.

  ## Theres no need to handle shingles specially. Shingles can also
  ## be thought of as numeric, and thus the default is more
  ## appropriate for functions like xyplot. In functions like
  ## bwplot, things will be adjusted elsewhere when one of the
  ## variables is a shingle.

  ## Note that at and labels will never be TRUE (it's set up that
  ## way), so it's enough to check if they are is.logical(), which
  ## means they are not explicitly specified.

  ## The variables about log scales are required for cases where at
  ## is explicitly specified. In such cases, at will be
  ## log(at,base=logbase), but labels would correspond to at.
  if (all(is.na(x)))
    return(list(at = numeric(0),
                labels = numeric(0),
                check.overlap = TRUE,
                num.limit = c(0,1)))

  ## Get ticks and labels depending on x (generic defined below)
  ans <- formattedTicksAndLabels(x, ...)

  ## remove labels outside limits
  rng <- range(ans$num.limit)
  ok <- ans$at >= min(rng) & ans$at <= max(rng)
  ans$at <- ans$at[ok]
  ans$labels <- ans$labels[ok]

  ## abbreviate labels if requested

  if (is.logical(abbreviate) && abbreviate)
    ans$labels <- abbreviate(ans$labels, minlength)

  ans
}

#' Combined wireframe and cloud panel
#'
#' Plots provided data on 2-D surfaces within 3-D framework. See \code{\link{disparity_through_time}}.
#' @param x,y,z,groups,subscripts,... Same as for \code{\link[lattice]{panel.cloud}}
#' @importFrom lattice panel.wireframe panel.cloud
#' @export
panel.disparity <- function(x,y,z,groups,subscripts,...) {
  args <- list(...)
  xlabelinfo <- calculateAxisComponents(args$xlim,
                                        at = args$scales.3d$x.scales$at,
                                        num.limit = NULL,
                                        labels = args$scales.3d$x.scales$labels,
                                        logsc = args$scales.3d$x.scales$log,
                                        abbreviate = args$scales.3d$x.scales$abbreviate,
                                        minlength = args$scales.3d$x.scales$minlength,
                                        format.posixt = args$scales.3d$x.scales$format,
                                        n = args$scales.3d$x.scales$tick.number)
  ylabelinfo <- calculateAxisComponents(args$ylim,
                                        at = args$scales.3d$y.scales$at,
                                        num.limit = NULL,
                                        labels = args$scales.3d$y.scales$labels,
                                        logsc = args$scales.3d$y.scales$log,
                                        abbreviate = args$scales.3d$y.scales$abbreviate,
                                        minlength = args$scales.3d$y.scales$minlength,
                                        format.posixt = args$scales.3d$y.scales$format,
                                        n = args$scales.3d$y.scales$tick.number)
  zlabelinfo <- calculateAxisComponents(args$zlim,
                                        at = args$scales.3d$z.scales$at,
                                        num.limit = NULL,
                                        labels = args$scales.3d$z.scales$labels,
                                        logsc = args$scales.3d$z.scales$log,
                                        abbreviate = args$scales.3d$z.scales$abbreviate,
                                        minlength = args$scales.3d$z.scales$minlength,
                                        format.posixt = args$scales.3d$z.scales$format,
                                        n = args$scales.3d$z.scales$tick.number)
  tmp <- expand.grid(x = sort(union(xlabelinfo$at, xlabelinfo$num.limit)), y = sort(union(ylabelinfo$at, ylabelinfo$num.limit)), gr = zlabelinfo$at)
  panel.wireframe(x=tmp$x,y=tmp$y,z=tmp$gr,groups=tmp$gr,subscripts=seq(1:nrow(tmp)),...)
  panel.cloud(x=x,y=y,z=z,groups=groups,subscripts=subscripts,par.box=list("col" = NA),...)
}

#' Disparity through time plot using lattice
#'
#' Plots points on 2-D surfaces within a 3-D framework. See \code{\link[lattice]{wireframe}} and \code{\link[lattice]{panel.cloud}} for customization options.
#'
#' @param x a formula (most likely of the form \code{z ~ x * y})
#' @param data a data frame in which variables in the formula are to be evaluated
#' @param groups a variable in \code{data} to be used as a grouping variable (this is probably the z variable)
#' @param scales a list specifying how the axes are drawn (see \code{\link[lattice]{xyplot}} for details)
#' @param pch the point type
#' @param col.point color(s) for points on surfaces
#' @param drape logical, whether the surfaces should be colored based on \code{col.regions} and \code{alpha.regions}
#' @param col.regions color(s) for surfaces
#' @param alpha.regions alpha value(s) for surfaces
#' @param colorkey logical, should a legend be drawn (or a list describing the legend; see \code{\link[lattice]{levelplot}} for details)
#' @param screen a list of the rotations that should be applied to each axis
#' @param R.mat a transformational matrix that is applied to the orientation of the axes
#' @param aspect a numeric vector of length 2, giving the relative aspects of the y-size/x-size and z-size/x-size of the enclosing cube
#' @param perspective logical, whether to plot a perspective view
#' @param par.settings plotting settings (see \code{\link[lattice]{trellis.par.set}})
#' @param lattice.options lattice settings (see \code{\link[lattice]{lattice.options}})
#' @param ... Other arguments passed to \code{\link[lattice]{wireframe}}
#' @importFrom lattice wireframe
#' @export
#' @examples
#' g <- data.frame(x = runif(100, 0, 60), y = runif(100,0,10), z = factor(rep(periods$name[1:5], each=20), levels = periods$name[1:5]))
#' disparity_through_time(z~x*y, data = g, groups = z, aspect = c(1.5,2), xlim = c(0,60), ylim = c(0,10), col.regions = "lightgreen", col.point = c("red","blue"))
disparity_through_time <- function(x, data, groups, pch = 16, col.point = c("blue"),
                                   scales = list(arrows = FALSE, distance = 1, col = "black",
                                                 z = list(rot = 90)),
                                   colorkey = FALSE, screen = list(z = 90, x = 70, y = 180), aspect = c(1.5,4),
                                   drape = TRUE, col.regions = c("white"), alpha.regions = c(1),
                                   perspective = FALSE, R.mat = matrix(c(1,1,0,0,
                                                                         0,1,0,0,
                                                                         0,0,1,0,
                                                                         0,0,0,1),
                                                                       4,4),
                                   par.settings = list(axis.line = list(col = "transparent"),
                                                       layout.heights = list(
                                                         top.padding = 0,
                                                         main.key.padding = 0,
                                                         key.axis.padding = 0,
                                                         axis.xlab.padding = 0,
                                                         xlab.key.padding = 0,
                                                         key.sub.padding = 0,
                                                         bottom.padding = 0),
                                                       layout.widths = list(
                                                         left.padding = 0,
                                                         key.ylab.padding = 0,
                                                         ylab.axis.padding = 0,
                                                         axis.key.padding = 0,
                                                         right.padding = 0)
                                   ),
                                   lattice.options = list(axis.padding = list(factor = 0)), ...) {
  eval(substitute(wireframe(x = x, data = data, groups = groups, pch = pch, col.point = col.point, scales = scales,
            colorkey = colorkey, screen = screen, panel = panel.disparity, aspect = aspect,
            drape = drape, col.regions = col.regions, alpha.regions = alpha.regions, perspective = perspective,
            R.mat = R.mat, par.settings = par.settings, lattice.options = lattice.options, ...)))
}

