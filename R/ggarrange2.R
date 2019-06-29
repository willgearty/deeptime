#' gtable_frame2
#'
#' @description Reformat the gtable associated with a ggplot object into a 7x7 gtable where the central cell corresponds to the plot panel(s), the rectangle of cells around that corresponds to the axes, and the rectangle of cells around that corresponds to the axis titles.
#' @param g gtable
#' @param width requested width
#' @param height requested height
#' @param debug logical draw gtable cells
#'
#' @importFrom gtable gtable_matrix gtable_add_grob gtable_add_cols gtable_add_rows gtable_filter
#' @importFrom grid unit unit.c nullGrob rectGrob grid.newpage grid.draw
#' @importFrom gridExtra gtable_rbind gtable_cbind
#' @return 7x7 gtable wrapping the plot
#' @export
#' @examples
#' library(grid)
#' library(gridExtra)
#' library(ggplot2)
#' p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point()
#'
#' p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() + facet_wrap( ~ cyl, ncol=2, scales = 'free') +
#'   guides(colour='none') +
#'   theme()
#'
#' p3 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() + facet_grid(. ~ cyl, scales = 'free')
#'
#' g1 <- ggplotGrob(p1);
#' g2 <- ggplotGrob(p2);
#' g3 <- ggplotGrob(p3);
#' fg1 <- gtable_frame2(g1)
#' fg2 <- gtable_frame2(g2)
#' fg12 <- gtable_frame2(gtable_rbind(fg1,fg2), width=unit(2,'null'), height=unit(1,'null'))
#' fg3 <- gtable_frame2(g3, width=unit(1,'null'), height=unit(1,'null'))
#' grid.newpage()
#' combined <- gtable_cbind(fg12, fg3)
#' grid.draw(combined)
gtable_frame2 <- function(g, width = unit(1, "null"), height = unit(1, "null"), debug = FALSE) {
  panels <- g$layout[grepl("panel", g$layout[["name"]]), ]
  ll <- unique(panels$l)
  margins <- if (length(ll) == 1)
    unit(0, "pt") else g$widths[ll[-length(ll)] + 2]
  tt <- unique(panels$t)
  fixed_ar <- g$respect
  if (fixed_ar) {
    # there lies madness, we want to align with aspect ratio constraints
    ar <- as.numeric(g$heights[tt[1]])/as.numeric(g$widths[ll[1]])
    # a*(b-c) != ab - ac in grid...
    height <- width * (ar/length(ll))  # - sum(margins)* (ar / length(ll))
    g$respect <- FALSE
  }

  core <- g[seq(min(tt), max(tt)), seq(min(ll), max(ll))]
  kept_names <- g$layout$name[!grepl("axis|xlab|ylab", g$layout$name)]

  fg <- nullGrob()

  #pull out the top axis, axis title, and other stuff
  top <- g[seq(1, min(tt) - 1), seq(min(ll), max(ll))]
  if (sum(grepl("axis", top$layout$name)) > 0){
    axist <- gtable_filter(top, "axis")
    # add a dummy grob to make sure the axis sticks to the panel
    axist <- gtable_add_grob(gtable_add_rows(axist, unit(1, "null"), 0), fg, t = 1, l = 1)
  } else {
    axist <- fg
  }
  if (sum(grepl("xlab", top$layout$name)) > 0){
    xlabt <- gtable_filter(top, "xlab")
  } else {
    xlabt <- fg
  }
  if (sum(!grepl("axis|xlab", top$layout$name)) > 0){
    top <- gtable_filter(top, paste(kept_names, sep = "", collapse = "|"))
  } else {
    top <- fg
  }

  #pull out the bottom axis, axis title, and other stuff
  bottom <- g[seq(max(tt) + 1, nrow(g)), seq(min(ll), max(ll))]
  if (sum(grepl("axis", bottom$layout$name)) > 0){
    axisb <- gtable_filter(bottom, "axis")
    # add a dummy grob to make sure the axis sticks to the panel
    axisb <- gtable_add_grob(gtable_add_rows(axisb, unit(1, "null"), -1), fg, t = nrow(axisb), l = 1)
  } else {
    axisb <- fg
  }
  if (sum(grepl("xlab", bottom$layout$name)) > 0){
    xlabb <- gtable_filter(bottom, "xlab")
  } else {
    xlabb <- fg
  }
  if (sum(!grepl("axis|xlab", bottom$layout$name)) > 0){
    bottom <- gtable_filter(bottom, paste(kept_names, sep = "", collapse = "|"))
  } else {
    bottom <- fg
  }

  #pull out the left axis, axis title, and other stuff
  left <- g[seq(min(tt), max(tt)), seq(1, min(ll) - 1)]
  if (sum(grepl("axis", left$layout$name)) > 0){
    axisl <- gtable_filter(left, "axis")
    # add a dummy grob to make sure the axis sticks to the panel
    axisl <- gtable_add_grob(gtable_add_cols(axisl, unit(1, "null"), 0), fg, 1, l = 1)
  } else {
    axisl <- fg
  }
  if (sum(grepl("ylab", left$layout$name)) > 0){
    ylabl <- gtable_filter(left, "ylab")
  } else {
    ylabl <- fg
  }
  if (sum(!grepl("axis|ylab", left$layout$name)) > 0){
    left <- gtable_filter(left, paste(kept_names, sep = "", collapse = "|"))
  } else {
    left <- fg
  }

  #pull out the right axis, axis title, and other stuff
  right <- g[seq(min(tt), max(tt)), seq(max(ll) + 1, ncol(g))]
  if (sum(grepl("axis", right$layout$name)) > 0){
    axisr <- gtable_filter(right, "axis")
    # add a dummy grob to make sure the axis sticks to the panel
    axisr <- gtable_add_grob(gtable_add_cols(axisr, unit(1, "null")), fg, 1, l = ncol(axisr))
  } else {
    axisr <- fg
  }
  if (sum(grepl("ylab", right$layout$name)) > 0){
    ylabr <- gtable_filter(right, "ylab")
  } else {
    ylabr <- fg
  }
  if (sum(!grepl("axis|ylab", right$layout$name)) > 0){
    right <- gtable_filter(right, paste(kept_names, sep = "", collapse = "|"))
  } else {
    right <- fg
  }

  # add dummy grobs to make sure the "other stuff" sticks to the internal components
  if (!is(left, "null")) {
    left <- gtable_add_cols(left, unit(1, "null"), 0)
    left <- gtable_add_grob(left, fg, 1, l = 1)
  }
  if (!is(right, "null")) {
    # add a dummy grob to make sure things stick to the panel
    right <- gtable_add_cols(right, unit(1, "null"))
    right <- gtable_add_grob(right, fg, 1, l = ncol(right))
  }
  if (!is(top, "null")) {
    # add a dummy grob to make sure things stick to the panel
    top <- gtable_add_rows(top, unit(1, "null"), 0)
    top <- gtable_add_grob(top, fg, t = 1, l = 1)
  }
  if (!is(bottom, "null")) {
    # add a dummy grob to make sure things stick to the panel
    bottom <- gtable_add_rows(bottom, unit(1, "null"), -1)
    bottom <- gtable_add_grob(bottom, fg, t = nrow(bottom), l = 1)
  }

  ## 7x7 cells (corners contain nullGrob)
  grobs <- list(fg,   fg,    fg,    top,    fg,    fg,    fg,
                fg,   fg,    fg,    xlabt,  fg,    fg,    fg,
                fg,   fg,    fg,    axist,  fg,    fg,    fg,
                left, ylabl, axisl, core,   axisr, ylabr, right,
                fg,   fg,    fg,    axisb,  fg,    fg,    fg,
                fg,   fg,    fg,    xlabb,  fg,    fg,    fg,
                fg,   fg,    fg,    bottom, fg,    fg,    fg)

  widths <- unit.c(if (is(left, "null")) unit(0, "null") else sum(left$widths),
                   if (is(ylabl, "zeroGrob") | is(ylabl, "null")) unit(0, "null") else sum(ylabl$widths),
                   if (is(axisl, "zeroGrob") | is(axisl, "null")) unit(0, "null") else sum(axisl$widths),
                   width,
                   if (is(axisr, "zeroGrob") | is(axisr, "null")) unit(0, "null") else sum(axisr$widths),
                   if (is(ylabr, "zeroGrob") | is(ylabr, "null")) unit(0, "null") else sum(ylabr$widths),
                   if (is(right, "null")) unit(0, "null") else sum(right$widths))
  heights <- unit.c(if (is(top, "null")) unit(0, "null") else sum(top$heights),
                    if (is(xlabt, "zeroGrob") | is(xlabt, "null")) unit(0, "null") else sum(xlabt$heights),
                    if (is(axist, "zeroGrob") | is(axist, "null")) unit(0, "null") else sum(axist$heights),
                    height,
                    if (is(axisb, "zeroGrob") | is(axisb, "null")) unit(0, "null") else sum(axisb$heights),
                    if (is(xlabb, "zeroGrob") | is(xlabb, "null")) unit(0, "null") else sum(xlabb$heights),
                    if (is(bottom, "null")) unit(0, "null") else sum(bottom$heights))
  all <- gtable_matrix("all", grobs = matrix(grobs, ncol = 7, nrow = 7, byrow = TRUE),
                               widths = widths, heights = heights)

  if (debug) {
    hints <- rectGrob(gp = gpar(fill = NA, lty = 2, lwd = 0.2))
    tl <- expand.grid(t = 1:7, l = 1:7)
    all <- gtable_add_grob(all, replicate(49, hints, simplify = FALSE), t = tl$t,
                                   l = tl$l, z = Inf, name = "debug")
  }

  #name grobs
  all$layout[1:49, "name"] <- "null"
  all$layout[c(4,11,18,22,23,24,25,26,27,28,32,39,46), "name"] <- c("left","ylab-l","axis-l","top","xlab-t","axis-t","panel","axis-b","xlab-b","bottom","axis-r","ylab-r","right")

  #move panel to bottom of plot
  all$layout$z[25] <- 0

  if (fixed_ar) {
    all$respect <- TRUE
  }

  all
}

.tmp <- gtable::gtable_matrix("placeholder", matrix(replicate(49, grid::nullGrob(), simplify = FALSE),
                                                    7, 7), widths = rep(unit(1, "null"), 7), heights = rep(unit(1, "null"), 7))
.tmp$layout$name[25] <- "panel"

#' @export
.dummy_gtable <- .tmp

# stolen from grid (because unexported)
as.unit.list <- function(unit) {
  if (inherits(unit, "unit.list")) {
    unit
  } else {
    l <- length(unit)
    result <- vector("list", l)
    for (i in seq_len(l)) result[[i]] <- unit[i]
    class(result) <- c("unit.list", "unit")
    result
  }
}

#' @importFrom grid textGrob
label_grid <- function(labels, x = 0, hjust = 0, y = 1, vjust = 1, ..., .fun = textGrob) {
  lapply(labels, .fun, x = x, hjust = hjust, y = y, vjust = vjust, ...)
}

#' ggarrange2
#'
#' @description Arrange multiple ggplot objects on a page, aligning the plot panels, axes, and axis titles.
#' @param ... ggplot objects
#' @param plots list of ggplots
#' @param nrow number of rows
#' @param ncol number of columns
#' @param heights list of requested heights
#' @param widths list of requested widths
#' @param byrow logical, fill by rows
#' @param top optional string, or grob
#' @param bottom optional string, or grob
#' @param left optional string, or grob
#' @param right optional string, or grob
#' @param padding unit of length one, margin around annotations
#' @param margin unit of length one, margin around entire gtable
#' @param clip argument of gtable
#' @param newpage logical: draw on a new page
#' @param draw logical: draw or return a grob
#' @param debug logical, show layout with thin lines
#' @param labels character labels used for annotation of subfigures
#' @param label.args label list of parameters for the formatting of labels
#' @importFrom grid is.unit is.grob gpar grobHeight grobWidth
#' @importFrom grDevices n2mfrow
#' @importFrom gridExtra gtable_cbind gtable_rbind
#' @importFrom ggplot2 ggplotGrob
#' @importFrom gtable gtable_add_padding
#' @importFrom methods is
#' @return gtable of aligned plots
#' @export
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point()
#' p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() + facet_wrap( ~ cyl, ncol=2, scales = 'free') +
#'   guides(colour='none') +
#'   theme()
#' ggarrange2(p1, p2, widths = c(2,1), labels = c('a', 'b'))
ggarrange2 <- function(..., plots = list(...), nrow = NULL, ncol = NULL, widths = NULL,
                      heights = NULL, byrow = TRUE, top = NULL, bottom = NULL, left = NULL, right = NULL,
                      padding = unit(0.5, "line"), margin = unit(0.5, "line"), clip = "on", draw = TRUE, newpage = TRUE, debug = FALSE,
                      labels = NULL, label.args = list(gp = gpar(font = 4, cex = 1.2))) {
  n <- length(plots)
  grobs <- lapply(plots, ggplotGrob)

  ## logic for the layout if nrow/ncol supplied, honour this if not, use length of
  ## widths/heights, if supplied if nothing supplied, work out sensible defaults

  ## nothing to be done but check inconsistency
  if (!is.null(ncol) && !is.null(widths)) {
    stopifnot(length(widths) == ncol)
  }
  if (!is.null(nrow) && !is.null(heights)) {
    stopifnot(length(heights) == nrow)
  }
  ## use widths/heights if supplied
  if (is.null(ncol) && !is.null(widths)) {
    ncol <- length(widths)
  }
  if (is.null(nrow) && !is.null(heights)) {
    nrow <- length(heights)
  }
  ## work out the missing one
  if (is.null(nrow) && !is.null(ncol)) {
    nrow <- ceiling(n/ncol)
  }
  if (is.null(ncol) && !is.null(nrow)) {
    ncol <- ceiling(n/nrow)
  }

  ## it may happen that sufficient info was passed, but incompatible with number of grobs
  ## (fewer cells)
  stopifnot(nrow * ncol >= n)

  ## last case: nothing exists
  if (is.null(nrow) && is.null(ncol) && is.null(widths) && is.null(heights)) {
    nm <- n2mfrow(n)
    nrow <- nm[1]
    ncol <- nm[2]
  }

  is_full <- ((nrow*ncol) %% n) > 0
  if (is_full) {
    message('adding dummy grobs')
    # trouble, we need to add dummy grobs to fill the layout
    grobs <- c(grobs, rep(list(.dummy_gtable), nrow * ncol - n))
    n <- length(grobs)

    # add dummy labels if needed
    if ((!is.null(labels)) && (length(labels) != nrow * ncol)) {
      labels <- c(labels, rep("", nrow * ncol - length(labels)))
    }
  }

  ## case numeric
  if (is.numeric(widths) && !inherits(widths, "unit")) {
    widths <- lapply(widths, unit, "null")
  }
  if (is.numeric(heights) && !inherits(heights, "unit")) {
    heights <- lapply(heights, unit, "null")
  }

  ## sizes
  if (is.null(widths))
    widths <- lapply(rep(1, n), unit, "null")
  if (is.null(heights))
    heights <- lapply(rep(1, n), unit, "null")

  # user may naively have passed grid units, but only unit.list units work well with `[`
  # so convert to this class
  if (is.unit(widths))
    widths <- as.unit.list(widths)
  if (is.unit(heights))
    widths <- as.unit.list(heights)

  # indexing is problematic, wrap in list
  if (is.unit(widths) && length(widths) == 1) {
    widths <- list(widths)
  }
  if (is.unit(heights) && length(heights) == 1) {
    heights <- list(heights)
  }

  ## split the list into rows/cols
  nrc <- if (byrow)
    nrow else ncol
  if (nrc == 1) {
    splits <- rep(1, n)
  } else {
    seqgrobs <- seq_along(grobs)
    splits <- cut(seqgrobs, nrc, labels = seq_len(nrc))
    ## widths and heights refer to the layout repeat for corresponding grobs

    repw <- rep_len(seq_along(widths), length.out=n)
    reph <- rep_len(seq_along(heights), length.out=n)
    widths <- c(matrix(widths[repw], ncol = nrc, byrow = !byrow))
    heights <- c(matrix(heights[reph], ncol = nrc, byrow = byrow))

  }

  fg <- mapply(gtable_frame2, g = grobs, width = widths, height = heights, MoreArgs = list(debug = debug),
               SIMPLIFY = FALSE)

  if (!is.null(labels)) {
    stopifnot(length(labels) == length(fg))
    # make grobs
    labels <- do.call(label_grid, c(list(labels), label.args))
    # add each grob to the whole gtable
    fg <- mapply(function(g, l) {
      gtable_add_grob(g, l, t = 1, l = 1, b = nrow(g), r = ncol(g), z = Inf,
                      clip = "off", name = "label")
    }, g = fg, l = labels, SIMPLIFY = FALSE)
  }

  spl <- split(fg, splits)
  if (byrow) {
    rows <- lapply(spl, function(.r) do.call(gtable_cbind, .r))
    gt <- do.call(gtable_rbind, rows)
  } else {
    # fill colwise
    cols <- lapply(spl, function(.c) do.call(gtable_rbind, .c))
    gt <- do.call(gtable_cbind, cols)
  }

  ## titles given as strings are converted to text grobs
  if (is.character(top)) {
    top <- textGrob(top)
  }
  if (is.grob(top)) {
    h <- grobHeight(top) + padding
    gt <- gtable_add_rows(gt, heights = h, 0)
    gt <- gtable_add_grob(gt, top, t = 1, l = 1, r = ncol(gt), z = Inf, clip = clip)
  }
  if (is.character(bottom)) {
    bottom <- textGrob(bottom)
  }
  if (is.grob(bottom)) {
    h <- grobHeight(bottom) + padding
    gt <- gtable_add_rows(gt, heights = h, -1)
    gt <- gtable_add_grob(gt, bottom, t = nrow(gt), l = 1, r = ncol(gt), z = Inf, clip = clip)
  }
  if (is.character(left)) {
    left <- textGrob(left, rot = 90)
  }
  if (is.grob(left)) {
    w <- grobWidth(left) + padding
    gt <- gtable_add_cols(gt, widths = w, 0)
    gt <- gtable_add_grob(gt, left, t = 1, b = nrow(gt), l = 1, r = 1, z = Inf, clip = clip)
  }
  if (is.character(right)) {
    right <- textGrob(right, rot = -90)
  }
  if (is.grob(right)) {
    w <- grobWidth(right) + padding
    gt <- gtable_add_cols(gt, widths = w, -1)
    gt <- gtable_add_grob(gt, right, t = 1, b = nrow(gt), l = ncol(gt), r = ncol(gt),
                          z = Inf, clip = clip)
  }

  gt <- gtable_add_padding(gt, margin)

  if (draw) {
    if (newpage)
      grid.newpage()
    grid.draw(gt)
  }
  class(gt) <- c("ggarrange2", class(gt))
  invisible(gt)  # return the full gtable
}

##' @noRd
##' @importFrom grDevices dev.interactive dev.new
##' @export
print.ggarrange2 <- function(x, ...) {
  grid.newpage()
  grid.draw(x)
}
