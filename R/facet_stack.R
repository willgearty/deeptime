#' Stack panels as layers in a pseudo-3D block
#'
#' `facet_stack` arranges panels in a single column like [ggplot2::facet_wrap()]
#' with `ncol = 1`, but draws them as stacked layers of a pseudo-3D block. It is
#' designed to be used with [coord_trans_xy()]: each panel is drawn as the
#' transformed (e.g., sheared) plane of the plotting area, vertical edges are
#' drawn between the corners of adjacent planes, and strips are drawn on the
#' right side of each plane.
#'
#' @details The vertical distance between planes is controlled by the
#'   `panel.spacing.y` theme element. Negative values will cause the planes to
#'   overlap. Planes are drawn from the bottom up, so an opaque
#'   `panel.background` fill will hide whatever is behind (i.e., below) it.
#'
#' @param edges Logical. Should vertical edges be drawn between the corners of
#'   adjacent planes? The edges use the `panel.border` theme element if it is
#'   not blank, otherwise they use `axis.line`.
#' @inheritParams ggplot2::facet_wrap
#' @importFrom ggplot2 ggproto facet_wrap
#' @family faceting functions
#' @export
#' @examples
#' library(ggplot2)
#' library(ggforce)
#' df <- data.frame(x = runif(60), y = runif(60),
#'                  time = rep(c("before", "during", "after"), each = 20))
#' df$time <- factor(df$time, levels = c("after", "during", "before"))
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   coord_trans_xy(trans = linear_trans(shear(1, 0))) +
#'   facet_stack(vars(time)) +
#'   theme_classic() +
#'   theme(panel.border = element_rect(fill = NA))
facet_stack <- function(facets, labeller = "label_value", drop = TRUE,
                        edges = TRUE) {
  check_bool(edges)
  # reuse facet_wrap's layout and data mapping; only drawing is customized
  fw <- facet_wrap(facets, ncol = 1, labeller = labeller, drop = drop,
                   strip.position = "right")
  params <- fw$params
  params$edges <- edges
  ggproto(NULL, FacetStack, shrink = fw$shrink, params = params)
}

#' @rdname facet_stack
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto FacetWrap calc_element render_strips zeroGrob
#' @importFrom ggplot2 element_blank theme
#' @importFrom grid unit gTree gList viewport polygonGrob segmentsGrob gpar
#' @importFrom grid grobWidth grobHeight unit.c
#' @importFrom gtable gtable gtable_add_grob
#' @export
FacetStack <- ggproto("FacetStack", FacetWrap,
  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges,
                         coord, data, theme, params) {
    if (!inherits(coord, "CoordTransXY")) {
      cli::cli_abort("{.fn facet_stack} only works with
                     {.fn coord_trans_xy}.")
    }
    n <- nrow(layout)
    # bottom panel first so that higher planes are drawn on top
    ord <- order(layout$ROW, decreasing = TRUE)
    layout <- layout[ord, , drop = FALSE]
    panels <- panels[ord]
    ranges <- ranges[layout$PANEL]

    gap <- calc_element("panel.spacing.y", theme) %||% unit(5.5, "pt")
    h <- (unit(1, "npc") - (n - 1) * gap) * (1 / n)
    ys <- lapply(seq_len(n) - 1, function(i) i * (h + gap))
    layer_vp <- function(i) viewport(y = ys[[i]], height = h, just = "bottom")

    # plane outlines in panel npc, in the order of the data corners
    corners <- lapply(ranges, function(r) plane_corners(coord, r))

    bg_el <- calc_element("panel.background", theme)
    border_el <- calc_element("panel.border", theme)
    edge_el <- if (inherits(border_el, "element_blank")) {
      calc_element("axis.line", theme)
    } else {
      border_el
    }
    # backgrounds and borders are redrawn as planes
    theme_bg <- theme + theme(panel.background = element_blank(),
                              panel.border = element_blank())

    grobs <- list()
    for (i in seq_len(n)) {
      layers <- panels[[i]]$children
      layers <- layers[-c(1, length(layers))]
      plane <- corners[[i]]
      grobs[[length(grobs) + 1]] <- gTree(
        children = gList(
          plane_grob(plane, bg_el, fill = TRUE),
          coord$render_bg(ranges[[i]], theme_bg),
          inject(gList(!!!layers)),
          plane_grob(plane, border_el, fill = FALSE)
        ),
        vp = layer_vp(i)
      )
      # edges up to the next plane are drawn above this plane but below the next
      if (params$edges && i < n && !inherits(edge_el, "element_blank")) {
        lo <- corners[[i]]
        hi <- corners[[i + 1]]
        grobs[[length(grobs) + 1]] <- segmentsGrob(
          x0 = unit(lo$x, "npc"), y0 = ys[[i]] + h * lo$y,
          x1 = unit(hi$x, "npc"), y1 = ys[[i + 1]] + h * hi$y,
          gp = edge_gpar(edge_el)
        )
      }
    }
    stack <- gTree(children = inject(gList(!!!grobs)))

    # strips on the right, one per plane
    labels <- layout[names(params$facets)]
    attr(labels, "facet") <- "wrap"
    strips <- render_strips(y = labels, labeller = params$labeller,
                            theme = theme)$y$right
    strip_col <- gTree(children = inject(gList(!!!lapply(seq_len(n), function(i) {
      gTree(children = gList(strips[[i]]), vp = layer_vp(i))
    }))))
    strip_width <- stack_max_width(strips)

    # y axes for each plane, x axis for the bottom plane only
    axes_y <- lapply(ranges, function(r) coord$render_axis_v(r, theme)$left)
    axis_y_col <- gTree(children = inject(gList(!!!lapply(seq_len(n), function(i) {
      gTree(children = gList(axes_y[[i]]), vp = layer_vp(i))
    }))))
    axis_x <- coord$render_axis_h(ranges[[1]], theme)$bottom

    strip_pad <- calc_element("strip.switch.pad.wrap", theme) %||% unit(0, "pt")
    table <- gtable(
      widths = unit.c(stack_max_width(axes_y), unit(1, "null"), strip_pad,
                      strip_width),
      heights = unit.c(unit(1, "null"), grobHeight(axis_x))
    )
    table <- gtable_add_grob(table, stack, t = 1, l = 2, clip = "off",
                             name = "panel-1-1")
    table <- gtable_add_grob(table, axis_y_col, t = 1, l = 1, clip = "off",
                             name = "axis-l-1-1")
    table <- gtable_add_grob(table, axis_x, t = 2, l = 2, clip = "off",
                             name = "axis-b-1-1")
    table <- gtable_add_grob(table, strip_col, t = 1, l = 4, clip = "off",
                             name = "strip-r-1")
    table
  }
)

# transformed corners of a panel's plotting area, in npc
plane_corners <- function(coord, panel_params) {
  xr <- panel_params$x$limits
  yr <- panel_params$y$limits
  df <- data.frame(x = xr[c(1, 2, 2, 1)], y = yr[c(1, 1, 2, 2)])
  out <- coord$transform(df, panel_params)
  out[c("x", "y")]
}

plane_grob <- function(plane, el, fill = TRUE) {
  if (inherits(el, "element_blank")) return(zeroGrob())
  polygonGrob(plane$x, plane$y, gp = gpar(
    fill = if (fill) el$fill else NA,
    col = el$colour,
    lwd = (el$linewidth %||% 0.5) * ggplot2::.pt,
    lty = el$linetype %||% 1
  ))
}

edge_gpar <- function(el) {
  gpar(col = el$colour, lwd = (el$linewidth %||% 0.5) * ggplot2::.pt,
       lty = el$linetype %||% 1)
}

stack_max_width <- function(grobs) {
  inject(max(!!!lapply(grobs, grobWidth)))
}
