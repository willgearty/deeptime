## declare variables that are used within aes() to prevent
## R CMD check from complaining
utils::globalVariables(c("isTip", "node"))

#' Label nodes on a phylogenetic tree plotted with ggtree
#'
#' This geom adds labels to all or a subset of the nodes of a phylogenetic tree
#' that has been plotted using [ggtree::ggtree()]. It is therefore very similar
#' to [ggtree::geom_tiplab()], [ggtree::geom_tiplab2()],
#' [ggtree::geom_nodelab()], and [ggtree::geom_nodelab2()]. However, unlike
#' those geoms, this geom is intended to work with all coordinate systems,
#' including [coord_geo()] and [coord_geo_radial()].
#'
#' Each label will be plotted with the same angle as the branch/edge leading to
#' its node by default. The `angle`, `hjust`, and `vjust` aesthetics can be used
#' to adjust this. If custom `angle` values are specified, these will be
#' **added** to the default angle as calculated as described above.
#'
#' As with [ggplot2::geom_text()], the limits of axes will not be
#' expanded to accommodate the new labels, so you may need to extend them
#' manually using the `limits` or `expand` arguments within `scale_` or `coord_`
#' functions. Note that [coord_geo()] by default sets `expand = FALSE`.
#'
#' @section Aesthetics: \code{geom_text_phylo()} understands the following
#'   aesthetics (required aesthetics are in bold):
#'
#' - **x** (pulled from the phylogeny by default)
#' - **y** (pulled from the phylogeny by default)
#' - **label** (pulled from the phylogeny by default)
#' - alpha
#' - angle
#' - color/colour
#' - family
#' - fontface
#' - group
#' - hjust
#' - lineheight
#' - size
#' - vjust
#'
#' @inheritSection ggplot2::geom_text Alignment
#' @param node_type Determines the subset of nodes to label. Valid options are
#'   "tip" for tip nodes, "internal" for non-tip nodes, and "all" for all nodes.
#' @param auto_adjust Should upside-down text labels automatically be rotated
#'   180° to improve readability?
#' @importFrom ggplot2 layer position_nudge
#' @importFrom rlang %||%
#' @importFrom utils modifyList
#' @inheritParams ggplot2::geom_text
#' @encoding UTF-8
#' @export
#' @examples
#' library(ggplot2)
#' @examplesIf require(ggtree)
#' library(ape)
#' tr <- rtree(10)
#' ggtree(tr) +
#'   geom_text_phylo() +
#'   coord_geo_radial()
geom_text_phylo <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity", ...,
                            parse = FALSE, nudge_x = 0, nudge_y = 0,
                            node_type = "tip", auto_adjust = TRUE,
                            check_overlap = FALSE, size.unit = "mm",
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE) {
  check_string(node_type)
  node_types <- c("all", "tip", "internal")
  type_match <- pmatch(node_type, node_types)
  if (is.na(type_match)) {
    cli::cli_abort("`node_type` should match or partially match one of 'all',
                   'tip', or 'internal'.")
  }
  node_type <- node_types[type_match]

  check_bool(auto_adjust)

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "Both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied.",
        "i" = "Only use one approach to alter the position."
      ))
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  mapping <- modifyList(aes(isTip = isTip, label = label),
                        mapping %||% list())

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextPhylo,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      node_type = node_type,
      auto_adjust = auto_adjust,
      check_overlap = check_overlap,
      size.unit = size.unit,
      na.rm = na.rm,
      ...
    )
  )
}

#' @importFrom ggplot2 ggproto GeomText aes ggproto_parent
#' @importFrom rlang %||%
GeomTextPhylo <- ggproto("GeomTextPhylo", GeomText,
  required_aes = c("x", "y", "label", "isTip"),
  non_missing_aes = "angle",
  extra_params = c("na.rm", "node_type", "auto_adjust"),

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  setup_data = function(data, params) {
    # subset the nodes as desired
    if (params$node_type == "tip") {
      data <- subset(data, isTip)
    } else if (params$node_type == "internal") {
      data <- subset(data, !isTip)
    }
    data
  },

  draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                        auto_adjust = TRUE, na.rm = FALSE,
                        check_overlap = FALSE, size.unit = "mm") {
    # offset the labels like in ggtree
    x_range <- panel_params$r.range %||% panel_params$x.range
    data$x <- data$x + diff(x_range) / 200

    # transform to calculate theta for polar/radial coordinate systems
    data_transform <- coord$transform(data, panel_params)
    # convert theta to degrees
    data$angle_adj <- (90 - (data_transform$theta %||% (pi / 2) * (180 / pi)))
    # add the adjustment to any existing angle
    data$angle <- data$angle + (data$angle_adj %||% 0) %% 360

    # auto adjust the text direction if desired
    if (auto_adjust) {
      to_flip <- data$angle > 90 & data$angle < 270
      data$angle <- ifelse(to_flip, data$angle + 180, data$angle)
      data$hjust <- ifelse(to_flip, 1 - data$hjust, data$hjust)
      data$vjust <- ifelse(to_flip, 1 - data$vjust, data$vjust)
    }

    # draw the text
    parent <- ggproto_parent(GeomText, self)
    parent$draw_panel(data, panel_params, coord, parse,
                      na.rm, check_overlap, size.unit)
  }
)
