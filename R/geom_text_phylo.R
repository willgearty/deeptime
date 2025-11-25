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
#' @param position A position adjustment to use on the data for this layer. This
#'   can be used in various ways, including to prevent overplotting and
#'   improving the display. The `position` argument accepts the following:
#'   * The result of calling a position function, such as `position_nudge()`.
#'     This method allows for passing extra arguments to the position.
#'   * A string naming the position adjustment. To give the position as a
#'     string, strip the function name of the `position_` prefix. For example,
#'     to use `position_nudge()`, give the position as `"nudge"`.
#' @param stat The statistical transformation to use on the data for this layer.
#'   When using a `geom_*()` function to construct a layer, the `stat`
#'   argument can be used the override the default coupling between geoms and
#'   stats. The `stat` argument accepts the following:
#'   * A `Stat` ggproto subclass, for example `StatCount`.
#'   * A string naming the stat. To give the stat as a string, strip the
#'     function name of the `stat_` prefix. For example, to use `stat_count()`,
#'     give the stat as `"count"`.
#' @param node_type Determines the subset of nodes to label. Valid options are
#'   "tip" for tip nodes, "internal" for non-tip nodes, and "all" for all nodes.
#' @param auto_adjust Should upside-down text labels automatically be rotated
#'   180° to improve readability?
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#'   Cannot be jointly specified with `position`.
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
#' library(ggtree)
#' tr <- rtree(10)
#' revts(ggtree(tr)) +
#'   geom_text_phylo() +
#'   coord_geo_radial("epochs")
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

#' @importFrom ggplot2 ggproto GeomPoint GeomText ggproto_parent
#' @importFrom rlang %||%
#' @importFrom utils modifyList
GeomTextPhylo <- ggproto("GeomTextPhylo", GeomText,
  required_aes = c("x", "y", "label", "isTip"),
  non_missing_aes = "angle",
  extra_params = c("na.rm", "node_type", "auto_adjust"),

  default_aes = modifyList(GeomPoint$default_aes,
                           list(hjust = 0, vjust = 0.5, size = 3.88, angle = 0),
                           keep.null = T),

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


#' Label clades on a phylogenetic tree plotted with ggtree
#'
#' This geom adds labels and corresponding vertical bars to specified clades of
#' a phylogenetic tree that has been plotted using [ggtree::ggtree()]. It is
#' therefore very similar to [ggtree::geom_cladelab()]. However, unlike
#' [ggtree::geom_cladelab()], this geom is intended to work with all coordinate
#' systems, including [coord_geo()] and [coord_geo_radial()].
#'
#' The clades to be labeled are specified using the `node` aesthetic which
#' identifies the most recent common ancestor of the clade. The `label`
#' aesthetic specifies the text label for each clade. The [ggfun::%<+%()]
#' operator should be used to combine custom clade labels with the tree (see
#' Examples). If no nodes are specified, a label will be added to every tip
#' by default (like [geom_text_phylo()]).
#'
#' The vertical bar for each clade extends from the minimum to the maximum
#' y-values of all descendant nodes of the specified `node`, with optional
#' extensions above and below these values controlled by the `extend` parameter.
#' Each label will be plotted center aligned and perpendicular to its
#' corresponding bar by default. The `angle`, `hjust`, and `vjust` aesthetics
#' can be used to adjust this. If custom `angle` values are specified, these
#' will be **added** to the default angle as calculated as described above.
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
#' - **parent** (pulled from the phylogeny by default)
#' - **node** (pulled from the phylogeny by default)
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
#' @section Alignment:
#' You can modify text alignment with the `vjust` and `hjust` aesthetics. These
#' can either be a number between 0 (left/bottom) and 1 (right/top) or a
#' character (`"left"`, `"middle"`, `"right"`, `"bottom"`, `"center"`, `"top"`).
#' There are two special alignments: `"inward"` and `"outward"`. Inward always
#' aligns text towards the center, and outward aligns it away from the center.
#' Note that numeric values outside of \[0, 1\] will also work and will move the
#' text beyond the normal alignment positions (e.g., the default `hjust` value
#' is -0.02).
#' @param position A position adjustment to use on the data for this layer. This
#'   can be used in various ways, including to prevent overplotting and
#'   improving the display. The `position` argument accepts the following:
#'   * The result of calling a position function, such as `position_nudge()`.
#'     This method allows for passing extra arguments to the position.
#'   * A string naming the position adjustment. To give the position as a
#'     string, strip the function name of the `position_` prefix. For example,
#'     to use `position_nudge()`, give the position as `"nudge"`.
#' @param text_geom Which geom to use for the text labels. Valid options are
#'  "text" for [ggplot2::geom_text()] and "label" for [ggplot2::geom_label()].
#' @param auto_adjust Should upside-down text labels automatically be rotated
#'   180° to improve readability?
#' @param extend A numeric vector of length 2 indicating how much to extend the
#'   vertical bar beyond the minimum and maximum y-values of the clade. The
#'   first value extends the bar above the maximum y-value, and the second value
#'   extends the bar below the minimum y-value.
#' @inheritParams ggplot2::geom_text
#' @inheritParams ggplot2::geom_linerange
#' @inheritParams ggplot2::geom_label
#' @encoding UTF-8
#' @export
#' @importFrom ggplot2 layer position_nudge
#' @importFrom rlang %||%
#' @importFrom utils modifyList
#' @examples
#' library(ggplot2)
#' @examplesIf require(ggtree) && require(phytools)
#' library(ggtree)
#' library(phytools)
#' data(primate.tree)
#' clades.df <- data.frame(
#'   clade = c("Lorisoidea", "Lemuroidea", "Tarsioidea", "Ceboidea",
#'             "Cercopithecoidea", "Hominoidea"),
#'   node = c(166, 146, 144, 120, 95, 114)
#' )
#' revts(ggtree(primate.tree)) %<+% clades.df +
#'   geom_text_clade(aes(label = clade), extend = c(0.1, 0.1)) +
#'   coord_geo_radial()
#'
#' # display with other tip data
#' data(primate.data)
#' activity <- subset(primate.data, select = "Activity_pattern")
#' revts(gheatmap(ggtree(primate.tree), activity, offset = -70,
#'                colnames = FALSE, width = 0.03, color = NA)) %<+% clades.df +
#'   geom_text_clade(aes(label = clade), extend = c(0.1, 0.1),
#'                   position = position_nudge(x = 10)) +
#'   coord_geo_radial()
geom_text_clade <- function(mapping = NULL, data = NULL, text_geom = "text",
                            stat = "identity", position = "identity", ...,
                            parse = FALSE, auto_adjust = TRUE, extend = c(0, 0),
                            check_overlap = FALSE, lineend = "butt",
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE) {
  rlang::check_installed("tidytree", reason = "to use `geom_text_clade()`")
  check_bool(auto_adjust)
  if (is.numeric(extend) && length(extend) != 2) {
    cli::cli_abort("`extend` must be a numeric vector of length 2.")
  }
  text_geom <- arg_match0(text_geom, c("text", "label"))
  mapping <- modifyList(aes(parent = parent, label = label, node = node),
                        mapping %||% list())
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextClade,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      text_geom = text_geom,
      extend = extend,
      auto_adjust = auto_adjust,
      check_overlap = check_overlap,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @importFrom ggplot2 ggproto GeomText GeomLabel GeomLinerange ggproto_parent
#' @importFrom rlang %||%
#' @importFrom grid unit
#' @importFrom utils modifyList
GeomTextClade <- ggproto("GeomTextClade", GeomText,
  required_aes = c("x", "y", "parent", "node", "label"),
  non_missing_aes = c("angle", "linetype", "linewidth", "fill"),
  extra_params = c("na.rm", "auto_adjust", "extend", "text_geom"),

  default_aes = modifyList(GeomLinerange$default_aes,
                           list(hjust = -0.02, vjust = 0.5, size = 3.88,
                                angle = 0, alpha = NA, fill = "white"),
                           keep.null = T),

  setup_data = function(data, params) {
    extend <- params$extend
    tbl_tree <- tidytree::as_tibble(data)
    class(tbl_tree) <- c("tbl_tree", class(tbl_tree))
    data_sub <- subset(data, !is.na(label))
    l <- lapply(seq_len(nrow(data_sub)), function(i) {
      offspring_df <- tidytree::offspring(tbl_tree, data_sub$node[i],
                                          self_include = TRUE)
      offspring_df <- subset(offspring_df,
                             !is.na(offspring_df$x) & !is.na(offspring_df$y))
      mx <- max(offspring_df$x)
      maxy <- max(offspring_df$y)
      miny <- min(offspring_df$y)
      data_row <- data_sub[i, , drop = FALSE]
      data_row$x <- mx
      data_row$y <- mean(c(miny, maxy))
      data_row$ymin <- miny - extend[2]
      data_row$ymax <- maxy + extend[1]
      data_row
    })
    do.call(rbind, l)
  },

  draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                        auto_adjust = TRUE, na.rm = FALSE, lineend = "butt",
                        check_overlap = FALSE, text_geom = "text",
                        size.unit = "mm", label.padding = unit(0.35, "lines"),
                        label.r = unit(0.15, "lines"),
                        border.colour = NULL, text.colour = NULL) {
    # offset the labels like in ggtree
    x_range <- panel_params$r.range %||% panel_params$x.range
    data$x <- data$x + diff(x_range) / 100

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

    # generate the text and bar grobs
    parent <- ggproto_parent(GeomLinerange, self)
    segment_grob <- parent$draw_panel(data, panel_params, coord,
                                      lineend = lineend, na.rm = na.rm)
    if (text_geom == "text") {
      parent <- ggproto_parent(GeomText, self)
      text_grob <- parent$draw_panel(data, panel_params, coord, parse, na.rm,
                                     check_overlap, size.unit = size.unit)
    } else {
      parent <- ggproto_parent(GeomLabel, self)
      text_grob <- parent$draw_panel(data, panel_params, coord, parse, na.rm,
                                     label.padding = label.padding,
                                     label.r = label.r, size.unit = size.unit,
                                     border.colour = border.colour,
                                     text.colour = text.colour)
    }
    grob_list <- gList(text_grob, segment_grob)
    gTree(name = "geom_text_clade", children = grob_list)
  }
)

