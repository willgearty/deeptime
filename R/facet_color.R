#' Lay out panels in a grid with colored strips
#'
#' `facet_grid_color` behaves similarly to [ggplot2::facet_grid()] in that it
#' forms a matrix of panels defined by row and column faceting variables. The
#' main difference is that it also allows the user to specify the background
#' colors of the individual facet strips using the `colors` argument. If you
#' have only one variable with many levels, try [facet_wrap_color()].
#'
#' @param colors Specifies which colors to use to replace the strip backgrounds.
#'   Either A) a function that returns a color for a given strip label, B) the
#'   character name of a function that does the same, C) a named character
#'   vector with names matching strip labels and values indicating the desired
#'   colors, or D) a data.frame representing a lookup table with columns named
#'   "name" (matching strip labels) and "color" (indicating desired colors). If
#'   the function returns
#' @param axes Determines which axes will be drawn. When `"margins"` (default),
#'   axes will be drawn at the exterior margins. `"all_x"` and `"all_y"` will
#'   draw the respective axes at the interior panels too, whereas `"all"` will
#'   draw all axes at all panels. Only works for ggplot2 version 3.5.0 and
#'   later.
#' @param axis.labels Determines whether to draw labels for interior axes when
#'   the `axes` argument is not `"margins"`. When `"all"` (default), all
#'   interior axes get labels. When `"margins"`, only the exterior axes get
#'   labels and the interior axes get none. When `"all_x"` or `"all_y"`, only
#'   draws the labels at the interior axes in the x- or y-direction
#'   respectively. Only works for ggplot2 version 3.5.0 and later.
#' @inheritParams ggplot2::facet_grid
#' @importFrom ggplot2 ggproto FacetGrid ggproto_parent
#' @importFrom rlang arg_match0 is_function
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:10, y = 1:10, period = c("Permian", "Triassic"))
#' ggplot(df) +
#'   geom_point(aes(x, y)) +
#'   facet_grid_color(cols = vars(period), colors = periods)
facet_grid_color <- function(rows = NULL, cols = NULL, scales = "fixed",
                             space = "fixed", shrink = TRUE,
                             labeller = "label_value", colors = stages,
                             as.table = TRUE, switch = NULL,
                             drop = TRUE, margins = FALSE,
                             axes = "margins", axis.labels = "all") {
  colors <- convert_colors(colors)

  # function and arguments copied from ggplot 3.4.4 and 3.5.0

  # Should become a warning in a future release
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }

  scales <- arg_match0(scales %||% "fixed", c("fixed", "free_x",
                                              "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  space <- arg_match0(space %||% "fixed", c("fixed", "free_x",
                                            "free_y", "free"))
  space_free <- list(
    x = any(space %in% c("free_x", "free")),
    y = any(space %in% c("free_y", "free"))
  )

  if (packageVersion("ggplot2") >= "3.5.0") {
    draw_axes <- arg_match0(axes, c("margins", "all_x", "all_y", "all"))
    draw_axes <- list(
      x = any(draw_axes %in% c("all_x", "all")),
      y = any(draw_axes %in% c("all_y", "all"))
    )

    # Omitting labels is special-cased internally, so even when no internal axes
    # are to be drawn, register as labelled.
    axis_labels <- arg_match0(axis.labels, c("margins", "all_x", "all_y", "all"))
    axis_labels <- list(
      x = !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
      y = !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
    )
  } else {
    if (axes != "margins") {
      warning("The `axes` argument is only supported for ggplot2 3.5.0 and
              later.")
    }
    if (axis.labels != "all") {
      warning("The `axis.labels` argument is only supported for ggplot2 3.5.0
              and later.")
    }
  }

  if (!is.null(switch)) {
    arg_match0(switch, c("both", "x", "y"))
  }

  facets_list <- grid_as_facets_list(rows, cols)

  # Check for deprecated labellers
  labeller <- check_labeller(labeller)

  if (packageVersion("ggplot2") >= "3.5.0") {
    params = list(rows = facets_list$rows, cols = facets_list$cols,
                  margins = margins, free = free, space_free = space_free,
                  labeller = labeller, colors = colors,
                  as.table = as.table, switch = switch, drop = drop,
                  draw_axes = draw_axes, axis_labels = axis_labels)
  } else {
    params = list(rows = facets_list$rows, cols = facets_list$cols,
                  margins = margins, free = free, space_free = space_free,
                  labeller = labeller, colors = colors,
                  as.table = as.table, switch = switch, drop = drop)
  }

  ggproto(NULL, FacetGridColor,
          shrink = shrink,
          params = params
  )
}

grid_as_facets_list <- function(...) {
  asNamespace("ggplot2")$grid_as_facets_list(...)
}

check_labeller <- function(...) {
  asNamespace("ggplot2")$check_labeller(...)
}

#' @importFrom rlang is_function
convert_colors <- function(colors) {
  # convert colors to a function
  if (!is_function(colors)) {
    if (is.character(colors) && !is.null(names(colors))) {
      name <- names(colors)
      color <- unname(colors)
      colors <- function(x) {
        if (x %in% name) color[which(x == name)[1]] else NA
      }
    } else if (is.character(colors) && length(colors) == 1) {
      colors <- match.fun(colors)
    } else if (is.data.frame(colors)) {
      if (all(c("name", "color") %in% names(colors))) {
        name <- colors$name
        color <- colors$color
        colors <- function(x) {
          if (x %in% name) color[which(x == name)[1]] else NA
        }
      } else {
        stop("If using a data.frame for `colors`, the data.frame must have
              columns named 'name' and 'color'.")
      }
    } else {
      stop("Invalid type for `colors`; only functions, function names, named
            character vectors, and data.frames are allowed.")
    }
  }
  return(colors)
}

#' @rdname facet_grid_color
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto FacetGrid ggproto_parent
FacetGridColor <- ggproto("FacetGridColor", FacetGrid,
  shrink = TRUE,
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params, self) {
    panel_table <-
      ggproto_parent(FacetGrid, self)$draw_panels(panels, layout,
                                                  x_scales, y_scales, ranges,
                                                  coord, data, theme, params)
    strips <- grep("strip", panel_table$layout$name)
    for (i in strips) {
      label <-
        panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label
      fill <- tryCatch(params$colors(label), error = function(e) NA)
      if (!is.na(fill)) {
        panel_table$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fill
      }
    }
    panel_table
  }
)

#' Wrap a 1d ribbon of panels into 2d with colored strips
#'
#' `facet_wrap_color` behaves similarly to [ggplot2::facet_wrap()] in that it
#' wraps a 1d sequence of panels into 2d. The main difference is that it also
#' allows the user to specify the background colors of the individual facet
#' strips using the `colors` argument. This is generally a better use of screen
#' space than [facet_grid_color()] because most displays are roughly
#' rectangular.
#'
#' @param colors Specifies which colors to use to replace the strip backgrounds.
#'   Either A) a function that returns a color for a given strip label, B) the
#'   character name of a function that does the same, C) a named character
#'   vector with names matching strip labels and values indicating the desired
#'   colors, or D) a data.frame representing a lookup table with columns named
#'   "name" (matching strip labels) and "color" (indicating desired colors). If
#'   the function returns
#' @param axes Determines which axes will be drawn in case of fixed scales. When
#'   `"margins"` (default), axes will be drawn at the exterior margins.
#'   `"all_x"` and `"all_y"` will draw the respective axes at the interior
#'   panels too, whereas `"all"` will draw all axes at all panels. Only works
#'   for ggplot2 version 3.5.0 and later.
#' @param axis.labels Determines whether to draw labels for interior axes when
#'   the scale is fixed and the `axis` argument is not `"margins"`. When `"all"`
#'   (default), all interior axes get labels. When `"margins"`, only the
#'   exterior axes get labels, and the interior axes get none. When `"all_x"` or
#'   `"all_y"`, only draws the labels at the interior axes in the x- or
#'   y-direction respectively. Only works for ggplot2 version 3.5.0 and later.
#' @inheritParams ggplot2::facet_wrap
#' @importFrom ggplot2 ggproto FacetWrap ggproto_parent
#' @importFrom rlang arg_match0
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:10, y = 1:10, period = c("Permian", "Triassic"))
#' ggplot(df) +
#'   geom_point(aes(x, y)) +
#'   facet_wrap_color(vars(period), colors = periods)
facet_wrap_color <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                             shrink = TRUE, labeller = "label_value",
                             colors = stages, as.table = TRUE, drop = TRUE,
                             dir = "h", strip.position = "top",
                             axes = "margins", axis.labels = "all") {
  colors <- convert_colors(colors)

  # function and arguments copied from ggplot 3.4.4 and 3.5.0
  scales <- arg_match0(scales %||% "fixed", c("fixed", "free_x",
                                              "free_y", "free"))
  dir <- arg_match0(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  if (packageVersion("ggplot2") >= "3.5.0") {
    # If scales are free, always draw the axes
    draw_axes <- arg_match0(axes, c("margins", "all_x", "all_y", "all"))
    draw_axes <- list(
      x = free$x || any(draw_axes %in% c("all_x", "all")),
      y = free$y || any(draw_axes %in% c("all_y", "all"))
    )

    # Omitting labels is special-cased internally, so only omit labels if
    # scales are not free and the axis is to be drawn
    axis_labels <- arg_match0(axis.labels, c("margins", "all_x", "all_y", "all"))
    axis_labels <- list(
      x = free$x || !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
      y = free$y || !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
    )
  }

  # Check for deprecated labellers
  labeller <- check_labeller(labeller)

  # Flatten all facets dimensions into a single one
  facets <- wrap_as_facets_list(facets)

  strip.position <- arg_match0(strip.position, c("top", "bottom",
                                                 "left", "right"))

  check_number_whole(ncol, allow_null = TRUE, min = 1)
  check_number_whole(nrow, allow_null = TRUE, min = 1)

  if (identical(dir, "v")) {
    # swap
    tmp <- ncol
    ncol <- nrow
    nrow <- tmp
  }

  if (packageVersion("ggplot2") >= "3.5.0") {
    params <- list(
      facets = facets,
      free = free,
      as.table = as.table,
      strip.position = strip.position,
      drop = drop,
      ncol = ncol,
      nrow = nrow,
      labeller = labeller,
      colors = colors,
      dir = dir,
      draw_axes = draw_axes,
      axis_labels = axis_labels
    )
  } else {
    params <- list(
      facets = facets,
      free = free,
      as.table = as.table,
      strip.position = strip.position,
      drop = drop,
      ncol = ncol,
      nrow = nrow,
      labeller = labeller,
      colors = colors,
      dir = dir
    )
  }

  ggproto(NULL, FacetWrapColor,
          shrink = shrink,
          params = params
  )
}

check_number_whole <- function(...) {
  asNamespace("rlang")$check_number_whole(...)
}

wrap_as_facets_list <- function(...) {
  asNamespace("ggplot2")$wrap_as_facets_list(...)
}

#' @rdname facet_wrap_color
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto FacetWrap ggproto_parent
FacetWrapColor <- ggproto("FacetWrapColor", FacetWrap,
  shrink = TRUE,
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params, self) {
    panel_table <-
      ggproto_parent(FacetWrap, self)$draw_panels(panels, layout,
                                                  x_scales, y_scales, ranges,
                                                  coord, data, theme, params)
    strips <- grep("strip", panel_table$layout$name)
    for (i in strips) {
      label <-
        panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label
      fill <- tryCatch(params$colors(label), error = function(e) NA)
      if (!is.na(fill)) {
        panel_table$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fill
      }
    }
    panel_table
  }
)
