#' Lay out panels in a grid with colored strips
#'
#' `facet_grid_color` behaves similarly to [ggplot2::facet_grid()] in that it
#' forms a matrix of panels defined by row and column faceting variables. The
#' main difference is that it also allows the user to specify the background and
#' label colors of the individual facet strips using the `colors` and
#' `lab_colors` arguments. If you have only one variable with many levels, try
#' [facet_wrap_color()].
#'
#' @param colors Specifies which colors to use to replace the strip backgrounds.
#'   Either A) a function that returns a color for a given strip label, B) the
#'   character name of a function that does the same, C) a named character
#'   vector with names matching strip labels and values indicating the desired
#'   colors, or D) a data.frame representing a lookup table with columns named
#'   "name" (matching strip labels) and "color" (indicating desired colors). If
#'   the function returns `NA`, the default background color will be used.
#' @param lab_colors Specifies which colors to use for the strip labels. Either
#'   A) a function that returns a color for a given strip label, B) the
#'   character name of a function that does the same, C) a named character
#'   vector with names matching strip labels and values indicating the desired
#'   colors, D) a data.frame representing a lookup table with columns named
#'   "name" (matching strip labels) and "lab_color" (indicating desired colors),
#'   or E) "auto" (the default), which set the labels to black or white,
#'   whichever has better contrast with the background color, based on
#'   [recommendations by the International Telecommunication Union](https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en).
#'   If the function returns `NA`, the default label color will be used.
#' @inheritParams ggplot2::facet_grid
#' @importFrom ggplot2 ggproto FacetGrid ggproto_parent
#' @importFrom rlang arg_match0 is_function
#' @family facetting functions
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
                             labeller = "label_value",
                             colors = stages, lab_colors = "auto",
                             as.table = TRUE, switch = NULL,
                             drop = TRUE, margins = FALSE,
                             axes = "margins", axis.labels = "all") {
  colors <- convert_colors(colors)
  lab_colors <- convert_lab_colors(lab_colors)

  # function and arguments copied from ggplot 3.5.0

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

  draw_axes <- arg_match0(axes, c("margins", "all_x", "all_y", "all"))
  draw_axes <- list(
    x = any(draw_axes %in% c("all_x", "all")),
    y = any(draw_axes %in% c("all_y", "all"))
  )

  # Omitting labels is special-cased internally, so even when no internal axes
  # are to be drawn, register as labelled.
  axis_labels <- arg_match0(axis.labels, c("margins", "all_x",
                                           "all_y", "all"))
  axis_labels <- list(
    x = !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
    y = !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
  )

  if (!is.null(switch)) {
    arg_match0(switch, c("both", "x", "y"))
  }

  facets_list <- grid_as_facets_list(rows, cols)

  # Check for deprecated labellers
  labeller <- check_labeller(labeller)

  params <- list(rows = facets_list$rows, cols = facets_list$cols,
                 margins = margins, free = free, space_free = space_free,
                 labeller = labeller, colors = colors, lab_colors = lab_colors,
                 as.table = as.table, switch = switch, drop = drop,
                 draw_axes = draw_axes, axis_labels = axis_labels)

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
        cli::cli_abort("If using a data.frame for `colors`, the data.frame must
                       have columns named 'name' and 'color'.")
      }
    } else {
      cli::cli_abort("Invalid type for `colors`; only functions, function names,
                     named character vectors, and data.frames are allowed.")
    }
  }
  return(colors)
}

#' @importFrom rlang is_function
convert_lab_colors <- function(lab_colors) {
  # convert colors to a function
  if (!is_function(lab_colors)) {
    if (is.character(lab_colors) && lab_colors == "auto") {
        return(lab_colors)
    } else if (is.character(lab_colors) && !is.null(names(lab_colors))) {
      name <- names(lab_colors)
      color <- unname(lab_colors)
      lab_colors <- function(x) {
        if (x %in% name) color[which(x == name)[1]] else NA
      }
    } else if (is.character(lab_colors) && length(lab_colors) == 1) {
      lab_colors <- match.fun(lab_colors)
    } else if (is.data.frame(lab_colors)) {
      if (all(c("name", "lab_color") %in% names(lab_colors))) {
        name <- lab_colors$name
        color <- lab_colors$lab_color
        lab_colors <- function(x) {
          if (x %in% name) color[which(x == name)[1]] else NA
        }
      } else {
        cli::cli_abort("If using a data.frame for `lab_colors`, the data.frame
                       must have columns named 'name' and 'lab_color'.")
      }
    } else {
      cli::cli_abort("Invalid type for `lab_colors`; only functions, function
                     names, named character vectors, data.frames, and \"auto\"
                     are allowed.")
    }
  }
  return(lab_colors)
}

#' @rdname facet_grid_color
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto FacetGrid ggproto_parent
FacetGridColor <- ggproto("FacetGridColor", FacetGrid,
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
        if (!is_function(params$lab_colors) && params$lab_colors == "auto") {
          panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$
            gp$col <- white_or_black(fill)
        }
      }
      if (is_function(params$lab_colors)) {
        color <- tryCatch(params$lab_colors(label), error = function(e) NA)
        if (!is.na(color)) {
          panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$
            gp$col <- color
        }
      }
    }
    panel_table
  }
)

#' Wrap a 1d ribbon of panels into 2d with colored strips
#'
#' `facet_wrap_color` behaves similarly to [ggplot2::facet_wrap()] in that it
#' wraps a 1d sequence of panels into 2d. The main difference is that it also
#' allows the user to specify the background and label colors of the individual
#' facet strips using the `colors` and `lab_colors` arguments. This is generally
#' a better use of screen space than [facet_grid_color()] because most displays
#' are roughly rectangular.
#'
#' @param colors Specifies which colors to use to replace the strip backgrounds.
#'   Either A) a function that returns a color for a given strip label, B) the
#'   character name of a function that does the same, C) a named character
#'   vector with names matching strip labels and values indicating the desired
#'   colors, or D) a data.frame representing a lookup table with columns named
#'   "name" (matching strip labels) and "color" (indicating desired colors). If
#'   the function returns `NA`, the default background color will be used.
#' @param lab_colors Specifies which colors to use for the strip labels. Either
#'   A) a function that returns a color for a given strip label, B) the
#'   character name of a function that does the same, C) a named character
#'   vector with names matching strip labels and values indicating the desired
#'   colors, D) a data.frame representing a lookup table with columns named
#'   "name" (matching strip labels) and "lab_color" (indicating desired colors),
#'   or E) "auto" (the default), which set the labels to black or white,
#'   whichever has better contrast with the background color, based on
#'   [recommendations by the International Telecommunication Union](https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en).
#'   If the function returns `NA`, the default label color will be used.
#' @inheritParams ggplot2::facet_wrap
#' @importFrom ggplot2 ggproto FacetWrap ggproto_parent
#' @importFrom rlang arg_match0
#' @family facetting functions
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
                             colors = stages, lab_colors = "auto",
                             as.table = TRUE, drop = TRUE,
                             dir = "h", strip.position = "top",
                             axes = "margins", axis.labels = "all") {
  colors <- convert_colors(colors)
  lab_colors <- convert_lab_colors(lab_colors)

  # function and arguments copied from ggplot 3.5.0
  scales <- arg_match0(scales %||% "fixed", c("fixed", "free_x",
                                              "free_y", "free"))
  dir <- arg_match0(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

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
    lab_colors = lab_colors,
    dir = dir,
    draw_axes = draw_axes,
    axis_labels = axis_labels
  )

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
#' @importFrom rlang is_function
FacetWrapColor <- ggproto("FacetWrapColor", FacetWrap,
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
        if (!is_function(params$lab_colors) && params$lab_colors == "auto") {
          panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$
            gp$col <- white_or_black(fill)
        }
      }
      if (is_function(params$lab_colors)) {
        color <- tryCatch(params$lab_colors(label), error = function(e) NA)
        if (!is.na(color)) {
          panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$
            gp$col <- color
        }
      }
    }
    panel_table
  }
)

new_grid_facets <- function(...) {
  asNamespace("ggh4x")$new_grid_facets(...)
}

assert_strip <- function(...) {
  asNamespace("ggh4x")$assert_strip(...)
}

#' Layout panels in a grid with nested colored strips
#'
#' `facet_nested_color` behaves similarly to [ggh4x::facet_nested()] in that it
#' forms a matrix of panels defined by row and column faceting variables and
#' nests grouped facets. The main difference is that it also allows the user to
#' specify the background and label colors of the individual facet strips using
#' the `colors` and `lab_colors` arguments.
#'
#' @inheritParams ggh4x::facet_nested
#' @inheritParams facet_grid_color
#' @inherit ggh4x::facet_nested details
#' @importFrom ggplot2 element_line element_blank
#' @importFrom ggh4x strip_nested
#' @importFrom grid unit
#' @family facetting functions
#' @export
#' @examples
#' df <- data.frame(x = 1:10, y = 1:10,
#'                  period = factor(c("Permian", "Triassic", "Jurassic",
#'                                    "Cretaceous", "Paleogene"),
#'                                  levels = c("Permian", "Triassic",
#'                                             "Jurassic", "Cretaceous",
#'                                             "Paleogene")),
#'                  era = factor(c("Paleozoic", "Mesozoic", "Mesozoic",
#'                                 "Mesozoic", "Cenozoic"),
#'                                 levels = c("Paleozoic", "Mesozoic",
#'                                            "Cenozoic")))
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   facet_nested_color(~ era + period, colors = rbind(periods, eras))
facet_nested_color <- function(rows = NULL, cols = NULL, scales = "fixed",
                               space = "fixed", axes = "margins",
                               remove_labels = "none", independent = "none",
                               shrink = TRUE, labeller = "label_value",
                               colors = stages, lab_colors = "auto",
                               as.table = TRUE, switch = NULL, drop = TRUE,
                               margins = FALSE,
                               nest_line = element_line(inherit.blank = TRUE),
                               solo_line = FALSE, resect = unit(0, "mm"),
                               render_empty = TRUE,
                               strip = strip_nested(), bleed = NULL) {
  colors <- convert_colors(colors)
  lab_colors <- convert_lab_colors(lab_colors)

  # copied from ggh4x v0.2.8
  strip <- assert_strip(strip)
  if (!is.null(bleed)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "facet_nested(bleed)",
      details = paste0("The `bleed` argument should be set in the ",
                       "`strip_nested()` function instead.")
    )
    strip$params$bleed <- isTRUE(bleed)
  }
  # Convert logical to elements for backward compatibility
  if (isTRUE(nest_line)) {
    nest_line <- element_line()
  }
  if (isFALSE(nest_line)) {
    nest_line <- element_blank()
  }
  if (!inherits(nest_line, c("element_line", "element_blank"))) {
    cli::cli_abort(
      "The {.arg nest_line} argument must be {.cls element_blank} or inherit \\
      from {.cls element_line}."
    )
  }

  params <- list(nest_line = nest_line, solo_line = isTRUE(solo_line),
                 resect = resect, colors = colors, lab_colors = lab_colors)

  new_grid_facets(
    rows, cols,
    scales, space, axes, remove_labels, independent,
    shrink, labeller, as.table, switch,
    drop, margins, render_empty, strip,
    params = params,
    super = FacetNestedColor
  )
}

#' @rdname facet_nested_color
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom ggh4x FacetNested
#' @importFrom rlang is_function
FacetNestedColor <- ggproto("FacetNestedColor", FacetNested,
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params, self) {
    panel_table <-
      ggproto_parent(FacetNested, self)$draw_panels(panels, layout,
                                                    x_scales, y_scales, ranges,
                                                    coord, data, theme, params)
    strips <- grep("strip", panel_table$layout$name)
    for (i in strips) {
      label <-
        panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label
      fill <- tryCatch(params$colors(label), error = function(e) NA)
      if (!is.na(fill)) {
        panel_table$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fill
        if (!is_function(params$lab_colors) && params$lab_colors == "auto") {
          panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$
            gp$col <- white_or_black(fill)
        }
      }
      if (is_function(params$lab_colors)) {
        color <- tryCatch(params$lab_colors(label), error = function(e) NA)
        if (!is.na(color)) {
          panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$
            gp$col <- color
        }
      }
    }
    panel_table
  }
)

new_wrap_facets <- function(...) {
  asNamespace("ggh4x")$new_wrap_facets(...)
}

#' Ribbon of panels with nested colored strips
#'
#' `facet_nested_wrap_color` behaves similarly to [ggh4x::facet_nested_wrap()]
#' in that it wraps a sequence of panels onto a two-dimensional layout, and
#' nests grouped facets where possible.. The main difference is that it also
#' allows the user to specify the background and label colors of the individual
#' facet strips using the `colors` and `lab_colors` arguments.
#'
#' @inheritParams ggh4x::facet_nested_wrap
#' @inheritParams facet_wrap_color
#' @inherit ggh4x::facet_nested_wrap details
#' @importFrom ggplot2 element_line element_blank
#' @importFrom ggh4x strip_nested
#' @importFrom grid unit
#' @family facetting functions
#' @export
#' @examples
#' df <- data.frame(x = 1:10, y = 1:10,
#'                  period = factor(c("Permian", "Triassic", "Jurassic",
#'                                    "Cretaceous", "Paleogene"),
#'                                  levels = c("Permian", "Triassic",
#'                                             "Jurassic", "Cretaceous",
#'                                             "Paleogene")),
#'                  era = factor(c("Paleozoic", "Mesozoic", "Mesozoic",
#'                                 "Mesozoic", "Cenozoic"),
#'                                 levels = c("Paleozoic", "Mesozoic",
#'                                            "Cenozoic")))
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   facet_nested_wrap_color(~ era + period, colors = rbind(periods, eras))
facet_nested_wrap_color <- function(
    facets, nrow = NULL, ncol = NULL,
    scales = "fixed", axes = "margins",
    remove_labels = "none",
    shrink = TRUE, labeller = "label_value",
    colors = stages, lab_colors = "auto",
    as.table = TRUE, drop = TRUE,
    dir = "h", strip.position = "top",
    nest_line = element_line(inherit.blank = TRUE),
    solo_line = FALSE,
    resect = unit(0, "mm"),
    trim_blank = TRUE,
    strip = strip_nested(),
    bleed = NULL
) {
  colors <- convert_colors(colors)
  lab_colors <- convert_lab_colors(lab_colors)

  # copied from ggh4x v0.2.8
  strip <- assert_strip(strip)
  if (!is.null(bleed)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "facet_nested_wrap(bleed)",
      details = paste0("The `bleed` argument should be set in the ",
                       "`strip_nested()` function instead.")
    )
    strip$params$bleed <- isTRUE(bleed)
  }
  # Convert logical to elements for backward compatibility
  if (isTRUE(nest_line)) {
    nest_line <- element_line()
  }
  if (isFALSE(nest_line)) {
    nest_line <- element_blank()
  }
  if (!inherits(nest_line, c("element_line", "element_blank"))) {
    cli::cli_abort(
      "The {.arg nest_line} argument must be {.cls element_blank} or inherit \\
      from {.cls element_line}."
    )
  }
  params <- list(nest_line = nest_line, solo_line = isTRUE(solo_line),
                 resect = resect, colors = colors, lab_colors = lab_colors)
  new_wrap_facets(
    facets, nrow, ncol,
    scales, axes, remove_labels,
    shrink, labeller,
    as.table, drop, dir,
    strip.position, strip,
    trim_blank, params,
    super = FacetNestedWrapColor
  )
}

#' @rdname facet_nested_wrap_color
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom ggh4x FacetNestedWrap
#' @importFrom rlang is_function
FacetNestedWrapColor <- ggproto("FacetNestedWrapColor", FacetNestedWrap,
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params, self) {
    panel_table <-
      ggproto_parent(FacetNestedWrap, self)$draw_panels(panels, layout,
                                                        x_scales, y_scales,
                                                        ranges, coord, data,
                                                        theme, params)
    strips <- grep("strip", panel_table$layout$name)
    for (i in strips) {
      label <-
        panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label
      fill <- tryCatch(params$colors(label), error = function(e) NA)
      if (!is.na(fill)) {
        panel_table$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fill
        if (!is_function(params$lab_colors) && params$lab_colors == "auto") {
          panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$
            gp$col <- white_or_black(fill)
        }
      }
      if (is_function(params$lab_colors)) {
        color <- tryCatch(params$lab_colors(label), error = function(e) NA)
        if (!is.na(color)) {
          panel_table$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$
            gp$col <- color
        }
      }
    }
    panel_table
  }
)
