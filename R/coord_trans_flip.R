#' Transformed and flipped Cartesian coordinate system
#'
#' `coord_trans_flip` behaves similarly to [ggplot2::coord_trans()] in that it
#' occurs after statistical transformation and will affect the visual appearance
#' of geoms. The main difference is that it also flips the x and y coordinates
#' like [ggplot2::coord_flip()].
#'
#' @importFrom ggplot2 ggproto
#' @inheritParams ggplot2::coord_trans
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(disp, wt)) +
#'   geom_point() +
#'   coord_trans_flip(x = "log10", y = "log10")
coord_trans_flip <- function(x = "identity", y = "identity",
                             xlim = NULL, ylim = NULL,
                             clip = "on", expand = TRUE) {
  # resolve transformers
  if (is.character(x)) x <- as.trans(x)
  if (!is.trans(x)) {
    cli::cli_abort("`x` must be a transformer function or a string.")
  }
  if (is.character(y)) y <- as.trans(y)
  if (!is.trans(y)) {
    cli::cli_abort("`y` must be a transformer function or a string.")
  }

  # check arguments
  clip <- arg_match0(clip, c("off", "on"))
  check_bool(expand)

  ggproto(NULL, CoordTransFlip,
    trans = list(x = x, y = y),
    limits = list(x = xlim, y = ylim),
    expand = expand,
    clip = clip
  )
}

# copied from ggplot2
flip_axis_labels <- function(x) {
  old_names <- names(x)

  new_names <- old_names
  new_names <- gsub("^x", "z", new_names)
  new_names <- gsub("^y", "x", new_names)
  new_names <- gsub("^z", "y", new_names)

  setNames(x, new_names)
}

#' @rdname coord_trans_flip
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto CoordTrans CoordFlip ggproto_parent
#' @export
CoordTransFlip <- ggproto("CoordTransFlip", CoordFlip,
  transform = function(self, data, panel_params) {
    # Need the panel params to be unflipped to correctly transform the data
    panel_params <- flip_axis_labels(panel_params)
    data <- ggproto_parent(CoordTrans, self)$transform(data, panel_params)
    flip_axis_labels(data)
  },
  backtransform_range = function(self, panel_params) {
    un_flipped_range <-
      ggproto_parent(CoordTrans, self)$backtransform_range(panel_params)
    list(x = un_flipped_range$y, y = un_flipped_range$x)
  },
  range = function(self, panel_params) {
    # summarise_layout() expects the original x and y ranges here,
    # not the ones we would get after flipping the axes
    un_flipped_range <- ggproto_parent(CoordTrans, self)$range(panel_params)
    list(x = un_flipped_range$y, y = un_flipped_range$x)
  },
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    parent <- ggproto_parent(CoordTrans, self)
    panel_params <- parent$setup_panel_params(scale_x, scale_y, params)
    flip_axis_labels(panel_params)
  },
  labels = function(labels, panel_params) {
    CoordTrans$labels(flip_axis_labels(labels), panel_params)
  },
  setup_layout = function(layout, params) {
    CoordFlip$setup_layout(layout, params)
  },
  modify_scales = function(scales_x, scales_y) {
    CoordFlip$modify_scales(scales_x, scales_y)
  }
)
