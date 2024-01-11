#' Display points and their range
#'
#' This geom is like [ggplot2::geom_pointrange()] in that it draws points and
#' lines. However, unlike [ggplot2::geom_pointrange()], this geom takes in sets
#' of x-y points and calculates the ranges/intervals based on those. It then
#' plots both the original points and the ranges using
#' [ggplot2::geom_linerange()]. In cases where not all points are connected
#' (because of grouping due to aesthetics), the `background_line` argument can
#' be used to add lines that span the entire point range for each `x` or `y`
#' category.
#'
#' @section Aesthetics: \code{geom_points_range()} understands the following
#'   aesthetics (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - size
#' - color/colour
#' - fill
#' - shape
#' - alpha
#' - group
#' - linetype
#' - linewidth
#'
#' @param background_line A named list of aesthetic values to use for plotted
#'   line segments that span the entire `y` or `x` range for each `x` or `y`
#'   category. The default aesthetics will be used for any aesthetics that are
#'   not specified in the list. If NULL (the default), no line segments will be
#'   plotted.
#' @param ... Arguments passed on to both [ggplot2::geom_linerange()] and
#'   [ggplot2::geom_point()].
#' @importFrom ggplot2 layer
#' @inheritParams ggplot2::geom_pointrange
#' @inheritSection ggplot2::geom_pointrange Orientation
#' @export
#' @examples
#' library(ggplot2)
#' @examplesIf require(palaeoverse)
#' library(palaeoverse)
#' data(tetrapods)
#' tetrapod_names <- tetrapods$accepted_name[1:50]
#' beds_sampled <- sample.int(n = 10, size = 50, replace = TRUE)
#' occdf <- data.frame(taxon = tetrapod_names, bed = beds_sampled)
#' ggplot(occdf, aes(y = reorder(taxon, bed, min), x = bed)) +
#'   geom_points_range()
geom_points_range <- function(mapping = NULL, data = NULL,
                              stat = "points_range", position = "identity",
                              ...,
                              na.rm = FALSE, orientation = NA,
                              background_line = NULL,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointsRange,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, orientation = orientation,
                  background_line = background_line, ...)
  )
}

#' @rdname geom_points_range
#' @section Computed variables: These are calculated by the 'stat' part of
#'   layers and can be accessed with [delayed evaluation][ggplot2::aes_eval].
#'   \code{stat_points_range()} provides the following variables, some of which
#'   depend on the orientation:
#'   \itemize{
#'     \item \code{after_stat(ymin)} \emph{or} \code{after_stat(xmin)}\cr
#'       the minimum extent of the point range
#'     \item \code{after_stat(ymax)} \emph{or} \code{after_stat(xmax)}\cr
#'       the maximum extent of the point range
#'   }
#' @importFrom ggplot2 layer
#' @inheritParams ggplot2::stat_identity
#' @export
stat_points_range <- function(mapping = NULL, data = NULL,
                              geom = "points_range", position = "identity",
                              ...,
                              na.rm = FALSE, orientation = NA,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPointsRange,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, orientation = orientation, ...)
  )
}

#' @importFrom ggplot2 ggproto Stat has_flipped_aes
StatPointsRange <- ggproto("StatPointsRange", Stat,
  required_aes = c("x", "y"),
  extra_params = c("na.rm", "orientation"),
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params,
                                          main_is_orthogonal = TRUE,
                                          group_has_equal = TRUE,
                                          main_is_optional = TRUE)
    params
  },
  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    data <- remove_missing(
      data,
      na.rm = params$na.rm,
      vars = "x",
      name = "stat_points_range"
    )
    flip_data(data, params$flipped_aes)
  },
  compute_group = function(data, scales, na.rm = FALSE, flipped_aes = FALSE) {
    # flip the data if needed
    data <- flip_data(data, flipped_aes)
    # calculate the y ranges
    data <- transform(data,
                      ymin = min(y, na.rm = na.rm),
                      ymax = max(y, na.rm = na.rm))
    # flip the data back if needed
    data <- flip_data(data, flipped_aes)
    data
  },
)

#' @importFrom ggplot2 ggproto Geom GeomPoint GeomLinerange draw_key_pointrange
#' @importFrom ggplot2 standardise_aes_names
#' @importFrom grid gList gTree
GeomPointsRange <- ggproto("GeomPointsRange", Geom,
  required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),
  extra_params = c("na.rm", "orientation"),
  default_aes = aes(shape = 19, colour = "black", size = 0.5, fill = NA,
                    alpha = NA, stroke = 0.5,
                    linewidth = 0.5, linetype = 1),
  draw_key = draw_key_pointrange,
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },
  setup_data = function(data, params) {
    data
  },
  draw_panel = function(self, data, panel_params, coord, fatten = 4,
                        flipped_aes = FALSE, background_line = NULL,
                        na.rm = FALSE) {
    grob_list <- gList()
    if (!is.null(background_line)) {
      # flip the data if needed
      data <- flip_data(data, flipped_aes)
      # split the data and calculate a full y range for each x category
      lst <- split(data, data$x)
      lst <- lapply(lst, function(df) {
        data.frame(x = unique(df$x), PANEL = unique(data$PANEL),
                   ymin = min(df$y), ymax = max(df$y))
      })
      # put it all together and
      df <- do.call(rbind, lst)
      for (name in names(background_line)) {
        df[[name]] <- background_line[[name]]
      }
      df <- flip_data(df, flipped_aes)
      names(df) <- standardise_aes_names(names(df))
      df <- self$use_defaults(df)
      # add background lines as a grob
      grob_list <- gList(grob_list,
                         GeomLinerange$draw_panel(df, panel_params, coord,
                                                  flipped_aes = flipped_aes,
                                                  na.rm = na.rm))
      # flip the data back if needed
      data <- flip_data(data, flipped_aes)
    }
    # add the normal grobs
    grob_list <- gList(
      grob_list,
      GeomLinerange$draw_panel(unique(data), panel_params, coord,
                               flipped_aes = flipped_aes, na.rm = na.rm),
      GeomPoint$draw_panel(transform(data, size = size * fatten),
                           panel_params, coord, na.rm = na.rm)
    )
    gTree(name = "geom_points_range", children = grob_list)
  },
)

#' @importFrom grid grobName
ggname <- function(prefix, grob) {
  # copied from ggplot2
  grob$name <- grobName(grob, prefix)
  grob
}
