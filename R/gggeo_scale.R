#' Add a geologic scale below ggplots using *grid*
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' This function takes a ggplot object and adds a geologic time scale at the
#' specified side using the grid and gtable packages.
#'
#' If custom data is provided (with `dat`), it should consist of at least 3
#' columns of data. See `data(periods)` for an example.
#' \itemize{
#'   \item The `name` column lists the names of each time interval. These will
#'     be used as labels if no abbreviations are provided.
#'   \item The `max_age` column lists the oldest boundary of each time interval.
#'   \item The `min_age` column lists the youngest boundary of each time
#'     interval.
#'   \item The `abbr` column is optional and lists abbreviations that may be
#'     used as labels.
#'   \item The `color` column is also optional and lists a hex color code
#'     (which can be obtained with `rgb()`) for each time interval.
#' }
#' @section Life cycle:
#'   This function is fully deprecated in favor of [coord_geo()] as of
#'   **deeptime** version 2.3.0. It will be removed in a future version.
#' @param lims The limits of the axis of the desired side of the plot. Only
#'   required if using a gtable object not created by this function.
#' @param dat Either A) a string indicating a built-in dataframe with interval
#'   data from the ICS ("periods", "epochs", "stages", "eons", or "eras"),
#'   B) a string indicating a timescale from macrostrat (see list here:
#'   <https://macrostrat.org/api/defs/timescales?all>),
#'   or C) a custom dataframe of time interval boundaries (see Details).
#' @param fill The fill color of the boxes. The default is to use the colors
#'   included in `dat`. If a custom dataset is provided with `dat` without color
#'   and without fill, a greyscale will be used. Custom fill colors can be
#'   provided with this option and will be recycled if/as necessary.
#' @param color The outline color of the interval boxes.
#' @param alpha The transparency of the fill colors.
#' @param height The height (or width if `pos` is `left` or `right`) of the
#'   scale.
#' @param pos Which side to add the scale to (left, right, top, or bottom).
#'   First letter may also be used.
#' @param lab Whether to include labels.
#' @param rot The amount of counter-clockwise rotation to add to the labels (in
#'   degrees).
#' @param abbrv If including labels, whether to use abbreviations instead of
#'   full interval names.
#' @param skip A vector of interval names indicating which intervals should not
#'   be labeled.
#' @param size Label size.
#' @param lwd Line width.
#' @param margin The width of the margin around the returned object (can be a
#'   vector of length 4).
#' @param neg Set this to true if your x-axis is using negative values.
#' @param bord A vector specifying on Which sides of the scale to add borders
#'   (same options as `pos`).
#' @param center_end_labels Should labels be centered within the visible range
#'   of intervals at the ends of the axis?
#' @return A geo_scale object. Basically a gtable object but with the axis
#'   limits included.
#' @importFrom gtable gtable_add_grob gtable_add_cols gtable_add_rows
#' @importFrom gtable gtable_add_padding
#' @importFrom grid unit
#' @importFrom ggplot2 ggplot geom_rect geom_segment geom_text annotate aes
#' @importFrom ggplot2 scale_fill_manual theme_void coord_cartesian coord_flip
#' @importFrom ggplot2 scale_x_reverse
#' @importFrom methods is
#' @keywords internal
#' @export
#' @param obj An object of class `ggplot`, `gtable`, or `geo_scale` (as produced
#'   by this function).
gggeo_scale <- function(obj, ...) {
  lifecycle::deprecate_stop("2.3.0", "gggeo_scale()", "coord_geo()")
}


