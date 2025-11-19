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
#' @param obj An object of class `ggplot`, `gtable`, or `geo_scale` (as produced
#'   by this function).
#' @return A geo_scale object. Basically a gtable object but with the axis
#'   limits included.
#' @keywords internal
#' @export
gggeo_scale <- function(obj, ...) {
  lifecycle::deprecate_stop("2.3.0", "gggeo_scale()", "coord_geo()")
}


