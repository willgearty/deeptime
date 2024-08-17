#' Geological Time Scale color scales
#'
#' Color scales using the colors in the Geological Time Scale graphics.
#'
#' @inheritParams ggplot2::scale_discrete_manual
#' @inheritDotParams ggplot2::discrete_scale -expand -position -palette -breaks
#' @importFrom ggplot2 discrete_scale
#' @rdname scale_geo
#' @param dat Either A) a string indicating a built-in dataframe with interval
#'   data from the ICS ("periods", "epochs", "stages", "eons", or "eras"),
#'   B) a string indicating a timescale from macrostrat (see list here:
#'   <https://macrostrat.org/api/defs/timescales?all>),
#'   or C) a custom data.frame of time interval boundaries
#'   (see [coord_geo()]).
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   x = runif(1000, 0, 10), y = runif(1000, 0, 10),
#'   color = sample(periods$name, 1000, TRUE), shape = 21
#' )
#' ggplot(df) +
#'   geom_point(aes(x = x, y = y, fill = color), shape = 21) +
#'   scale_fill_geo("periods", name = "Period") +
#'   theme_classic()
#'
#' # cut continuous variable into discrete
#' df <- data.frame(x = runif(1000, 0, 1000), y = runif(1000, 0, 8))
#' df$color <- cut(df$x, c(periods$min_age, periods$max_age[22]), periods$name)
#' ggplot(df) +
#'   geom_point(aes(x = x, y = y, color = color)) +
#'   scale_x_reverse() +
#'   scale_color_geo("periods", name = "Period") +
#'   coord_geo(xlim = c(1000, 0), ylim = c(0, 8)) +
#'   theme_classic()
scale_color_geo <- function(dat, ...) {
  scale_discrete_geo(dat, "color", ...)
}

#' @export
#' @rdname scale_geo
#' @usage NULL
scale_colour_geo <- scale_color_geo

#' @rdname scale_geo
#' @export
scale_fill_geo <- function(dat, ...) {
  scale_discrete_geo(dat, "fill", ...)
}

#' @export
#' @rdname scale_geo
#' @importFrom stats setNames
scale_discrete_geo <- function(dat, aesthetics, ...) {
  if (is(dat, "data.frame")) {
    # just use the supplied data
  } else if (is.character(dat)) {
    dat <- get_scale_data(dat)
  } else {
    cli::cli_abort("`dat` must be either a dataframe or a string.")
  }
  if (!all(c("name", "color") %in% colnames(dat))) {
    cli::cli_abort("Either `name` or `color` is not a named column in `dat`")
  }
  values <- setNames(dat$color, dat$name)

  pal <- function(n) {
    if (n > length(values)) {
      cli::cli_abort("Insufficient values in manual scale. {n} needed but only
                     {length(values)} provided.")
    }
    values
  }

  discrete_scale(aesthetics, palette = pal, breaks = names(values), ...)
}
