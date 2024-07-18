#' Get a FGDC geologic plotting pattern
#'
#' Retrieve a single geologic pattern as defined by the [FGDC Digital
#' Cartographic Standard for Geologic Map
#' Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd/download.php) by
#' the [Geologic Data Subcommittee
#' (GDS)](https://ngmdb.usgs.gov/fgdc_gds/index.php) of the [Federal Geographic
#' Data Committee (FGDC)](https://www.fgdc.gov/).
#'
#' @param code The number corresponding to the pattern to return. Strings and
#'   numbers are permitted. See the "pattern numbers" in the [full pattern
#'   chart](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd/fgdc-geolsym-patternchart.pdf)
#'   for valid `code` values.
#' @param scale The visual scale of the pattern.
#' @param col The color to use for the lines of the pattern.
#' @param fill The color with which to fill the pattern.
#' @param alpha The transparency to use for the fill of the pattern.
#' @param bg The background color to use for the pattern.
#'
#' @details These patterns were originally processed and optimized by Daven
#'   Quinn and are hosted on
#'   [GitHub](https://github.com/davenquinn/geologic-patterns/).
#'
#' @return A [GridPattern][grid::patterns] object.
#' @importFrom grid editGrob rectGrob gList gTree gPath pattern gpar
#' @export
#'
#' @examples
#' library(grid)
#' # Get a generic igneous pattern
#' pattern1 <- geo_pattern(code = "313-K")
#' # Get the pattern for a sandstone
#' pattern2 <- geo_pattern(code = "607")
#'
#' # plot the two patterns
#' grid.newpage()
#' grid.draw(rectGrob(gp = gpar(fill = pattern1)))
#' grid.newpage()
#' grid.draw(rectGrob(gp = gpar(fill = pattern2)))
geo_pattern <- function(code, scale = 2,
                        col = NULL, fill = NULL, alpha = NULL,
                        bg = "white") {
  code <- as.character(code)
  # get the grob for the given code
  code <- gsub("(-.*)?", "", code)
  if (code %in% names(fgdc_grobs)) {
    img_grob <- fgdc_grobs[[code]]
  } else {
    cli::cli_abort("`code` does not match a USGS code.")
  }
  # recolor the grob (this modifies all children)
  gp <- list()
  if (!is.null(col)) gp$col <- col
  if (!is.null(fill)) gp$fill <- fill
  if (!is.null(alpha)) gp$alpha <- alpha
  img_grob <- editGrob(img_grob,
                       gp = do.call(gpar, gp),
                       gPath("*"), grep = TRUE, global = TRUE)

  # now add the background
  img_grob <- gList(rectGrob(gp = gpar(fill = bg, col = "transparent")),
                    img_grob)

  # needed to make it work for ggplot stuff
  img_grob <- gTree(children = img_grob)

  # convert to pattern
  img_grob$vp <- viewport(width = unit(scale, 'cm'), height = unit(scale, 'cm'))
  pattern(img_grob, width = unit(scale, 'cm'), height = unit(scale, 'cm'),
          extend = "repeat")
}

#' Geologic pattern fill scale
#'
#' Fill scale using the [FGDC Digital Cartographic Standard for Geologic Map
#' Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd/download.php). Fill
#' values should correspond to the "pattern numbers" in the [full pattern
#' chart](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd/fgdc-geolsym-patternchart.pdf).
#' See [geo_pattern()] for more details.
#'
#' @inheritParams ggplot2::scale_fill_manual
#' @section Warning: Pattern fills are not supported on all graphics devices.
#'   Where they are not supported, closed shapes will be rendered with a
#'   transparent fill. For example, on Windows machines, the default device in
#'   RStudio and in the knitr package is [png()], which does not support
#'   patterns. In RStudio, you can go to ‘Tools > Global Options > General >
#'   Graphics’ and choose the ‘ragg’ or ‘Cairo PNG’ device from the dropdown
#'   menu to display patterns.
#' @export
#' @importFrom ggplot2 scale_fill_manual
#' @examples
#' library(ggplot2)
#' vals <- c("101", "313", "603", "733")
#' ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
#'   geom_bar() +
#'   scale_fill_geopattern(name = NULL)
scale_fill_geopattern <- function(...) {
  # TODO: support colors and scaling?
  vals <- sapply(names(fgdc_grobs), geo_pattern, simplify = FALSE)
  scale_fill_manual(values = vals, ...)
}
