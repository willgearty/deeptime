.onLoad <- function(...) {
  # coord_geo_polar theme elements
  ggplot2::register_theme_elements(
    deeptime.plot.background = ggplot2::element_rect(fill = NA, color = NA),
    deeptime.axis.line.r = ggplot2::element_line(),
    deeptime.axis.text.r = ggplot2::element_text(size = 3.5, vjust = -2,
                                                 hjust = -0.5),
    deeptime.axis.ticks.r = ggplot2::element_line(),
    deeptime.axis.ticks.length.r = grid::unit(1.5, "points"),
    element_tree = list(
      deeptime.scale.background = ggplot2::el_def("element_rect", "rect"),
      deeptime.axis.line.r = ggplot2::el_def("element_line", "line",
                                             "axis.line"),
      deeptime.axis.text.r = ggplot2::el_def("element_text", "text",
                                             "axis.text"),
      deeptime.axis.ticks.r = ggplot2::el_def("element_line", "line",
                                              "axis.ticks"),
      deeptime.axis.ticks.length.r = ggplot2::el_def("unit",
                                                     "axis.ticks.length")
    )
  )

  options(ggpattern_geometry_funcs = list(geo = grid.pattern_geo))
}
