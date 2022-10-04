.onLoad <- function(...) {
  ggplot2::register_theme_elements(
    axis.line.r = ggplot2::element_line(),
    axis.text.r = ggplot2::element_text(size = 3.5, vjust = -2, hjust = NA),
    axis.ticks.r = ggplot2::element_line(),
    axis.ticks.length.r = grid::unit(1.5, "points"),
    element_tree = list(
      axis.line.r = ggplot2::el_def("element_line", "line", "axis.line"),
      axis.text.r = ggplot2::el_def("element_text", "text", "axis.text"),
      axis.ticks.r = ggplot2::el_def("element_line", "line", "axis.ticks"),
      axis.ticks.length.r = ggplot2::el_def("unit", "axis.ticks.length")
    )
  )
}
