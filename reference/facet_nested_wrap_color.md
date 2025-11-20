# Ribbon of panels with nested colored strips

`facet_nested_wrap_color` behaves similarly to
[`ggh4x::facet_nested_wrap()`](https://teunbrand.github.io/ggh4x/reference/facet_nested_wrap.html)
in that it wraps a sequence of panels onto a two-dimensional layout, and
nests grouped facets where possible.. The main difference is that it
also allows the user to specify the background and label colors of the
individual facet strips using the `colors` and `lab_colors` arguments.

## Usage

``` r
facet_nested_wrap_color(
  facets,
  colors,
  nrow = NULL,
  ncol = NULL,
  scales = "fixed",
  axes = "margins",
  remove_labels = "none",
  shrink = TRUE,
  labeller = "label_value",
  lab_colors = "auto",
  as.table = TRUE,
  drop = TRUE,
  dir = "h",
  strip.position = "top",
  nest_line = element_line(inherit.blank = TRUE),
  solo_line = FALSE,
  resect = unit(0, "mm"),
  trim_blank = TRUE,
  strip = strip_nested(),
  bleed = NULL
)

facet_nested_wrap_geo(
  facets,
  colors = rbind(periods, stages),
  nrow = NULL,
  ncol = NULL,
  scales = "fixed",
  axes = "margins",
  remove_labels = "none",
  shrink = TRUE,
  labeller = "label_value",
  lab_colors = "auto",
  as.table = TRUE,
  drop = TRUE,
  dir = "h",
  strip.position = "top",
  nest_line = element_line(inherit.blank = TRUE),
  solo_line = FALSE,
  resect = unit(0, "mm"),
  trim_blank = TRUE,
  strip = strip_nested(),
  bleed = NULL
)
```

## Arguments

- facets:

  A set of variables or expressions quoted by
  [`vars()`](https://ggplot2.tidyverse.org/reference/vars.html) and
  defining faceting groups on the rows or columns dimension. The
  variables can be named (the names are passed to `labeller`).

  For compatibility with the classic interface, can also be a formula or
  character vector. Use either a one sided formula, `~a + b`, or a
  character vector, `c("a", "b")`.

- colors:

  Specifies which colors to use to replace the strip backgrounds.
  Either A) a function that returns a color for a given strip label, B)
  the character name of a function that does the same, C) a named
  character vector with names matching strip labels and values
  indicating the desired colors, or D) a data.frame representing a
  lookup table with columns named "name" (matching strip labels) and
  "color" (indicating desired colors). If the function returns `NA`, the
  default background color will be used.

- nrow, ncol:

  Number of rows and columns.

- scales:

  A `character(1)` or `logical(1)` whether scales are shared across
  facets or allowed to vary. One of the following:

  `"fixed"` or `FALSE`

  :   Scales are shared across all facets (default).

  `"free_x"`

  :   x-scales are allowed to vary.

  `"free_y"`

  :   y-scales are allowed to vary.

  `"free"` or `TRUE`

  :   Both scales can vary

- axes:

  A `character(1)` or `logical(1)` where axes should be drawn. One of
  the following:

  `"margins"` or `FALSE`

  :   Only draw axes at the outer margins (default).

  `"x"`

  :   Draw axes at the outer margins and all inner x-axes too.

  `"y"`

  :   Draw axes at the outer margins and all inner y-axes too.

  `"all"` or `TRUE`

  :   Draw the axes for every panel.

- remove_labels:

  A `character(1)` or `logical(1)` determining whether axis text is
  displayed at inner panels. One of the following:

  `"none"` or `FALSE`

  :   Display axis text at all axes (default).

  `"x"`

  :   Display axis text at outer margins and all inner y-axes.

  `"y"`

  :   Display axis text at outer margins and all inner x-axes.

  `"all"` or `TRUE`

  :   Only display axis text at the outer margins.

- shrink:

  If `TRUE`, will shrink scales to fit output of statistics, not raw
  data. If `FALSE`, will be range of raw data before statistical
  summary.

- labeller:

  A function that takes one data frame of labels and returns a list or
  data frame of character vectors. Each input column corresponds to one
  factor. Thus there will be more than one with `vars(cyl, am)`. Each
  output column gets displayed as one separate line in the strip label.
  This function should inherit from the "labeller" S3 class for
  compatibility with
  [`labeller()`](https://ggplot2.tidyverse.org/reference/labeller.html).
  You can use different labeling functions for different kind of labels,
  for example use
  [`label_parsed()`](https://ggplot2.tidyverse.org/reference/labellers.html)
  for formatting facet labels.
  [`label_value()`](https://ggplot2.tidyverse.org/reference/labellers.html)
  is used by default, check it for more details and pointers to other
  options.

- lab_colors:

  Specifies which colors to use for the strip labels. Either A) a
  function that returns a color for a given strip label, B) the
  character name of a function that does the same, C) a named character
  vector with names matching strip labels and values indicating the
  desired colors, D) a data.frame representing a lookup table with
  columns named "name" (matching strip labels) and "lab_color"
  (indicating desired colors), or E) "auto" (the default), which set the
  labels to black or white, whichever has better contrast with the
  background color, based on [recommendations by the International
  Telecommunication
  Union](https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en). If the
  function returns `NA`, the default label color will be used.

- as.table:

  If `TRUE`, the default, the facets are laid out like a table with
  highest values at the bottom-right. If `FALSE`, the facets are laid
  out like a plot with the highest value at the top-right.

- drop:

  If `TRUE`, the default, all factor levels not used in the data will
  automatically be dropped. If `FALSE`, all factor levels will be shown,
  regardless of whether or not they appear in the data.

- dir:

  Direction: either `"h"` for horizontal, the default, or `"v"`, for
  vertical.

- strip.position:

  By default, the labels are displayed on the top of the plot. Using
  `strip.position` it is possible to place the labels on either of the
  four sides by setting
  `strip.position = c("top", "bottom", "left", "right")`

- nest_line:

  a theme element, either
  [`element_blank()`](https://ggplot2.tidyverse.org/reference/element.html)
  or inheriting from
  [`ggplot2::element_line()`](https://ggplot2.tidyverse.org/reference/element.html).
  Lines are drawn between layers of strips indicating hierarchy. The
  element inherits from the
  [`ggh4x.facet.nestline`](https://teunbrand.github.io/ggh4x/reference/theme_extensions.html)
  element in the theme.

- solo_line:

  A `logical(1)` indicating whether parent strips with a single child
  should be drawn with a `nest_line` (`TRUE`) or the line only applies
  to parents with multiple children (`FALSE`, default). Only relevant
  when `nest_line` is drawn.

- resect:

  a `unit` vector of length 1, indicating how much the nesting line
  should be shortened.

- trim_blank:

  A `logical(1)`. When `TRUE` (default), does not draw rows and columns
  containing no panels. When `FALSE`, the `nrow` and `ncol` arguments
  are taken literally, even when there are more than needed to fit all
  panels.

- strip:

  An object created by a call to a strip function, such as
  [`strip_nested()`](https://teunbrand.github.io/ggh4x/reference/strip_nested.html).

- bleed:

  **\[deprecated\]** the `bleed` argument has moved to the
  `strip_nested()` function.

## Details

`facet_nested_wrap_geo(...)` is an alias of `facet_nested_wrap_color()`
with the default of `colors` set to `rbind(periods, stages)`.

## See also

Other faceting functions:
[`facet_grid_color()`](https://williamgearty.com/deeptime/reference/facet_grid_color.md),
[`facet_nested_color()`](https://williamgearty.com/deeptime/reference/facet_nested_color.md),
[`facet_wrap_color()`](https://williamgearty.com/deeptime/reference/facet_wrap_color.md)

## Examples

``` r
library(ggplot2)
df <- data.frame(x = 1:10, y = 1:10,
                 period = factor(c("Permian", "Triassic", "Jurassic",
                                   "Cretaceous", "Paleogene"),
                                 levels = c("Permian", "Triassic",
                                            "Jurassic", "Cretaceous",
                                            "Paleogene")),
                 era = factor(c("Paleozoic", "Mesozoic", "Mesozoic",
                                "Mesozoic", "Cenozoic"),
                                levels = c("Paleozoic", "Mesozoic",
                                           "Cenozoic")))
ggplot(df, aes(x, y)) +
  geom_point() +
  facet_nested_wrap_color(~ era + period, colors = rbind(periods, eras))
```
