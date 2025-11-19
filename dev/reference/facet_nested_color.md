# Layout panels in a grid with nested colored strips

`facet_nested_color` behaves similarly to
[`ggh4x::facet_nested()`](https://teunbrand.github.io/ggh4x/reference/facet_nested.html)
in that it forms a matrix of panels defined by row and column faceting
variables and nests grouped facets. The main difference is that it also
allows the user to specify the background and label colors of the
individual facet strips using the `colors` and `lab_colors` arguments.

## Usage

``` r
facet_nested_color(
  colors,
  rows = NULL,
  cols = NULL,
  scales = "fixed",
  space = "fixed",
  axes = "margins",
  remove_labels = "none",
  independent = "none",
  shrink = TRUE,
  labeller = "label_value",
  lab_colors = "auto",
  as.table = TRUE,
  switch = NULL,
  drop = TRUE,
  margins = FALSE,
  nest_line = element_line(inherit.blank = TRUE),
  solo_line = FALSE,
  resect = unit(0, "mm"),
  render_empty = TRUE,
  strip = strip_nested(),
  bleed = NULL
)

facet_nested_geo(
  colors = rbind(periods, stages),
  rows = NULL,
  cols = NULL,
  scales = "fixed",
  space = "fixed",
  axes = "margins",
  remove_labels = "none",
  independent = "none",
  shrink = TRUE,
  labeller = "label_value",
  lab_colors = "auto",
  as.table = TRUE,
  switch = NULL,
  drop = TRUE,
  margins = FALSE,
  nest_line = element_line(inherit.blank = TRUE),
  solo_line = FALSE,
  resect = unit(0, "mm"),
  render_empty = TRUE,
  strip = strip_nested(),
  bleed = NULL
)
```

## Arguments

- colors:

  Specifies which colors to use to replace the strip backgrounds.
  Either A) a function that returns a color for a given strip label, B)
  the character name of a function that does the same, C) a named
  character vector with names matching strip labels and values
  indicating the desired colors, or D) a data.frame representing a
  lookup table with columns named "name" (matching strip labels) and
  "color" (indicating desired colors). If the function returns `NA`, the
  default background color will be used.

- rows, cols:

  A set of variables or expressions quoted by
  [`vars()`](https://ggplot2.tidyverse.org/reference/vars.html) and
  defining faceting groups on the rows or columns dimension. The
  variables can be named (the names are passed to `labeller`).

  For compatibility with the classic interface, `rows` can also be a
  formula with the rows (of the tabular display) on the LHS and the
  columns (of the tabular display) on the RHS; the dot in the formula is
  used to indicate there should be no faceting on this dimension (either
  row or column).

- scales:

  A `character(1)` or `logical(1)` whether scales are shared across
  facets or allowed to vary. Interacts with the `independent` argument.
  One of the following:

  `"fixed"` or `FALSE`

  :   Scales are shared across all facets (default).

  `"free_x"`

  :   x-scales are allowed to vary across rows.

  `"free_y"`

  :   y-scales are allowed to vary across columns.

  `"free"` or `TRUE`

  :   Scales can vary across rows and columns.

- space:

  A `character(1)` or `logical(1)` determining whether the size of
  panels are proportional to the length of the scales. When the
  `independent` argument allows for free scales in a dimension, the
  panel sizes cannot be proportional. Note that the `scales` argument
  must be free in the same dimension as the `space` argument to have an
  effect.One of the following:

  `"fixed"` or `FALSE`

  :   All panels have the same size (default).

  `"free_x"`

  :   Panel widths are proportional to the x-scales.

  `"free_y"`

  :   Panel heights are proportional to the y-scales.

  `"free"` or `TRUE`

  :   Both the widths and heights vary according to scales.

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

- independent:

  A `character(1)` or `logical(1)` determining whether scales can vary
  within a row or column of panels, like they can be in
  [ggplot2::facet_wrap](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  The `scales` argument must be free for the same dimension before they
  can be set to independent. One of the following:

  `"none"` or `FALSE`

  :   All y-scales should be fixed in a row and all x-scales are fixed
      in a column (default).

  `"x"`

  :   x-scales are allowed to vary within a column.

  `"y"`

  :   y-scales are allowed to vary within a row.

  `"all"` or `TRUE`

  :   Both x- and y-scales are allowed to vary within a column or row
      respectively.

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

- switch:

  By default, the labels are displayed on the top and right of the plot.
  If `"x"`, the top labels will be displayed to the bottom. If `"y"`,
  the right-hand side labels will be displayed to the left. Can also be
  set to `"both"`.

- drop:

  If `TRUE`, the default, all factor levels not used in the data will
  automatically be dropped. If `FALSE`, all factor levels will be shown,
  regardless of whether or not they appear in the data.

- margins:

  Either a logical value or a character vector. Margins are additional
  facets which contain all the data for each of the possible values of
  the faceting variables. If `FALSE`, no additional facets are included
  (the default). If `TRUE`, margins are included for all faceting
  variables. If specified as a character vector, it is the names of
  variables for which margins are to be created.

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

- render_empty:

  A `logical(1)`: whether to draw panels without any data (`TRUE`,
  default) or display these as blanks (`FALSE`).

- strip:

  An object created by a call to a strip function, such as
  [`strip_nested()`](https://teunbrand.github.io/ggh4x/reference/strip_nested.html).

- bleed:

  **\[deprecated\]** the `bleed` argument has moved to the
  `strip_nested()` function.

## Details

`facet_nested_geo(...)` is an alias of `facet_nested_color()` with the
default of `colors` set to `rbind(periods, stages)`.

## See also

Other faceting functions:
[`facet_grid_color()`](https://williamgearty.com/deeptime/dev/reference/facet_grid_color.md),
[`facet_nested_wrap_color()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_wrap_color.md),
[`facet_wrap_color()`](https://williamgearty.com/deeptime/dev/reference/facet_wrap_color.md)

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
  facet_nested_color(~ era + period, colors = rbind(periods, eras))
```
