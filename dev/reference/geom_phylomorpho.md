# Plot a 2-D phylomorphospace in ggplot2

This behaves similar to
[`phytools::phylomorphospace()`](https://rdrr.io/pkg/phytools/man/phylomorphospace.html),
but is for plotting a 2-D phylomorphospace with
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
This function works like any other `ggplot2` geom; it can be combined
with other geoms (see the example below), and the output can be modified
using scales, themes, etc.

## Usage

``` r
geom_phylomorpho(
  tree,
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  seg_args = list(),
  point_args = list(),
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- tree:

  An object of class "phylo".

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

- ...:

  Other arguments passed on to both
  [`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
  and
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- seg_args:

  A list of arguments passed only to
  [`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html).

- point_args:

  A list of arguments passed only to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- arrow:

  specification for arrow heads, as created by
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

- arrow.fill:

  fill colour to use for the arrow head (if closed). `NULL` means use
  `colour` aesthetic.

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Details

The ancestral states are estimated using
[`phytools::fastAnc()`](https://rdrr.io/pkg/phytools/man/fastAnc.html).
Note that `phytools` is not necessarily installed with `deeptime`, but
it is required to use this function. Following the estimation of the
ancestral states, the nodes are connected using
[`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html),
while the tips are indicated using
[`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

The default expectation is that the order of the data is the same order
as the tip labels of the tree (`tree$tip.label`). However, if this is
not the case, you can map the optional `label` aesthetic to a column in
the data that contains the tip names (see example below).

## Examples

``` r
library(ggplot2)
library(ape)
tr <- rtree(10)
dat <- data.frame(
  x = runif(10), y = runif(10), label = tr$tip.label,
  row.names = tr$tip.label
)
ggplot(dat, aes(x = x, y = y, label = label)) +
  geom_phylomorpho(tr) +
  geom_label(size = 5)
```
