# Label nodes on a phylogenetic tree plotted with ggtree

This geom adds labels to all or a subset of the nodes of a phylogenetic
tree that has been plotted using
[`ggtree::ggtree()`](https://rdrr.io/pkg/ggtree/man/ggtree.html). It is
therefore very similar to
[`ggtree::geom_tiplab()`](https://rdrr.io/pkg/ggtree/man/geom_tiplab.html),
[`ggtree::geom_tiplab2()`](https://rdrr.io/pkg/ggtree/man/geom_tiplab2.html),
[`ggtree::geom_nodelab()`](https://rdrr.io/pkg/ggtree/man/geom_nodelab.html),
and
[`ggtree::geom_nodelab2()`](https://rdrr.io/pkg/ggtree/man/geom_nodelab2.html).
However, unlike those geoms, this geom is intended to work with all
coordinate systems, including
[`coord_geo()`](https://williamgearty.com/deeptime/reference/coord_geo.md)
and
[`coord_geo_radial()`](https://williamgearty.com/deeptime/reference/coord_geo_radial.md).

## Usage

``` r
geom_text_phylo(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  node_type = "tip",
  auto_adjust = TRUE,
  check_overlap = FALSE,
  size.unit = "mm",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

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

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used the override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

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

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- parse:

  If `TRUE`, the labels will be parsed into expressions and displayed as
  described in [`?plotmath`](https://rdrr.io/r/grDevices/plotmath.html).

- nudge_x, nudge_y:

  Horizontal and vertical adjustment to nudge labels by. Useful for
  offsetting text from points, particularly on discrete scales. Cannot
  be jointly specified with `position`.

- node_type:

  Determines the subset of nodes to label. Valid options are "tip" for
  tip nodes, "internal" for non-tip nodes, and "all" for all nodes.

- auto_adjust:

  Should upside-down text labels automatically be rotated 180° to
  improve readability?

- check_overlap:

  If `TRUE`, text that overlaps previous text in the same layer will not
  be plotted. `check_overlap` happens at draw time and in the order of
  the data. Therefore data should be arranged by the label column before
  calling
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).
  Note that this argument is not supported by
  [`geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- size.unit:

  How the `size` aesthetic is interpreted: as millimetres (`"mm"`,
  default), points (`"pt"`), centimetres (`"cm"`), inches (`"in"`), or
  picas (`"pc"`).

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

Each label will be plotted with the same angle as the branch/edge
leading to its node by default. The `angle`, `hjust`, and `vjust`
aesthetics can be used to adjust this. If custom `angle` values are
specified, these will be **added** to the default angle as calculated as
described above.

As with
[`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html),
the limits of axes will not be expanded to accommodate the new labels,
so you may need to extend them manually using the `limits` or `expand`
arguments within `scale_` or `coord_` functions. Note that
[`coord_geo()`](https://williamgearty.com/deeptime/reference/coord_geo.md)
by default sets `expand = FALSE`.

## Aesthetics

`geom_text_phylo()` understands the following aesthetics (required
aesthetics are in bold):

- **x** (pulled from the phylogeny by default)

- **y** (pulled from the phylogeny by default)

- **label** (pulled from the phylogeny by default)

- alpha

- angle

- color/colour

- family

- fontface

- group

- hjust

- lineheight

- size

- vjust

## Alignment

You can modify text alignment with the `vjust` and `hjust` aesthetics.
These can either be a number between 0 (left/bottom) and 1 (right/top)
or a character (`"left"`, `"middle"`, `"right"`, `"bottom"`, `"center"`,
`"top"`). There are two special alignments: `"inward"` and `"outward"`.
Inward always aligns text towards the center, and outward aligns it away
from the center.

## Examples

``` r
library(ggplot2)
library(ape)
library(ggtree)
tr <- rtree(10)
revts(ggtree(tr)) +
  geom_text_phylo() +
  coord_geo_radial("epochs")
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
```
