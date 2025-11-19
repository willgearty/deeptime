# Display points and their range

This geom is like
[`ggplot2::geom_pointrange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
in that it draws points and lines. However, unlike
[`ggplot2::geom_pointrange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html),
this geom takes in sets of x-y points and calculates the
ranges/intervals based on those. It then plots both the original points
and the ranges using
[`ggplot2::geom_linerange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html).
In cases where not all points are connected (because of grouping due to
aesthetics), the `background_line` argument can be used to add lines
that span the entire point range for each `x` or `y` category.

## Usage

``` r
geom_points_range(
  mapping = NULL,
  data = NULL,
  stat = "points_range",
  position = "identity",
  ...,
  na.rm = FALSE,
  orientation = NA,
  background_line = NULL,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_points_range(
  mapping = NULL,
  data = NULL,
  geom = "points_range",
  position = "identity",
  ...,
  na.rm = FALSE,
  orientation = NA,
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

  Arguments passed on to both
  [`ggplot2::geom_linerange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
  and
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- orientation:

  The orientation of the layer. The default (`NA`) automatically
  determines the orientation from the aesthetic mapping. In the rare
  event that this fails it can be given explicitly by setting
  `orientation` to either `"x"` or `"y"`. See the *Orientation* section
  for more detail.

- background_line:

  A named list of aesthetic values to use for plotted line segments that
  span the entire `y` or `x` range for each `x` or `y` category. The
  default aesthetics will be used for any aesthetics that are not
  specified in the list. This can be useful if the plotted groups of
  points don't overlap but you want a continuous line connecting all
  points for a given `x` or `y` category. If NULL (the default), no line
  segments will be plotted.

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

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

## Aesthetics

`geom_points_range()` understands the following aesthetics (required
aesthetics are in bold):

- **x**

- **y**

- size

- color/colour

- fill

- shape

- alpha

- group

- linetype

- linewidth

## Computed variables

These are calculated by the 'stat' part of layers and can be accessed
with [delayed
evaluation](https://ggplot2.tidyverse.org/reference/aes_eval.html).
`stat_points_range()` provides the following variables, some of which
depend on the orientation:

- `after_stat(ymin)` *or* `after_stat(xmin)`  
  the minimum extent of the point range

- `after_stat(ymax)` *or* `after_stat(xmax)`  
  the maximum extent of the point range

## Orientation

This geom treats each axis differently and, thus, can thus have two
orientations. Often the orientation is easy to deduce from a combination
of the given mappings and the types of positional scales in use. Thus,
ggplot2 will by default try to guess which orientation the layer should
have. Under rare circumstances, the orientation is ambiguous and
guessing may fail. In that case the orientation can be specified
directly using the `orientation` parameter, which can be either `"x"` or
`"y"`. The value gives the axis that the geom should run along, `"x"`
being the default orientation you would expect for the geom.

## Examples

``` r
library(ggplot2)
library(palaeoverse)
data(tetrapods)
tetrapod_names <- tetrapods$accepted_name[1:50]
beds_sampled <- sample.int(n = 10, size = 50, replace = TRUE)
occdf <- data.frame(taxon = tetrapod_names, bed = beds_sampled)
ggplot(occdf, aes(y = reorder(taxon, bed, min), x = bed)) +
  geom_points_range()
```
