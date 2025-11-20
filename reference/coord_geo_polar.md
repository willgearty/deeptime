# Polar coordinate system with geological timescale

**\[deprecated\]**

`coord_geo_polar` behaves similarly to
[`ggplot2::coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
in that it occurs after statistical transformation and will affect the
visual appearance of geoms. The main difference is that it also adds a
geological timescale to the background of the plot.

## Usage

``` r
coord_geo_polar(
  dat = "periods",
  theta = "y",
  start = -pi/2,
  direction = -1,
  clip = "on",
  fill = NULL,
  alpha = 1,
  lwd = 0.25,
  color = "grey80",
  lty = "solid",
  lab = FALSE,
  abbrv = TRUE,
  skip = c("Quaternary", "Holocene", "Late Pleistocene"),
  neg = TRUE,
  prop = 1,
  textpath_args = list()
)
```

## Arguments

- dat:

  Either A) a string indicating a built-in dataframe with interval data
  from the ICS ("periods", "epochs", "stages", "eons", or "eras"), B) a
  string indicating a timescale from macrostrat (see list here:
  <https://macrostrat.org/api/defs/timescales?all>), or C) a custom
  data.frame of time interval boundaries (see Details).

- theta:

  variable to map angle to (`x` or `y`)

- start:

  Offset of starting point from 12 o'clock in radians. Offset is applied
  clockwise or anticlockwise depending on value of `direction`.

- direction:

  1, clockwise; -1, anticlockwise

- clip:

  Should drawing be clipped to the extent of the plot panel? A setting
  of `"on"` (the default) means yes, and a setting of `"off"` means no.
  For details, please see
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html).

- fill:

  The fill color of the background. The default is to use the `color`
  column included in `dat`. If a custom dataset is provided with `dat`
  without a `color` column and without fill, a greyscale will be used.
  Custom fill colors can be provided with this option (overriding the
  `color` column) and will be recycled if/as necessary.

- alpha:

  The transparency of the fill colors.

- lwd:

  Line width for lines between intervals. Set to `NULL` to remove lines.

- color:

  The color of the lines between intervals.

- lty:

  Line type for lines between intervals.

- lab:

  Whether to include labels. Requires the `geomtextpath` package.

- abbrv:

  If including labels, whether to use abbreviations instead of full
  interval names.

- skip:

  A vector of interval names indicating which intervals should not be
  labeled. If `abbrv` is `TRUE`, this can also include interval
  abbreviations.

- neg:

  Set this to true if your theta-axis is using negative values. This is
  usually true if you are using `ggtree`.

- prop:

  This is the rotational proportion of the background that the scale
  takes up.

- textpath_args:

  A list of named arguments to provide to
  [`geomtextpath::geom_textpath()`](https://allancameron.github.io/geomtextpath/reference/geom_textpath.html).
  Only used if `lab` is set to `TRUE`. Useful arguments include `color`
  (font color), `family` (font family), `fontface`, `hjust` (radial
  adjustment), and `size` (font size).

## Details

If a custom data.frame is provided (with `dat`), it should consist of at
least 2 columns of data. See `data(periods)` for an example.

- The `max_age` column lists the oldest boundary of each time interval.

- The `min_age` column lists the youngest boundary of each time
  interval.

- The `abbr` column is optional and lists abbreviations that may be used
  as labels.

- The `color` column is optional and lists a
  [color](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)
  for the background for each time interval.

`dat` may also be a list of values and/or dataframes if multiple time
scales should be added to the background. Scales will be added
sequentially starting at `start` and going in the specified `direction`.
By default the scales will all be equal in circular/rotational
proportion, but this can be overridden with `prop`. If `dat` is a list,
`fill`, `alpha`, `lwd`, `color`, `lty`, `lab`, `abbrv`, `skip`, `neg`,
`prop`, and `textpath_args` can also be lists (N.B. `textpath_args`
would be a list of lists). If these lists are not as long as `dat`, the
elements will be recycled. If individual values (or vectors) are used
for these parameters, they will be applied to all time scales (and
recycled as necessary).

If the sum of the `prop` values is greater than 1, the proportions will
be scaled such that they sum to 1. However, the `prop` values may sum to
less than 1 if the user would like blank space in the background.

`coord_geo_polar` manually generates the `r` axis, meaning it does not
support changing the guide features of ggplot v. 2.5.0 or later.
However, the `deeptime.axis.line.r`, `deeptime.axis.text.r`,
`deeptime.axis.ticks.r`, and `deeptime.axis.ticks.length.r` ggplot2
[theme elements](https://ggplot2.tidyverse.org/reference/theme.html) can
be modified just like their x and y counterparts to change the
appearance of the radius axis. The default settings work well for a
horizontal axis pointing towards the right, but these theme settings
will need to be modified for other orientations. The default value for
`deeptime.axis.line.r` is
[`element_line()`](https://ggplot2.tidyverse.org/reference/element.html).
The default value for `deeptime.axis.text.r` is
`element_text(size = 3.5, vjust = -2, hjust = -0.5)`. The default value
for `deeptime.axis.ticks.r` is
[`element_line()`](https://ggplot2.tidyverse.org/reference/element.html).
The default value for `deeptime.axis.ticks.length.r` is
`unit(1.5, "points")`. However, note that the units for this element are
meaningless and only the numeric value will be used (but a `unit` must
still be used).

Care must be taken when adding labels to plots, as they are very likely
to overlap with the plot under the default settings. The `textpath_args`
argument can be used to adjust the settings for the plotting of the
labels. See
[`geomtextpath::geom_textpath()`](https://allancameron.github.io/geomtextpath/reference/geom_textpath.html)
for details about the available arguments. Also note that the curvature
of the labels may vary based on the distance from the origin. This is
why `abbrv` is set to `TRUE` by default.

## Life cycle

This function is soft-deprecated in favor of
[`coord_geo_radial()`](https://williamgearty.com/deeptime/reference/coord_geo_radial.md)
as of **deeptime** version 1.1.0. There is currently no plan to remove
this function, but users are strongly encouraged to migrate to the new
function for enhanced polar functionality.

## Examples

``` r
library(ggplot2)
library(ggtree)
set.seed(1)
tree <- rtree(100)
# single scale
revts(ggtree(tree)) +
  coord_geo_polar(dat = "stages")
#> Warning: `coord_geo_polar()` was deprecated in deeptime 1.1.0.
#> ℹ Please use `coord_geo_radial()` instead.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.


# multiple scales
revts(ggtree(tree)) +
  coord_geo_polar(
    dat = list("stages", "periods"), alpha = .5,
    prop = list(0.75, .25), start = pi / 4, lty = "dashed"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  theme(deeptime.axis.text.r = element_text(size = 3.5, hjust = .75,
                                            vjust = .75))
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.

library(ggplot2)
library(paleotree)
data(RaiaCopesRule)
ggtree(ceratopsianTreeRaia,
       position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
  coord_geo_polar(dat = "stages")
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
```
