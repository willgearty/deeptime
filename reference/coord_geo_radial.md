# Enhanced polar coordinate system with geological timescale

`coord_geo_radial` behaves similarly to
[`ggplot2::coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
in that it occurs after statistical transformation and will affect the
visual appearance of geoms. The main difference is that it also adds a
geological timescale to the background of the plot. `coord_geo_radial`
is similar to
[`coord_geo_polar()`](https://williamgearty.com/deeptime/reference/coord_geo_polar.md)
but has more options related to the polar coordinate plotting that are
inherited from
[`ggplot2::coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
(e.g., `end`, `r_axis_inside`, `inner.radius`). Furthermore, unlike
`coord_geo_polar`, `coord_geo_radial` uses the ggplot2 internals to draw
the `r` and `theta` axes, gridlines, etc. This means that users can
tweak the [guide](https://ggplot2.tidyverse.org/reference/guides.html)
and [theme](https://ggplot2.tidyverse.org/reference/theme.html) settings
for these features (see examples).

## Usage

``` r
coord_geo_radial(
  dat = "periods",
  theta = "y",
  start = -0.5 * pi,
  end = 1.25 * pi,
  expand = TRUE,
  direction = 1,
  reverse = "none",
  r_axis_inside = NULL,
  inner.radius = 0.05,
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
  textpath_args = list(),
  clip = "off",
  rotate_angle = FALSE
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

- end:

  Position from 12 o'clock in radians where plot ends, to allow for
  partial polar coordinates. The default, `NULL`, is set to
  `start + 2 * pi`.

- expand:

  If `TRUE`, the default, adds a small expansion factor to the limits to
  prevent overlap between data and axes. If `FALSE`, limits are taken
  directly from the scale.

- direction:

  1, clockwise; -1, anticlockwise

- reverse:

  A string giving which directions to reverse. `"none"` (default) keep
  directions as is. `"theta"` reverses the angle and `"r"` reverses the
  radius. `"thetar"` reverses both the angle and the radius.

- r_axis_inside, rotate_angle:

  **\[deprecated\]**

- inner.radius:

  A `numeric` between 0 and 1 setting the size of a inner radius hole.

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

- clip:

  Should drawing be clipped to the extent of the plot panel? A setting
  of `"on"` (the default) means yes, and a setting of `"off"` means no.
  For details, please see
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html).

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

Care must be taken when adding labels to plots, as they are very likely
to overlap with the plot under the default settings. The `textpath_args`
argument can be used to adjust the settings for the plotting of the
labels. See
[`geomtextpath::geom_textpath()`](https://allancameron.github.io/geomtextpath/reference/geom_textpath.html)
for details about the available arguments. Also note that the curvature
of the labels may vary based on the distance from the origin. This is
why `abbrv` is set to `TRUE` by default.

## Examples

``` r
library(ggplot2)
library(ggtree)
set.seed(1)
tree <- rtree(100)
# single scale
revts(ggtree(tree)) +
  coord_geo_radial(dat = "stages") +
  scale_y_continuous(guide = "none", breaks = NULL) +
  theme_gray()
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.


# multiple scales
revts(ggtree(tree)) +
  coord_geo_radial(
    dat = list("stages", "periods"), alpha = .5,
    prop = list(0.75, .25), start = pi / 4, end = 2 * pi, lty = "dashed"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02)),
                     guide = "none", breaks = NULL) +
  theme_gray()
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
  coord_geo_radial(dat = "stages") +
  scale_y_continuous(guide = "none", breaks = NULL) +
  theme_classic()
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
#> Warning: Unknown or uninitialised column: `subgroup`.
```
