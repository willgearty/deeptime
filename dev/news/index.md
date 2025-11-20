# Changelog

## deeptime (development version)

## deeptime 2.3.0

CRAN release: 2025-11-20

This new minor version of deeptime includes a small number of new
features and some minor housekeeping.

New features:

- The `pos` argument for
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
  is now recycled when `dat` is a list, removing the need for repeated
  `pos` values when adding multiple time scales to the same side
  ([\#86](https://github.com/willgearty/deeptime/issues/86))
- Added built-in dataset `fgdc_names` which includes FGDC pattern codes
  and verbatim names for named patterns
- Added
  [`fgdc_dict()`](https://williamgearty.com/deeptime/dev/reference/fgdc_dict.md)
  which can be used to generate a labeling dictionary for FGDC pattern
  codes ([\#79](https://github.com/willgearty/deeptime/issues/79))
- Added an optional theme element (`deeptime.scale.background`) to
  customize the background of the time scale area in
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
  ([\#87](https://github.com/willgearty/deeptime/issues/87))

Deprecation:

- Removed `getScaleData()`.
- Advanced the deprecation of
  [`gggeo_scale()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale.md).
  This function has been fully deprecated from deeptime in favor of
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md).

Miscellaneous:

- Updated citation information for the package
  ([\#89](https://github.com/willgearty/deeptime/issues/89))
- Updated package logo to reflect the recent inclusion of patterns
  ([\#90](https://github.com/willgearty/deeptime/issues/90))
- Added contribution docs
  ([\#91](https://github.com/willgearty/deeptime/issues/91))

## deeptime 2.2.0

CRAN release: 2025-06-19

This new minor version of deeptime introduces several new features and
bug fixes. It also is compatible with version 4 of ggplot2.

New features:

- Added
  [`geom_text_phylo()`](https://williamgearty.com/deeptime/dev/reference/geom_text_phylo.md)
  for adding node/tip labels to phylogenies when using
  [`coord_geo_radial()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_radial.md)
- Updated built-in timescales to 2024/12 version of GTS
- Removed the defaults for the `colors` argument in
  [`facet_grid_color()`](https://williamgearty.com/deeptime/dev/reference/facet_grid_color.md),
  [`facet_wrap_color()`](https://williamgearty.com/deeptime/dev/reference/facet_wrap_color.md),
  [`facet_nested_color()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_color.md),
  and
  [`facet_nested_wrap_color()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_wrap_color.md)
  - Added
    [`facet_grid_geo()`](https://williamgearty.com/deeptime/dev/reference/facet_grid_color.md),
    [`facet_wrap_geo()`](https://williamgearty.com/deeptime/dev/reference/facet_wrap_color.md),
    [`facet_nested_geo()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_color.md),
    and
    [`facet_nested_wrap_geo()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_wrap_color.md)
    which now serve as aliases for these functions with their original
    defaults ([\#81](https://github.com/willgearty/deeptime/issues/81))
- Added legends to the examples in the “Plotting
  geological/stratigraphical patterns” vignette
  ([\#78](https://github.com/willgearty/deeptime/issues/78))
- Added facetting examples to the “Plotting temporal data” vignette
  ([\#80](https://github.com/willgearty/deeptime/issues/80))

Deprecation:

- Advanced the deprecation of
  [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md).
  Use of this function will result in a warning. This function will be
  removed in a future version of deeptime.
- Advanced the deprecation of `getScaleData()`. This function has been
  fully deprecated from deeptime in favor of
  [`get_scale_data()`](https://williamgearty.com/deeptime/dev/reference/get_scale_data.md).

Bug fixes:

- Fixed
  [`gtable_frame2()`](https://williamgearty.com/deeptime/dev/reference/gtable_frame2.md)
  and
  [`ggarrange2()`](https://williamgearty.com/deeptime/dev/reference/ggarrange2.md)
  for plots that use
  [`theme_void()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  ([\#74](https://github.com/willgearty/deeptime/issues/74))
- Fixed
  [`disparity_through_time()`](https://williamgearty.com/deeptime/dev/reference/disparity_through_time.md)
  for rare edge cases
  ([\#71](https://github.com/willgearty/deeptime/issues/71))
- Updated `GeomPointsRange` to use the aesthetic defaults from
  [`ggplot2::GeomPointrange`](https://ggplot2.tidyverse.org/reference/Geom.html)
- Updated `GeomTextPhylo` to use the aesthetic defaults from
  [`ggplot2::GeomText`](https://ggplot2.tidyverse.org/reference/Geom.html)

## deeptime 2.1.0

CRAN release: 2024-10-25

This new minor version of deeptime introduces several new features and
bug fixes.

New features:

- Added default label color values to all built-in data as a new
  `lab_color` column
  - The label colors are either white or black, whichever has better
    contrast with the background color based on [recommendations by the
    International Telecommunication
    Union](https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en)
- [`get_scale_data()`](https://williamgearty.com/deeptime/dev/reference/get_scale_data.md)
  now returns label colors (as above) for Macrostrat timescales
- Varying strip label colors can now be set with the `lab_colors`
  argument in
  [`facet_wrap_color()`](https://williamgearty.com/deeptime/dev/reference/facet_wrap_color.md)
  and
  [`facet_grid_color()`](https://williamgearty.com/deeptime/dev/reference/facet_grid_color.md)
  ([\#68](https://github.com/willgearty/deeptime/issues/68))
  - By default, the strip label colors are set to white or black (as
    above)
- Added
  [`facet_nested_color()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_color.md)
  and
  [`facet_nested_wrap_color()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_wrap_color.md)
  to plot nested faceted plots with colored strips
  ([\#55](https://github.com/willgearty/deeptime/issues/55))
- [`get_scale_data()`](https://williamgearty.com/deeptime/dev/reference/get_scale_data.md)
  documentation now lists the available Macrostrat timescales
  ([\#69](https://github.com/willgearty/deeptime/issues/69))
- [`get_scale_data()`](https://williamgearty.com/deeptime/dev/reference/get_scale_data.md)
  now supports partial matching to Macrostrat timescales (e.g.,
  `get_scale_data("mammal")` will retrieve the “North American land
  mammal ages” timescale)

Notable changes:

- The FGDC pattern data added in deeptime 2.0.0 has been moved to a new
  data R package called `deeptimedata` to reduce the size of the
  deeptime package and the footprint of future deeptime updates

Bug fixes:

- Fixed importing some functions from `ggplot2` for
  [`coord_geo_radial()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_radial.md)
  and
  [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md)

## deeptime 2.0.0

CRAN release: 2024-08-19

**deeptime now requires ggplot2 version 3.5.0 or higher**.  
This new major version of deeptime includes several new features:

- Added FGDC/USGS geologic patterns to internal data
  ([\#52](https://github.com/willgearty/deeptime/issues/52))
- Added functions to retrieve and plot FGDC/USGS geologic patterns
  ([\#52](https://github.com/willgearty/deeptime/issues/52))
- Added functions to use FGDC/USGS patterns as ggplot2 fills or with the
  `ggpattern` package (see new vignette)
  ([\#52](https://github.com/willgearty/deeptime/issues/52))
- Added the `true_colors` option to
  [`get_scale_data()`](https://williamgearty.com/deeptime/dev/reference/get_scale_data.md),
  which, when set to `FALSE`, can be used to retrieve the custom
  Macrostrat colors that were previously in the built-in datasets
  ([\#62](https://github.com/willgearty/deeptime/issues/62))
- Updated built-in timescales to 2023/06 version of GTS
- Added option to
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
  to abbreviate interval labels based on only the intervals that are
  being plotted
  ([\#64](https://github.com/willgearty/deeptime/issues/64))
- Added
  [`guide_geo()`](https://williamgearty.com/deeptime/dev/reference/guide_geo.md)
  as an alternative to
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md),
  mostly for use with radial plots
  ([\#53](https://github.com/willgearty/deeptime/issues/53))

Bug fixes:

- Fixed the built-in timescale datasets to have true CGMP colors for the
  Precambrian ([\#62](https://github.com/willgearty/deeptime/issues/62))

Notable changes:

- The `phytools` package is no longer required to install `deeptime` but
  is required to use the
  [`geom_phylomorpho()`](https://williamgearty.com/deeptime/dev/reference/geom_phylomorpho.md)
  function
- The `geomtextpath` package is no longer required to install `deeptime`
  but is required to add labels with the
  [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md)
  or
  [`coord_geo_radial()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_radial.md)
  functions
- The
  [`gggeo_scale_old()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale_old.md)
  function has been fully deprecated

## deeptime 1.1.1

CRAN release: 2024-03-08

deeptime now requires rlang version 1.1.0 or higher. This patch version
includes several bug fixes:

- Fixed several cases where vignette and documentation building and/or
  unit testing would fail when the Macrostrat API was down
- Fixed
  [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md)
  and
  [`coord_geo_radial()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_radial.md)
  when the r-axis has expansion
  ([\#60](https://github.com/willgearty/deeptime/issues/60))
- Improved argument type checking and error handling across the entire
  package

## deeptime 1.1.0

CRAN release: 2024-03-05

deeptime now requires ggplot2 version 3.4.0 or higher. This version
introduces several new features and bug fixes:

- Added
  [`geom_points_range()`](https://williamgearty.com/deeptime/dev/reference/geom_points_range.md),
  a function designed for visualizing temporal occurrence data
- Added
  [`facet_grid_color()`](https://williamgearty.com/deeptime/dev/reference/facet_grid_color.md)
  and
  [`facet_wrap_color()`](https://williamgearty.com/deeptime/dev/reference/facet_wrap_color.md)
  for changing strip background colors
  ([\#50](https://github.com/willgearty/deeptime/issues/50))
- Added
  [`coord_geo_radial()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_radial.md),
  an enhanced version of
  [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md)
  (only works with ggplot2 version 3.5.0 and higher)
  ([\#56](https://github.com/willgearty/deeptime/issues/56))
- Added `family` and `fontface` arguments to
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
- Added the ability to include interval labels with
  [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md)
  ([\#48](https://github.com/willgearty/deeptime/issues/48))
- Fixed the interaction between
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
  and [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  ([\#49](https://github.com/willgearty/deeptime/issues/49))
- Fixed `size = "auto"` when `center_end_labels = TRUE` in
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
- Updated several functions to work with ggplot2 version 3.5.0

Notable changes:

- The names for the theme elements that are used for
  [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md)
  are now prepended with “deeptime.” (e.g., `deeptime.axis.line.r`)
  ([\#51](https://github.com/willgearty/deeptime/issues/51))
- ggplot2 version 3.5.0 has broken several edge cases of
  [`coord_trans_xy()`](https://williamgearty.com/deeptime/dev/reference/coord_trans_xy.md).
  See the documentation for more details. This will be fixed in a future
  deeptime release.

## deeptime 1.0.1

CRAN release: 2023-02-16

- Fixed the bounds of the Kungurian stage in the built-in stages data
- Updated License to GPL \>= 3
- Changed tidyverse to dplyr and magrittr in Suggests and vignettes

## deeptime 1.0.0

CRAN release: 2023-01-20

- Updated built-in timescales to 2022/10 version of GTS
- Added a geom for making phylomorphospaces
  ([`geom_phylomorpho()`](https://williamgearty.com/deeptime/dev/reference/geom_phylomorpho.md))
- Added vignettes
- Renamed `getScaleData()` to
  [`get_scale_data()`](https://williamgearty.com/deeptime/dev/reference/get_scale_data.md)
- Soft deprecated
  [`gggeo_scale()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale.md)
  and
  [`gggeo_scale_old()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale_old.md)

## deeptime 0.3.0

CRAN release: 2022-11-09

deeptime now requires ggplot2 version 3.3.0 or higher. Several new
features and fixed compatibility with various ggplot2 versions.

- Added
  [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md),
  for adding timescales to polar plots
  ([\#33](https://github.com/willgearty/deeptime/issues/33))
- Added color and fill discrete scales that use the GTS colors
  ([\#43](https://github.com/willgearty/deeptime/issues/43))
- Fixed
  [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md)
  and
  [`coord_trans_xy()`](https://williamgearty.com/deeptime/dev/reference/coord_trans_xy.md)
  for ggplot2 version 3.4.0
  ([\#44](https://github.com/willgearty/deeptime/issues/44))
- Fixed
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
  for ggplot2 version 3.4.0
- Added tests for both ggplot2 version 3.3.6 and 3.4.0
- Removed uses of
  [`getFromNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html)
  ([\#45](https://github.com/willgearty/deeptime/issues/45))

## deeptime 0.2.3

CRAN release: 2022-09-20

- Updated built-in timescales to 2022/02 version of GTS
- Better name handling and documentation for `getScaleData()`
  ([\#42](https://github.com/willgearty/deeptime/issues/42))

## deeptime 0.2.2

CRAN release: 2022-05-18

- Skip tests when suggested packages are unavailable
- Added the ability to specify label color
  ([\#40](https://github.com/willgearty/deeptime/issues/40))

## deeptime 0.2.1

CRAN release: 2022-01-10

Updated built-in timescales

- Updated the built-in timescales using the Macrostrat API
  ([\#39](https://github.com/willgearty/deeptime/issues/39))
- Fixed CRAN checks when ggtree is missing
  ([\#38](https://github.com/willgearty/deeptime/issues/38))

## deeptime 0.2.0

CRAN release: 2021-09-02

Several new features and bug fixes.

- coord_geo() now works with discrete axes
  ([\#32](https://github.com/willgearty/deeptime/issues/32) and
  [\#35](https://github.com/willgearty/deeptime/issues/35))
- Fixed a bug that prevented the use of custom interval data
- Added option to use ggfittext to automatically resize labels
  ([\#19](https://github.com/willgearty/deeptime/issues/19))
- Added
  [`coord_trans_flip()`](https://williamgearty.com/deeptime/dev/reference/coord_trans_flip.md)
  ([\#20](https://github.com/willgearty/deeptime/issues/20))
- Fixed axis labels for
  [`coord_trans_xy()`](https://williamgearty.com/deeptime/dev/reference/coord_trans_xy.md)
  ([\#36](https://github.com/willgearty/deeptime/issues/36))
- Added tests with testthat
  ([\#34](https://github.com/willgearty/deeptime/issues/34))

## deeptime 0.1.0

CRAN release: 2021-07-23

First minor release (released on CRAN!)

- Fixed ability to use text transformations (from `scales`) in
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
  ([\#30](https://github.com/willgearty/deeptime/issues/30))
- Added automatic R CMD check Github Actions
- Fixed S3 method consistency and other R CMD check warnings and notes
- Fixed
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
  when axis breaks were reversed or NULL
  ([\#23](https://github.com/willgearty/deeptime/issues/23) and
  [\#29](https://github.com/willgearty/deeptime/issues/29))
- Added packages from examples and README to `Suggests`

## deeptime 0.0.6

Several bug fixes and minor features.

- Added ability to specify a layout for
  [`ggarrange2()`](https://williamgearty.com/deeptime/dev/reference/ggarrange2.md)
  ([\#21](https://github.com/willgearty/deeptime/issues/21))
- Added ability to center labels for time intervals that are broken by
  axis limits ([\#18](https://github.com/willgearty/deeptime/issues/18))
- Fixed
  [`gggeo_scale()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale.md)
  for `ggplot2`\>=3.3.0
  ([\#22](https://github.com/willgearty/deeptime/issues/22))
- Replaced README examples with examples using real data
  ([\#28](https://github.com/willgearty/deeptime/issues/28))
- Fixed using first letters for the `pos` argument
- Fixed label order
  ([\#26](https://github.com/willgearty/deeptime/issues/26))
- Switched from Travis to Github Actions
  ([\#27](https://github.com/willgearty/deeptime/issues/27))
- Fixed handling of axis tick labels for coord_trans_xy
  ([\#31](https://github.com/willgearty/deeptime/issues/31))

## deeptime 0.0.5

Added
[`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md),
which is a coordinate system for `ggplot2` that allows for adding highly
customized timescales to `ggplot` objects. Both
[`gggeo_scale()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale.md)
and `gggeo_scale_old` are preserved but will receive notably less
maintenance moving forward.

- Updated examples and documentation to use
  [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
- Added
  [`coord_trans_xy()`](https://williamgearty.com/deeptime/dev/reference/coord_trans_xy.md),
  which is a coordinate system for `ggplot2` that is similar to
  [`coord_trans()`](https://ggplot2.tidyverse.org/reference/coord_transform.html)
  but allows for 2-dimensional transformations
- Added
  [`disparity_through_time()`](https://williamgearty.com/deeptime/dev/reference/disparity_through_time.md)
  which allows for plotting 2-dimensional data across a 3rd dimension
  (using `lattice`)

## deeptime 0.0.4

[`ggarrange2()`](https://williamgearty.com/deeptime/dev/reference/ggarrange2.md)
now accepts `geo_scale` objects and other grobified ggplots

- Fixed missing axes and axis titles

## deeptime 0.0.3

Minor release for bug fixes

- Added options to customize borders and line width
- Fixed abbreviations for stages named “Series X”
  ([\#9](https://github.com/willgearty/deeptime/issues/9))
- Fixed scale when x-axis crosses 0
  ([\#10](https://github.com/willgearty/deeptime/issues/10),
  [\#12](https://github.com/willgearty/deeptime/issues/12))
- Preserve margins of original plots
  ([\#13](https://github.com/willgearty/deeptime/issues/13))
- Fixed scale for plots with multiple panels

## deeptime 0.0.2

Complete redesign of
[`gggeo_scale()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale.md)
that adds the scale outside of the plotting space using `gtable`. The
old version of
[`gggeo_scale()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale.md)
is preserved as
[`gggeo_scale_old()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale_old.md).

- Can now pull timescales from the Macrostrat API
  ([\#8](https://github.com/willgearty/deeptime/issues/8))

## deeptime 0.0.1

First full release. Adds scale onto the bottom of a ggplot.

- Depends on R\>3.4
  ([\#1](https://github.com/willgearty/deeptime/issues/1))
- Fixed adding scales to faceted plots
  ([\#2](https://github.com/willgearty/deeptime/issues/2))
- Added timescale data from PBDB API
  ([\#3](https://github.com/willgearty/deeptime/issues/3))
- Fixed the use of ggtree when a geom has its own data
  ([\#5](https://github.com/willgearty/deeptime/issues/5))
- Fixed the stacking of multiple scales
  ([\#6](https://github.com/willgearty/deeptime/issues/6))
- Fixed documentation spelling
  ([\#7](https://github.com/willgearty/deeptime/issues/7))
