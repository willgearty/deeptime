# deeptime (development version)

# deeptime 2.2.0

This new minor version of deeptime introduces several new features and bug fixes. It also is compatible with version 4 of ggplot2.

New features:

* Added `geom_text_phylo()` for adding node/tip labels to phylogenies when using `coord_geo_radial()`
* Updated built-in timescales to 2024/12 version of GTS
* Removed the defaults for the `colors` argument in `facet_grid_color()`, `facet_wrap_color()`, `facet_nested_color()`, and `facet_nested_wrap_color()`
  * Added `facet_grid_geo()`, `facet_wrap_geo()`, `facet_nested_geo()`, and `facet_nested_wrap_geo()` which now serve as aliases for these functions with their original defaults (#81)
* Added legends to the examples in the "Plotting geological/stratigraphical patterns" vignette (#78)
* Added facetting examples to the "Plotting temporal data" vignette (#80)

Deprecation:

* Advanced the deprecation of `coord_geo_polar()`. Use of this function will result in a warning. This function will be removed in a future version of deeptime.
* Advanced the deprecation of `getScaleData()`. This function has been fully deprecated from deeptime in favor of `get_scale_data()`.

Bug fixes:

* Fixed `gtable_frame2()` and `ggarrange2()` for plots that use `theme_void()` (#74)
* Fixed `disparity_through_time()` for rare edge cases (#71)
* Updated `GeomPointsRange` to use the aesthetic defaults from `ggplot2::GeomPointrange`
* Updated `GeomTextPhylo` to use the aesthetic defaults from `ggplot2::GeomText`

# deeptime 2.1.0

This new minor version of deeptime introduces several new features and bug fixes.

New features:

* Added default label color values to all built-in data as a new `lab_color` column
  * The label colors are either white or black, whichever has better contrast with the background color based on [recommendations by the International Telecommunication Union](https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en)
* `get_scale_data()` now returns label colors (as above) for Macrostrat timescales
* Varying strip label colors can now be set with the `lab_colors` argument in `facet_wrap_color()` and `facet_grid_color()` (#68)
  * By default, the strip label colors are set to white or black (as above)
* Added `facet_nested_color()` and `facet_nested_wrap_color()` to plot nested faceted plots with colored strips (#55)
* `get_scale_data()` documentation now lists the available Macrostrat timescales (#69)
* `get_scale_data()` now supports partial matching to Macrostrat timescales (e.g., `get_scale_data("mammal")` will retrieve the "North American land mammal ages" timescale)

Notable changes:

* The FGDC pattern data added in deeptime 2.0.0 has been moved to a new data R package called `deeptimedata` to reduce the size of the deeptime package and the footprint of future deeptime updates

Bug fixes:

* Fixed importing some functions from `ggplot2` for `coord_geo_radial()` and `coord_geo_polar()`

# deeptime 2.0.0
__deeptime now requires ggplot2 version 3.5.0 or higher__.\
This new major version of deeptime includes several new features:

* Added FGDC/USGS geologic patterns to internal data (#52)
* Added functions to retrieve and plot FGDC/USGS geologic patterns (#52)
* Added functions to use FGDC/USGS patterns as ggplot2 fills or with the `ggpattern` package (see new vignette) (#52)
* Added the `true_colors` option to `get_scale_data()`, which, when set to `FALSE`, can be used to retrieve the custom Macrostrat colors that were previously in the built-in datasets (#62)
* Updated built-in timescales to 2023/06 version of GTS
* Added option to `coord_geo()` to abbreviate interval labels based on only the intervals that are being plotted (#64)
* Added `guide_geo()` as an alternative to `coord_geo()`, mostly for use with radial plots (#53)

Bug fixes:

* Fixed the built-in timescale datasets to have true CGMP colors for the Precambrian (#62)

Notable changes:

* The `phytools` package is no longer required to install `deeptime` but is required to use the `geom_phylomorpho()` function
* The `geomtextpath` package is no longer required to install `deeptime` but is required to add labels with the `coord_geo_polar()` or `coord_geo_radial()` functions
* The `gggeo_scale_old()` function has been fully deprecated

# deeptime 1.1.1
deeptime now requires rlang version 1.1.0 or higher. This patch version includes several bug fixes:

* Fixed several cases where vignette and documentation building and/or unit testing would fail when the Macrostrat API was down
* Fixed `coord_geo_polar()` and `coord_geo_radial()` when the r-axis has expansion (#60)
* Improved argument type checking and error handling across the entire package

# deeptime 1.1.0
deeptime now requires ggplot2 version 3.4.0 or higher. This version introduces several new features and bug fixes:

* Added `geom_points_range()`, a function designed for visualizing temporal occurrence data
* Added `facet_grid_color()` and `facet_wrap_color()` for changing strip background colors (#50)
* Added `coord_geo_radial()`, an enhanced version of `coord_geo_polar()` (only works with ggplot2 version 3.5.0 and higher) (#56)
* Added `family` and `fontface` arguments to `coord_geo()`
* Added the ability to include interval labels with `coord_geo_polar()` (#48)
* Fixed the interaction between `coord_geo()` and `ggsave()` (#49)
* Fixed `size = "auto"` when `center_end_labels = TRUE` in `coord_geo()`
* Updated several functions to work with ggplot2 version 3.5.0

Notable changes:

* The names for the theme elements that are used for `coord_geo_polar()` are now prepended with "deeptime." (e.g., `deeptime.axis.line.r`) (#51)
* ggplot2 version 3.5.0 has broken several edge cases of `coord_trans_xy()`. See the documentation for more details. This will be fixed in a future deeptime release.

# deeptime 1.0.1

* Fixed the bounds of the Kungurian stage in the built-in stages data
* Updated License to GPL >= 3
* Changed tidyverse to dplyr and magrittr in Suggests and vignettes

# deeptime 1.0.0

* Updated built-in timescales to 2022/10 version of GTS
* Added a geom for making phylomorphospaces (`geom_phylomorpho()`)
* Added vignettes
* Renamed `getScaleData()` to `get_scale_data()`
* Soft deprecated `gggeo_scale()` and `gggeo_scale_old()`

# deeptime 0.3.0
deeptime now requires ggplot2 version 3.3.0 or higher. Several new features and fixed compatibility with various ggplot2 versions.

* Added `coord_geo_polar()`, for adding timescales to polar plots (#33)
* Added color and fill discrete scales that use the GTS colors (#43)
* Fixed `coord_geo_polar()` and `coord_trans_xy()` for ggplot2 version 3.4.0 (#44)
* Fixed `coord_geo()` for ggplot2 version 3.4.0
* Added tests for both ggplot2 version 3.3.6 and 3.4.0
* Removed uses of `getFromNamespace()` (#45)

# deeptime 0.2.3

* Updated built-in timescales to 2022/02 version of GTS
* Better name handling and documentation for `getScaleData()` (#42)

# deeptime 0.2.2

* Skip tests when suggested packages are unavailable
* Added the ability to specify label color (#40)

# deeptime 0.2.1
Updated built-in timescales

* Updated the built-in timescales using the Macrostrat API (#39)
* Fixed CRAN checks when ggtree is missing (#38)

# deeptime 0.2.0
Several new features and bug fixes.

* coord_geo() now works with discrete axes (#32 and #35)
* Fixed a bug that prevented the use of custom interval data
* Added option to use ggfittext to automatically resize labels (#19)
* Added `coord_trans_flip()` (#20)
* Fixed axis labels for `coord_trans_xy()` (#36)
* Added tests with testthat (#34)

# deeptime 0.1.0
First minor release (released on CRAN!)

* Fixed ability to use text transformations (from `scales`) in `coord_geo()` (#30)
* Added automatic R CMD check Github Actions
* Fixed S3 method consistency and other R CMD check warnings and notes
* Fixed `coord_geo()` when axis breaks were reversed or NULL (#23 and #29)
* Added packages from examples and README to `Suggests`

# deeptime 0.0.6
Several bug fixes and minor features.

* Added ability to specify a layout for `ggarrange2()` (#21)
* Added ability to center labels for time intervals that are broken by axis limits (#18)
* Fixed `gggeo_scale()` for `ggplot2`>=3.3.0 (#22)
* Replaced README examples with examples using real data (#28)
* Fixed using first letters for the `pos` argument
* Fixed label order (#26)
* Switched from Travis to Github Actions (#27)
* Fixed handling of axis tick labels for coord_trans_xy (#31)

# deeptime 0.0.5
Added `coord_geo()`, which is a coordinate system for `ggplot2` that allows for adding highly customized timescales to `ggplot` objects. Both `gggeo_scale()` and `gggeo_scale_old` are preserved but will receive notably less maintenance moving forward.

* Updated examples and documentation to use `coord_geo()`
* Added `coord_trans_xy()`, which is a coordinate system for `ggplot2` that is similar to `coord_trans()` but allows for 2-dimensional transformations
* Added `disparity_through_time()` which allows for plotting 2-dimensional data across a 3rd dimension (using `lattice`)

# deeptime 0.0.4
`ggarrange2()` now accepts `geo_scale` objects and other grobified ggplots

* Fixed missing axes and axis titles

# deeptime 0.0.3
Minor release for bug fixes

* Added options to customize borders and line width
* Fixed abbreviations for stages named "Series X" (#9)
* Fixed scale when x-axis crosses 0 (#10, #12)
* Preserve margins of original plots (#13)
* Fixed scale for plots with multiple panels

# deeptime 0.0.2
Complete redesign of `gggeo_scale()` that adds the scale outside of the plotting space using `gtable`. The old version of `gggeo_scale()` is preserved as `gggeo_scale_old()`.

* Can now pull timescales from the Macrostrat API (#8)

# deeptime 0.0.1
First full release. Adds scale onto the bottom of a ggplot.

* Depends on R>3.4 (#1)
* Fixed adding scales to faceted plots (#2)
* Added timescale data from PBDB API (#3)
* Fixed the use of ggtree when a geom has its own data (#5)
* Fixed the stacking of multiple scales (#6)
* Fixed documentation spelling (#7)
