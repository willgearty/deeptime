# Package index

## Adding geological timescales to plots

The original purpose of deeptime,
[`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
adds a highly customizable timescale to any
[ggplot2](https://ggplot2.tidyverse.org) plot. Other functions now
supplement it to aid in the visualization of data over long time scales.

- [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
  : Transformed coordinate system with geological timescale
- [`scale_color_geo()`](https://williamgearty.com/deeptime/dev/reference/scale_geo.md)
  [`scale_fill_geo()`](https://williamgearty.com/deeptime/dev/reference/scale_geo.md)
  [`scale_discrete_geo()`](https://williamgearty.com/deeptime/dev/reference/scale_geo.md)
  : Geological Time Scale color scales
- [`get_scale_data()`](https://williamgearty.com/deeptime/dev/reference/get_scale_data.md)
  : Get geological timescale data
- [`guide_geo()`](https://williamgearty.com/deeptime/dev/reference/guide_geo.md)
  : Geological timescale axis guide

### Built-in interval data

deeptime includes a selection of built-in interval data from the most
up-to-date Geological Time Scale. These dataframes include name, max
age, min age, abbreviation, and color columns.

- [`eons`](https://williamgearty.com/deeptime/dev/reference/eons.md) :
  Eon data from the International Commission on Stratigraphy (v2024/12)
- [`eras`](https://williamgearty.com/deeptime/dev/reference/eras.md) :
  Era data from the International Commission on Stratigraphy (v2024/12)
- [`periods`](https://williamgearty.com/deeptime/dev/reference/periods.md)
  : Period data from the International Commission on Stratigraphy
  (v2024/12)
- [`epochs`](https://williamgearty.com/deeptime/dev/reference/epochs.md)
  : Epoch data from the International Commission on Stratigraphy
  (v2024/12)
- [`stages`](https://williamgearty.com/deeptime/dev/reference/stages.md)
  : Stage data from the International Commission on Stratigraphy
  (v2024/12)

### Deprecated functions

The following function(s) represent early iterations of what is now
[`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md).
These function(s) still work but are considered “deprecated”. They will
be removed in a future version of deeptime, and users are strongly
encouraged to use
[`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
instead, which has more features and is still under active development.

- [`gggeo_scale()`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale.md)
  [`print(`*`<geo_scale>`*`)`](https://williamgearty.com/deeptime/dev/reference/gggeo_scale.md)
  **\[deprecated\]** :

  Add a geologic scale below ggplots using *grid*

## Adding geological timescales to phylogenies

These functions can be used to add timescales specifically to
phylogenies that are plotted with
[ggtree](https://www.bioconductor.org/packages/ggtree).

- [`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
  : Transformed coordinate system with geological timescale
- [`coord_geo_radial()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_radial.md)
  : Enhanced polar coordinate system with geological timescale
- [`coord_geo_polar()`](https://williamgearty.com/deeptime/dev/reference/coord_geo_polar.md)
  **\[deprecated\]** : Polar coordinate system with geological timescale
- [`scale_color_geo()`](https://williamgearty.com/deeptime/dev/reference/scale_geo.md)
  [`scale_fill_geo()`](https://williamgearty.com/deeptime/dev/reference/scale_geo.md)
  [`scale_discrete_geo()`](https://williamgearty.com/deeptime/dev/reference/scale_geo.md)
  : Geological Time Scale color scales
- [`get_scale_data()`](https://williamgearty.com/deeptime/dev/reference/get_scale_data.md)
  : Get geological timescale data
- [`guide_geo()`](https://williamgearty.com/deeptime/dev/reference/guide_geo.md)
  : Geological timescale axis guide

### Adding text labels to phylogenies

When using deeptime coordinate systems, this geom may work better than
geoms from ggtree.

- [`geom_text_phylo()`](https://williamgearty.com/deeptime/dev/reference/geom_text_phylo.md)
  : Label nodes on a phylogenetic tree plotted with ggtree

## Transforming coordinate systems

These functions can be used to modify the way that your data is plotted
by [ggplot2](https://ggplot2.tidyverse.org).

- [`coord_trans_flip()`](https://williamgearty.com/deeptime/dev/reference/coord_trans_flip.md)
  : Transformed and flipped Cartesian coordinate system
- [`coord_trans_xy()`](https://williamgearty.com/deeptime/dev/reference/coord_trans_xy.md)
  : Transformed XY Cartesian coordinate system

## Plotting trait data

These functions can be used for visualizing species trait data.

- [`geom_phylomorpho()`](https://williamgearty.com/deeptime/dev/reference/geom_phylomorpho.md)
  : Plot a 2-D phylomorphospace in ggplot2
- [`disparity_through_time()`](https://williamgearty.com/deeptime/dev/reference/disparity_through_time.md)
  : Disparity through time plot using lattice
- [`panel.disparity()`](https://williamgearty.com/deeptime/dev/reference/panel.disparity.md)
  : Combined wireframe and cloud panel
- [`coord_trans_xy()`](https://williamgearty.com/deeptime/dev/reference/coord_trans_xy.md)
  : Transformed XY Cartesian coordinate system

## Plotting temporal data

These functions can be used for visualizing categorical temporal data.

- [`geom_points_range()`](https://williamgearty.com/deeptime/dev/reference/geom_points_range.md)
  [`stat_points_range()`](https://williamgearty.com/deeptime/dev/reference/geom_points_range.md)
  : Display points and their range
- [`facet_grid_color()`](https://williamgearty.com/deeptime/dev/reference/facet_grid_color.md)
  [`facet_grid_geo()`](https://williamgearty.com/deeptime/dev/reference/facet_grid_color.md)
  : Lay out panels in a grid with colored strips
- [`facet_wrap_color()`](https://williamgearty.com/deeptime/dev/reference/facet_wrap_color.md)
  [`facet_wrap_geo()`](https://williamgearty.com/deeptime/dev/reference/facet_wrap_color.md)
  : Wrap a 1d ribbon of panels into 2d with colored strips
- [`facet_nested_color()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_color.md)
  [`facet_nested_geo()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_color.md)
  : Layout panels in a grid with nested colored strips
- [`facet_nested_wrap_color()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_wrap_color.md)
  [`facet_nested_wrap_geo()`](https://williamgearty.com/deeptime/dev/reference/facet_nested_wrap_color.md)
  : Ribbon of panels with nested colored strips

## Plotting geologic data

These functions can be used for visualizing geologic/stratigraphic data.

- [`grid.pattern_geo()`](https://williamgearty.com/deeptime/dev/reference/grid.pattern_geo.md)
  : Plot an individual FGDC pattern using grid
- [`geo_pattern()`](https://williamgearty.com/deeptime/dev/reference/geo_pattern.md)
  [`geo_grob()`](https://williamgearty.com/deeptime/dev/reference/geo_pattern.md)
  : Get a FGDC geologic plotting pattern
- [`scale_fill_geopattern()`](https://williamgearty.com/deeptime/dev/reference/scale_fill_geopattern.md)
  : Geologic pattern fill scale
- [`fgdc_names`](https://williamgearty.com/deeptime/dev/reference/fgdc_names.md)
  : Pattern numbers and names for patterns from the FGDC Digital
  Cartographic Standard for Geologic Map Symbolization
- [`fgdc_dict()`](https://williamgearty.com/deeptime/dev/reference/fgdc_dict.md)
  : FGDC pattern labeling function/dictionary

## Combining and arranging plots

These functions can be used to combine and arrange plots into
publishable-quality figures.

- [`ggarrange2()`](https://williamgearty.com/deeptime/dev/reference/ggarrange2.md)
  : Combine and arrange multiple ggplot-like objects
- [`gtable_frame2()`](https://williamgearty.com/deeptime/dev/reference/gtable_frame2.md)
  : Decompose a ggplot gtable
