# Plot an individual FGDC pattern using grid

This function can be used to plot a single geologic pattern as defined
in the [FGDC Digital Cartographic Standard for Geologic Map
Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd.php) by the
[U.S. Geological Survey](https://www.usgs.gov/) and the [Geologic Data
Subcommittee (GDS)](https://ngmdb.usgs.gov/fgdc_gds/index.php) of the
[Federal Geographic Data Committee (FGDC)](https://www.fgdc.gov/). The
pattern is plotted on the existing canvas (i.e., use
[`grid::grid.newpage()`](https://rdrr.io/r/grid/grid.newpage.html) to
make a new canvas).

## Usage

``` r
grid.pattern_geo(params, boundary_df, aspect_ratio, legend = FALSE)
```

## Arguments

- params:

  A list of pattern parameters to customize the plotted pattern (see
  "Details").

- boundary_df:

  A `data.frame` consisting of three columns: "x" (x-coordinates), "y"
  (y-coordinates), and "id" (polygon group ID). This `data.frame`
  defines the boundary (as a closed polygon) of the plotted pattern.

- aspect_ratio:

  Unused.

- legend:

  Unused.

## Details

The following `params` are accepted:

- **`pattern_alpha`**:

  Alpha transparency for pattern. default: 1

- **`pattern_colour`**:

  Color used for strokes and points in the pattern. default: 'black'

- **`pattern_fill`**:

  Color used to fill various closed shapes (e.g., circles) in the
  pattern. default: `NA`

- **`pattern_scale`**:

  Scale. default: 2

- **`pattern_type`**:

  Code for the FGDC pattern to use. See
  [`geo_pattern()`](https://williamgearty.com/deeptime/dev/reference/geo_pattern.md)
  for more details. default: "101"

- **`fill`**:

  Color used for the background. default: "white"

## Warning

Pattern fills are not supported on all graphics devices. Not all devices
are under active development, and such devices are unlikely to add
support for new features (such as pattern fills). The new features have
only been implemented on a subset of graphics devices so far:
[`cairo_pdf()`](https://rdrr.io/r/grDevices/cairo.html),
[`cairo_ps()`](https://rdrr.io/r/grDevices/cairo.html),
[`x11(type="cairo")`](https://rdrr.io/r/grDevices/x11.html),
[`png(type="cairo")`](https://rdrr.io/r/grDevices/png.html),
[`jpeg(type="cairo")`](https://rdrr.io/r/grDevices/png.html),
[`tiff(type="cairo")`](https://rdrr.io/r/grDevices/png.html),
[`svg()`](https://rdrr.io/r/grDevices/cairo.html), and
[`pdf()`](https://rdrr.io/r/grDevices/pdf.html). Although there is no
support yet for [`quartz()`](https://rdrr.io/r/grDevices/quartz.html) or
[`windows()`](https://rdrr.io/r/grDevices/windows.html), almost all of
the graphics devices above will work on all major platforms. Further,
the [ragg](https://ragg.r-lib.org/) and
[svglite](https://svglite.r-lib.org/index.html) packages contain
graphics devices that support patterns. When using a graphics device
where patterns are not supported, closed shapes will be rendered with a
transparent fill. Note that, at least on Windows machines, the default
device in RStudio and in the knitr package is
[`png()`](https://rdrr.io/r/grDevices/png.html), which does not support
patterns. In RStudio, you can go to ‘Tools \> Global Options \> General
\> Graphics’ and choose the ‘Cairo PNG’ device from the dropdown menu to
display patterns. Similar issues may arise when using RStudio on other
operating systems.

## See also

FGDC patterns:
[`fgdc_dict()`](https://williamgearty.com/deeptime/dev/reference/fgdc_dict.md),
[`fgdc_names`](https://williamgearty.com/deeptime/dev/reference/fgdc_names.md),
[`geo_pattern()`](https://williamgearty.com/deeptime/dev/reference/geo_pattern.md),
[`scale_fill_geopattern()`](https://williamgearty.com/deeptime/dev/reference/scale_fill_geopattern.md)

## Examples

``` r
# use the function directly to make a hexagon with the pattern
library(grid)
x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
grid.newpage()
grid.pattern_geo(params = list(pattern_type = "633", pattern_scale = 4),
                 boundary_df = data.frame(x, y, id = 1))

# use the function via ggpattern by specifying `pattern = 'geo'`
library(ggplot2)
library(ggpattern)
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df, aes(trt, outcome)) +
  geom_col_pattern(aes(color = trt, pattern_type = trt), pattern = 'geo',
  pattern_color = "black", fill = "white", pattern_fill = "white") +
  scale_pattern_type_manual(values = c("101", "313", "634")) +
  scale_color_viridis_d() +
  theme(legend.key.size = unit(1.5, 'cm'))
```
