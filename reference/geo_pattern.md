# Get a FGDC geologic plotting pattern

Retrieve a single geologic pattern as defined in the [FGDC Digital
Cartographic Standard for Geologic Map
Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd.php) by the
[U.S. Geological Survey](https://www.usgs.gov/) and the [Geologic Data
Subcommittee (GDS)](https://ngmdb.usgs.gov/fgdc_gds/index.php) of the
[Federal Geographic Data Committee (FGDC)](https://www.fgdc.gov/).

## Usage

``` r
geo_pattern(
  code,
  scale = 2,
  col = NULL,
  fill = NULL,
  alpha = NULL,
  bg = "white"
)

geo_grob(code, col = NULL, fill = NULL, alpha = NULL, bg = "white")
```

## Arguments

- code:

  The number corresponding to the pattern to return. Strings and numbers
  are permitted.

- scale:

  The visual scale of the pattern (higher values mean the pattern is
  more zoomed in).

- col:

  The color to use for the lines of the pattern.

- fill:

  The color used to fill various closed shapes (e.g., circles) in the
  pattern.

- alpha:

  The transparency to use for the fill of the pattern.

- bg:

  The background color to use for the pattern.

## Value

`geo_grob()` returns a [grob](https://rdrr.io/r/grid/grid.grob.html)
object with a single instance of the desired pattern. `geo_pattern()`
returns a [GridPattern](https://rdrr.io/r/grid/patterns.html) object
with a repeated instance of the desired pattern.

## Details

For specific codes, see the "pattern numbers" in the [full pattern
chart](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd/fgdc-geolsym-patternchart.pdf)
for valid `code` values. Daven Quinn has also assembled more accessible
documentation of the [map
patterns/codes](https://davenquinn.com/projects/geologic-patterns/#pattern-reference)
and [lithology
patterns/codes](https://davenquinn.com/projects/geologic-patterns/#series-600).
The set of patterns with names is also included in the built-in dataset
[fgdc_names](https://williamgearty.com/deeptime/reference/fgdc_names.md)
(and a label dictionary is available using
[`fgdc_dict()`](https://williamgearty.com/deeptime/reference/fgdc_dict.md).
[`rmacrostrat::def_lithologies()`](https://rmacrostrat.palaeoverse.org/reference/def_lithologies.html)
can also be used to look up pattern codes for various lithologies (see
the "fill" column). Note that codes associated with color variants
(e.g., "101-M") are supported but will result in the default color
variant instead (usually black and white, e.g., "101-K").

These patterns were originally processed and optimized by Daven Quinn
and are hosted on
[GitHub](https://github.com/davenquinn/geologic-patterns/).

## See also

FGDC patterns:
[`fgdc_dict()`](https://williamgearty.com/deeptime/reference/fgdc_dict.md),
[`fgdc_names`](https://williamgearty.com/deeptime/reference/fgdc_names.md),
[`grid.pattern_geo()`](https://williamgearty.com/deeptime/reference/grid.pattern_geo.md),
[`scale_fill_geopattern()`](https://williamgearty.com/deeptime/reference/scale_fill_geopattern.md)

## Examples

``` r
library(grid)
# Get a generic igneous pattern
pattern1 <- geo_pattern(code = "313-K")
# Get the pattern for a sandstone
pattern2 <- geo_pattern(code = "607")

# plot the two patterns
grid.newpage()
grid.draw(rectGrob(gp = gpar(fill = pattern1)))

grid.newpage()
grid.draw(rectGrob(gp = gpar(fill = pattern2)))
```
