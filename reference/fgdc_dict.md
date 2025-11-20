# FGDC pattern labeling function/dictionary

Generates a [labeling
function](https://scales.r-lib.org/reference/label_dictionary.html)/dictionary
based on the built-in
[fgdc_names](https://williamgearty.com/deeptime/reference/fgdc_names.md)
dataset that can be used to retrieve the names of patterns based on
their pattern numbers/codes as defined in the [FGDC Digital Cartographic
Standard for Geologic Map
Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd.php) by the
[U.S. Geological Survey](https://www.usgs.gov/) and the [Geologic Data
Subcommittee (GDS)](https://ngmdb.usgs.gov/fgdc_gds/index.php) of the
[Federal Geographic Data Committee (FGDC)](https://www.fgdc.gov/). Like
[fgdc_names](https://williamgearty.com/deeptime/reference/fgdc_names.md),
only patterns with names are included (i.e., patterns 601-733).

## Usage

``` r
fgdc_dict(clean = TRUE, nomatch = NULL, wrap = NULL)
```

## Arguments

- clean:

  `logical`. Should the pattern names be cleaned? If `TRUE` (default),
  the names will be cleaned to remove strings such as "(2nd option)". If
  `FALSE`, the names will be verbatim from the FGDC standard. Note that
  pattern names may not be unique when this is set to `TRUE`.

- nomatch:

  `character`. A string to label values that do not match any name in
  the dictionary. When `NULL` (default), the values are kept as-is.

- wrap:

  `integer`. If not `NULL`, the pattern names will be wrapped to the
  specified width (in characters) by insertting line breaks between
  words using [`strwrap()`](https://rdrr.io/r/base/strwrap.html) (words
  will not be broken). This can be useful for making long names fit
  better in legends. If `NULL` (default), no wrapping is done.

## Value

A [labeling
function](https://scales.r-lib.org/reference/label_dictionary.html) that
takes a vector `x` of pattern numbers and returns a character vector of
`length(x)` giving the corresponding pattern names. The function is
designed to be used with the `labels` argument of ggplot2 scales, such
as
[`scale_fill_geopattern()`](https://williamgearty.com/deeptime/reference/scale_fill_geopattern.md)
and
[`ggpattern::scale_pattern_type_identity()`](https://trevorldavis.com/R/ggpattern/reference/scale_pattern_identity.html).

## See also

FGDC patterns:
[`fgdc_names`](https://williamgearty.com/deeptime/reference/fgdc_names.md),
[`geo_pattern()`](https://williamgearty.com/deeptime/reference/geo_pattern.md),
[`grid.pattern_geo()`](https://williamgearty.com/deeptime/reference/grid.pattern_geo.md),
[`scale_fill_geopattern()`](https://williamgearty.com/deeptime/reference/scale_fill_geopattern.md)

## Examples

``` r
library(ggplot2)
vals <- c("603", "626", "720", "733")
ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
  geom_bar() +
  scale_fill_geopattern(name = NULL, labels = fgdc_dict())
```
