# Pattern numbers and names for patterns from the FGDC Digital Cartographic Standard for Geologic Map Symbolization

A dataset containing the FGDC pattern numbers and verbatim names from
the [FGDC Digital Cartographic Standard for Geologic Map
Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd.php) by the
[U.S. Geological Survey](https://www.usgs.gov/) and the [Geologic Data
Subcommittee (GDS)](https://ngmdb.usgs.gov/fgdc_gds/index.php) of the
[Federal Geographic Data Committee (FGDC)](https://www.fgdc.gov/). Only
patterns with names are included (i.e., patterns 601-733).

## Usage

``` r
fgdc_names
```

## Format

A data frame with 117 rows and 2 variables:

- code:

  the pattern number according to the FGDC standard

- name:

  the verbatim name for the pattern according to the FGDC standard

## Source

<https://ngmdb.usgs.gov/fgdc_gds/geolsymstd/fgdc-geolsym-patternchart.pdf>
via <https://github.com/davenquinn/geologic-patterns/>

## Details

These pattern numbers and names were originally extracted by Daven Quinn
and are hosted on
[GitHub](https://github.com/davenquinn/geologic-patterns/).

## See also

FGDC patterns:
[`fgdc_dict()`](https://williamgearty.com/deeptime/dev/reference/fgdc_dict.md),
[`geo_pattern()`](https://williamgearty.com/deeptime/dev/reference/geo_pattern.md),
[`grid.pattern_geo()`](https://williamgearty.com/deeptime/dev/reference/grid.pattern_geo.md),
[`scale_fill_geopattern()`](https://williamgearty.com/deeptime/dev/reference/scale_fill_geopattern.md)
