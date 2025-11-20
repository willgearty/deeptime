# Geologic pattern fill scale

Fill scale using the [FGDC Digital Cartographic Standard for Geologic
Map Symbolization](https://ngmdb.usgs.gov/fgdc_gds/geolsymstd.php). Fill
values should correspond to specific pattern codes (see "Details").

## Usage

``` r
scale_fill_geopattern(na.value = "grey50", ...)
```

## Arguments

- na.value:

  The aesthetic value to use for missing (NA) values. May be either a
  color or a [GridPattern](https://rdrr.io/r/grid/patterns.html) object
  (such as that returned by
  [`geo_pattern()`](https://williamgearty.com/deeptime/reference/geo_pattern.md)).

- ...:

  Arguments passed on to
  [`ggplot2::discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

  `scale_name`

  :   **\[deprecated\]** The name of the scale that should be used for
      error messages associated with this scale.

  `name`

  :   The name of the scale. Used as the axis or legend title. If
      [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html),
      the default, the name of the scale is taken from the first mapping
      used for that aesthetic. If `NULL`, the legend title will be
      omitted.

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (the scale limits)

      - A character vector of breaks

      - A function that takes the limits as input and returns breaks as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (none for discrete, one minor break
        between each major break for continuous)

      - A numeric vector of positions

      - A function that given the limits returns a vector of minor
        breaks. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. When the function has two arguments, it will
        be given the limits and major break positions.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default labels computed by the transformation object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `limits`

  :   One of:

      - `NULL` to use the default scale values

      - A character vector that defines possible values of the scale and
        their order

      - A function that accepts the existing (automatic) values and
        returns new ones. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `na.translate`

  :   Unlike continuous scales, discrete scales can easily show missing
      values, and do so by default. If you want to remove missing values
      from a discrete scale, specify `na.translate = FALSE`.

  `drop`

  :   Should unused factor levels be omitted from the scale? The
      default, `TRUE`, uses the levels that appear in the data; `FALSE`
      includes the levels in the factor. Please note that to display
      every level in a legend, the layer should use
      `show.legend = TRUE`.

  `guide`

  :   A function used to create a guide or its name. See
      [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html)
      for more information.

  `call`

  :   The `call` used to construct the scale for reporting messages.

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
[`fgdc_dict()`](https://williamgearty.com/deeptime/reference/fgdc_dict.md),
[`fgdc_names`](https://williamgearty.com/deeptime/reference/fgdc_names.md),
[`geo_pattern()`](https://williamgearty.com/deeptime/reference/geo_pattern.md),
[`grid.pattern_geo()`](https://williamgearty.com/deeptime/reference/grid.pattern_geo.md)

## Examples

``` r
library(ggplot2)
vals <- c("101", "313", "603", "733")
ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
  geom_bar() +
  scale_fill_geopattern(name = NULL)
```
