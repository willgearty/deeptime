# Add a geologic scale below ggplots using *grid*

**\[deprecated\]**

This function takes a ggplot object and adds a geologic time scale at
the specified side using the grid and gtable packages.

If custom data is provided (with `dat`), it should consist of at least 3
columns of data. See `data(periods)` for an example.

- The `name` column lists the names of each time interval. These will be
  used as labels if no abbreviations are provided.

- The `max_age` column lists the oldest boundary of each time interval.

- The `min_age` column lists the youngest boundary of each time
  interval.

- The `abbr` column is optional and lists abbreviations that may be used
  as labels.

- The `color` column is also optional and lists a hex color code (which
  can be obtained with [`rgb()`](https://rdrr.io/r/grDevices/rgb.html))
  for each time interval.

## Usage

``` r
gggeo_scale(obj, ...)
```

## Arguments

- obj:

  An object of class `ggplot`, `gtable`, or `geo_scale` (as produced by
  this function).

## Value

A geo_scale object. Basically a gtable object but with the axis limits
included.

## Life cycle

This function is fully deprecated in favor of
[`coord_geo()`](https://williamgearty.com/deeptime/reference/coord_geo.md)
as of **deeptime** version 2.3.0. It will be removed in a future
version.
