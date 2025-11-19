# Add a geologic scale on top of ggplots

**\[deprecated\]**

This function takes a ggplot object and adds a geologic time scale at
the specified side.

## Usage

``` r
gggeo_scale_old(
  gg,
  dat = "periods",
  fill = NULL,
  color = "black",
  alpha = 1,
  height = 0.05,
  gap = 0,
  pos = "bottom",
  lab = TRUE,
  rot = 0,
  abbrv = TRUE,
  skip = c("Quaternary", "Holocene", "Late Pleistocene"),
  size = 5,
  neg = FALSE
)
```

## Arguments

- gg:

  The ggplot object.

- dat:

  Either A) a string indicating a built-in dataframe with interval data
  from the ICS ("periods", "epochs", "stages", "eons", or "eras"), B) a
  string indicating a timescale from macrostrat (see list here:
  <https://macrostrat.org/api/defs/timescales?all>), or C) a custom
  dataframe of time interval boundaries (see Details).

- fill:

  The fill color of the boxes. The default is to use the colors included
  in `dat`. If a custom dataset is provided with `dat` without color and
  without fill, a greyscale will be used. Custom fill colors can be
  provided with this option and will be recycled if/as necessary.

- color:

  The outline color of the interval boxes.

- alpha:

  The transparency of the fill colors.

- height:

  The proportional height (or width if `pos` is `left` or `right`) of
  the entire plot to use for the scale.

- gap:

  The proportional height (or width) of the entire plot to use as a gap
  between the axis and the scale.

- pos:

  Which side to add the scale to (left, right, top, or bottom). First
  letter may also be used.

- lab:

  Whether to include labels.

- rot:

  The amount of counter-clockwise rotation to add to the labels (in
  degrees).

- abbrv:

  If including labels, whether to use abbreviations instead of full
  interval names.

- skip:

  A vector of interval names indicating which intervals should not be
  labeled.

- size:

  Label size.

- neg:

  Set this to true if your x-axis is using negative values.

## Value

A ggplot object.

## Details

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

## Life cycle

This function is fully deprecated in favor of
[`coord_geo()`](https://williamgearty.com/deeptime/dev/reference/coord_geo.md)
as of **deeptime** version 2.0.0. It will be removed in a future
version.
