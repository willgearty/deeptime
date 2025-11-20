# Get geological timescale data

This function takes a name of a geological timescale and returns data
for the timescale. Valid names include those of built-in `data.frames`
([`periods()`](https://williamgearty.com/deeptime/reference/periods.md),
[`epochs()`](https://williamgearty.com/deeptime/reference/epochs.md),
[`stages()`](https://williamgearty.com/deeptime/reference/stages.md),
[`eons()`](https://williamgearty.com/deeptime/reference/eons.md), or
[`eras()`](https://williamgearty.com/deeptime/reference/eras.md)),
partial matches of those names (e.g., "per" or "age"), and partial or
exact matches to those hosted by Macrostrat (see Details below). Note
that the colors in the built-in `data.frames` are according to the
Commission for the Geological Map of the World. If you would like to
obtain custom Macrostrat colors that are better for mapping, you should
specify the full name of a timescale (e.g., "international periods") and
set `true_colors` to `FALSE`. Note that these colors only vary for the
Precambrian.

## Usage

``` r
get_scale_data(name, true_colors = TRUE)
```

## Arguments

- name:

  The name of the desired timescale.

- true_colors:

  Return original international time scale colors? (as opposed to custom
  Macrostrat plotting colors)

## Value

A `data.frame` with the following columns:

- name:

  the names of the time intervals

- max_age:

  the oldest boundaries of the time intervals, in millions of years

- min_age:

  the youngest boundaries of the time intervals, in millions of years

- abbr:

  either traditional abbreviations of the names of the time intervals
  (if they exist) or custom abbreviations created with R

- color:

  hex color codes associated with the time intervals (if applicable)

- lab_color:

  default label colors for the time interals, either white or black,
  whichever has better contrast with the background color, based on
  [recommendations by the International Telecommunication
  Union](https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en)

## Details

The following timescales are available from the Macrostrat API as of
2025-11-19:

- international ages

- international epochs

- international periods

- calcareous nannoplankton zones

- New Zealand ages

- custom COSUNA

- North American land mammal ages

- international intervals

- COSUNA

- international eras

- international eons

- Trilobite Zonation - Laurentia

- Conodont Zonation

- North American Regional

- Ammonite Zonation - Boreal

- Ammonite Zonation - Western Interior

- international intervals covering all time

- Scotese Reconstruction

- Geomagnetic Polarity Chron

- Geomagnetic Polarity Subchron

- Planktic foraminiferal Primary Biozones

- Planktic foraminiferal Secondary Biozones

- Planktic foraminiferal datums

- Martian Periods

- Martian Epochs

- Cretaceous Planktic foraminifer zonations

- Low latitude radiolarian zonation

- Neogene North Pacific Diatom Biochronology

- Neogene North Pacific Diatom Biochronology Subzones

- Siberian Regional

- Australian Regional

- Western Europe Regional

- Russian Platform Regional Stages

- Russian Precambrian Eras

- Russian Precambrian Eons

- Russian Epochs

- Russian Stages

The most up-to-date list can be found via the Macrostrat API
[here](https://macrostrat.org/api/defs/timescales?all).
