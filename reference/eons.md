# Eon data from the International Commission on Stratigraphy (v2024/12)

A dataset containing the boundary ages, abbreviations, and colors for
the eons of the Geologic Time Scale. Based on The ICS International
Chronostratigraphic Chart (v2024/12), by Cohen, Finney, Gibbard, and
Fan.

## Usage

``` r
eons
```

## Format

A data frame with 3 rows and 5 variables:

- name:

  eon name

- max_age:

  maximum age, in millions of years

- min_age:

  minimum age, in millions of years

- abbr:

  eon name abbreviations

- color:

  the colors for each eon, according to the Commission for the
  Geological Map of the World

- lab_color:

  the label colors for each eon, either white or black, whichever has
  better contrast with the background color, based on [recommendations
  by the International Telecommunication
  Union](https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en)

## Source

<https://stratigraphy.org> via
<https://macrostrat.org/api/v2/defs/intervals?timescale=international%20eons>

## See also

Other built-in timescales:
[`epochs`](https://williamgearty.com/deeptime/reference/epochs.md),
[`eras`](https://williamgearty.com/deeptime/reference/eras.md),
[`periods`](https://williamgearty.com/deeptime/reference/periods.md),
[`stages`](https://williamgearty.com/deeptime/reference/stages.md)
