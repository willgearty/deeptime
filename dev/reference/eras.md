# Era data from the International Commission on Stratigraphy (v2024/12)

A dataset containing the boundary ages, abbreviations, and colors for
the eras of the Geologic Time Scale. Based on The ICS International
Chronostratigraphic Chart (v2024/12), by Cohen, Finney, Gibbard, and
Fan.

## Usage

``` r
eras
```

## Format

A data frame with 10 rows and 5 variables:

- name:

  era name

- max_age:

  maximum age, in millions of years

- min_age:

  minimum age, in millions of years

- abbr:

  era name abbreviations

- color:

  the colors for each era, according to the Commission for the
  Geological Map of the World

- lab_color:

  the label colors for each era, either white or black, whichever has
  better contrast with the background color, based on [recommendations
  by the International Telecommunication
  Union](https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en)

## Source

<https://stratigraphy.org> via
<https://macrostrat.org/api/v2/defs/intervals?timescale=international%20eras>

## See also

Other built-in timescales:
[`eons`](https://williamgearty.com/deeptime/dev/reference/eons.md),
[`epochs`](https://williamgearty.com/deeptime/dev/reference/epochs.md),
[`periods`](https://williamgearty.com/deeptime/dev/reference/periods.md),
[`stages`](https://williamgearty.com/deeptime/dev/reference/stages.md)
