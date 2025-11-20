# Combined wireframe and cloud panel

Plots the provided data on 2-D surfaces within a 3-D framework. See
[`disparity_through_time()`](https://williamgearty.com/deeptime/reference/disparity_through_time.md).

## Usage

``` r
panel.disparity(x, y, z, groups, subscripts, ...)
```

## Arguments

- x, y, z, groups, subscripts, ...:

  Same as for
  [`lattice::panel.cloud()`](https://rdrr.io/pkg/lattice/man/panel.cloud.html)

## Value

No return value, plots the results of both
[`lattice::panel.cloud()`](https://rdrr.io/pkg/lattice/man/panel.cloud.html)
and
[`lattice::panel.wireframe()`](https://rdrr.io/pkg/lattice/man/panel.cloud.html).
