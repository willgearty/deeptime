# Decompose a ggplot gtable

Reformat the gtable associated with a ggplot object into a 7x7 gtable
where the central cell corresponds to the plot panel(s), the rectangle
of cells around that corresponds to the axes, and the rectangle of cells
around that corresponds to the axis titles.

## Usage

``` r
gtable_frame2(
  g,
  width = unit(1, "null"),
  height = unit(1, "null"),
  debug = FALSE
)
```

## Arguments

- g:

  gtable

- width:

  requested width

- height:

  requested height

- debug:

  logical draw gtable cells

## Value

7x7 gtable wrapping the plot

## Examples

``` r
library(grid)
library(gridExtra)
library(ggplot2)
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()

p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() +
  facet_wrap(~cyl, ncol = 2, scales = "free") +
  guides(colour = "none") +
  theme()

p3 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() +
  facet_grid(. ~ cyl, scales = "free")

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
fg1 <- gtable_frame2(g1)
fg2 <- gtable_frame2(g2)
fg12 <- gtable_frame2(gtable_rbind(fg1, fg2), width = unit(2, "null"),
                      height = unit(1, "null"))
fg3 <- gtable_frame2(g3, width = unit(1, "null"), height = unit(1, "null"))
grid.newpage()
combined <- gtable_cbind(fg12, fg3)
grid.draw(combined)
```
