Tools (currently just one function) to help with plotting data over long time intervals.

To install:
```r
library(devtools)
install_github("willgearty/deeptime")
```

To use:
```r
library(deeptime)
library(ggplot2)
p <- ggplot() +
  geom_point(aes(y = runif(1000, .5, 8), x = runif(1000, 0, 1000))) +
  scale_x_reverse() +
  coord_cartesian(xlim = c(0, 1000), ylim = c(0,8), expand = FALSE) +
  theme_classic()
gggeo_scale(p)
```
