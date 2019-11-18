[![Build Status](https://travis-ci.com/willgearty/deeptime.svg?branch=master)](https://travis-ci.com/willgearty/deeptime)
[![DOI](https://zenodo.org/badge/152502088.svg)](https://zenodo.org/badge/latestdoi/152502088)

# deeptime
Tools to help with plotting data over long time intervals.

*Note #1: This package is under active development. It's strongly suggested that you upgrade to the most recent version.*

*Note #2: Versions 0.2+ do not work for facetted plots with multiple columns. For such cases, you can still use the old version with `gggeo_scale_old()`. Note that this may result in the scale overlapping with your data. Please use caution and set your axis limits manually to account for this. Make sure to include any fill or color scales beforehand, as you will not be able to modify these attributes for your main plot after applying this function.*

## To install
```r
library(devtools)
install_github("willgearty/deeptime")
```

## To use

### Load packages
```r
library(deeptime)
library(ggplot2)
```

### Default scale on bottom axis
```r
p <- ggplot() +
  geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 1000))) +
  scale_x_reverse() +
  coord_cartesian(xlim = c(0, 1000), ylim = c(0,8), expand = FALSE) +
  theme_classic()
gggeo_scale(p)
```

![example bottom scale](/images/example_bottom.png?raw=true)

### Move scale to left axis
```r
p <- ggplot() +
  geom_point(aes(x = runif(1000, 0, 8), y = runif(1000, 0, 541))) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(0, 8), ylim = c(0,541), expand = FALSE) +
  theme_classic()
gggeo_scale(p, pos = "left", rot = 90)
```

![example left scale](/images/example_left.png?raw=true)

### Stack multiple scales (e.g. periods, epochs, stages)
Scales are added immediately under the plot, so you'll want to work inwards from the scale you want closest to the axis to the scale you want closest to the plot.
```r
p <- ggplot() +
  geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 100))) +
  scale_x_reverse() +
  coord_cartesian(xlim = c(0, 100), ylim = c(0,8), expand = FALSE) +
  theme_classic()
p <- gggeo_scale(p, abbrv = FALSE)
p <- gggeo_scale(p, dat = "epochs", height = unit(4, "lines"), rot = 90, size = 2.5, abbrv = FALSE)
gggeo_scale(p, dat = "stages", height = unit(4, "lines"), rot = 90, size = 2.5, abbrv = FALSE)
```

![example stacked scales](/images/example_stack.png?raw=true)

### Show intervals from different scales (ICS stages vs. North American Land Mammal Ages)
```r
p <- ggplot() +
    geom_point(aes(x = runif(1000, 0, 10), y = runif(1000, 0, 65))) +
    scale_y_reverse() +
    coord_cartesian(xlim = c(0, 10), ylim = c(0,65), expand = FALSE) +
    theme_classic()
p <- gggeo_scale(p, dat = "stages", pos = "left", height = unit(4, "lines"), size = 2.5, abbrv = FALSE)
gggeo_scale(p, dat = "North American Land Mammal Ages", pos = "right", height = unit(4, "lines"), size = 2.5, abbrv = FALSE)
```

![example separate scales](/images/separate_scales.png?raw=true)

### Scale on faceted plot
Adding scales to a facetted plot with more than one column only works with the old version at the moment.
```r
df <- data.frame(x = runif(1000, 0, 541), y = runif(1000, .5, 8), z = sample(c(1,2,3,4), 1000, TRUE))
p <- ggplot(df) +
    geom_point(aes(x, y)) +
    scale_x_reverse() +
    coord_cartesian(xlim = c(0, 541), ylim = c(0,8), expand = FALSE) +
    theme_classic() +
    facet_wrap(~z, nrow = 2)
gggeo_scale_old(p)
```

![example faceted scale](/images/example_facet.png?raw=true)

Adding a scale to a facetted plot with only one column works great with the new version!
```r
df <- data.frame(x = runif(1000, 0, 541), y = runif(1000, 0, 8), z = sample(c(1,2,3,4), 1000, TRUE))
p <- ggplot(df) +
    geom_point(aes(x, y)) +
    scale_x_reverse() +
    coord_cartesian(xlim = c(0, 541), ylim = c(0,8), expand = FALSE) +
    theme_classic() +
    facet_wrap(~z, ncol = 1)
gggeo_scale(p)
```

![example faceted scale](/images/example_facet_new.png?raw=true)

### Add scale to phylogeny
```r
library(phytools)
library(ggtree)
tree <- pbtree(b = .03, d = .01,  n=100)
p <- ggtree(tree) +
  coord_cartesian(xlim = c(0,-500), ylim = c(-2,Ntip(tree)), expand = FALSE) +
  scale_x_continuous(breaks=seq(-500,0,100), labels=abs(seq(-500,0,100))) +
  theme_tree2()
p <- revts(p)
gggeo_scale(p, neg = TRUE)
```

![example phylogeny](/images/example_phylo.png?raw=true)

### Add scale to a phylogeny with only fossil taxa
```r
library(phytools)
library(ggtree)
tree<-read.tree(text="(A:3,(B:2,(C:5,D:3):2):3);")
tree$root.time<-20
p <- ggtree(tree, position = position_nudge(x = -tree$root.time)) +
  coord_cartesian(xlim = c(0,-25), ylim = c(.75,Ntip(tree)), expand = FALSE) +
  scale_x_continuous(breaks=seq(-50,50,10), labels=abs(seq(-50,50,10))) +
  theme_tree2()
p <- gggeo_scale(p, skip = NULL, abbrv = FALSE, neg = TRUE)
gggeo_scale(p, dat = "epochs", skip = "Holocene", abbrv = FALSE, size = 4, neg = TRUE)
```

![example fossil_phylogeny](/images/example_fossil_phylo.png?raw=true)

### Combine plots with timescales and plots without timescales
```r
library(ggplot2)
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()
p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_wrap( ~ cyl, ncol=2, scales = 'free') +
  guides(colour='none') +
  theme()
p <- ggplot() +
  geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 1000))) +
  scale_x_reverse() +
  coord_cartesian(xlim = c(0, 1000), ylim = c(0,8), expand = FALSE) +
  theme_classic()
p3 <- gggeo_scale(p)
ggarrange2(ggarrange2(p1, p2, widths = c(2,1), draw = FALSE), p3, nrow = 2)
```

![example ggarrange2](/images/ggarrange2.png?raw=true)

### Plot disparity through time with ggplot
```r
#make transformer
library(ggforce)
trans <- linear_trans(shear(2, 0))

#set up data to be plotted
square <- data.frame(x = c(0, 0, 4, 4), y = c(0, 1, 1, 0))
points <- data.frame(x = runif(100, 0, 4), y = runif(100, 0, 1), group = rep(1:5, each = 20))

#plot data normally
library(ggplot2)
ggplot(data = points, aes(x = x, y = y)) +
  geom_polygon(data = square, fill = NA, color = "black") +
  geom_point(color = 'black') +
  coord_cartesian(expand = FALSE) +
  theme_classic() +
  facet_wrap(~group, ncol = 1, strip.position = "right") +
  theme(panel.spacing = unit(0, "lines"), panel.background = element_blank())

#plot data with transformation
ggplot(data = points, aes(x = x, y = y)) +
  geom_polygon(data = square, fill = NA, color = "black") +
  geom_point(color = 'black') +
  coord_trans_xy(trans = trans, expand = FALSE) +
  theme_classic() +
  facet_wrap(~group, ncol = 1, strip.position = "right") +
  theme(panel.spacing = unit(0, "lines"), panel.background = element_blank())
```

![example disparity_ggplot](/images/disparity_ggplot.png?raw=true)