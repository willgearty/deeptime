[![Build Status](https://travis-ci.com/willgearty/deeptime.svg?branch=master)](https://travis-ci.com/willgearty/deeptime)
[![DOI](https://zenodo.org/badge/152502088.svg)](https://zenodo.org/badge/latestdoi/152502088)

# deeptime
Tools to help with plotting data over long time intervals.

*Note #1: This package is under active development. It's strongly suggested that you upgrade to the most recent version.*

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
library(divDyn)
library(tidyverse)
data(corals)
# this is not a proper diversity curve but it gets the point across
coral_div <- corals %>% filter(stage != "") %>%
  group_by(stage) %>%
  summarise(n = n()) %>%
  mutate(stage_age = (stages$max_age[match(stage, stages$name)] + stages$min_age[match(stage, stages$name)])/2)
ggplot(coral_div) +
  geom_line(aes(x = stage_age, y = n)) +
  scale_x_reverse("Age (Ma)") +
  ylab("Coral Genera") +
  coord_geo(xlim = c(250, 0), ylim = c(0, 1700)) +
  theme_classic()
```

![example bottom scale](/images/example_bottom.png?raw=true)

### Scale on left axis
```r
library(gsloid)
ggplot(lisiecki2005) +
  geom_line(aes(x = d18O, y = Time/1000), orientation = "y") +
  scale_y_reverse("Time (Ma)") +
  scale_x_reverse() +
  coord_geo(dat = "Geomagnetic Polarity Chron", xlim = c(6,2), ylim = c(6,0), pos = "left", rot = 90) +
  theme_classic()
```

![example left scale](/images/example_left.png?raw=true)

### Stack multiple scales (e.g. periods and eras)
Specify multiple scales by giving a list for `pos`. Scales are added from the inside to the outside. Other arguments can be lists or single values (either of which will be recycled if necessary).
```r
#uses coral diversity data from above
ggplot(coral_div) +
  geom_line(aes(x = stage_age, y = n)) +
  scale_x_reverse("Age (Ma)") +
  ylab("Coral Genera") +
  coord_geo(dat = list("periods", "eras"), xlim = c(250, 0), ylim = c(0, 1700),
            pos = list("b", "b"), abbrv = list(TRUE, FALSE)) +
  theme_classic()
```

![example stacked scales](/images/example_stack.png?raw=true)

### Show intervals from different scales (e.g. Geochrons vs. Foram biozones)
```r
ggplot(lisiecki2005) +
  geom_line(aes(x = d18O, y = Time/1000), orientation = "y") +
  scale_y_reverse("Time (Ma)") +
  scale_x_reverse() +
  coord_geo(dat = list("Geomagnetic Polarity Chron", "Planktic foraminiferal Primary Biozones"),
            xlim = c(6,2), ylim = c(5.5,0), pos = list("l", "r"), rot = 90, skip = "PL4", size = list(5, 4)) +
  theme_classic()
```

![example separate scales](/images/separate_scales.png?raw=true)

### Scale on faceted plot
You can change on which facets the time scale is plotted by changing the `scales` argument in `facet_wrap()`.
```r
df <- data.frame(x = runif(1000, 0, 541), y = runif(1000, 0, 8), z = sample(c(1,2,3,4), 1000, TRUE))
ggplot(df) +
    geom_point(aes(x, y)) +
    scale_x_reverse() +
    coord_geo(xlim = c(541, 0), ylim = c(0,8)) +
    theme_classic() +
    facet_wrap(~z, nrow = 2, scales = "free_x")
```

![example faceted scale](/images/example_facet.png?raw=true)

### Add scale to a phylogeny
```r
library(phytools)
library(ggtree)
data(mammal.tree)
p <- ggtree(mammal.tree) +
  coord_geo(xlim = c(-75,0), ylim = c(-2,Ntip(mammal.tree)), neg = TRUE, abbrv = FALSE) +
  scale_x_continuous(breaks=seq(-80,0,20), labels=abs(seq(-80,0,20))) +
  theme_tree2()
revts(p)
```

![example phylogeny](/images/example_phylo.png?raw=true)

### Add scale to a phylogeny with only fossil taxa
```r
library(phytools)
library(ggtree)
library(paleotree)
data(RaiaCopesRule)
ggtree(ceratopsianTreeRaia, position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
  coord_geo(xlim = c(-163.5,-66), ylim = c(-2,Ntip(ceratopsianTreeRaia)), pos = list("bottom", "bottom"),
            skip = c("Paleocene", "Middle Jurassic"), dat = list("epochs", "periods"), abbrv = FALSE,
            size = list(4,5), neg = TRUE, center_end_labels = TRUE) +
  scale_x_continuous(breaks = -rev(epochs$max_age), labels = rev(epochs$max_age)) +
  theme_tree2() +
  theme(plot.margin = margin(7,11,7,11))
```

![example fossil_phylogeny](/images/example_fossil_phylo.png?raw=true)

### Combine plots with timescales and plots without timescales
```r
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()
p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_wrap( ~ cyl, ncol=2, scales = 'free') +
  guides(colour='none') +
  theme()
p3 <- ggplot() +
  geom_point(aes(y = runif(1000, 0, 8), x = runif(1000, 0, 1000))) +
  scale_x_reverse() +
  coord_geo(xlim = c(1000, 0), ylim = c(0,8)) +
  theme_classic()
ggarrange2(ggarrange2(p1, p2, widths = c(2,1), draw = FALSE), p3, nrow = 2)
```

![example ggarrange2](/images/ggarrange2.png?raw=true)

### Plot disparity through time
#### With ggplot
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

#### With base R/lattice
```r
#make some data
g <- data.frame(x = runif(100, 0, 60), y = runif(100,0,10),
                z = factor(rep(periods$name[1:5], each=20), levels = periods$name[1:5]))

#plot data
disparity_through_time(z~x*y, data = g, groups = z, aspect = c(1.5,2), xlim = c(0,60), ylim = c(0,10),
                       col.regions = "lightgreen", col.point = c("red","blue"))
```

![example disparity_lattice](/images/disparity_lattice.png?raw=true)
