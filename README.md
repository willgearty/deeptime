# deeptime
Tools (currently just one function) to help with plotting data over long time intervals.

## To install
```r
library(devtools)
install_github("willgearty/deeptime")
```

## To use

### Default scale on bottom axis
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

![example bottom scale](/images/example_bottom.png?raw=true)

### Move scale to left axis
```r
p <- ggplot() +
  geom_point(aes(x = runif(1000, .5, 8), y = runif(1000, 0, 541))) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(0, 8), ylim = c(0,541), expand = FALSE) +
  theme_classic()
gggeo_scale(p, pos = "left", rot = 90)
```

![example left scale](/images/example_left.png?raw=true)

### Stack multiple scales (e.g. periods, epochs, stages)
```r
p <- ggplot() +
  geom_point(aes(y = runif(1000, 1, 8), x = runif(1000, 0, 1000))) +
  scale_x_reverse() +
  coord_cartesian(xlim = c(0, 100), ylim = c(0,8), expand = FALSE) +
  theme_classic()
p <- gggeo_scale(p, height = .03, abbrv = FALSE)
p <- gggeo_scale(p, dat = "epochs", gap = .03, height = .1, rot = 90, size = 3)
gggeo_scale(p, dat = "stages", gap = .13, height = .1, rot = 90, size = 3)
```

![example stacked scales](/images/example_stack.png?raw=true)

### Scale on faceted plot
```r
df = data.frame(x = runif(1000, 0, 541), y = runif(1000, .5, 8), z = sample(c(1,2,3,4), 1000, TRUE))
p <- ggplot(df) +
    geom_point(aes(x, y)) +
    scale_x_reverse() +
    coord_cartesian(xlim = c(0, 541), ylim = c(0,8), expand = FALSE) +
    theme_classic() +
    facet_wrap(~z, nrow = 2)
gggeo_scale(p)
```

![example faceted scale](/images/example_facet.png?raw=true)

### Add scale to phylogeny
```r
library(phytools)
library(ggtree)
tree <- pbtree(b = .03, d = .01,  n=100)
p <- ggtree(tree) +
  coord_cartesian(xlim = c(0,-500), ylim = c(-10,Ntip(tree)), expand = FALSE) +
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
  coord_cartesian(xlim = c(0,-25), ylim = c(.5,Ntip(tree)), expand = FALSE) +
  scale_x_continuous(breaks=seq(-50,50,10), labels=abs(seq(-50,50,10))) +
  theme_tree2()
p <- gggeo_scale(p, neg = TRUE, skip = NULL, abbrv = FALSE)
gggeo_scale(p, dat = "epochs", gap = .05, neg = TRUE, skip = "Holocene", abbrv = FALSE, size = 4)
```

![example fossil_phylogeny](/images/example_fossil_phylo.png?raw=true)
