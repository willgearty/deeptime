# deeptime
Tools (currently just one function) to help with plotting data over long time intervals.

## To install
```r
library(devtools)
install_github("willgearty/deeptime")
```

## To use
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

```r
p <- ggplot() +
  geom_point(aes(x = runif(1000, .5, 8), y = runif(1000, 0, 541))) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(0, 8), ylim = c(0,541), expand = FALSE) +
  theme_classic()
gggeo_scale(p, pos = "left")
```

![example left scale](/images/example_left.png?raw=true)

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

