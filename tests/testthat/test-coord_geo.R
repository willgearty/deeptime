suppressPackageStartupMessages(library(divDyn, quietly = TRUE))
suppressPackageStartupMessages(library(gsloid, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
data(corals)
coral_div <- corals %>% filter(stage != "") %>%
  group_by(stage) %>%
  summarise(n = n()) %>%
  mutate(stage_age = (stages$max_age[match(stage, stages$name)] + stages$min_age[match(stage, stages$name)])/2)

coral_div_diet <- corals %>% filter(stage != "") %>%
  group_by(diet, stage) %>%
  summarise(n = n()) %>%
  mutate(stage_age = (stages$max_age[match(stage, stages$name)] + stages$min_age[match(stage, stages$name)])/2)

coral_div_dis <- corals %>% filter(period != "") %>%
  group_by(diet, period) %>%
  summarise(n = n()) %>%
  mutate(period_age = (periods$max_age[match(period, periods$name)] + periods$min_age[match(period, periods$name)])/2) %>%
  arrange(-period_age)

test_that("coord_geo works", {
  gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_geo(xlim = c(250, 0), ylim = c(0, 1700)) +
    theme_classic()
  expect_true(is(ggplot_build(gg)$layout$coord, "CoordGeo"))
  expect_equal(ggplot_build(gg)$layout$panel_params[[1]]$x.range, c(-250, 0))
  gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_continuous("Age (Ma)") +
    ylab("Coral Genera") +
    coord_geo(xlim = c(250, 0), ylim = c(10, 1700), ytrans = "log10") +
    theme_classic()
  expect_true(is.ggplot(gg))
  params <- ggplot_build(gg)$layout$panel_params[[1]]
  expect_equal(params$x.range, c(0, 250))
  expect_equal(params$y.range, c(log10(10), log10(1700)))
})

test_that("stacking scales works", {
  gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_geo(dat = list("periods", "eras"), xlim = c(250, 0), ylim = c(0, 1700),
              pos = list("b", "b"), abbrv = list(TRUE, FALSE)) +
    theme_classic()
  expect_doppelganger("stacked scales", gg)
})

test_that("scales on different sides works", {
  gg <- ggplot(lisiecki2005) +
    geom_line(aes(x = d18O, y = Time/1000), orientation = "y") +
    scale_y_reverse("Time (Ma)") +
    scale_x_reverse() +
    coord_geo(dat = list("Geomagnetic Polarity Chron", "Planktic foraminiferal Primary Biozones"),
              xlim = c(6,2), ylim = c(5.5,0), pos = list("l", "r"), rot = 90, skip = "PL4", size = list(5, 4)) +
    theme_classic()
  expect_doppelganger("scales on different sides", gg)
})

test_that("scales on different sides works", {
  gg <- ggplot(lisiecki2005) +
    geom_line(aes(x = d18O, y = Time/1000), orientation = "y") +
    scale_y_reverse("Time (Ma)") +
    scale_x_reverse() +
    coord_geo(dat = list("Geomagnetic Polarity Chron", "Planktic foraminiferal Primary Biozones"),
              xlim = c(6,2), ylim = c(5.5,0), pos = list("l", "r"), rot = 90, skip = "PL4", size = list(5, 4)) +
    theme_classic()
  expect_doppelganger("scales on different sides", gg)
})

test_that("scales on facetted plot works", {
  gg <- ggplot(coral_div_diet) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_geo(xlim = c(250, 0)) +
    theme_classic()
  expect_doppelganger("scale on only one facet", gg + facet_wrap(~diet, nrow = 3))
  expect_doppelganger("scale on all facets", gg + facet_wrap(~diet, nrow = 3, scales = "free_x"))
})

test_that("auto-discrete scale works", {
  gg <- ggplot(coral_div_dis) +
    geom_col(aes(x = period, y = n, fill = diet)) +
    scale_x_discrete("Period", limits = unique(coral_div_dis$period), labels = NULL, expand = expansion(add = .5)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    coord_geo(expand = TRUE, skip = NULL, abbrv = FALSE) +
    theme_classic() +
    theme(axis.ticks.length.x = unit(0, "lines"))
  expect_doppelganger("scale on discrete axis", gg)
})

test_that("custom discrete scale works", {
  eras_custom <- data.frame(name = c("Mesozoic", "Cenozoic"), max_age = c(0.5, 3.5), min_age = c(3.5, 6.5), color = c("#67C5CA", "#F2F91D"))

  gg <- ggplot(coral_div_dis) +
    geom_col(aes(x = period, y = n, fill = diet)) +
    scale_x_discrete(NULL, limits = unique(coral_div_dis$period), labels = NULL, expand = expansion(add = .5)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    coord_geo(dat = list("periods", eras_custom), pos = c("b", "b"), expand = TRUE, skip = NULL, abbrv = FALSE, dat_is_discrete = list(FALSE, TRUE)) +
    theme_classic() +
    theme(axis.ticks.length.x = unit(0, "lines"))
  expect_doppelganger("custom discrete scale", gg)
})

test_that("geom_fit_text() works", {
  gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_geo(dat = "periods", xlim = c(250, 0), ylim = c(0, 1700),
              abbrv = FALSE, size = "auto", fittext_args = list(size = 20)) +
    theme_classic()
  expect_doppelganger("geom_fit_text()", gg)
})

suppressPackageStartupMessages(library(phytools, quietly = TRUE))
suppressPackageStartupMessages(library(ggtree, quietly = TRUE))

test_that("ggtree scale works", {
  data(mammal.tree)
  gg <- ggtree(mammal.tree) +
    coord_geo(xlim = c(-75,0), ylim = c(-2,Ntip(mammal.tree)), neg = TRUE, abbrv = FALSE) +
    scale_x_continuous(breaks=seq(-80,0,20), labels=abs(seq(-80,0,20))) +
    theme_tree2()
  expect_doppelganger("scale on ggtree", revts(gg))
})

suppressPackageStartupMessages(library(paleotree, quietly = TRUE))

test_that("ggtree scale works with only fossil taxa", {
  data(RaiaCopesRule)
  gg <- ggtree(ceratopsianTreeRaia, position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
    coord_geo(xlim = c(-163.5,-66), ylim = c(-2,Ntip(ceratopsianTreeRaia)), pos = list("bottom", "bottom"),
              skip = c("Paleocene", "Middle Jurassic"), dat = list("epochs", "periods"), abbrv = FALSE,
              size = list(4,5), neg = TRUE, center_end_labels = TRUE) +
    scale_x_continuous(breaks = -rev(epochs$max_age), labels = rev(epochs$max_age)) +
    theme_tree2() +
    theme(plot.margin = margin(7,11,7,11))
  expect_doppelganger("scale on fossil ggtree", gg)
})
