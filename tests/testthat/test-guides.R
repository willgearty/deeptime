test_that("guide_geo fails for old ggplot", {
  skip_if(packageVersion("ggplot2") >= "3.5.0")
  skip_if_not_installed("divDyn")
  ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse(guide = guide_geo())
})

skip_if(packageVersion("ggplot2") < "3.5.0")

test_that("guide_geo works", {
  skip_if_not_installed("divDyn")
  gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)", guide = guide_geo()) +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic()
  expect_true(is(ggplot_build(gg)$layout$panel_params[[1]]$guides$guides[[1]],
                 "GuideGeo"))
  # test class inheritance
  expect_true(is(ggplot_build(gg)$layout$panel_params[[1]]$guides$guides[[1]],
                 "GuideAxis"))
  expect_true(is.ggplot(gg))

  # check using guides()
  gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic() +
    guides(x = guide_geo())
  expect_true(is(ggplot_build(gg)$layout$panel_params[[1]]$guides$guides[[1]],
                 "GuideGeo"))
  expect_true(is.ggplot(gg))

  # with guide_axis_stack()
  gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)",
                    guide = guide_axis_stack(guide_geo(),
                                             "axis",
                                             spacing = unit(0, "line"))) +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic()
  expect_doppelganger_deeptime("stacked guides", gg)
  ggbuild <- ggplot_build(gg)
  expect_true(is(ggbuild$layout$panel_params[[1]]$guides$guides[[1]],
                 "GuideAxisStack"))
  expect_true(
    is(ggbuild$layout$panel_params[[1]]$guides$guides[[1]]$params$guides[[1]],
       "GuideGeo"))
  expect_true(
    is(ggbuild$layout$panel_params[[1]]$guides$guides[[1]]$params$guides[[2]],
       "GuideAxis"))

  # error checking
  gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic()

  expect_error(gg + guides(x = guide_geo(alpha = -2)))
  expect_error(gg + guides(x = guide_geo(lab = "yes")))
  expect_error(gg + guides(x = guide_geo(rot = "right")))
  expect_error(gg + guides(x = guide_geo(alpha = -2)))
  expect_error(gg + guides(x = guide_geo(abbrv = "no")))
  expect_error(gg + guides(x = guide_geo(family = 3)))
  expect_error(gg + guides(x = guide_geo(fontface = 4)))
  expect_error(gg + guides(x = guide_geo(skip = TRUE)))
  expect_error(gg + guides(x = guide_geo(size = "big")))
  expect_error(gg + guides(x = guide_geo(lwd = "small")))
  expect_error(gg + guides(x = guide_geo(neg = "yes")))
  expect_error(gg + guides(x = guide_geo(bord = FALSE)))
  expect_error(gg + guides(x = guide_geo(end_labels = "no")))
  expect_error(gg + guides(x = guide_geo(dat_is_discrete = "no")))
})

test_that("guides on facetted plot works", {
  skip_if_not_installed("divDyn")
  gg <- ggplot(coral_div_diet) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), expand = FALSE) +
    theme_classic() +
    guides(x = guide_axis_stack(guide_geo(),
                                "axis",
                                spacing = unit(0, "line"))) +
    facet_wrap(~diet)
  expect_doppelganger_deeptime("guide with facets", gg)
})

test_that("auto-discrete guide works", {
  skip_if_not_installed("divDyn")
  gg <- ggplot(coral_div_dis) +
    geom_col(aes(x = period, y = n, fill = diet)) +
    scale_x_discrete("Period", limits = unique(coral_div_dis$period),
                     labels = NULL, expand = expansion(add = .5)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    theme_classic() +
    theme(axis.ticks.length.x = unit(0, "lines")) +
    guides(x = guide_geo(skip = NULL, abbrv = FALSE))
  expect_doppelganger_deeptime("guide on discrete axis", gg)
})

test_that("custom discrete scale works", {
  skip_if_not_installed("divDyn")
  eras_custom <- data.frame(name = c("Mesozoic", "Cenozoic"),
                            max_age = c(0.5, 3.5), min_age = c(3.5, 6.5),
                            color = c("#67C5CA", "#F2F91D"))

  gg <- ggplot(coral_div_dis) +
    geom_col(aes(x = period, y = n, fill = diet)) +
    scale_x_discrete(NULL, limits = unique(coral_div_dis$period),
                     labels = NULL, expand = expansion(add = .5)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    theme_classic() +
    theme(axis.ticks.length.x = unit(0, "lines")) +
    guides(x = guide_axis_stack(guide_geo(skip = NULL, abbrv = FALSE),
                                guide_geo(eras_custom, abbrv = FALSE,
                                          dat_is_discrete = TRUE),
                                spacing = unit(0, "line")))
  expect_doppelganger_deeptime("custom discrete guide", gg)
})

test_that("geom_fit_text() works", {
  skip_if_not_installed("divDyn")
  gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic() +
    guides(x = guide_axis_stack(guide_geo(abbrv = FALSE, size = "auto",
                                          fittext_args = list(size = 20)),
                                "axis", spacing = unit(0, "line")))
  expect_doppelganger_deeptime("guide with geom_fit_text()", gg)
  expect_error({
    gg <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic() +
    guides(x = guide_geo(fittext_args = 20))
  })
})

test_that("ggtree guide works", {
  skip_if_not_installed("ggtree")
  skip_if_not_installed("phytools")
  gg <- revts(ggtree(mammal.tree)) +
    coord_cartesian(xlim = c(-75, 0), ylim = c(-2, Ntip(mammal.tree)),
                    expand = FALSE) +
    scale_x_continuous(breaks = seq(-80, 0, 20),
                       labels = abs(seq(-80, 0, 20))) +
    theme_tree2() +
    guides(x = guide_axis_stack(guide_geo(neg = TRUE, abbrv = FALSE),
                                "axis", spacing = unit(0, "line")))
  expect_doppelganger_deeptime("guide on ggtree", gg)
})

test_that("ggtree scale works with only fossil taxa", {
  skip_if_not_installed("ggtree")
  skip_if_not_installed("paleotree")
  gg <- ggtree(ceratopsianTreeRaia,
               position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
    coord_cartesian(xlim = c(-163.5, -50),
                    ylim = c(-2, Ntip(ceratopsianTreeRaia)),
                    expand = FALSE) +
    scale_x_continuous(breaks = -rev(epochs$max_age),
                       labels = rev(epochs$max_age)) +
    theme_tree2() +
    theme(plot.margin = margin(7, 11, 7, 11)) +
    guides(x = guide_axis_stack(
      guide_geo("epochs", neg = TRUE, abbrv = FALSE, size = 4,
                skip = c("Paleocene", "Middle Jurassic")),
      guide_geo(neg = TRUE, abbrv = FALSE, size = 5),
      "axis", spacing = unit(0, "line")))
  expect_doppelganger_deeptime("guides on fossil ggtree", gg)
})

test_that("guide_geo works with coord_geo_radial", {
  skip_if_not_installed("ape")
  gg <- revts(ggtree(tree)) +
    coord_geo_radial(dat = "epochs", end = 1.49 * pi,
                     fill = c("grey80", "grey90")) +
    scale_y_continuous(guide = "none", breaks = NULL,
                       expand = expansion(c(0.05, 0.1))) +
    scale_x_continuous(expand = expansion()) +
    theme_classic() +
    guides(r = guide_axis_stack(
      guide_geo(neg = TRUE, abbrv = FALSE, size = "auto", rot = -90,
                height = unit(.75, "line"), skip = NULL),
      "axis", spacing = unit(0, "line")))
  expect_doppelganger_deeptime("guide with coord_geo_radial", gg)
})

test_that("ggtree scale works with only fossil taxa", {
  skip_if_not_installed("paleotree")
  gg <- ggtree(ceratopsianTreeRaia,
               position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
    coord_geo_radial(dat = "stages", fill = c("grey80", "grey95"),
                     end = 1.49 * pi) +
    guides(
      r = guide_axis_stack(guide_geo(abbrv = FALSE, size = "auto", rot = -90,
                                     neg = TRUE, height = unit(.75, "line")),
                           "axis", spacing = unit(0, "npc"))
    ) +
    scale_y_continuous(guide = "none", breaks = NULL) +
    theme_classic()
  expect_doppelganger_deeptime("guide on fossil coord_geo_radial", gg)
})
