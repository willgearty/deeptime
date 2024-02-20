test_that("facet_grid_color works", {
  skip_if_not_installed("divDyn")
  gg <- ggplot(coral_div_dis) +
    geom_col(aes(x = diet, y = n, fill = diet)) +
    scale_x_discrete("Diet", labels = NULL) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    facet_grid_color(cols = vars(period), colors = periods) +
    theme(axis.ticks.length.x = unit(0, "lines"))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetGridColor"))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetGrid"))
  gg_gtable <- ggplot_gtable(ggplot_build(gg))
  first_strip <- gg_gtable$grobs[[grep("strip", gg_gtable$layout$name)[1]]]
  expect_equal(first_strip$grobs[[1]]$children[[1]]$gp$fill,
               periods$color[
                 match(first_strip$grobs[[1]]$children[[2]]$children[[1]]$label,
                       periods$name)
               ]
  )
  expect_doppelganger_deeptime("facet_grid_color", gg)
})

test_that("facet_wrap_color works", {
  skip_if_not_installed("divDyn")
  gg <- ggplot(coral_div_dis) +
    geom_col(aes(x = diet, y = n, fill = diet)) +
    scale_x_discrete("Diet", labels = NULL) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    facet_wrap_color(vars(period), colors = periods) +
    theme(axis.ticks.length.x = unit(0, "lines"))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetWrapColor"))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetWrap"))
  gg_gtable <- ggplot_gtable(ggplot_build(gg))
  first_strip <- gg_gtable$grobs[[grep("strip", gg_gtable$layout$name)[1]]]
  expect_equal(first_strip$grobs[[1]]$children[[1]]$gp$fill,
               periods$color[
                 match(first_strip$grobs[[1]]$children[[2]]$children[[1]]$label,
                       periods$name)
               ]
  )
  expect_doppelganger_deeptime("facet_wrap_color", gg)
})
