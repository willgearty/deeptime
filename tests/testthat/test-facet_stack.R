test_that("facet_stack() works", {
  set.seed(1)
  df <- data.frame(x = runif(60, -0.4, 0.4), y = runif(60, -0.4, 0.4),
                   time = rep(c("before", "during", "after"), each = 20))
  df$time <- factor(df$time, levels = c("after", "during", "before"))
  gg <- ggplot(df, aes(x, y)) +
    geom_point() +
    coord_trans_xy(trans = ggforce::linear_trans(shear(2, 0))) +
    facet_stack(vars(time)) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA),
          panel.spacing.y = unit(1, "cm"))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetStack"))
  gg_gtable <- ggplot_gtable(ggplot_build(gg))
  expect_true(all(c("panel-1-1", "strip-r-1", "axis-b-1-1") %in%
                    gg_gtable$layout$name))
  expect_doppelganger_deeptime("facet_stack", gg)
  expect_doppelganger_deeptime("facet_stack without edges",
                               gg + facet_stack(vars(time), edges = FALSE))

  expect_error(ggplot_gtable(ggplot_build(
    ggplot(df, aes(x, y)) + geom_point() + facet_stack(vars(time))
  )), "coord_trans_xy")
  expect_error(facet_stack(vars(time), edges = "yes"))
})
