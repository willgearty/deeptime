test_that("gggeo_scale() works", {
  p <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic()
  gg <- gggeo_scale(p)
  expect_true(gtable::is.gtable(gg))
  expect_doppelganger("gggeo_scale()", gg)
})
