test_that("gggeo_scale() is deprecated", {
  skip_if_not_installed("divDyn")
  p <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic()
  lifecycle::expect_deprecated(gggeo_scale(p))
})
