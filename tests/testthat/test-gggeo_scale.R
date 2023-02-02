test_that("gggeo_scale() is deprecated", {
  p <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic()
  lifecycle::expect_deprecated(gggeo_scale(p))
})

test_that("gggeo_scale() works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_not_installed("divDyn")
  p <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic()
  gg <- gggeo_scale(p, center_end_labels = TRUE)
  expect_true(gtable::is.gtable(gg))
  expect_doppelganger_deeptime("gggeo_scale()", print(gg))

  gg <- gggeo_scale(p, pos = "top")
  expect_true(gtable::is.gtable(gg))
  expect_doppelganger_deeptime("gggeo_scale() top", gg)

  skip_if_not_installed("gsloid")
  skip_if_offline(host = "macrostrat.org")
  p <- ggplot(lisiecki2005) +
    geom_line(aes(x = d18O, y = Time / 1000), orientation = "y") +
    scale_y_reverse("Time (Ma)") +
    scale_x_reverse() +
    coord_cartesian(xlim = c(6, 2), ylim = c(5.5, 0)) +
    theme_classic()
  gg <- gggeo_scale(p, dat = "Geomagnetic Polarity Chron", pos = "left", rot = 90, skip = "PL4", size = 5)
  gg <- gggeo_scale(gg, dat = "Planktic foraminiferal Primary Biozones", pos = "r", rot = 90, skip = "PL4", size = 4)
  expect_true(gtable::is.gtable(gg))
  expect_doppelganger_deeptime("gggeo_scale() left and right", gg)
})
