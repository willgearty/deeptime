test_that("gggeo_scale_old() is deprecated", {
  p <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic()
  lifecycle::expect_deprecated(gggeo_scale_old(p))
})

test_that("gggeo_scale_old() works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_not_installed("divDyn")
  p <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(-95, 1700), expand = FALSE) +
    theme_classic()
  gg <- gggeo_scale_old(p)
  expect_true(is.ggplot(gg))
  expect_doppelganger_deeptime("gggeo_scale_old()", gg)

  skip_if_not_installed("gsloid")
  p <- ggplot(lisiecki2005) +
    geom_line(aes(x = d18O, y = Time / 1000), orientation = "y") +
    scale_y_reverse("Time (Ma)") +
    scale_x_reverse() +
    coord_cartesian(xlim = c(6, 2), ylim = c(5.5, 0)) +
    theme_classic()
  gg <- gggeo_scale_old(p, dat = "Geomagnetic Polarity Chron",
                        pos = "left", rot = 90, skip = "PL4", size = 5)
  gg <- gggeo_scale_old(gg, dat = "Planktic foraminiferal Primary Biozones",
                        pos = "r", rot = 90, skip = "PL4", size = 4)
  expect_true(is.ggplot(gg))
  expect_doppelganger_deeptime("gggeo_scale_old() left and right", gg)
})
