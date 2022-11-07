test_that("coord_trans_xy() works", {
  trans <- ggforce::linear_trans(shear(2, 0), rotate(-pi / 3))
  square <- data.frame(x = c(-2, -2, 2, 2), y = c(-2, 2, 2, -2))
  points <- data.frame(x = rep(seq(-2, 2, 0.25), each = 17),
                       y = rep(seq(-2, 2, 0.25), 17),
                       color = rep(seq(1, 17, 1), each = 17))
  gg <- ggplot(data = points, aes(x = x, y = y, color = color)) +
    geom_polygon(data = square, fill = NA, color = "black") +
    geom_point() +
    scale_x_continuous(sec.axis = sec_axis(~.)) +
    coord_trans_xy(trans = trans, expand = FALSE) +
    theme_classic()
  expect_true(is.ggplot(gg))
  params <- ggplot_build(gg)$layout$panel_params[[1]]
  points_trans <- trans$transform(points$x, points$y)
  expect_equal(params$x.range, range(points_trans$x))
  expect_equal(params$y.range, range(points_trans$y))
  expect_true(is(ggplot_build(gg)$layout$panel_params[[1]]$x.sec, "ViewScale"))
  expect_equal(ggplot_build(gg)$layout$panel_params[[1]]$x$limits, c(-2, 2))
  expect_doppelganger_deeptime("coord_trans_xy() without expansion", gg)

  gg <- ggplot(data = points, aes(x = x, y = y, color = color)) +
    geom_polygon(data = square, fill = NA, color = "black") +
    geom_point() +
    scale_x_continuous(sec.axis = sec_axis(~.)) +
    scale_y_continuous(sec.axis = sec_axis(~.)) +
    coord_trans_xy(trans = trans, expand = TRUE) +
    theme_classic()
  expect_true(is.ggplot(gg))
  expect_equal(ggplot_build(gg)$layout$panel_params[[1]]$x$limits, c(-2, 2))
  expect_doppelganger_deeptime("coord_trans_xy() with expansion", gg)
})
