test_that("coord_trans_xy() works", {
  trans <- ggforce::linear_trans(shear(2, 0), rotate(-pi / 3))
  square <- data.frame(x = c(-2, -2, 2, 2), y = c(-2, 2, 2, -2))
  points <- data.frame(
    x = rep(seq(-2, 2, 0.25), each = 17),
    y = rep(seq(-2, 2, 0.25), 17),
    color = rep(seq(1, 17, 1), each = 17)
  )
  gg <- ggplot(data = points, aes(x = x, y = y, color = color)) +
    geom_polygon(data = square, fill = NA, color = "black") +
    geom_point() +
    scale_x_continuous(sec.axis = sec_axis(~.)) +
    coord_trans_xy(trans = trans, expand = FALSE) +
    theme_classic()
  expect_true(is_ggplot(gg))
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
  expect_true(is_ggplot(gg))
  expect_equal(ggplot_build(gg)$layout$panel_params[[1]]$x$limits, c(-2, 2))
  expect_doppelganger_deeptime("coord_trans_xy() with expansion", gg)

  # check that reflect transformation works
  trans <- ggforce::linear_trans(shear(0.5, 0.5), reflect(1, 0))
  gg <- ggplot(points, aes(x, y)) +
    geom_polygon(data = square, fill = NA, colour = "black") +
    geom_point(size = 0.4) +
    scale_x_continuous(sec.axis = sec_axis(~.)) +
    scale_y_continuous(sec.axis = sec_axis(~.)) +
    coord_trans_xy(trans = trans) +
    theme_classic()
  expect_doppelganger_deeptime("coord_trans_xy() with reflect", gg)

  # check that reverse scales work
  trans <- ggforce::linear_trans(shear(0.5, 0), rotate(-pi / 4))
  gg <- ggplot(points, aes(x, y)) +
    geom_polygon(data = square, fill = NA, colour = "black") +
    geom_point(size = 0.4) +
    scale_x_reverse(sec.axis = sec_axis(~ .)) +
    scale_y_reverse(sec.axis = sec_axis(~ .)) +
    coord_trans_xy(trans = trans, expand = FALSE) +
    theme_classic()
  expect_doppelganger_deeptime("coord_trans_xy() with reverse scales", gg)

  # check errors
  gg <- ggplot(data = points, aes(x = x, y = y, color = color)) +
    geom_polygon(data = square, fill = NA, color = "black") +
    geom_point() +
    scale_x_continuous(sec.axis = sec_axis(~.)) +
    coord_trans_xy(expand = FALSE) +
    theme_classic()
  expect_true(is_ggplot(gg))
  params <- ggplot_build(gg)$layout$panel_params[[1]]
  expect_equal(params$x.range, range(points$x))
  expect_equal(params$y.range, range(points$y))
  expect_doppelganger_deeptime("coord_trans_xy() with no trans", gg)
  expect_error({
    ggplot(data = points, aes(x = x, y = y, color = color)) +
      geom_polygon(data = square, fill = NA, color = "black") +
      geom_point() +
      coord_trans_xy(trans = 5, expand = TRUE)
  })
})
