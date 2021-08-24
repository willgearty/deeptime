library(ggforce)

test_that("coord_trans_xy() works", {
  trans <- linear_trans(shear(2, 0), rotate(-pi / 3))
  square <- data.frame(x = c(0, 0, 4, 4), y = c(0, 1, 1, 0))
  points <- data.frame(x = rep(seq(0, 4, 0.2), each = 6), y = rep(seq(0, 1, 0.2), 21))
  gg <- ggplot(data = points, aes(x = x, y = y)) +
    geom_polygon(data = square, fill = NA, color = "black") +
    geom_point(color = 'black') +
    coord_trans_xy(trans = trans, expand = FALSE) +
    theme_classic()
  expect_true(is.ggplot(gg))
  params <- ggplot_build(gg)$layout$panel_params[[1]]
  points_trans <- trans$transform(points$x, points$y)
  expect_equal(params$x.range, range(points_trans$x))
  expect_equal(params$y.range, range(points_trans$y))
  expect_doppelganger("coord_trans_xy()", gg)
})
