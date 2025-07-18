test_that("coord_trans_flip() works", {
  gg <- ggplot(mtcars, aes(disp, wt)) +
    geom_point() +
    coord_trans_flip(x = "log10", y = "log10",
                     xlim = c(50, 500), ylim = c(1, 6), expand = FALSE)
  expect_true(is_ggplot(gg))
  params <- ggplot_build(gg)$layout$panel_params[[1]]
  expect_equal(params$x.range, log10(c(1, 6)))
  expect_equal(params$y.range, log10(c(50, 500)))
  expect_doppelganger_deeptime("coord_trans_flip()", gg)

  gg <- ggplot(mtcars, aes(disp, wt)) +
    geom_point() +
    coord_trans_flip(x = "sqrt", y = "pseudo_log",
                     xlim = c(50, 500), ylim = c(1, 6), expand = FALSE)
  expect_true(is_ggplot(gg))
  params <- ggplot_build(gg)$layout$panel_params[[1]]
  expect_equal(params$x.range, scales::pseudo_log_trans()$transform(c(1, 6)))
  expect_equal(params$y.range, sqrt(c(50, 500)))
  expect_doppelganger_deeptime("coord_trans_flip() with diff trans",
                               gg)
})
