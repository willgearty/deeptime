test_that("scale_fill_geopattern works", {
  # invalid code
  vals <- c("101", "313", "603", "999")
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern()
  expect_error(print(gg))

  skip_if(R.Version()$os != "mingw32") # only test the rest on Windows
  vals <- c("101", "313", "603", "733")
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern()
  expect_doppelganger_deeptime("scale_fill_geopattern", gg, patterns = TRUE)

  # custom labels
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern(labels = c("101" = "test"))
  expect_doppelganger_deeptime("scale_fill_geopattern_labels", gg, patterns = TRUE)

  # custom limits
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern(limits = c("101", "313"))
  expect_doppelganger_deeptime("scale_fill_geopattern_limits", gg, patterns = TRUE)

  # test with NA values
  vals <- c("101", "313", "603", NA)
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern()
  expect_doppelganger_deeptime("scale_fill_geopattern_NA", gg, patterns = TRUE)

  # test with custom NA value
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern(na.value = geo_pattern("701"))
  expect_doppelganger_deeptime("scale_fill_geopattern_NA2", gg, patterns = TRUE)
})

test_that("geo_grob works", {
  grob <- geo_grob("101")
  expect_true(grid::is.grob(grob))

  expect_error(geo_grob(999))
  expect_error(geo_grob("test"))

  expect_doppelganger_deeptime("geo_grob", {
    grid.newpage()
    grid.draw(grob)
  })
})

test_that("geo_pattern works", {
  patt <- geo_pattern("101")
  expect_true(is(patt, "GridPattern"))

  expect_error(geo_pattern(999))
  expect_error(geo_pattern("test"))

  pattern1 <- geo_pattern(code = "313-K")
  pattern2 <- geo_pattern(code = "607")

  expect_doppelganger_deeptime("geo_pattern1", {
    grid.newpage()
    grid.draw(rectGrob(gp = gpar(fill = pattern1)))
  }, patterns = TRUE)

  expect_doppelganger_deeptime("geo_pattern2", {
    grid.newpage()
    grid.draw(rectGrob(gp = gpar(fill = pattern2)))
  }, patterns = TRUE)
})

test_that("grid.pattern_geo works", {
  x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
  y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))

  expect_doppelganger_deeptime("grid.pattern_geo", {
    grid.newpage()
    grid.pattern_geo(params = list(pattern_type = "633", pattern_scale = 4),
                     boundary_df = data.frame(x, y, id = 1))
  }, patterns = TRUE)
})

test_that("ggpattern works", {
  skip_if_not_installed("ggpattern")
  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  gg <- ggplot(df, aes(trt, outcome)) +
    geom_col_pattern(aes(color = trt, pattern_type = trt), pattern = 'geo',
                     pattern_color = "black", fill = "white",
                     pattern_fill = "white") +
    scale_pattern_type_manual(values = c("101", "313", "999")) +
    scale_color_viridis_d() +
    theme(legend.key.size = unit(1.5, 'cm'))
  expect_error(print(gg))

  skip_if(R.Version()$os != "mingw32") # only test the rest on Windows
  gg <- ggplot(df, aes(trt, outcome)) +
    geom_col_pattern(aes(color = trt, pattern_type = trt), pattern = 'geo',
                     pattern_color = "black", fill = "white",
                     pattern_fill = "white") +
    scale_pattern_type_manual(values = c("101", "313", "634")) +
    scale_color_viridis_d() +
    theme(legend.key.size = unit(1.5, 'cm'))
  expect_doppelganger_deeptime("ggpattern", gg, patterns = TRUE)
})
