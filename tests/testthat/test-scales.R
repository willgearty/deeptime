test_that("scale_fill_geo works", {
  set.seed(1234)
  df <- data.frame(x = runif(1000, 0, 10), y = runif(1000, 0, 10),
                   color = sample(periods$name, 1000, TRUE), shape = 21)
  gg <- ggplot(df) +
    geom_point(aes(x = x, y = y, fill = color), shape = 21) +
    scale_fill_geo("periods", name = "Period") +
    theme_classic()
  expect_doppelganger_deeptime("scale_fill_geo", gg)
})

test_that("scale_color_geo works", {
  set.seed(1234)
  df <-data.frame(x = runif(1000, 0, 1000), y = runif(1000, 0, 8))
  df$color <- cut(df$x, c(periods$min_age, periods$max_age[22]), periods$name)
  gg <- ggplot(df) +
    geom_point(aes(x = x, y = y, color = color)) +
    scale_x_reverse() +
    scale_color_geo("periods", name = "Period") +
    coord_geo(xlim = c(1000, 0), ylim = c(0,8)) +
    theme_classic()
  expect_doppelganger_deeptime("scale_color_geo", gg)
})

test_that("scale_discrete_geo works", {
  set.seed(1234)
  df <-data.frame(x = runif(1000, 0, 1000), y = runif(1000, 0, 8))
  df$color <- cut(df$x, c(periods$min_age, periods$max_age[22]), periods$name)
  gg <- ggplot(df) +
    geom_point(aes(x = x, y = y, color = color, fill = color), shape = 21) +
    scale_x_reverse() +
    scale_discrete_geo("periods", c("color", "fill"), name = "Period") +
    coord_geo(xlim = c(1000, 0), ylim = c(0,8)) +
    theme_classic()
  expect_doppelganger_deeptime("scale_discrete_geo", gg)
})

