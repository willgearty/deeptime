skip_if_not_installed("ggtree")
test_that("coord_geo_polar works", {
  skip_if_not_installed("ape")
  lifecycle::expect_deprecated({
    gg <- revts(ggtree(tree)) +
      coord_geo_polar(dat = "stages") +
      scale_x_continuous(limits = c(NA, 0))
  })
  expect_true(is.ggplot(gg))
  expect_true(is(ggplot_build(gg)$layout$coord, "CoordGeoPolar"))
  expect_doppelganger_deeptime("coord_geo_polar", gg)
  expect_equal(ggplot_build(gg)$layout$panel_params[[1]]$r.range,
               c(-max(node.depth.edgelength(tree)), 0))

  withr::local_options(lifecycle_verbosity = "quiet")
  gg2 <- revts(ggtree(tree)) +
    coord_geo_polar(dat = "stages", fill = c("grey60", "grey80")) +
    scale_x_continuous(limits = c(NA, 0))
  expect_true(is.ggplot(gg2))
  expect_true(is(ggplot_build(gg2)$layout$coord, "CoordGeoPolar"))
  expect_doppelganger_deeptime("coord_geo_polar_grey", gg2)

  # this doesn't make sense, but let's test it anyways
  gg3 <- ggtree(tree) +
    coord_geo_polar(dat = "stages", neg = FALSE) +
    scale_x_continuous(limits = c(0, NA))
  expect_true(is.ggplot(gg3))
  expect_true(is(ggplot_build(gg3)$layout$coord, "CoordGeoPolar"))
  expect_doppelganger_deeptime("coord_geo_polar_pos", gg3)
  expect_equal(ggplot_build(gg3)$layout$panel_params[[1]]$r.range,
               c(0, max(node.depth.edgelength(tree))))

  expect_error({
    ggtree(tree) +
      coord_geo_polar(direction = "up")
  })
  gg4 <- ggtree(tree) +
    coord_geo_polar(dat = 5)
  expect_error(plot(gg4))
})

test_that("stacking scales works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_not_installed("phytools")
  gg <- revts(ggtree(mammal.tree)) +
    coord_geo_polar(
      dat = list("stages", "periods"), alpha = .5,
      prop = list(0.75, .25), start = pi / 4, lty = "dashed"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    theme(deeptime.axis.text.r = element_text(size = 3.5, hjust = .25,
                                              vjust = .75))
  expect_doppelganger_deeptime("stacked scales", gg)
})

test_that("ggtree scale works with only fossil taxa", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_not_installed("paleotree")
  gg <- ggtree(ceratopsianTreeRaia,
               position = position_nudge(
                 x = -ceratopsianTreeRaia$root.time
               )) +
    coord_geo_polar(dat = "stages")
  expect_doppelganger_deeptime("scale on fossil ggtree", gg)
  skip_if(R.Version()$os != "mingw32") # only test this on Windows
  gg <- ggtree(ceratopsianTreeRaia,
               position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
    coord_geo_polar(dat = list("stages", "periods"),
                    lab = list(FALSE, TRUE), abbrv = TRUE) +
    scale_x_continuous(expand = expansion(add = c(20, 30)))
  expect_doppelganger_deeptime("scale on fossil ggtree with labels", gg)
})
