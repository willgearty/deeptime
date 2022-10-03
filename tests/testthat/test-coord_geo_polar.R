test_that("coord_geo_polar works", {
  if(!suppressPackageStartupMessages(require(ggtree, quietly = TRUE))) {
    skip("ggtree not available for coord_geo_polar")
  }
  if(!suppressPackageStartupMessages(require(ape, quietly = TRUE))) {
    skip("ape not available for coord_geo_polar")
  }
  gg <- revts(ggtree(tree)) +
    coord_geo_polar(dat = "stages") +
    scale_x_continuous(limits = c(NA, 0))
  expect_true(is.ggplot(gg))
  expect_true(is(ggplot_build(gg)$layout$coord, "CoordGeoPolar"))
  expect_doppelganger("coord_geo_polar", gg)
  if(!suppressPackageStartupMessages(require(phytools, quietly = TRUE))) {
    skip("phytools not available for coord_geo_polar")
  }
  expect_equal(ggplot_build(gg)$layout$panel_params[[1]]$r.range, c(-max(nodeHeights(tree)), 0))
})

test_that("stacking scales works", {
  if(!suppressPackageStartupMessages(require(ggtree, quietly = TRUE))) {
    skip("ggtree not available for stacked coord_geo_polar")
  }
  gg <- revts(ggtree(mammal.tree)) +
    coord_geo_polar(dat = list("stages", "periods"), alpha = .5,
                    prop = list(0.75, .25), start = pi/4, lty = "dashed") +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    theme(axis.text.r = element_text(size = 3.5, hjust = .25, vjust = .75))
  expect_doppelganger("stacked scales", gg)
})

test_that("ggtree scale works with only fossil taxa", {
  if(!suppressPackageStartupMessages(require(ggtree, quietly = TRUE))) {
    skip("ggtree not available for polar fossil tree")
  }
  if(!suppressPackageStartupMessages(require(paleotree, quietly = TRUE))) {
    skip("paleotree not available for polar fossil tree")
  }
  gg <- ggtree(ceratopsianTreeRaia, position = position_nudge(x = -ceratopsianTreeRaia$root.time)) +
    coord_geo_polar(dat = "stages")
  expect_doppelganger("scale on fossil ggtree", gg)
})
