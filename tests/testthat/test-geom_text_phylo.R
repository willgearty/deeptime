test_that("geom_text_phylo works", {
  skip_if_not_installed("ape")
  skip_if_not_installed("ggtree")
  library(ape)
  library(ggtree)
  set.seed(1234)
  tr <- rtree(10)
  gg <- revts(ggtree(tr)) +
    geom_text_phylo() +
    coord_geo_radial("epochs")
  expect_true(is_ggplot_deeptime(gg))
  expect_true(is(gg$layers[[3]]$geom, "GeomTextPhylo"))
  expect_true(is(gg$layers[[3]]$stat, "StatIdentity"))
  expect_doppelganger_deeptime("geom_text_phylo", gg)

  expect_error({
    ggtree(tr) +
      geom_text_phylo(node_type = "wrong") +
      coord_geo_radial("epochs")
  })
  expect_error({
    ggtree(tr) +
      geom_text_phylo(auto_adjust = "wrong") +
      coord_geo_radial("epochs")
  })
})
