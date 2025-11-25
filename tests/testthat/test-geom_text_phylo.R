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
  expect_true(is_ggplot(gg))
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

test_that("geom_text_clade works", {
  skip_if_not_installed("ape")
  skip_if_not_installed("ggtree")
  skip_if_not_installed("phytools")
  library(ape)
  library(ggtree)
  library(phytools)
  data(primate.tree)
  clades.df <- data.frame(
    clade = c("Lorisoidea", "Lemuroidea", "Tarsioidea", "Ceboidea",
              "Cercopithecoidea", "Hominoidea"),
    node = c(166, 146, 144, 120, 95, 114)
  )
  gg <- revts(ggtree(primate.tree)) %<+% clades.df +
    geom_text_clade(aes(label = clade), extend = c(0.1, 0.1)) +
    coord_geo_radial()
  expect_true(is_ggplot(gg))
  expect_true(is(gg$layers[[3]]$geom, "GeomTextClade"))
  expect_true(is(gg$layers[[3]]$stat, "StatIdentity"))
  expect_doppelganger_deeptime("geom_text_clade", gg)

  # geom_label
  gg <- revts(ggtree(primate.tree)) %<+% clades.df +
    geom_text_clade(aes(label = clade), extend = c(0.1, 0.1),
                    text_geom = "label") +
    coord_geo_radial()
  expect_doppelganger_deeptime("geom_text_clade-labels", gg)

  expect_error({
    revts(ggtree(primate.tree)) %<+% clades.df +
      geom_text_clade(aes(label = clade), text_geom = "rect") +
      coord_geo_radial()
  })
  expect_error({
    revts(ggtree(primate.tree)) %<+% clades.df +
      geom_text_clade(aes(label = clade), extend = 0.5) +
      coord_geo_radial()
  })

  # annotation
  gg <- revts(ggtree(primate.tree)) +
    geom_text_clade(label = "Hominoidea", node = 114, extend = c(0.1, 0.1)) +
    coord_geo_radial()
  expect_doppelganger_deeptime("geom_text_clade-annotation", gg)
  expect_error({
    revts(ggtree(primate.tree)) +
      geom_text_clade(label = "Hominoidea", extend = c(0.1, 0.1)) +
      coord_geo_radial()
  })
  expect_error({
    revts(ggtree(primate.tree)) +
      geom_text_clade(label = c("Hominoidea", "Lorisoidea"), node = 114,
                      extend = c(0.1, 0.1)) +
      coord_geo_radial()
  })
  expect_error({
    revts(ggtree(primate.tree)) +
      geom_text_clade(aes(label = label), label = "Hominoidea", node = 114,
                      extend = c(0.1, 0.1)) +
      coord_geo_radial()
  })
})
