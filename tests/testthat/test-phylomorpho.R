test_that("geom_phylomorpho works", {
  skip_if_not_installed("ape")
  library(ape)
  set.seed(1234)
  tr <- rtree(10)
  dat <- data.frame(x = runif(10), y = runif(10), label = tr$tip.label,
                    row.names = tr$tip.label)
  gg <- ggplot(dat) +
    geom_phylomorpho(tr, aes(x = x, y = y, label = label),
                     seg_args = list(color = "blue")) +
    geom_label(aes(x = x, y = y, label = label), size = 5) +
    theme_classic(base_size = 16)
  expect_true(is.ggplot(gg))
  expect_true(is(gg$layers[[1]]$geom, "GeomSegment"))
  expect_true(is(gg$layers[[1]]$stat, "StatPhylomorpho"))
  expect_true(is(gg$layers[[2]]$geom, "GeomPoint"))
  expect_true(is(gg$layers[[2]]$stat, "StatIdentity"))
  expect_doppelganger_deeptime("geom_phylomorpho", gg)
})
