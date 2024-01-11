test_that("geom_points_range works", {
  skip_if_not_installed("palaeoverse")
  tetrapod_names <- tetrapods$accepted_name[1:50]
  beds_sampled <- sample.int(n = 10, size = 50, replace = TRUE)
  occdf <- data.frame(taxon = tetrapod_names, bed = beds_sampled)

  # check that vertical orientation works
  gg <- ggplot(occdf, aes(x = reorder(taxon, bed, min), y = bed)) +
    geom_points_range(size = 1) +
    theme_classic(base_size = 16)
  expect_true(is.ggplot(gg))
  expect_true(is(gg$layers[[1]]$geom, "GeomPointsRange"))
  expect_true(is(gg$layers[[1]]$stat, "StatPointsRange"))
  expect_doppelganger_deeptime("geom_points_range_v", gg)

  # check that horizontal orientation works
  gg <- ggplot(occdf, aes(y = reorder(taxon, bed, min), x = bed)) +
    geom_points_range(size = .5) +
    theme_classic(base_size = 16)
  expect_true(is.ggplot(gg))
  expect_true(is(gg$layers[[1]]$geom, "GeomPointsRange"))
  expect_true(is(gg$layers[[1]]$stat, "StatPointsRange"))
  expect_doppelganger_deeptime("geom_points_range_h", gg)

  # check that aesthetics and groupings work
  occdf$certainty <- factor(rep(1, 50), levels = c(0, 1))
  occdf <- rbind(occdf,
                 data.frame(taxon = "Eryops",  bed = c(12, 15), certainty = 0))
  gg <- ggplot(occdf, aes(y = reorder(taxon, bed, min), x = bed,
                          fill = certainty, linetype = certainty)) +
    geom_points_range(size = .5, shape = 21) +
    scale_fill_manual(values = c("white", "black")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_classic(base_size = 16)
  expect_true(is.ggplot(gg))
  expect_true(is(gg$layers[[1]]$geom, "GeomPointsRange"))
  expect_true(is(gg$layers[[1]]$stat, "StatPointsRange"))
  expect_doppelganger_deeptime("geom_points_range_aes", gg)

  # check that background lines work
  gg <- ggplot(occdf, aes(y = reorder(taxon, bed, min), x = bed,
                          fill = certainty, linetype = certainty)) +
    geom_points_range(size = .5, shape = 21,
                      background_line = list(linetype = "dashed")) +
    scale_fill_manual(values = c("white", "black")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_classic(base_size = 16)
  expect_true(is.ggplot(gg))
  expect_true(is(gg$layers[[1]]$geom, "GeomPointsRange"))
  expect_true(is(gg$layers[[1]]$stat, "StatPointsRange"))
  expect_doppelganger_deeptime("geom_points_range_bg", gg)
})
