test_that("ggarrange2() works", {
  p1 <- ggplot(ammoniteTraitsRaia) +
    geom_point(aes(x = Log_D, y = FD)) +
    labs(x = "Body size", y = "Suture complexity") +
    theme_classic()
  p2 <- ggplot(ammoniteTraitsRaia) +
    geom_point(aes(x = Log_D, y = log_dur)) +
    labs(x = "Body size", y = "Stratigraphic duration (myr)") +
    theme_classic()
  gg1 <- ggarrange2(p1, p2, widths = c(2,1), draw = FALSE, labels = c("A", "B"))
  expect_true(gtable::is.gtable(gg1))
  expect_doppelganger("ggarrange2()", gg1)
  expect_doppelganger("ggarrange2() layout", gtable::gtable_show_layout(gg1))

  library(ggtree)
  p3 <- ggtree(ammoniteTreeRaia, position = position_nudge(x = -ammoniteTreeRaia$root.time)) +
    coord_geo(xlim = c(-415,-66), ylim = c(-2,Ntip(ammoniteTreeRaia)), pos = "bottom",
              size = 4, abbrv = FALSE, neg = TRUE) +
    scale_x_continuous(breaks = seq(-425, -50, 25), labels = -seq(-425, -50, 25)) +
    theme_tree2() +
    theme(plot.margin = margin(7,11,7,11))

  gg2 <- ggarrange2(gg1, p3, nrow = 2, heights = c(unit(20, "lines"), unit(40, "lines")), draw = FALSE,
                    top = "Test1", bottom = "Test2", left = "Test3", right = "Test4")
  expect_true(gtable::is.gtable(gg2))
  expect_doppelganger("double ggarrange2()", gg2)
  expect_doppelganger("double ggarrange2() layout", gtable::gtable_show_layout(gg2))
})
