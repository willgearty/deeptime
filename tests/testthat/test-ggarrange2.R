test_that("ggarrange2() works", {
  skip_if_not_installed("paleotree")
  p1 <- ggplot(ammoniteTraitsRaia) +
    geom_point(aes(x = Log_D, y = FD)) +
    labs(x = "Body size", y = "Suture complexity") +
    theme_classic()
  p2 <- ggplot(ammoniteTraitsRaia) +
    geom_point(aes(x = Log_D, y = log_dur)) +
    labs(x = "Body size", y = "Stratigraphic duration (myr)") +
    theme_classic()
  gg1 <- ggarrange2(p1, p2, widths = c(2, 1), draw = FALSE,
                    labels = c("A", "B"))
  expect_true(gtable::is.gtable(gg1))
  expect_doppelganger_deeptime("ggarrange2()", gg1)
  expect_doppelganger_deeptime("ggarrange2() layout",
                               gtable::gtable_show_layout(gg1))

  gg2 <- ggarrange2(p1, p2, layout = matrix(c(2, 0, 0, 1), nrow = 2),
                    widths = c(2, 1), draw = FALSE, labels = c("A", "B"),
                    debug = TRUE)
  expect_doppelganger_deeptime("ggarrange2() with layout", gg2)
  expect_doppelganger_deeptime("ggarrange2() with layout (layout)",
                               gtable::gtable_show_layout(gg2))

  skip_if_not_installed("ggtree")
  p3 <- ggtree(ammoniteTreeRaia,
               position = position_nudge(x = -ammoniteTreeRaia$root.time)) +
    coord_geo(xlim = c(-415, -66), ylim = c(-2, Ntip(ammoniteTreeRaia)),
              pos = "bottom", size = 4, abbrv = FALSE, neg = TRUE) +
    scale_x_continuous(breaks = seq(-425, -50, 25),
                       labels = -seq(-425, -50, 25)) +
    theme_tree2() +
    theme(plot.margin = margin(7, 11, 7, 11))

  gg3 <- ggarrange2(gg1, p3,
    nrow = 2, widths = unit(60, "lines"),
    heights = c(unit(20, "lines"), unit(40, "lines")),
    top = "Test1", bottom = "Test2", left = "Test3", right = "Test4",
    newpage = TRUE, draw = FALSE
  )
  expect_true(gtable::is.gtable(gg3))
  expect_doppelganger_deeptime("double ggarrange2()", print(gg3))
  expect_doppelganger_deeptime("double ggarrange2() layout",
                               gtable::gtable_show_layout(gg3))

  gg4 <- ggarrange2(p1, .dummy_gtable,
                    ncol = 2, heights = unit(20, "lines"),
                    top = "Test1", bottom = "Test2",
                    left = "Test3", right = "Test4",
                    newpage = TRUE, draw = FALSE)
  expect_true(gtable::is.gtable(gg4))
  expect_doppelganger_deeptime("ggarrange2() no heights", print(gg4))
  expect_doppelganger_deeptime("ggarrange2() no heights layout",
                               gtable::gtable_show_layout(gg4))

  gg5 <- ggarrange2(p1, .dummy_gtable,
                    top = "Test1", bottom = "Test2",
                    left = "Test3", right = "Test4",
                    newpage = TRUE, draw = FALSE)
  expect_true(gtable::is.gtable(gg5))
  expect_doppelganger_deeptime("ggarrange2() no dims", print(gg5))
  expect_doppelganger_deeptime("ggarrange2() no dims layout",
                               gtable::gtable_show_layout(gg5))
})
