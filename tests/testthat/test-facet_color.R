test_that("facet_grid_color works", {
  skip_if_not_installed("divDyn")
  gg_base <- ggplot(coral_div_dis) +
    geom_col(aes(x = diet, y = n, fill = diet)) +
    scale_x_discrete("Diet", labels = NULL) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    theme(axis.ticks.length.x = unit(0, "lines"))
  gg <- gg_base + facet_grid_color(cols = vars(period), colors = periods)
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetGridColor"))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetGrid"))
  gg_gtable <- ggplot_gtable(ggplot_build(gg))
  first_strip <- gg_gtable$grobs[[grep("strip", gg_gtable$layout$name)[1]]]
  expect_equal(first_strip$grobs[[1]]$children[[1]]$gp$fill,
               periods$color[
                 match(first_strip$grobs[[1]]$children[[2]]$children[[1]]$label,
                       periods$name)
               ]
  )
  expect_equal(first_strip$grobs[[1]]$children[[2]]$children[[1]]$gp$col,
               periods$lab_color[
                 match(first_strip$grobs[[1]]$children[[2]]$children[[1]]$label,
                       periods$name)
               ]
  )
  expect_doppelganger_deeptime("facet_grid_color", gg)
  expect_error(gg_base +
                 facet_grid_color(cols = vars(period), colors = "blue"))
  expect_error(gg_base +
                 facet_grid_color(cols = vars(period), colors = 5))
  expect_error(gg_base +
                 facet_grid_color(cols = vars(period),
                                  colors = data.frame(colors = c("blue", "red"))
                                  ))
  gg <- gg_base +
    facet_grid_color(cols = vars(period),
                     colors = setNames(periods$color, periods$name))
  expect_no_error(gg)
  expect_doppelganger_deeptime("facet_grid_color with named vector", gg)

  period_color <- function(period) periods$color[match(period, periods$name)]
  gg <- gg_base +
    facet_grid_color(cols = vars(period), colors = period_color)
  expect_no_error(gg)
  expect_doppelganger_deeptime("facet_grid_color with function", gg)

  gg <- gg_base + facet_grid_color(cols = vars(period), colors = periods,
                                   lab_colors = periods)
  expect_doppelganger_deeptime("facet_grid_color-lab_colors", gg)

  gg <- gg_base + facet_grid_color(cols = vars(period), colors = periods,
                                   lab_colors = function(label) "blue")
  expect_doppelganger_deeptime("facet_grid_color-blue labels", gg)

  gg <- gg_base + facet_grid_color(cols = vars(period), colors = periods,
                                   lab_colors = c("Triassic" = "blue"))
  expect_doppelganger_deeptime("facet_grid_color-Triassic blue", gg)

  expect_error(gg_base +
                 facet_grid_color(cols = vars(period), colors = periods,
                                  lab_colors = "blue"))
  expect_error(gg_base +
                 facet_grid_color(cols = vars(period), colors = periods,
                                  lab_colors = 5))
  expect_error(gg_base +
                 facet_grid_color(cols = vars(period),
                                  colors = periods,
                                  lab_colors =
                                    data.frame(colors = c("blue", "red"))
                 ))
})

test_that("facet_wrap_color works", {
  skip_if_not_installed("divDyn")
  gg_base <- ggplot(coral_div_dis) +
    geom_col(aes(x = diet, y = n, fill = diet)) +
    scale_x_discrete("Diet", labels = NULL) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    theme(axis.ticks.length.x = unit(0, "lines"))
  gg <- gg_base +
    facet_wrap_color(vars(period), colors = periods)
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetWrapColor"))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetWrap"))
  gg_gtable <- ggplot_gtable(ggplot_build(gg))
  first_strip <- gg_gtable$grobs[[grep("strip", gg_gtable$layout$name)[1]]]
  expect_equal(first_strip$grobs[[1]]$children[[1]]$gp$fill,
               periods$color[
                 match(first_strip$grobs[[1]]$children[[2]]$children[[1]]$label,
                       periods$name)
               ]
  )
  expect_doppelganger_deeptime("facet_wrap_color", gg)

  gg <- gg_base + facet_wrap_color(vars(period), colors = periods,
                                   lab_colors = function(label) "blue")
  expect_doppelganger_deeptime("facet_wrap_color-blue labels", gg)
  expect_error(gg_base +
                 facet_wrap_color(vars(period), colors = periods,
                                  lab_colors = "blue"))
  expect_error(gg_base +
                 facet_wrap_color(vars(period), colors = periods,
                                  lab_colors = 5))
  expect_error(gg_base +
                 facet_wrap_color(vars(period), colors = periods,
                                  lab_colors =
                                    data.frame(colors = c("blue", "red"))
                 ))
})

test_that("facet_nested_color works", {
  skip_if_not_installed("divDyn")
  coral_div_dis$era <- factor("Mesozoic", levels = c("Mesozoic", "Cenozoic"))
  coral_div_dis$era[coral_div_dis$period %in%
                      c("Paleogene", "Neogene", "Quaternary")] <- "Cenozoic"
  gg_base <- ggplot(coral_div_dis) +
    geom_col(aes(x = diet, y = n, fill = diet)) +
    scale_x_discrete("Diet", labels = NULL) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    theme(axis.ticks.length.x = unit(0, "lines"))
  gg <- gg_base +
    facet_nested_color(~era + period, colors = rbind(periods, eras))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetNestedColor"))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetNested"))
  gg_gtable <- ggplot_gtable(ggplot_build(gg))
  first_strip <- gg_gtable$grobs[[grep("strip", gg_gtable$layout$name)[1]]]
  expect_equal(first_strip$grobs[[1]]$children[[1]]$gp$fill,
               eras$color[
                 match(first_strip$grobs[[1]]$children[[2]]$children[[1]]$label,
                       eras$name)
               ]
  )
  expect_doppelganger_deeptime("facet_nested_color", gg)

  expect_error(gg_base +
                 facet_nested_color(~era + period,
                                    colors = rbind(periods, eras),
                                    lab_colors = "blue"))
  expect_error(gg_base +
                 facet_nested_color(~era + period,
                                    colors = rbind(periods, eras),
                                    lab_colors = 5))
  expect_error(gg_base +
                 facet_nested_color(~era + period,
                                    colors = rbind(periods, eras),
                                    lab_colors =
                                      data.frame(colors = c("blue", "red"))
                 ))
})

test_that("facet_nested_wrap_color works", {
  skip_if_not_installed("divDyn")
  coral_div_dis$era <- factor("Mesozoic", levels = c("Mesozoic", "Cenozoic"))
  coral_div_dis$era[coral_div_dis$period %in%
                      c("Paleogene", "Neogene", "Quaternary")] <- "Cenozoic"
  gg_base <- ggplot(coral_div_dis) +
    geom_col(aes(x = diet, y = n, fill = diet)) +
    scale_x_discrete("Diet", labels = NULL) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_d() +
    ylab("Coral Genera") +
    theme(axis.ticks.length.x = unit(0, "lines"))
  gg <- gg_base +
    facet_nested_wrap_color(~era + period, colors = rbind(periods, eras))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetNestedWrapColor"))
  expect_true(is(ggplot_build(gg)$layout$facet, "FacetNestedWrap"))
  gg_gtable <- ggplot_gtable(ggplot_build(gg))
  first_strip <- gg_gtable$grobs[[grep("strip", gg_gtable$layout$name)[1]]]
  expect_equal(first_strip$grobs[[1]]$children[[1]]$gp$fill,
               eras$color[
                 match(first_strip$grobs[[1]]$children[[2]]$children[[1]]$label,
                       eras$name)
               ]
  )
  expect_doppelganger_deeptime("facet_nested_wrap_color", gg)

  expect_error(gg_base +
                 facet_nested_wrap_color(~era + period,
                                         colors = rbind(periods, eras),
                                         lab_colors = "blue"))
  expect_error(gg_base +
                 facet_nested_wrap_color(~era + period,
                                         colors = rbind(periods, eras),
                                         lab_colors = 5))
  expect_error(gg_base +
                 facet_nested_wrap_color(~era + period,
                                         colors = rbind(periods, eras),
                                         lab_colors =
                                           data.frame(colors = c("blue", "red"))
                 ))
})
