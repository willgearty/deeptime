test_that("gggeo_scale() works", {
  library(divDyn)
  library(tidyverse)
  data(corals)
  # this is not a proper diversity curve but it gets the point across
  coral_div <- corals %>% filter(stage != "") %>%
    group_by(stage) %>%
    summarise(n = n()) %>%
    mutate(stage_age = (stages$max_age[match(stage, stages$name)] + stages$min_age[match(stage, stages$name)])/2)
  p <- ggplot(coral_div) +
    geom_line(aes(x = stage_age, y = n)) +
    scale_x_reverse("Age (Ma)") +
    ylab("Coral Genera") +
    coord_cartesian(xlim = c(250, 0), ylim = c(0, 1700), expand = FALSE) +
    theme_classic()
  gg <- gggeo_scale(p)
  expect_true(gtable::is.gtable(gg))
  expect_doppelganger("gggeo_scale()", gg)
})
