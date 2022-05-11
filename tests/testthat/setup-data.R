suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))

if(suppressPackageStartupMessages(require(divDyn, quietly = TRUE))) {
  data(corals)
  coral_div <- corals %>% filter(stage != "") %>%
    group_by(stage) %>%
    summarise(n = n()) %>%
    mutate(stage_age = (stages$max_age[match(stage, stages$name)] + stages$min_age[match(stage, stages$name)])/2)

  coral_div_diet <- corals %>% filter(stage != "") %>%
    group_by(diet, stage) %>%
    summarise(n = n()) %>%
    mutate(stage_age = (stages$max_age[match(stage, stages$name)] + stages$min_age[match(stage, stages$name)])/2)

  coral_div_dis <- corals %>% filter(period != "") %>%
    group_by(diet, period) %>%
    summarise(n = n()) %>%
    mutate(period_age = (periods$max_age[match(period, periods$name)] + periods$min_age[match(period, periods$name)])/2) %>%
    arrange(-period_age)
}

if(suppressPackageStartupMessages(require(paleotree, quietly = TRUE))) {
  data(RaiaCopesRule)
}

if(suppressPackageStartupMessages(require(phytools, quietly = TRUE))) {
  data(mammal.tree)
}

if(suppressPackageStartupMessages(require(dispRity, quietly = TRUE))) {
  data(demo_data)
}
