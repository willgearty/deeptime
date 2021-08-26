suppressPackageStartupMessages(library(divDyn, quietly = TRUE))
suppressPackageStartupMessages(library(gsloid, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
data(corals)
suppressPackageStartupMessages(library(paleotree, quietly = TRUE))
data(RaiaCopesRule)

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
