suppressPackageStartupMessages(library(ggplot2, quietly = TRUE))

if (suppressPackageStartupMessages(require(divDyn, quietly = TRUE))) {
  data(corals)
  corals_stages_clean <- subset(corals, stage != "")
  coral_div <- aggregate(cbind(n = genus) ~ stage,
    data = corals_stages_clean,
    FUN = function(x) length(x)
  )
  coral_div$stage_age <- (stages$max_age[match(coral_div$stage, stages$name)] +
    stages$min_age[match(coral_div$stage, stages$name)]) / 2

  coral_div_diet <- aggregate(cbind(n = genus) ~ stage + diet,
    data = corals_stages_clean,
    FUN = function(x) length(x)
  )
  coral_div_diet$stage_age <-
    (stages$max_age[match(coral_div_diet$stage, stages$name)] +
       stages$min_age[match(coral_div_diet$stage, stages$name)]) / 2

  corals_periods_clean <- subset(corals, period != "")
  coral_div_dis <- aggregate(cbind(n = genus) ~ period + diet,
    data = corals_periods_clean,
    FUN = function(x) length(x)
  )
  coral_div_dis$period_age <-
    (periods$max_age[match(coral_div_dis$period, periods$name)] +
       periods$min_age[match(coral_div_dis$period, periods$name)]) / 2
  coral_div_dis <-
    coral_div_dis[rev(order(coral_div_dis$period_age)), , drop = FALSE]
  coral_div_dis$period <- factor(coral_div_dis$period,
                                 levels = unique(coral_div_dis$period))
}

if (suppressPackageStartupMessages(require(ape, quietly = TRUE))) {
  set.seed(1)
  tree <- rtree(100)
}

if (suppressPackageStartupMessages(require(paleotree, quietly = TRUE))) {
  data(RaiaCopesRule)
}

if (suppressPackageStartupMessages(require(phytools, quietly = TRUE))) {
  data(mammal.tree)
}

if (suppressPackageStartupMessages(require(dispRity, quietly = TRUE))) {
  data(demo_data)
}

if (suppressPackageStartupMessages(require(palaeoverse, quietly = TRUE))) {
  data(tetrapods)
}

suppressPackageStartupMessages(require(gsloid, quietly = TRUE))
suppressPackageStartupMessages(require(ggtree, quietly = TRUE))
suppressPackageStartupMessages(require(dispRity, quietly = TRUE))
suppressPackageStartupMessages(require(grid, quietly = TRUE))
suppressPackageStartupMessages(require(ggpattern, quietly = TRUE))

# copied from vdiffr
str_standardise <- function(s, sep = "-") {
  s <- gsub("[^a-z0-9]", sep, tolower(s))
  s <- gsub(paste0(sep, sep, "+"), sep, s)
  s <- gsub(paste0("^", sep, "|", sep, "$"), "", s)
  s
}

# copied from https://github.com/r-lib/vdiffr/issues/132#issuecomment-1477006436
write_svg_bleeding_edge <- function(plot, file, title = "") {
  svglite::svglite(file)
  on.exit(grDevices::dev.off())
  # warning: `print_plot()` is not exported by {vdiffr}
  vdiffr:::print_plot(plot, title)
}

expect_doppelganger_deeptime <- function(title, fig, patterns = FALSE) {
  title_new <- paste(title, "new")
  fig_name <- str_standardise(title_new)
  file <- paste0(fig_name, ".svg")
  announce_snapshot_file(name = file)

  variant <- switch(packageVersion("ggplot2") > "3.5.2", "ggplot4", NULL)

  if (patterns) {
    expect_doppelganger(title_new, fig, writer = write_svg_bleeding_edge,
                        variant = variant)
  } else {
    expect_doppelganger(title_new, fig, variant = variant)
  }
}
