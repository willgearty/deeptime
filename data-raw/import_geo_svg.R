# convert svgs to vectorized grobs
# this removes the need for users to have rsvg/grImport2
library(rsvg)
library(grImport2)

old_wd <- getwd()

setwd("data-raw/")

svgs <- list.files("geo_svg/")

geo_grobs <- list()
for (svg in svgs) {
  filename <- file.path(tempdir(), "temp.svg")
  rsvg_svg(paste0(getwd(), "/geo_svg/", svg), filename)
  code <- gsub("(-.*)?.svg", "", svg)
  geo_grobs[[code]] <- pictureGrob(readPicture(filename, warn = FALSE),
                                   expansion = 0)
}

usethis::use_data(geo_grobs, internal = TRUE, compress = "xz", overwrite = TRUE)

setwd(old_wd)
