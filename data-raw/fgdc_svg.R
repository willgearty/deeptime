# convert svgs to vectorized grobs
# this removes the need for users to have rsvg/grImport2
library(rsvg)
library(grImport2)

old_wd <- getwd()

setwd("data-raw/")

svgs <- list.files("usgs_svg/")

fgdc_grobs <- list()
for (svg in svgs) {
  filename <- file.path(tempdir(), "temp.svg")
  rsvg_svg(paste0(getwd(), "/usgs_svg/", svg), filename)
  code <- gsub("(-.*)?.svg", "", svg)
  fgdc_grobs[[code]] <- pictureGrob(readPicture(filename, warn = FALSE),
                                   expansion = 0)
}

usethis::use_data(fgdc_grobs, internal = TRUE, compress = "xz", overwrite = TRUE)

setwd(old_wd)
