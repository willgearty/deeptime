int_types <- c("eons" = "eons", "eras" = "eras", "periods" = "periods",
               "epochs" = "epochs", "stages" = "ages")

for(int_type in names(int_types)){
  # Get ICS data from Macrostrat
  raw_dat <- read.csv(url(paste("https://macrostrat.org/api/v2/defs/intervals?format=csv&timescale=international", int_types[int_type], sep = "%20")), stringsAsFactors = FALSE)
  clean_dat <- raw_dat[,c("name", "b_age", "t_age", "abbrev", "color")]
  colnames(clean_dat) <- c("name", "max_age", "min_age", "abbr", "color")

  # Set abbreviations where they don't already exist
  no_abbr <- (is.na(clean_dat$abbr) | clean_dat$abbr == "")
  clean_dat$abbr[no_abbr] <- abbreviate(clean_dat$name, minlength = 1, use.classes = TRUE, named = FALSE)[no_abbr]
  clean_dat$abbr[clean_dat$name == "Stage 10"] = "S10" # fix abbreviation
  assign(int_type, clean_dat)
  do.call(eval(parse(text="usethis::use_data")), list(as.name(int_type), overwrite = TRUE))
}
