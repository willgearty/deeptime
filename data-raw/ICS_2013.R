#Get ICS data from PBDB
raw_dat <- read.csv(url("https://paleobiodb.org/data1.2/intervals/list.txt?scale_id=1"), stringsAsFactors = FALSE)
clean_dat <- raw_dat[,c("interval_name", "max_ma", "min_ma", "abbrev", "color","scale_level")]
colnames(clean_dat) <- c("name", "max_age", "min_age", "abbr", "color", "scale_level")

#Eon data
eons <- within(subset(clean_dat, scale_level == 1), rm(scale_level))
no_abbr <- (is.na(eons$abbr) | eons$abbr == "")
eons$abbr[no_abbr] <- abbreviate(eons$name, minlength = 1, use.classes = FALSE, named = FALSE)[no_abbr]
devtools::use_data(eons, overwrite = TRUE)

#Era data
eras <- within(subset(clean_dat, scale_level == 2), rm(scale_level))
no_abbr <- (is.na(eras$abbr) | eras$abbr == "")
eras$abbr[no_abbr] <- abbreviate(eras$name, minlength = 1, use.classes = FALSE, named = FALSE)[no_abbr]
devtools::use_data(eras, overwrite = TRUE)

#Period data
periods <- within(subset(clean_dat, scale_level == 3), rm(scale_level))
no_abbr <- (is.na(periods$abbr) | periods$abbr == "")
periods$abbr[no_abbr] <- abbreviate(periods$name, minlength = 1, use.classes = FALSE, named = FALSE)[no_abbr]
devtools::use_data(periods, overwrite = TRUE)

#Epoch data
epochs <- within(subset(clean_dat, scale_level == 4), rm(scale_level))
no_abbr <- (is.na(epochs$abbr) | epochs$abbr == "")
epochs$abbr[no_abbr] <- abbreviate(epochs$name, minlength = 1, use.classes = FALSE, named = FALSE)[no_abbr]
devtools::use_data(epochs, overwrite = TRUE)

#Stage data
stages <- within(subset(clean_dat, scale_level == 5), rm(scale_level))
no_abbr <- (is.na(stages$abbr) | stages$abbr == "")
stages$abbr[no_abbr] <- abbreviate(stages$name, minlength = 1, use.classes = FALSE, named = FALSE)[no_abbr]
devtools::use_data(stages, overwrite = TRUE)
