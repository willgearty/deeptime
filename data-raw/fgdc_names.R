# names.txt is modified from the file in Daven Quinn's GitHub repository:
# https://github.com/davenquinn/geologic-patterns/blob/master/docs/names.txt
fgdc_names <- read.table("data-raw/names.txt", sep = "\t", header = FALSE)
colnames(fgdc_names) <- c("code", "name")

usethis::use_data(fgdc_names, overwrite = TRUE)
