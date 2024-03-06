# Saving Framingham data to package

framingham <- read.table("data-raw/framingham.txt", header = TRUE)
names(framingham) <- c("disease", "sbp1", "sbp2", "smoking")


usethis::use_data(framingham, overwrite = TRUE)
