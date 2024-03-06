
nhanes_survival <- read.csv(file = "data-raw/nhanes_survival.csv",
                            row.names = "X")

# Use only complete cases of smoking
smoke_NA <- is.na(nhanes_survival$smoke)

nhanes_survival <- nhanes_survival[!smoke_NA,]

usethis::use_data(nhanes_survival, overwrite = TRUE)
