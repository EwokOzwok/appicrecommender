## code to prepare `appicgeo` dataset goes here
appicgeo <- read.csv("data-raw/appicgeo.csv")
usethis::use_data(appicgeo, overwrite = TRUE)
