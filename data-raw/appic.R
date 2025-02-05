## code to prepare `appic` dataset goes here
appic<-read.csv("data-raw/appic_clean.csv", header = T, sep = ",", encoding = "utf-8-rom")

usethis::use_data(appic, overwrite = TRUE)
