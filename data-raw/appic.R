## code to prepare `appic` dataset goes here
appic<-read.csv("APPIC_with_web_data.csv", header = T, sep = ",", encoding = "utf-8-rom")

usethis::use_data(appic, overwrite = TRUE)
