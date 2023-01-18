source("helpers.R")

stations <- c("NARS","BJOR","ELLA","KOBB")

years <- 2017:2021
months <- 1:12


NARS <- scrapeWeather("NARS", years, months)
BJOR <- scrapeWeather("BJOR", years, months)
ELLA <- scrapeWeather("ELLA", years, months) 
KOBB <- scrapeWeather("KOBB", years, months)

library(readr)
write_rds(NARS, "NARS.rds")
write_rds(BJOR, "BJOR.rds")
write_rds(ELLA, "ELLA.rds")
write_rds(KOBB, "KOBB.rds")
