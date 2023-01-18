library(tidyverse)
library(magrittr)
library(lubridate)
library(readr)

source("helpers.R")

# Unzip the data before running this script
tracking_dir <- "Tracking"

series_dir <- list.files(tracking_dir) %>% 
  extract(which(str_detect(., "\\.csv$"))) 

track_data <- tibble(
  series = series_dir
) %>% 
  mutate(tracks = map(paste0(tracking_dir, "/", series), 
                      read_tsv, na = "NA", show_col_types = F),
         tracks = map(tracks, 
                      ~mutate(., 
                              DateTime = ymd_hms(DateTime),
                              TrackID = factor(TrackID)))) %>% 
  mutate(series = str_remove(series, "\\.csv$"))

dateRange <- track_data %>% 
  unnest(tracks) %>% 
  pull(DateTime) %>% 
  range(na.rm = T) %>% 
  floor_date("day")

weather_data <- tibble(station = c(
  "NARS",
  "BJOR",
  "ELLA",
  "KOBB"
)) %>% 
  mutate(data = map(station, function(x) read_rds(paste0(x, ".rds")) %>% 
                      mutate(clds = as.numeric(clds)))) %>% 
  unnest(data) %>% 
  mutate(DT = as_datetime(valid_time_gmt)) %>% 
  group_by(station, day = yday(DT), year = year(DT)) %>% 
  summarize(
    maxDailyTemp = max(temp, na.rm = T),
    minDailyTemp = min(temp, na.rm = T),
    DT = DT %>% first %>% lubridate::floor_date("day"),
    .groups = "drop"
  ) %>% 
  select(!c(day, year)) %>% 
  mutate(DTF = factor(as.character(ymd(DT)), 
                      levels = as.character(ymd(seq(dateRange[1],dateRange[2], by = "1 day"))))) %>% 
  group_by(station) %>% 
  complete(DTF)

yyday <- function(date) {
  paste0(year(date), "-", yday(date))
}


combined_data <- track_data %>%
  unnest(tracks) %>% 
  group_by(Site = str_extract(series, "[:alpha:]+")) %>% 
  filter(Site %in% c("BJOR","NARS","KOBB","ELLA")) %>%
  mutate(temp = weather_data %>% 
           dplyr::select(station, minDailyTemp, maxDailyTemp) %>% 
           filter(station == Site[1]) %>% 
           select(!station) %>%
           {.[(year(DateTime) - 2017) * 365 + yday(DateTime),]} 
    ) %>% 
  ungroup %>% 
  unnest(temp) %>% 
  mutate(DayLight = daylight(yday(DateTime), longitude(series)))