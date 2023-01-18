library(rjson)

conv <- (2 * pi) / 360

declination <- function(DoY) {
  asin(0.39779 * cos(conv * (0.98565 * (DoY + 10) + 1.914 * sin(conv * (0.98565 * (DoY - 2))))))
}

daylight <- function(DoY, longitude) {
  cosines <- tan(declination(DoY)) * tan(longitude * conv)
  cosines[cosines < -1] <- -1
  cosines[cosines > 1] <- 1
  acos(cosines) * 24/pi
}

seriesPredictors <- function(x) {
  x %>% 
    mutate(DateTime = yday(DateTime)) %>% 
    arrange(DateTime) %>% 
    summarize(
      firstFlowerTime = DateTime[which(Class == "Flower")] %>% 
        first,
      firstFlowerDaylight = DayLight[which(Class == "Flower")] %>% 
        first,
      lastFlowerTime = DateTime[which(Class == "Flower")] %>% 
        last,
      lastFlowerDaylight = DayLight[which(Class == "Flower")] %>% 
        last,
      durationFlowerTime = lastFlowerTime - firstFlowerTime,
      successfulFruit = any(Class == "Mature"),
      .groups = "drop"
    )
}

longitude <- function(location) {
  location <- str_extract(location, "BJOR|NARS|ELLA|KOBB")
  
  sapply(location, function(x) switch(x,
         "BJOR" = 78.196667,
         "NARS" = 61.1567,
         "ELLA" = 72.851,
         "KOBB" = 64.176667))
} 

latitude <- function(location) {
  location <- str_extract(location, "BJOR|NARS|ELLA|KOBB")
  
  sapply(location, function(x) switch(x,
                                      "BJOR" = 15.3097,
                                      "NARS" = -45.4254,
                                      "ELLA" = -25.0333,
                                      "KOBB" = -51.5477))
}

scrapeWeather <- function(location = "NARS", year, month) {
  LATITUDE <- latitude(location) %>% 
    round(3)
  LONGITUDE <- longitude(location) %>% 
    round(3)
  
  lapply(year, function(YEAR) {
    lapply(month, function(MONTH) {
        DT <- ym(paste0(YEAR, ":", MONTH))
        MONTH <- as.character(MONTH)
        MONTH <- if (nchar(MONTH) == 2) MONTH else paste0("0", MONTH)
        
        fDay <- paste0(YEAR, MONTH, "01")
        lDay <- paste0(YEAR, MONTH, days_in_month(DT))
        
        paste0("https://api.weather.com/v1/geocode/", 
               LONGITUDE,
               "/",
               LATITUDE,
               "/observations/historical.json?apiKey=",
               "e1f10a1e78da46f5b10a1e78da96f525", # An API key can be put here 
               "&units=e&startDate=", 
               fDay, 
               "&endDate=",
               lDay) %>%
          read_file %>% 
          fromJSON %$%
          observations
    }) %>% 
      Reduce(c, .)
  }) %>% 
    Reduce(c, .) %>% 
    tibble(data = .) %>% 
    unnest_wider(data)
}


