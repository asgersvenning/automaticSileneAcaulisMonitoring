getTempOverPeriod <- function(Series, Year, Range) {
  Start <- Range[1]
  End <- Range[2]
  
  
  weather_data %>% 
    ungroup %>% 
    filter(station == as.character(Series)) %>% 
    filter(year(DT) == as.numeric(as.character(Year))) %>% 
    filter(between(yday(DT), Start, End)) %>% 
    select(DT, maxDailyTemp, minDailyTemp) %>% 
    summarize(
      meanTemp = mean(maxDailyTemp + minDailyTemp, na.rm = T)/2
    ) %>% 
    pull(meanTemp)
}

ind_agg <- track_data %>% 
  mutate(Site = str_extract(series, "[:alpha:]+") %>% factor,
         Year = str_extract(series, "[:digit:]{4}$") %>% factor,
         Series = factor(series)) %>% 
  unnest(tracks) %>% 
  drop_na %>% 
  group_by(series, TrackID) %>% 
  filter(sum(Class == "Flower") > 0 & 
         n() > 10) %>% 
  summarize(DateRange = range(yday(DateTime)) %>% diff %>% abs,
            nFlower = range(yday(DateTime[Class == "Flower"])) %>% diff %>% abs,
            flowerTemp = getTempOverPeriod(first(Site), 
                                           first(Year), 
                                           quantile(yday(DateTime[Class == "Flower"]), c(0, 1))),
            Fruit = factor(sum(Class == "Mature", na.rm = T) > 10),
            n = n(),
            .groups = "drop") %>% 
  filter(is.finite(nFlower))  

flowerLength <- track_data %>% 
  mutate(Site = str_extract(series, "[:alpha:]+") %>% factor,
         Year = str_extract(series, "[:digit:]{4}$") %>% factor,
         Series = factor(series)) %>% 
  unnest(tracks) %>% 
  drop_na %>% 
  group_by(series, TrackID) %>% 
  filter(sum(Class == "Flower") > 0 & 
           n() > 10) %>% 
  filter(Class == "Flower") %>% 
  summarize(
    lFlower = quantile(yday(DateTime) + hour(DateTime)/24, c(0, 1)) %>% diff %>% abs,
    .groups = "drop"
  ) %>% 
  pull(lFlower)
  
ind_agg %>% 
  mutate(Site = str_extract(series, "[:alpha:]+") %>% factor,
         Year = str_extract(series, "[:digit:]{4}$") %>% factor,
         Series = factor(series)) %>% 
  mutate(Fruit = as.numeric(Fruit) - 1,
         flowerLength = flowerLength) %>% 
  bam(Fruit ~ s(flowerLength) + s(flowerTemp) + s(DateRange) + s(n) + s(Site, Year, Series, bs = "re"), 
      data = .,
      discrete = T, na.action = na.omit, family = "binomial") %>%
  # summary
  # plot(residuals = T,
  #      scale = 0,
  #      tck = 0)
  broom::tidy() %>% 
  mutate(term = c(
    "Length of Flowering Period in Days",
    "Mean Daily Flowering Period Temperature",
    "Tracking Period Length",
    "Number of Frames Observed in",
    "Random Effects (Study Location, Year, Series)"
  )) %>% 
  set_colnames(c("Parameter", "Estimated Degrees of Freedom", "Reference Degrees of Freedom", "Chi-Statistic", "P-Value")) %>% 
  select(!`Reference Degrees of Freedom`) %>% 
  kableExtra::kable("latex", escape = F, booktabs = T) %>% 
  kableExtra::kable_styling(full_width = F)
