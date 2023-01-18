track_ready <- track_data %>% 
  unnest(tracks) %>% 
  group_by(series, TrackID) %>% 
  mutate(n = n()) %>% 
  ungroup %>% 
  filter(Class == "Flower") %>% 
  mutate(DoY = yday(DateTime)) %>% 
  nest(individual = !c(TrackID, series)) %>% 
  nest(cushion = !series) %>% 
  mutate(SiteYear = factor(paste0(str_extract(series, "[:alpha:]+"), 
                                  str_extract(series, "[:digit:]{4}$")))) %>% 
  nest(site = !SiteYear)

bootvar_glmm <- lapply(1:100, function(BOOT) {
  quant <- runif(1, 0.2, 0.3)
  quant <- c(quant, 1 - quant)
  
  track_ready %>% 
    mutate(site = map(site, ~slice_sample(.x, n = nrow(.x), replace = T))) %>% 
    unnest(site) %>% 
    mutate(cushion = map(cushion, ~slice_sample(.x, n = nrow(.x), replace = T))) %>% 
    unnest(cushion) %>% 
    unnest(individual) %>% 
    group_by(series, TrackID) %>% 
    mutate(individualRange = quantile(DoY, quant[2]) - quantile(DoY, quant[1])) %>% 
    group_by(series) %>% 
    summarize(
      flowerEnd = quantile(DoY, quant[2]),
      flowerStart = quantile(DoY, quant[1]),
      cushionRange = flowerEnd - flowerStart,
      muIndRange = weighted.mean(individualRange, n),
      sdIndRange = sqrt(sum(n * (individualRange - muIndRange)^2)/((n() - 1) * sum(n) / n())),
      n = n(),
      .groups = "drop"
    ) %>%
  mutate(Site = str_extract(series, "[:alpha:]+") %>% 
           factor,
         weight = sdIndRange/sqrt(n),
         weight = weight/mean(weight)) %>% 
  mutate(temp = pmap(list(series, flowerEnd, flowerStart), function(Series, End, Start) {
    weather_data %>% 
      ungroup %>% 
      filter(station == str_extract(Series, "[:alpha:]+")) %>% 
      filter(year(DT) == as.numeric(str_extract(Series, "[:digit:]{4}$"))) %>% 
      filter(between(yday(DT), Start, End)) %>% 
      select(DT, maxDailyTemp, minDailyTemp)
  })) %>% 
  mutate(meanTemp = map_dbl(temp, function(x) {
    x %>% 
      summarize(meanTemp = (mean(minDailyTemp + maxDailyTemp)/2 - 32) * 5/9) %>% 
      pull(meanTemp)
  })) %>% 
  filter(weight > 0) %>% 
  gam(I(cushionRange/muIndRange) ~ meanTemp + s(Site, bs = "re"), 
      data = .,
      family = "quasipoisson"
      ) %>%
  coefficients
  }
)

empiric_p <- function(e) {
  (1 + sum(sign(e) != sign(median(e))))/(1 + length(e))
}

bootvar_glmm %>% 
  Reduce(rbind, .) %>% 
  as_tibble %>% 
  pivot_longer(everything(), names_to = "Parameter", values_to = "Estimate") %>% 
  group_by(Parameter) %>% 
  summarize(
    p = empiric_p(Estimate), 
    lower = quantile(Estimate, .025),
    upper = quantile(Estimate, 0.975),
    Estimate = median(Estimate),
  ) %>% 
  filter(Parameter == "meanTemp") %>% 
  select(Estimate, lower, upper, p) %>% 
  kableExtra::kable("latex", col.names = c("Estimate", "2.5\\%-quantile", "97.5\\%-quantile", "Adjusted Empiric P-value"),
                    escape = F,
                    booktabs = T) 