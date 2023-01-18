library(tidyverse)
library(magrittr)
library(lubridate)
library(readr)
library(extrafont)
library(ggforce)

tracking_dir <- "compare_tracking"

series_dir <- list.files(tracking_dir) %>% 
  extract(which(str_detect(., "\\.csv$"))) 

# plan("multisession")
track_data <- tibble(
  series = series_dir
) %>% 
  # filter(series == "BJOR_01") %>% 
  mutate(tracks = map(paste0(tracking_dir, "/", series), 
                      read_tsv, na = "NA", show_col_types = F),
         tracks = map(tracks, 
                      ~mutate(., 
                              DateTime = ymd_hms(DateTime),
                              TrackID = factor(TrackID)))) %>% 
  mutate(series = str_remove(series, "\\.csv$"))

driftPlot <- track_data %>%
  unnest(tracks) %>% 
  group_by(TrackID) %>%
  filter(n() > 25) %>%
  ungroup %>%
  mutate(series = ifelse(str_detect(series, "good"), "Without Kalman Filter", "With Kalman Filter")) %>% 
  ggplot(aes(x,y,color=TrackID)) +
  # geom_rect(aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
  #           color = "transparent", fill = "#00000011") +
  geom_point(size = 1) +
  geom_path(size = 2) +
  coord_equal(xlim = c(0.9, 1.2), ylim = c(0.4, 0.8)) +
  ggpubr::theme_pubr(base_family = "CMU Serif") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 16, family = "CMU Serif"),
        plot.background = element_rect("transparent", "transparent")) +
  facet_wrap(~series)

ggsave("Figures/drift.png", driftPlot,
       dpi = 400, scale = 2, width = 4, height = 4)