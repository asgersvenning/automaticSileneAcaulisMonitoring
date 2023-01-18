library(extrafont)

classHistogram <- track_data %>% 
  filter(!str_detect(series, "TIMELAPSE")) %>% 
  unnest(tracks) %>% 
  mutate(DateTime = floor_date(DateTime, "2 days"),
         Class = ifelse(is.na(Class), "Missing", Class) %>% 
           factor(levels = c(
             "Bud", "Flower", "Immature", "Mature", "Missing"
           ))) %>% 
  count(series, DateTime, Class, .drop = F) %>% 
  group_by(series) %>% 
  mutate(n = ifelse(Class == "Missing", max(n, na.rm = T), n)) %>% 
  group_by(series, DateTime) %>% 
  mutate(n = (n + 0.001)/sum(n, na.rm = T)) %>% 
  ungroup %>% 
  mutate(Class = ifelse(Class == "Missing", NA, as.character(Class)) %>% 
           factor(levels = c("Bud", "Flower", "Immature", "Mature")),
         Site = str_extract(series, "[:alpha:]+") %>% 
           factor(levels = c("NA", "BJOR", "NARS", "ELLA", "KOBB")),
         Year = year(DateTime) %>% 
           factor(levels = c("NA", as.character(2017:2021)))) %>% 
  nest(plotData = !c(Site, Year)) %>% 
  mutate(plt = pmap(list(plotData, Site, Year), function(pd, S, Y) {
    pd %>% 
      mutate(series = factor(series) %>% 
               droplevels) %>% 
      group_by(series, DateTime) %>% 
      arrange(series, DateTime, Class) %>% 
      mutate(
        y = as.numeric(series) + cumsum(n) - n/2 - 1/2
      ) %>% 
      ungroup %>% 
      ggplot(aes(DateTime, y, fill = Class, height=n)) +
      geom_tile(width = 2 * 88000,
                key_glyph = draw_key_point) +
      scale_fill_brewer(palette = "Dark2",
                        na.value = "gray75",
                        labels = function(x) c("Bud", "Flower", "Immature Fruit", "Mature Fruit", "None")) +
      scale_x_datetime(date_breaks = "1 month",
                       date_labels = "%b") +
      coord_cartesian(expand = F) +
      guides(
        fill = guide_legend(
          override.aes = list(
            shape = 21,
            color = "black",
            size = 10,
            stroke = .75
          ),
          nrow = 2
        )
      ) +
      labs(fill = "Flower Lifestage") +
      ggpubr::theme_pubr(legend = "bottom",
                         base_family = "CMU Serif") +
      theme(
        axis.title = element_blank(),
        plot.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.ticks.y = element_blank()
            ) 
  })) %>% 
  complete(Site, Year, fill = list(plt = list(ggplot() + theme_void()))) %>% 
  mutate(plt = pmap(list(Site, Year, plt), function(S,Y,oplt) {
    if (S != "NA" & Y != "NA") return(oplt)
    if (S == "NA" & Y == "NA") return(ggplot() + theme_void())
    if (S == "NA") {
      p <- ggplot() +
        theme_void() +
        labs(x = Y) +
        theme(axis.title.x = element_text(face = "bold", 
                                          size = 24,
                                          family = "CMU Serif"))
    }
    if (Y == "NA") {
      p <- ggplot() +
        theme_void() +
        labs(y = S) +
        theme(axis.title.y = element_text(face = "bold", 
                                          size = 24,
                                          angle = 90,
                                          family = "CMU Serif"))
    }
    
    return(p)
  })) %>% 
  arrange(Year, Site) %>% 
  pull(plt) %>%
  wrap_plots(
    guides = "collect",
    byrow = F,
    nrow = 5,
    ncol = 6,
    heights = c(0.1, 3, 3, 1, 1),
    widths = c(0.1, 1, 1, 1, 1, 1)
  ) &
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold",
                                    size = 24,
                                    vjust = .5),
        legend.text = element_text(size = 16),
        plot.title = element_text(face = "plain",
                                  family = "CMU Serif",
                                  size = 32,
                                  hjust = .5))

ggsave("classHistogram.png", classHistogram, type = "ragg-png",
       dpi = 500, width = 16, height = 12, scale = 1.5)


library(furrr)

threeLevelNested <- track_data %>% 
  filter(!str_detect(series, "TIMELAPSE")) %>% 
  unnest(tracks) %>% 
  filter(!is.na(Class)) %>% 
  mutate(DateTime = floor_date(DateTime, "1 days"),
         Class = ifelse(is.na(Class), "Missing", Class) %>% 
           factor(levels = c(
             "Bud", "Flower", "Immature", "Mature"
           ))) %>% 
  select(series, TrackID, Class, DateTime) %>% 
  mutate(Class = sapply(Class, function(x) as.numeric(levels(x) == x)) %>% 
           t %>% 
           divide_by(rowSums(.))) %>% 
  group_by(series,TrackID,DateTime) %>% 
  summarize(Class = colSums(Class) %>% t,
            .groups = "drop") %>% 
  nest(individual = !c(series, TrackID)) %>% 
  nest(cushion = !series) %>% 
  mutate(Site = str_extract(series, "[:alpha:]+")) %>% 
  nest(location = !Site)

bootS <- function(x, w) {
  if (missing(w)) {
    # Without weights
    if (is.vector(x)) {
      sample(x, length(x), T)
    } 
    else {
      x[sample(nrow(x), nrow(x), T),]
    }
  }
  else {
    # With weights
    if (is.vector(x)) {
      sample(x, length(x), T, prob = w/sum(w))
    } 
    else {
      x[sample(nrow(x), nrow(x), T), prob = w/sum(w)]
    }
  }
}

classDensity <- function(date, w) {
  weightSum <- sum(w)
  if (weightSum == 0) weightSum <- 1
  
  density(yday(date), 
          weights = w/weightSum,
          n = abs(diff(range(yday(date)))),
          from = c(min(yday(date)) - 1, max(yday(date)) + 1)) %>% 
    {
      .[1:2]
    } %>% 
    as_tibble
}

plan("multisession", workers = 4)
boot_dens <- tibble(boot = 1:100) %>% 
  mutate(res = future_map(boot, function(DUMMY) {
    threeLevelNested %>% 
      mutate(location = map(location, function(l) {
        l %>% 
          bootS %>% 
          mutate(cushion = map(cushion, function(x) {
            bootS(x, sqrt(map_dbl(x, individual, nrow)))
            }))
      })) %>% 
      unnest(location) %>% 
      unnest(cushion) %>% 
      unnest(individual) %>% 
      mutate(Class = Class %>% 
               set_colnames(c("Bud","Flower","Immature Fruit", "Mature Fruit")) %>% 
               as_tibble) %>% 
      unnest(Class) %>% 
      group_by(Site) %>% 
      summarize(
        across(c(Bud, Flower, `Immature Fruit`, `Mature Fruit`), ~classDensity(DateTime, .x)),
        .groups = "drop"
      ) %>%
      pivot_longer(c(Bud, Flower, `Immature Fruit`, `Mature Fruit`), 
                   names_to = "Class", values_to = "Density") %>% 
      unnest(Density)
  },
  .options = furrr_options(seed = T))
  )
plan("sequential")


quant_stat <- function(x, mid, min, max, na.rm = T) {
  c(quantile(x, mid, na.rm = na.rm), quantile(x, min, na.rm = na.rm), quantile(x, max, na.rm = na.rm)) %>% 
    unname %>% 
    set_names(c("y","ymin","ymax")) 
}

quant_stat_mean <- function(x, min, max, na.rm = T) {
  c(quantile(x, min, na.rm = na.rm), quantile(x, max, na.rm = na.rm)) %>% 
    unname %>% 
    c(mean(.),.) %>% 
    set_names(c("y","ymin","ymax")) 
}

densityPlot <- function(Site, Class, data, y_max, x_min, x_max, xaxis = T) {
  data %>% 
    mutate(x = date_decimal(2020 + x/366)) %>% 
    ggplot(aes(x,y)) +
    stat_summary_bin(geom = "errorbar",
                     fun.data = ~quant_stat(., .875, .75, .95),
                     color = "firebrick",
                     size = 1,
                     binwidth = 86400 * 7,
                     show.legend = F) +
    stat_summary_bin(geom = "errorbar",
                     fun.data = ~quant_stat(., 0.125, 0.05, .25),
                     color = "firebrick",
                     size = 1,
                     binwidth = 86400 * 7,
                     show.legend = F) +
    stat_summary_bin(aes(height = after_stat(ymax - ymin)),
                     position = position_nudge(y = 0),
                     width = 1,
                     geom = "tile",
                     color = "firebrick",
                     fill = "#00000000",
                     fun.data = ~quant_stat_mean(., .25, .75),
                     binwidth = 86400 * 7,
                     size = 1,
                     key_glyph = draw_key_point) +
    geom_path(aes(group = factor(boot)),
              alpha = .05, size = .5,
              show.legend = F) +
    # scale_color_brewer(palette = "Set1") +
    scale_x_datetime(date_breaks = "1 month",
                     date_labels = "%b") +
    coord_cartesian(ylim = c(0, y_max*1.1), expand = F) +
    guides(color = guide_legend(override.aes = list(shape = 16,
                                                    size = 5),
                                nrow = 2)) +
    ggpubr::theme_pubr(legend = "bottom",
                       base_family = "CMU Serif") +
    theme(
      aspect.ratio = .5,
      panel.spacing = unit(1, "lines"),
      title = element_text(face = "bold",
                           size = 16),
      plot.title = element_text(face = "plain",
                                size = 12),
      # strip.background = element_blank(),
      axis.text.x = if (xaxis) element_text() else element_blank(),
      strip.text = element_text(face = "bold",
                                size = 16)) +
    labs(x = NULL, y = NULL, title = NULL)
  #title = paste0(Site, " | ", Class))
}

bootstrap_density <- boot_dens %>% 
  unnest(res) %>% 
  group_by(Site, Class, x_int = cut_interval(x, 20)) %>% 
  mutate(y_max = quantile(y, 0.95)) %>% 
  group_by(Site, Class) %>% 
  mutate(y_max = max(y_max)) %>% 
  select(!x_int) %>% 
  ungroup %>% 
  group_by(Site) %>% 
  mutate(x_min = min(x),
         x_max = max(x)) %>% 
  ungroup %>% 
  nest(plotData = !c(Site, Class, y_max, x_min, x_max)) %>% 
  mutate(Class = as.character(Class) %>% 
           str_replace(" ", "\n") %>% 
           factor(levels = c(
             "NA",
             "Bud",
             "Flower",
             "Immature\nFruit",
             "Mature\nFruit"
           )),
         Site = factor(Site, levels = c(
           "NA", 
           "NARS", 
           "BJOR", 
           "ELLA", 
           "KOBB"
         )),
         nClass = as.numeric(Class) == 5) %>% 
  mutate(plt = pmap(list(Site,Class,plotData,y_max,x_min,x_max, nClass), densityPlot)) %>% 
  complete(Site, Class, fill = list(plt = list(ggplot() + theme_void()))) %>% 
  mutate(plt = pmap(list(Site, Class, plt), function(S, C, oplt) {
    if (S != "NA" & C != "NA") return(oplt)
    if (S == "NA" & C == "NA") return(ggplot() + theme_void())
    if (S == "NA") {
      p <- ggplot() +
        theme_void() +
        labs(y = C) +
        theme(axis.title.y = element_text(face = "bold", 
                                          size = 24,
                                          angle = 90,
                                          family = "CMU Serif"),
              aspect.ratio = 1)
    }
    if (C == "NA") {
      p <- ggplot() +
        theme_void() +
        labs(x = S) +
        theme(axis.title.x = element_text(face = "bold", 
                                          size = 24,
                                          # angle = 90,
                                          family = "CMU Serif"),
              plot.margin = margin(0,0,0,0,unit = "lines"),
              aspect.ratio = 1)
    }
    
    return(p)
  })) %>% 
  arrange(Site, Class) %>% 
  pull(plt) %>%
  wrap_plots(
    guides = "collect",
    byrow = F,
    nrow = 5,
    ncol = 6,
    widths = c(0.1, 1, 1, 1, 1),
    heights = c(0.1, 1, 1, 1, 1, 1)
  ) &
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold",
                                    size = 24,
                                    vjust = .5),
        legend.text = element_text(size = 16),
        plot.title = element_text(face = "plain",
                                  family = "CMU Serif",
                                  size = 32,
                                  hjust = .5))

ggsave("bootstrap_density.png", bootstrap_density,
       type = "ragg-png", dpi = 300,
       height = 6, width = 8, scale = 2)


classCounts <- track_data %>% 
  mutate(Year = str_extract(series, "[:digit:]{4}$"),
         Site = str_extract(series, "[:alpha:]+")) %>% 
  mutate(Class = map(tracks, ~count(.x, Class))) %>% 
  select(!tracks) %>% 
  unnest(Class) %>% 
  drop_na() %>% 
  ggplot(aes(Class, n, color = Site)) +
  geom_boxplot(key_glyph = draw_key_point,
               size = 1,
               position = position_dodge(1)) +
  # stat_summary(fun.data = median_hilow,
  #              position = position_dodge(1),
  #              key_glyph = draw_key_point,
  #              size = 1,
  #              fatten = 3) +
  scale_y_log10(labels = scales::label_number(),
                minor_breaks = sapply(seq(1, 100, length.out = 20), function(x) x * c(1, 100, 10000)),
                breaks = c(1, 100, 10000, 1000000),
                limits = c(1, 10^6)) +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  ggpubr::theme_pubr(legend = "right",
                     base_family = "CMU Serif") +
  theme(aspect.ratio = 1,
        title = element_text(face = "bold"),
        panel.grid.minor = element_line(colour = "gray80", size = .25)) +
  labs(x = NULL, y = "Count", color = "Study Site")

ggsave("classCounts.png", classCounts,
       dpi = 300, width = 4, height = 3, scale = 2)

