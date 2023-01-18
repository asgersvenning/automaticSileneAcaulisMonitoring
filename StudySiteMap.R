library(sf)
library(rnaturalearth)
library(ggforce)
source("helpers.R")

map <- ne_countries(10, returnclass = "sf") %>% 
  mutate(ymean = map_dbl(geometry, function(x) mean(as.matrix(x)[,2])),
         xmean = map_dbl(geometry, function(x) mean(as.matrix(x)[,1]))) %>% 
  filter(ymean > 40 & between(xmean, -90, 30)) %>% 
  st_transform(crs = st_crs("ESRI:53032"))

SiteLoc <- sapply(c("NARS","BJOR","ELLA","KOBB"), function(x) c(longitude(x), latitude(x))) %>% 
  t %>% 
  set_colnames(c("Lon", "Lat")) %>% 
  as.data.frame %>% 
  rownames_to_column(var = "Location") %>% 
  as_tibble %>%
  mutate(RealName = c("Narsasauq", "Bjørndalen", "Ella Island", "Kobbefjord")) %>% 
  st_as_sf(coords = c("Lat", "Lon"),
           crs = st_crs(4326)) %>% 
  st_transform(crs = st_crs("ESRI:53032"))

studyMap <- ggplot() +
  geom_sf(data = map,
          fill = "gray45",
          color = "black",
          size = .2) +
  stat_sf_coordinates(data = SiteLoc,
                   aes(fill = Location, label = RealName),
          geom = GeomMarkCircle,
          expand = unit(2, "mm"),
          n = 200,
          label.fill = "#FFFFFF55",
          alpha = 1,
          na.rm = F,
          label.fontsize = 14,
          key_glyph = draw_key_point) +
  scale_fill_brewer(palette = "Dark2") +
  ggpubr::theme_pubr(legend = "right",
                     base_size = 12,
                     base_family = "CMU Serif") +
  guides(fill = guide_legend(override.aes = list(shape = 21,
                                                  size = 5,
                                                  color = "#00000000"))) +
  scale_x_continuous(breaks = seq(-170, 170, 10)) +
  scale_y_continuous(breaks = seq(-170, 170, 5)) +
  coord_sf(xlim = c(-90, 20), ylim = c(57, 85),
           expand = F,
           default_crs = st_crs(4236),
           default = F) +
  theme(legend.position = c(0.85, 0.2),
        title = element_text(face = "bold", size = 18, hjust = .5),
        panel.background = element_rect(fill = colorspace::lighten("royalblue3", .5)),
        panel.grid.major = element_line(colour = "gray50", size = .5, linetype = "dashed"),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.background = element_rect(fill = "transparent", color = "transparent")) +
  labs(x = NULL, y = NULL, fill = "Study\nLocation") +
  theme(aspect.ratio = 1)

ggsave("Figures/studyMap.png", studyMap,
       dpi = 400, width = 4, height = 4, scale = 2)
