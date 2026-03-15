# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/ward_map/code")
# Rscript ward_map.R ../input/Wards_2024.geojson ../output/ward_map_2024.pdf

library(sf)
library(ggplot2)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)

wards <- st_read(args[1], quiet = TRUE) %>%
  mutate(ward = as.integer(ward))

# 50 distinct colors cycling through a qualitative palette
set.seed(42)
palette_50 <- rep(RColorBrewer::brewer.pal(10, "Set3"), 5)[1:50]
color_map  <- tibble(ward = 1:50, fill_color = sample(palette_50))

wards <- left_join(wards, color_map, by = "ward")
centroids <- st_centroid(wards)

p <- ggplot(wards) +
  geom_sf(aes(fill = fill_color), color = "white", linewidth = 0.3) +
  geom_sf_text(data = centroids, aes(label = ward), size = 2, fontface = "bold",
               color = "gray20", check_overlap = FALSE) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave(args[2], plot = p, width = 5, height = 6, device = "pdf")
