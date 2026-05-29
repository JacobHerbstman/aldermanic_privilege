
library(sf)
library(ggplot2)
library(dplyr)

# setwd("tasks/audits/ward_map/code")

wards <- st_read("../input/Wards_2024.geojson", quiet = TRUE) %>%
  mutate(ward = as.integer(ward))

# 50 distinct non-grey colors for an illustrative ward map
set.seed(42)
hues <- seq(15, 375, length.out = 51)[1:50]
palette_50 <- grDevices::hcl(h = hues, c = 75, l = 72)
color_map  <- tibble(ward = 1:50, fill_color = sample(palette_50))

wards <- left_join(wards, color_map, by = "ward", relationship = "many-to-one")
centroids <- st_point_on_surface(wards)

p <- ggplot(wards) +
  geom_sf(aes(fill = fill_color), color = "white", linewidth = 0.3) +
  geom_sf_text(data = centroids, aes(label = ward), size = 2, fontface = "bold",
               color = "gray20", check_overlap = FALSE) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("../output/ward_map_2024.pdf", plot = p, width = 5, height = 6, device = "pdf")
