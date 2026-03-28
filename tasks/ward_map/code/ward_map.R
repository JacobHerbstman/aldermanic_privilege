
library(sf)
library(ggplot2)
library(dplyr)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/ward_map/code")
# wards_input <- "../input/Wards_2024.geojson"
# ward_map_output <- "../output/ward_map_2024.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(wards_input, ward_map_output)
}


wards <- st_read(args[1], quiet = TRUE) %>%
  mutate(ward = as.integer(ward))

# 50 distinct non-grey colors for an illustrative ward map
set.seed(42)
hues <- seq(15, 375, length.out = 51)[1:50]
palette_50 <- grDevices::hcl(h = hues, c = 75, l = 72)
color_map  <- tibble(ward = 1:50, fill_color = sample(palette_50))

wards <- left_join(wards, color_map, by = "ward")
centroids <- st_point_on_surface(wards)

p <- ggplot(wards) +
  geom_sf(aes(fill = fill_color), color = "white", linewidth = 0.3) +
  geom_sf_text(data = centroids, aes(label = ward), size = 2, fontface = "bold",
               color = "gray20", check_overlap = FALSE) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave(args[2], plot = p, width = 5, height = 6, device = "pdf")
