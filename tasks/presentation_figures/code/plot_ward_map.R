# setwd("tasks/presentation_figures/code")
# Slide map of Chicago's 50 wards on the 2024 ward map, each ward in its own color and labeled with its number.
source("../../setup_environment/code/packages.R")

wards <- st_read("../input/Wards_2024.geojson", quiet = TRUE) |>
  st_transform(3435) |>
  mutate(ward = as.integer(ward))
stopifnot(nrow(wards) == 50, setequal(wards$ward, 1:50))
labels <- st_point_on_surface(wards)

plot <- ggplot(wards) +
  geom_sf(aes(fill = factor(ward)), color = "white", linewidth = 0.3) +
  geom_sf_text(data = labels, aes(label = ward), size = 1.8, color = "gray20") +
  # A fixed shuffle of the palette (17 and 50 share no factor), so consecutive ward numbers get distinct colors.
  scale_fill_manual(values = grDevices::hcl.colors(50, "Set 2")[(1:50 * 17) %% 50 + 1], guide = "none") +
  theme_void()
ggsave("../output/ward_map_2024.pdf", plot, width = 5, height = 6, bg = "white")
