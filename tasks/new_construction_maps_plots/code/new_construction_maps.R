# This script makes maps of new construction around the city with ward boundaries overlaid 

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# load analysis sample for mapping 
cat("Loading and preparing data...\n")
parcels <- st_read("../input/parcels_with_geometry.gpkg")
ward_panel <- st_read("../input/ward_panel.gpkg")

# Filter parcels for construction years between 2006 and 2014
parcels_2006_2014 <- parcels %>%
  filter(unitscount > 1) %>% 
  filter(construction_year >= 2006 & construction_year <= 2014) 

parcels_2015_2023 <- parcels %>%
  filter(unitscount > 1) %>% 
  filter(construction_year >= 2016 & construction_year <= 2023)

parcels_2023_2025 <- parcels %>%
  filter(unitscount > 1) %>% 
  filter(construction_year >= 2023 & construction_year <= 2025)

# Use the 2014 ward boundaries, which were in effect for the 2006-2014 period
wards_2014 <- ward_panel %>%
  filter(year == 2014) 

wards_2015 <- ward_panel %>%
  filter(year == 2015) 

wards_2025 <- ward_panel %>%
  filter(year == 2025) 

# -----------------------------------------------------------------------------
# 2. CREATE AND SAVE THE MAP
# -----------------------------------------------------------------------------

cat("Generating map of new construction...\n")

# Create the plot with ggplot2
construction_map <- ggplot() +
  # Add the ward boundaries with a white fill and clear, dark gray outlines
  geom_sf(data = wards_2014, fill = "white", color = "grey20") +
  
  # Add the parcel locations, with color mapped to Floor Area Ratio (FAR)
  geom_sf(data = parcels_2006_2014, aes(color = density_far), size = 0.3, alpha = 0.5) +
  
  # Use a capped color scale to handle outliers and show more variation
  scale_color_viridis_c(
    option = "turbo", 
    name = "Floor Area Ratio (FAR)",
    limits = c(0, 2),  # Set the range of the color scale
    oob = scales::squish # "Squish" out-of-bounds values to the limits
  ) +
  
  # Add labels and a clean theme
  labs(
    title = "New Residential Construction in Chicago (2006-2014)",
    subtitle = "Construction locations colored by density (FAR)"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


construction_map

# Save the plot to the output directory
ggsave("../output/construction_map_2003_2014_w_density.pdf", plot = construction_map, width = 8, height = 10, dpi = 300)

cat("Map saved to ../output/construction_map_2003_2014_w_density.pdf\n")



# -----------------------------------------------------------------------------
# 2. CREATE AND SAVE THE MAP
# -----------------------------------------------------------------------------

cat("Generating map of new construction...\n")

# Create the plot with ggplot2
construction_map <- ggplot() +
  # Add the ward boundaries with a white fill and clear, dark gray outlines
  geom_sf(data = wards_2015, fill = "white", color = "gray20") +
  
  # Add the parcel locations, with color mapped to Floor Area Ratio (FAR)
  geom_sf(data = parcels_2015_2023, aes(color = density_far), size = 0.3, alpha = 0.5) +
  
  # Use a capped color scale to handle outliers and show more variation
  scale_color_viridis_c(
    option = "turbo", 
    name = "Floor Area Ratio (FAR)",
    limits = c(0, 2),  # Set the range of the color scale
    oob = scales::squish # "Squish" out-of-bounds values to the limits
  ) +
  
  # Add labels and a clean theme
  labs(
    title = "New Residential Construction in Chicago (2015-2023)",
    subtitle = "Construction locations colored by density (FAR)"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


construction_map

ggsave("../output/construction_map_2016_2023_w_density.pdf", plot = construction_map, width = 8, height = 10, dpi = 300)







# -----------------------------------------------------------------------------
# 2. CREATE AND SAVE THE MAP
# -----------------------------------------------------------------------------

cat("Generating map of new construction...\n")

# Create the plot with ggplot2
construction_map <- ggplot() +
  # Add the ward boundaries with a white fill and clear, dark gray outlines
  geom_sf(data = wards_2025, fill = "white", color = "gray20") +
  
  # Add the parcel locations, with color mapped to Floor Area Ratio (FAR)
  geom_sf(data = parcels_2023_2025, aes(color = density_far), size = 0.3, alpha = 0.5) +
  
  # Use a capped color scale to handle outliers and show more variation
  scale_color_viridis_c(
    option = "turbo", 
    name = "Floor Area Ratio (FAR)",
    limits = c(0, 2),  # Set the range of the color scale
    oob = scales::squish # "Squish" out-of-bounds values to the limits
  ) +
  
  # Add labels and a clean theme
  labs(
    title = "New Residential Construction in Chicago (2015-2023)",
    subtitle = "Construction locations colored by density (FAR)"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


construction_map

ggsave("../output/construction_map_2023_2025_w_density.pdf", plot = construction_map, width = 8, height = 10, dpi = 300)








### discontinuity pair searching 
# 
# 
# to_meters <- function(g) {
#   if (sf::st_is_longlat(g)) sf::st_transform(g, 26971) else g  # NAD83 / Illinois East (m)
# }
# wards_m   <- to_meters(wards_2014)
# parcels_m <- to_meters(parcels_2006_2014)
# 
# # helpful ward id field
# wards_m <- wards_m %>% mutate(ward = as.integer(ward))
# 
# 
# # adjacency (pairs of touching polygons)
# touch_idx <- st_touches(wards_m)
# pairs_df <- map2_dfr(seq_len(nrow(wards_m)), touch_idx, ~{
#   if (length(.y) == 0) return(tibble())
#   tibble(ward_i = wards_m$ward[.x], ward_j = wards_m$ward[.y])
# }) %>%
#   filter(ward_i < ward_j) %>% distinct()
# 
# # shared border (line) geometry for each pair
# get_shared_line <- function(i, j) {
#   wi <- wards_m %>% filter(ward == i) %>% st_boundary()
#   wj <- wards_m %>% filter(ward == j) %>% st_boundary()
#   st_intersection(wi, wj) %>% st_line_merge()
# }
# 
# pairs_geom <- pairs_df %>%
#   mutate(geom = map2(ward_i, ward_j, get_shared_line)) %>%
#   st_as_sf(sf_column_name = "geom", crs = st_crs(wards_m)) %>%
#   filter(st_is(geom, c("LINESTRING","MULTILINESTRING")))
# 
# 
# 
# border_buffer_compare <- function(i, j, buffer_m = 250, min_side_n = 20) {
#   wi <- wards_m %>% filter(ward == i)
#   wj <- wards_m %>% filter(ward == j)
#   line_ij <- get_shared_line(i, j)
#   if (is.null(line_ij) || length(line_ij) == 0) return(NULL)
#   
#   buf <- st_buffer(line_ij, dist = buffer_m)
#   near_parcels <- parcels_m[st_intersects(parcels_m, buf, sparse = FALSE)[,1], , drop = FALSE]
#   if (nrow(near_parcels) == 0) return(NULL)
#   
#   # side assignment by polygon containment
#   near_i <- near_parcels[st_intersects(near_parcels, wi, sparse = FALSE)[,1], , drop = FALSE]
#   near_j <- near_parcels[st_intersects(near_parcels, wj, sparse = FALSE)[,1], , drop = FALSE]
#   
#   if (nrow(near_i) < min_side_n || nrow(near_j) < min_side_n) return(NULL)
#   
#   # pick your outcome(s) — FAR is in your map; add others if present
#   summarize_side <- function(x) {
#     tibble(
#       n = nrow(x),
#       mean_far   = mean(x$density_far, na.rm = TRUE),
#       median_far = median(x$density_far, na.rm = TRUE),
#       share_high = mean(x$density_far >= 1.5, na.rm = TRUE)  # tweak threshold as you like
#     )
#   }
#   
#   si <- summarize_side(near_i)
#   sj <- summarize_side(near_j)
#   
#   tibble(
#     ward_i = i, ward_j = j,
#     n_i = si$n, n_j = sj$n,
#     mean_far_i = si$mean_far, mean_far_j = sj$mean_far,
#     median_far_i = si$median_far, median_far_j = sj$median_far,
#     share_high_i = si$share_high, share_high_j = sj$share_high,
#     diff_mean_far   = si$mean_far   - sj$mean_far,
#     diff_median_far = si$median_far - sj$median_far,
#     diff_share_high = si$share_high - sj$share_high,
#     abs_gap = max(abs(c(
#       si$mean_far   - sj$mean_far,
#       si$median_far - sj$median_far,
#       si$share_high - sj$share_high
#     ))),
#     geom = buf
#   ) %>% st_as_sf(sf_column_name = "geom", crs = st_crs(wards_m))
# }
# 
# candidates_sf <- map2_dfr(pairs_df$ward_i, pairs_df$ward_j, ~border_buffer_compare(.x, .y, buffer_m = 250, min_side_n = 30))
# candidates_sf <- candidates_sf %>% arrange(desc(abs_gap))
# 
# 
# # read ward-month panel (your earlier export) and scores
# panel <- readr::read_csv("../output/chicago_alderman_panel.csv")
# scores <- readr::read_csv("../output/alderman_restrictiveness_scores_month_FEs.csv")
# 
# # average ward strictness over 2003–2014
# library(zoo)
# panel <- panel %>%
#   mutate(month = zoo::as.yearmon(month)) %>%
#   filter(month >= as.yearmon("2003-01") & month <= as.yearmon("2014-12"))
# 
# ward_strictness <- panel %>%
#   left_join(scores, by = "alderman") %>%
#   group_by(ward) %>%
#   summarise(ward_mean_index = mean(strictness_index, na.rm = TRUE),
#             .groups = "drop")
# 
# candidates_ranked <- candidates_sf %>%
#   left_join(ward_strictness %>% rename(ward_i = ward, idx_i = ward_mean_index), by = "ward_i") %>%
#   left_join(ward_strictness %>% rename(ward_j = ward, idx_j = ward_mean_index), by = "ward_j") %>%
#   mutate(idx_gap = idx_i - idx_j,
#          abs_idx_gap = abs(idx_gap)) %>%
#   arrange(desc(abs_gap), desc(abs_idx_gap))
# 
# # write the table for inspection
# readr::write_csv(st_drop_geometry(candidates_ranked), "../output/border_discontinuity_candidates_2003_2014.csv")
# 
# 
# 
# plot_border_pair <- function(i, j, buffer_m = 250) {
#   wi <- wards_m %>% filter(ward == i)
#   wj <- wards_m %>% filter(ward == j)
#   line_ij <- get_shared_line(i, j); buf <- st_buffer(line_ij, dist = buffer_m)
#   
#   pts <- parcels_m[st_intersects(parcels_m, buf, sparse = FALSE)[,1], , drop = FALSE]
#   pts <- pts %>%
#     mutate(side = case_when(
#       st_intersects(., wi, sparse = FALSE)[,1] ~ paste0("Ward ", i),
#       st_intersects(., wj, sparse = FALSE)[,1] ~ paste0("Ward ", j),
#       TRUE ~ "Other"
#     ))
#   
#   ggplot() +
#     geom_sf(data = st_crop(wards_m, st_bbox(buf)), fill = "white", color = "grey50") +
#     geom_sf(data = buf, fill = NA, color = "black", linetype = "dashed") +
#     geom_sf(data = line_ij, color = "black", size = 0.8) +
#     geom_sf(data = pts, aes(color = density_far, shape = side), size = 0.7, alpha = 0.7) +
#     scale_color_viridis_c(option = "turbo", limits = c(0,2), oob = scales::squish, name = "FAR") +
#     labs(title = paste0("Near-border construction, Ward ", i, " vs Ward ", j, " (", buffer_m, " m buffer)"),
#          subtitle = "Points colored by FAR; shapes indicate side of border") +
#     theme_void() + theme(legend.position = "bottom")
# }
# 
# # Example: plot the top-ranked border
# top_pair <- candidates_ranked %>% slice(1)
# print(plot_border_pair(top_pair$ward_i, top_pair$ward_j, buffer_m = 250))
# ggsave("../output/best_border_case_study.pdf",
#        plot = plot_border_pair(top_pair$ward_i, top_pair$ward_j, 250),
#        width = 7, height = 7)
# 
# 


parcels_2023_2025 %>% filter(str_detect(ward_pair, "34"))
