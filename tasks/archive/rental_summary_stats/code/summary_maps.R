# summary_maps.R
# Generates spatial visualizations of the rental data: coverage, price growth, and amenities.

source("../../setup_environment/code/packages.R")

# 1. Load Data
# -----------------------------------------------------------------------------
message("Loading data...")
rent_panel <- read_parquet("../input/chicago_rent_panel.parquet")
wards      <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

# Load BOTH ward maps:
# 2015 Map: For historical analysis (2015-2023)
wards_2015 <- wards %>% filter(year == 2015)
# 2024 Map: For current/recent analysis (2021-2025) to capture the new Ward 34 location
wards_2024 <- wards %>% filter(year == 2024)

# Create a City Outline (Union of 2015 wards is sufficient for clipping)
city_outline <- st_union(wards_2015)

# 2. Clean & Prepare Rental Data
# -----------------------------------------------------------------------------
# Strict cleaning + conversion to SF (Projected to 2015 CRS initially)
rent_sf <- rent_panel %>%
  filter(
    !is.na(longitude), !is.na(latitude),
    rent_price > 200, rent_price < 20000
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(wards_2015)) 

# CRITICAL STEP: Spatial filter
# Only keep points strictly inside the city boundary
message("Clipping points to city boundary...")
rent_city <- rent_sf[city_outline, ]

# Extract projected coordinates for stat_bin_2d
coords <- st_coordinates(rent_city)
rent_city$X_proj <- coords[,1]
rent_city$Y_proj <- coords[,2]

# 3. Map 1: Spatial Coverage Heatmap
# -----------------------------------------------------------------------------
message("Generating Coverage Map...")

p1 <- ggplot() +
  # 1. The Heatmap (Bottom Layer)
  stat_bin_2d(
    data = rent_city,
    aes(x = X_proj, y = Y_proj),
    bins = 100
  ) +
  
  # 2. Ward Boundaries (Top Layer)
  geom_sf(data = wards_2015, fill = NA, color = "white", size = 0.1, alpha = 0.5) +
  
  # 3. City Outline (Thicker border)
  geom_sf(data = city_outline, fill = NA, color = "black", size = 0.3) +
  
  # Styling
  scale_fill_viridis_c(
    option = "magma",
    trans = "log",
    name = "Listings\n(Log Scale)",
    labels = scales::comma,
    guide = guide_colorbar(barheight = 10)
  ) +
  theme_void() +
  labs(
    title = "Spatial Coverage of Rental Listings",
    subtitle = paste0("Density of ", scales::comma(nrow(rent_city)), " listings (2014-2025)"),
    caption = "Clipped to Chicago Ward Boundaries (2015 Map)"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("../output/map_rental_coverage_density.pdf", p1, width = 8, height = 10, bg = "white")


# 4. OPTIMIZED SPATIAL JOIN (2015 Boundaries)
# -----------------------------------------------------------------------------
message("Preparing optimized spatial join (2015 Map)...")

# A. Filter Data for Historical Maps (Map 2 & 3)
target_years_hist <- c(2015, 2022, 2023)

rent_subset_hist <- rent_city %>%
  mutate(year = year(file_date)) %>%
  filter(year %in% target_years_hist) %>%
  select(rent_price, year, gym)

# B. Simplify & Join
wards_simple_15 <- st_simplify(wards_2015, dTolerance = 10)
rent_in_wards_15 <- st_join(rent_subset_hist, wards_simple_15["ward"], join = st_intersects)

# 5. Map 2: Rent Growth by Ward (2015 vs 2023) -- Uses 2015 Map
# -----------------------------------------------------------------------------
message("Generating Rent Growth Map (2015-2023)...")

ward_rent_stats_15_23 <- rent_in_wards_15 %>%
  st_drop_geometry() %>%
  filter(year %in% c(2015, 2023)) %>%
  group_by(ward, year) %>%
  summarise(median_rent = median(rent_price, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = median_rent, names_prefix = "y") %>%
  mutate(pct_growth = (y2023 - y2015) / y2015)

map_growth_data_15_23 <- wards_2015 %>%
  left_join(ward_rent_stats_15_23, by = "ward")

p2 <- ggplot(map_growth_data_15_23) +
  geom_sf(aes(fill = pct_growth), color = "white", size = 0.2) +
  scale_fill_distiller(
    palette = "RdYlBu", 
    direction = -1, 
    labels = scales::percent,
    name = "% Growth\n(2015-2023)"
  ) +
  theme_void() +
  labs(
    title = "Rent Growth by Ward (2015 vs 2023)",
    subtitle = "Change in median listing price",
    caption = "Map: 2015 Ward Boundaries"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("../output/map_rent_growth_ward.pdf", p2, width = 8, height = 8, bg = "white")


# 6. Map 3: The 'Luxury' Frontier -- Uses 2015 Map
# -----------------------------------------------------------------------------
message("Generating Amenities Map...")

ward_amenities <- rent_in_wards_15 %>%
  st_drop_geometry() %>%
  filter(year >= 2022) %>%
  group_by(ward) %>%
  summarise(share_gym = mean(gym, na.rm = TRUE))

map_amenity_data <- wards_2015 %>%
  left_join(ward_amenities, by = "ward")

p3 <- ggplot(map_amenity_data) +
  geom_sf(aes(fill = share_gym), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "cividis", 
    labels = scales::percent,
    name = "% Listings\nw/ Gym"
  ) +
  theme_void() +
  labs(
    title = "The 'Luxury' Frontier",
    subtitle = "Share of listings with a Gym (2022-2023)",
    caption = "Map: 2015 Ward Boundaries"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("../output/map_luxury_amenities_ward.pdf", p3, width = 8, height = 8, bg = "white")


# 7. Map 4: Recent Rent Growth (2021 vs 2025) -- Uses NEW 2024 Map
# -----------------------------------------------------------------------------
message("Generating Rent Growth Map (2021-2025) with NEW Ward Boundaries...")

# A. Prepare Data
rent_subset_recent <- rent_city %>%
  mutate(year = year(file_date)) %>%
  filter(year %in% c(2021, 2025)) %>%
  select(rent_price, year)

# B. Transform to 2024 CRS (if different) and Simplify 2024 Wards
# Even if CRS matches, good practice to ensure alignment
wards_simple_24 <- st_simplify(wards_2024, dTolerance = 10)
rent_subset_recent <- st_transform(rent_subset_recent, st_crs(wards_simple_24))

# C. Spatial Join with NEW Wards
rent_in_wards_24 <- st_join(rent_subset_recent, wards_simple_24["ward"], join = st_intersects)

# D. Aggregate
ward_rent_stats_21_25 <- rent_in_wards_24 %>%
  st_drop_geometry() %>%
  group_by(ward, year) %>%
  summarise(
    median_rent = median(rent_price, na.rm = TRUE),
    n_listings = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year, 
    values_from = c(median_rent, n_listings),
    names_glue = "{.value}_{year}"
  ) %>%
  mutate(
    pct_growth = (median_rent_2025 - median_rent_2021) / median_rent_2021
  ) 

# Save diagnostics for the new map
write_csv(ward_rent_stats_21_25, "../output/ward_rent_growth_diagnostics_2024map.csv")

# E. Map
map_growth_data_21_25 <- wards_2024 %>%
  left_join(ward_rent_stats_21_25, by = "ward")

p4 <- ggplot(map_growth_data_21_25) +
  geom_sf(aes(fill = pct_growth), color = "white", size = 0.2) +
  scale_fill_distiller(
    palette = "RdYlBu", 
    direction = -1, 
    labels = scales::percent,
    name = "% Growth\n(2021-2025)"
  ) +
  theme_void() +
  labs(
    title = "Recent Rent Growth (2021 vs 2025)",
    subtitle = "Change in median listing price",
    caption = "Map: 2024 Ward Boundaries (Current)"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("../output/map_rent_growth_ward_2021_2025.pdf", p4, width = 8, height = 8, bg = "white")

message("All maps saved.")