## this code takes chicago ward shapefiles from the uchicago justice project and merges them for a panel with census blocks from 2010-2019
## (https://img.shields.io/github/repo-size/uchicago-justice-project/data)

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# -----------------------------------------------------------------------------
# 1. CREATE ANNUAL WARD PANEL
# -----------------------------------------------------------------------------

## bring in old ward boundary data
ward_bound2005 <- st_read("../input/CHI_2005/CHI_2005.shp")

## make annual panel for the pre-2015 ward map
ward_bound2005 <- ward_bound2005 %>%
  rowwise() %>%
  mutate(year = list(2005:2014)) %>% # Switched to annual sequence
  unnest(year) %>%
  rename(ward = ward2005) %>%
  select(year, ward, geometry)

## bring in new ward boundary data
ward_bound2015 <- st_read("../input/CHI_2015/CHI_2015.shp")

## make annual panel for the post-2015 ward map
ward_bound2015 <- ward_bound2015 %>%
  rowwise() %>%
  mutate(year = list(2015:2024)) %>% # Switched to annual sequence
  unnest(year) %>%
  rename(ward = ward2015) %>%
  select(year, ward, geometry)

## join to one large annual panel
ward_panel_annual <- rbind(ward_bound2005, ward_bound2015) %>%
  arrange(ward, year)

# Save the annual ward panel
st_write(ward_panel_annual, "../output/ward_panel.gpkg", delete_layer = TRUE)


# -----------------------------------------------------------------------------
# 2. PREPARE CROSS-SECTIONAL CENSUS BLOCK DATA
# -----------------------------------------------------------------------------

## bring in census block data and harmonize CRS
census_blocks <- read_csv("../input/census_blocks_2010.csv") %>%
  rename(geometry = the_geom)

census_blocks <- st_as_sf(census_blocks, wkt = "geometry", crs = 4269) %>%
  st_transform(st_crs(ward_panel_annual)) %>% # Use the new annual panel for CRS
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id))

## filter ward panel to the desired 2010-2019 period
ward_panel <- ward_panel_annual %>%
  filter(year >= 2010 & year <= 2019) %>%
  arrange(ward, year)


# -----------------------------------------------------------------------------
# 3. EFFICIENTLY MERGE BLOCKS AND WARDS
# -----------------------------------------------------------------------------

# Create two simple cross-sections of the ward maps
ward_map_pre_2015 <- ward_panel %>%
  filter(year == 2014) %>% # Use a representative year
  select(ward, geometry)

ward_map_post_2015 <- ward_panel %>%
  filter(year == 2015) %>% # Use a representative year
  select(ward, geometry)

block_centroids <- st_centroid(census_blocks)

# For each block, find the index of the NEAREST pre- and post-2015 ward
nearest_pre_idx <- st_nearest_feature(block_centroids, ward_map_pre_2015)
nearest_post_idx <- st_nearest_feature(block_centroids, ward_map_post_2015)

blocks_cross_section <- tibble(
  block_id = census_blocks$block_id,
  ward_pre = ward_map_pre_2015$ward[nearest_pre_idx],
  ward_post = ward_map_post_2015$ward[nearest_post_idx]
)

# Expand this small data frame into a full ANNUAL panel.
start_year <- 2010
end_year   <- 2019

joined_panel <- blocks_cross_section %>%
  mutate(year = list(start_year:end_year)) %>%
  unnest(year) %>%
  mutate(
    ward = ifelse(year < 2015, ward_pre, ward_post)
  ) %>%
  select(year, block_id, ward) %>%
  arrange(block_id, year)

# -----------------------------------------------------------------------------
# 4. CALCULATE SWITCH INDICATOR AND SAVE
# -----------------------------------------------------------------------------

joined_panel_switch <- joined_panel %>%
  arrange(block_id, year) %>%
  group_by(block_id) %>%
  mutate(
    first_switch_year = min(year[ward != lag(ward, default = first(ward))], na.rm = TRUE),
    ward_switch = as.integer(year >= first_switch_year)
  ) %>%
  ungroup() %>%
  select(-first_switch_year)

# Write to GeoPackage (no date conversion is needed for integer years)
st_write(joined_panel_switch, "../output/census_blocks_ward_switchers.gpkg", delete_dsn = TRUE)

## see which blocks switched
# switching_blocks <- joined_panel %>%
#   st_drop_geometry() %>%
#   group_by(block_id) %>%
#   summarise(
#     number_of_wards = n_distinct(ward),
#     wards_assigned = paste(sort(unique(ward)), collapse = " -> ")
#   ) %>%
#   filter(number_of_wards > 1)






## map 
  # ggplot() +
  # # Add ward boundaries with dark black lines and blank interiors
  # geom_sf(data = filter(joined_panel, year == 2015),
  #         fill = NA,
  #         color = scales::alpha("blue", 0.4), # Lower alpha value for more transparency
  #         size = 0.1) +
  # 
  # geom_sf(data = filter(ward_panel, year == 2015), fill = NA, color = scales::alpha("black", 0.8), size = 1) +
  # 
  # # Add census tract boundaries with blue lines and blank interiors
  # 
  # # Customize the theme
  # theme_minimal() +
  # 
  # # Add labels for clarity
  # labs(
  #   title = "Chicago Wards and Census Tracts",
  #   subtitle = "Wards outlined in black, Census Blocks in blue",
  #   x = NULL, y = NULL
  # )
# 
# ggsave(paste0(root, "merge_tracts_wards/output/wards_tracts_overlay.pdf"))




