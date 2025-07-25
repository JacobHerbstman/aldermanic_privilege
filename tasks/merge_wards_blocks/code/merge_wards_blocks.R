## this code takes chicago ward shapefiles from the uchicago justice project and merges them for a panel with census blocks from 2010-2019
## (https://img.shields.io/github/repo-size/uchicago-justice-project/data)

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

## bring in old ward boundary data
ward_bound2005 <- st_read("../input/CHI_2005/CHI_2005.shp")

## make monthly panel for the pre-2015 ward map
# Define start and end dates in yearmon format
start_date_old <- as.yearmon("Jan 2005")
end_date_old   <- as.yearmon("Apr 2015") # Ends the month before the cutoff

ward_bound2005 <- ward_bound2005 %>%
  rowwise() %>%
  mutate(month = list(seq(from = start_date_old, to = end_date_old, by = 1/12))) %>%
  unnest(month) %>%
  rename(ward = ward2005) %>%
  select(month, ward, geometry)


## bring in new ward boundary data
ward_bound2015 <- st_read("../input/CHI_2015/CHI_2015.shp")

## make monthly panel for the post-2015 ward map
# Define start and end dates in yearmon format
start_date_new <- as.yearmon("May 2015") # Starts at the cutoff
end_date_new   <- as.yearmon("Dec 2024") # Goes to end of original panel period

ward_bound2015 <- ward_bound2015 %>%
  rowwise() %>%
  mutate(month = list(seq(from = start_date_new, to = end_date_new, by = 1/12))) %>%
  unnest(month) %>%
  rename(ward = ward2015) %>%
  select(month, ward, geometry)

## join to one large monthly panel
ward_panel_monthly <- rbind(ward_bound2005, ward_bound2015) %>% 
  arrange(ward, month)

# To save the file correctly, convert the 'yearmon' column to a standard Date
# and write to a modern GeoPackage (.gpkg) file.
ward_panel_to_write <- ward_panel_monthly %>%
  mutate(date = as.Date(month)) %>% # Represents each month as the first day
  select(-month)                   # Remove the original yearmon column before saving

st_write(ward_panel_to_write, "../output/ward_panel_monthly.gpkg", delete_layer = TRUE)




##bring in census block and harmonize with ward boundaries
census_blocks <- read_csv("../input/census_blocks_2010.csv") %>% 
  rename(geometry = the_geom)
census_blocks <- st_as_sf(census_blocks, wkt = "geometry", crs = 4269) %>% 
  st_transform(st_crs(ward_panel_monthly))

# Create a monthly panel of census blocks and their geometries
start_date <- as.yearmon("Jan 2010")
end_date   <- as.yearmon("Dec 2019")

## create yearly panel of census blocks and their geometries
census_blocks_monthly <- census_blocks %>%
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id)) %>%
  rowwise() %>%
  mutate(month = list(seq(from = start_date, to = end_date, by = 1/12))) %>%
  unnest(month) %>%
  select(month, block_id, geometry) %>%
  arrange(block_id, month)


# --- Merge Panels using Centroid Method ---

# 1. Split both panel datasets into a list of data frames, one for each year.
## keep just ward panel years 2010-2019
ward_panel <- ward_panel_monthly %>%
  filter(month >= as.yearmon("Jan 2010") & month <= as.yearmon("Dec 2019")) %>%
  arrange(ward, month)

blocks_by_month <- group_split(census_blocks_monthly, month)
wards_by_month  <- group_split(ward_panel, month)

joined_panel <- map2_dfr(
  blocks_by_month,
  wards_by_month,
  ~ st_join(
    st_centroid(.x), # Use the centroid of the blocks for the join
    .y,              # The ward polygons
    join = st_within
  ) %>%
    select(
      month = month.x, # Select and rename the month column
      block_id,
      ward
    )
) %>%
  arrange(block_id, month)


joined_panel_switch <- joined_panel %>%
  # arrange by the grouping and time variables
  arrange(block_id, year) %>%
  # Group by each individual census block
  group_by(block_id) %>%
  mutate(
    # Find the very first year a switch occurs for this block.
    # If a block never switches, this will result in 'Inf' (infinity).
    first_switch_year = min(year[ward != lag(ward, default = first(ward))], na.rm = TRUE),
    # Create the indicator:
    ward_switch = as.integer(year >= first_switch_year)
  ) %>%
  ungroup() %>%
  select(-first_switch_year) #remove the helper column

joined_panel_switch <- joined_panel %>%
  arrange(block_id, month) %>%
  group_by(block_id) %>%
  mutate(
    # Find the first month a switch occurs.
    first_switch_month = min(month[ward != lag(ward, default = first(ward))], na.rm = TRUE),
    # Create the indicator
    ward_switch = as.integer(month >= first_switch_month)
  ) %>%
  ungroup() %>%
  select(-first_switch_month) # Remove the helper column

joined_panel_to_write <- joined_panel_switch %>%
  mutate(month = as.Date(month))

##write to shapefile
st_write(joined_panel_switch,"../output/census_blocks_ward_switchers.gpkg", delete_dsn = TRUE)



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




