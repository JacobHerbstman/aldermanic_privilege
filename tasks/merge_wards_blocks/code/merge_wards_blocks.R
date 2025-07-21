## this code takes chicago ward shapefiles from the uchicago justice project and merges them for a panel with census blocks from 2010-2019
## (https://img.shields.io/github/repo-size/uchicago-justice-project/data)

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

## bring in ward boundary data
ward_bound2005 <- st_read("../input/CHI_2005/CHI_2005.shp")

## make panel from 2005-2014
ward_bound2005 <- ward_bound2005 %>%
  mutate(start_year = 2005, end_year = 2014)
ward_bound2005 <- ward_bound2005 %>%
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>% 
  dplyr::rename(ward = ward2005) %>% 
  dplyr::select(year, ward, geometry)


ward_bound2015 <- st_read("../input/CHI_2015/CHI_2015.shp")
## make panel from 2015-2024
ward_bound2015 <- ward_bound2015 %>%
  mutate(start_year = 2015, end_year = 2024)
ward_bound2015 <- ward_bound2015 %>%
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>% 
  dplyr::rename(ward = ward2015) %>% 
  dplyr::select(year, ward, geometry)

##join to one large panel
ward_panel <- rbind(ward_bound2005, ward_bound2015)

# write just ward panel in case I want it later
st_write(ward_panel, "../output/ward_panel.shp", append = F, delete_dsn = T)





##bring in census block and harmonize with ward boundaries
census_blocks <- read_csv("../input/census_blocks_2010.csv") %>% 
  rename(geometry = the_geom)
census_blocks <- st_as_sf(census_blocks, wkt = "geometry", crs = 4269) %>% 
  st_transform(st_crs(ward_panel))

## create yearly panel of census blocks and their geometries
census_blocks <- census_blocks %>%
  rename(block_id = GEOID10) %>% 
  mutate(start_year = 2010, end_year = 2019) %>% 
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>%
  mutate(block_id = as.character(block_id)) %>% 
  dplyr::select(year, block_id, geometry) %>% 
  arrange(block_id, year) 



# --- Merge Panels using Centroid Method ---

# 1. Split both panel datasets into a list of data frames, one for each year.
## keep just ward panel years 2010-2019
ward_panel <- ward_panel %>%
  filter(year >= 2010 & year <= 2019) %>%
  arrange(year, ward)

blocks_by_year <- group_split(census_blocks, year)
wards_by_year  <- group_split(ward_panel, year)


joined_panel <- map2_dfr(
  blocks_by_year,
  wards_by_year,
  ~ st_join(
    st_centroid(.x), # Use the centroid of the blocks for the join
    .y,              # The ward polygons
    join = st_within
  ) %>%
    # CORRECTED: Select year.x and rename it back to year.
    select(
      year = year.x,
      block_id,
      ward
    )
) %>% 
  arrange(block_id, year) 

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

##write to shapefile
st_write(joined_panel_switch,"../output/census_blocks_ward_switchers.shp", append = F, delete_dsn = TRUE)



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




