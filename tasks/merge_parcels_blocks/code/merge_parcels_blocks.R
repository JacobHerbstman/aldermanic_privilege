## this code takes chicago ward shapefiles from the uchicago justice project and merges them for a panel with census blocks from 2010-2019
## (https://img.shields.io/github/repo-size/uchicago-justice-project/data)

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

ward_panel <- st_read("../input/ward_panel.shp")

##bring in census block and harmonize with ward boundaries
census_blocks <- read_csv("../input/census_blocks_2010.csv") %>% 
  rename(geometry = the_geom)
census_blocks <- st_as_sf(census_blocks, wkt = "geometry", crs = 4269) %>% 
  st_transform(st_crs(ward_panel))

## create yearly panel of census blocks and their geometries
census_blocks <- census_blocks %>%
  rename(block_id = GEOID10) %>% 
  mutate(block_id = as.character(block_id)) %>% 
  dplyr::select(block_id, geometry)


## bring in parcels built after 2003
parcels <- st_read("../input/year_built_sample.shp") %>% 
  arrange(attom_id, taxyear) %>% 
  mutate(FAR = sa_sqft / sa_lotsize)


if (st_crs(parcels) != st_crs(census_blocks)) {
  message("CRS mismatch detected. Transforming parcels CRS to match census_blocks.")
  parcels <- st_transform(parcels, st_crs(census_blocks))
}
## join parcels to blocks
parcels_with_block_id <- st_join(parcels, census_blocks, join = st_within)

## aggregate to block level since thiis is the unit of analysis
block_level_aggregates <- parcels_with_block_id %>%
  # Drop geometry for much faster non-spatial aggregation
  st_drop_geometry() %>%
  # Group by the block identifier and the year
  group_by(block_id, sa_yr_blt) %>%
  # Calculate summary statistics for each block-year
  summarise(
    n_parcels = n(),
    avg_sqft = mean(sa_sqft, na.rm = TRUE),
    avg_lotsize = mean(sa_lotsize, na.rm = TRUE),
    mean_FAR = mean(FAR, na.rm = TRUE),
    # Add any other aggregations you need here
    .groups = 'drop' # avoid downstream grouping issues
  ) %>%
  # Rename taxyear to year for merging with your main panel
  rename(year = sa_yr_blt) %>% 
  arrange(block_id, year)


## bring in panel of ward switches at the block level to merge in treatment variable
joined_panel_switch <- st_read("../input/census_blocks_ward_switchers.shp") %>% 
  rename(block_id = block_d, 
         ward_switch = wrd_swt)  ## fix names since they get messed up by the .shp file format

final_regression_panel <- left_join(
  joined_panel_switch,
  block_level_aggregates,
  by = c("block_id", "year")
) %>% 
  st_drop_geometry()

# Save the final regression panel
write_csv(final_regression_panel, "../output/final_regression_panel.csv")











