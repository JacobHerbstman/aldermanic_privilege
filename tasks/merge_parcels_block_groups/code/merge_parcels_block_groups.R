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
census_block_groups <- census_blocks %>%
  rename(block_id = GEOID10) %>% 
  mutate(block_id = as.character(block_id)) %>% 
  mutate(
    block_group_id = substr(block_id, 1, 12)
  ) %>%
  group_by(block_group_id) %>% 
  summarise(
    geometry = st_union(geometry)
  )


## bring in parcels built after 2003
parcels <- st_read("../input/year_built_sample.shp") %>% 
  arrange(attom_id, taxyear) %>% 
  mutate(FAR = sa_sqft / sa_lotsize)


if (st_crs(parcels) != st_crs(census_block_groups)) {
  message("CRS mismatch detected. Transforming parcels CRS to match census_block groups.")
  parcels <- st_transform(parcels, st_crs(census_block_groups))
}
## join parcels to blocks
parcels_with_block_id <- st_join(parcels, census_block_groups, join = st_within)

## aggregate to block level since thiis is the unit of analysis
block_group_level_aggregates <- parcels_with_block_id %>%
  # Drop geometry for much faster non-spatial aggregation
  st_drop_geometry() %>%
  # Group by the block identifier and the year
  group_by(block_group_id, sa_yr_blt) %>%
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
  arrange(block_group_id, year)


## bring in panel of ward switches at the block level to merge in treatment variable
joined_panel_switch <- st_read("../input/census_block_groups_ward_switchers.shp") %>% 
  rename(block_group_id = blck_g_, 
         ward_switch = wrd_swt, 
         area_proportion = ar_prpr)  ## fix names since they get messed up by the .shp file format

final_regression_panel <- left_join(
  joined_panel_switch,
  block_group_level_aggregates,
  by = c("block_group_id", "year")
) %>% 
  st_drop_geometry()

# Save the final regression panel
write_csv(final_regression_panel, "../output/attom_regression_panel_block_groups.csv")




## effective sample size 

# 1. Get a list of all unique block_ids that are ever treated
switching_block_ids <- final_regression_panel %>%
  filter(ward_switch == 1) %>%
  distinct(block_group_id) %>%
  pull(block_group_id) # pull() extracts the column into a simple vector

# 2. Filter the main panel to just these treated blocks and run the diagnostic
effective_sample_summary <- final_regression_panel %>%
  filter(!is.na(avg_sqft)) %>%
  filter(block_group_id %in% switching_block_ids) %>%
  group_by(block_group_id) %>%
  summarise(
    has_pre_period_data = any(year < 2015, na.rm = TRUE),
    has_post_period_data = any(year >= 2015, na.rm = TRUE)
  ) %>%
  # Keep only blocks that have data on both sides of the switch
  filter(has_pre_period_data & has_post_period_data) %>% 
  pull(block_group_id)

n_useful_blocks <- nrow(effective_sample_summary)
print(paste("Number of switching blocks you can actually use for identification:", n_useful_blocks))


effective_sample <- final_regression_panel %>%
  filter(!is.na(avg_sqft)) %>%
  filter(block_group_id %in% effective_sample_summary) %>%
  group_by(block_group_id) %>% 
  arrange(block_group_id, year)

test <- lm(avg_lotsize ~ ward_switch, data = effective_sample)
summary(test)




