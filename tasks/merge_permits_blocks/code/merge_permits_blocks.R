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
  group_by(block_id) 



## bring in parcels built after 2003
permits <- st_read("../input/building_permits_clean.gpkg") %>% 
  mutate(across(c(application_start_date_ym, issue_date_ym), 
                function(x) as.yearmon(x)))


if (st_crs(permits) != st_crs(census_blocks)) {
  message("CRS mismatch detected. Transforming parcels CRS to match census_block groups.")
  permits <- st_transform(permits, st_crs(census_blocks))
}
## join permits to blocks
permits_with_block_id <- st_join(permits, census_blocks, join = st_within)

## aggregate to block level since thiis is the unit of analysis
block_level_aggregates <- permits_with_block_id %>%
  # Drop geometry for much faster non-spatial aggregation
  st_drop_geometry() %>%
  # Group by the block identifier and the year
  group_by(block_id, application_start_date_ym) %>%
  # Calculate summary statistics for each block-year
  summarise(
    n_permits = n(),
    avg_processing_time = mean(processing_time, na.rm = TRUE),
    avg_reported_cost = mean(reported_cost, na.rm = TRUE),
    avg_total_fee = mean(total_fee, na.rm = TRUE),
    # Add any other aggregations you need here
    .groups = 'drop' # avoid downstream grouping issues
  ) %>%
  # Rename taxyear to year for merging with your main panel
  arrange(block_id, application_start_date_ym) %>% 
  rename(month = application_start_date_ym)


## bring in panel of ward switches at the block level to merge in treatment variable
joined_panel_switch <- st_read("../input/census_blocks_ward_switchers.gpkg") 

final_regression_panel <- left_join(
  joined_panel_switch,
  block_level_aggregates,
  by = c("block_id", "month")
) %>% 
  st_drop_geometry()

# Save the final regression panel
write_csv(final_regression_panel, "../output/final_regression_panel.csv")




## effective sample size 

# 1. Get a list of all unique block_ids that are ever treated
switching_block_ids <- final_regression_panel %>%
  filter(ward_switch == 1) %>%
  distinct(block_id) %>%
  pull(block_id) # pull() extracts the column into a simple vector

# 2. Filter the main panel to just these treated blocks and run the diagnostic
effective_sample_summary <- final_regression_panel %>%
  filter(!is.na(avg_sqft)) %>% 
  filter(block_id %in% switching_block_ids) %>%
  group_by(block_id) %>%
  summarise(
    has_pre_period_data = any(year < 2015, na.rm = TRUE),
    has_post_period_data = any(year >= 2015, na.rm = TRUE)
  ) %>%
  # Keep only blocks that have data on both sides of the switch
  filter(has_pre_period_data & has_post_period_data) 

n_useful_blocks <- nrow(effective_sample_summary)

print(paste("Number of switching blocks you can actually use for identification:", n_useful_blocks))




