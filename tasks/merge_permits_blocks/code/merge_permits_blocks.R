## this code takes chicago ward shapefiles from the uchicago justice project and merges them for a panel with census blocks from 2010-2019
## (https://img.shields.io/github/repo-size/uchicago-justice-project/data)

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

ward_panel <- st_read("../input/ward_panel.gpkg")

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
  mutate(application_start_date = year(application_start_date_ym)) %>% 
  mutate(issue_date = year(issue_date_ym))

if (st_crs(permits) != st_crs(census_blocks)) {
  message("CRS mismatch detected. Transforming parcels CRS to match census_block groups.")
  permits <- st_transform(permits, st_crs(census_blocks))
}

################################################
###### filter to HIGH DISCRETION_PERMITS_ONLY
permits_high_discretion <- permits %>% 
  filter(high_discretion == 1)
##############################################

## join permits to blocks
permits_with_block_id <- st_join(permits_high_discretion, census_blocks, join = st_within)

## 16 duplicates, they look fine and normal so I am keeping them and just dropping the duplicated version
permits_with_block_id <- permits_with_block_id %>%
  distinct(id, .keep_all = TRUE)


## aggregate to block level since this is the unit of analysis
block_level_aggregates <- permits_with_block_id %>%
  # Drop geometry for much faster non-spatial aggregation
  st_drop_geometry() %>%
  # Group by the block identifier and the year
  group_by(block_id, application_start_date) %>%
  # Calculate summary statistics for each block-year
  summarise(
    # Original variables
    n_permits = n(),
    avg_processing_time = mean(processing_time, na.rm = TRUE),
    avg_reported_cost = mean(reported_cost, na.rm = TRUE),
    avg_total_fee = mean(total_fee, na.rm = TRUE),
    
    # --- Added Variables ---
    
    # Proportions for your binary (0/1) dummy variables
    prop_permit_issued = mean(permit_issued, na.rm = TRUE),
    prop_corporate_applicant = mean(corporate_applicant, na.rm = TRUE),
    # Waived fees indicator
    prop_had_zoning_fee_waiver = mean(zoning_fee_waived > 0, na.rm = TRUE),
    prop_had_other_fee_waiver = mean(other_fee_waived > 0, na.rm = TRUE),
    prop_had_any_fee_waiver = mean(subtotal_waived > 0, na.rm = TRUE),
    
    # Averages for fee components
    avg_building_fee_paid = mean(building_fee_paid, na.rm = TRUE),
    avg_zoning_fee_paid = mean(zoning_fee_paid, na.rm = TRUE),
    avg_building_fee_subtotal = mean(building_fee_subtotal, na.rm = TRUE),
    avg_zoning_fee_subtotal = mean(zoning_fee_subtotal, na.rm = TRUE),
    
    # It's good practice to specify how to handle groups after summarizing
    .groups = 'drop' 
  ) %>%
  # Rename taxyear to year for merging with your main panel
  arrange(block_id, application_start_date) %>% 
  rename(year = application_start_date)


## bring in panel of ward switches at the block level to merge in treatment variable
joined_panel_switch <- st_read("../input/census_blocks_ward_switchers.gpkg") 

joined_panel_switch <- left_join(
  joined_panel_switch,
  block_level_aggregates,
  by = c("block_id", "year")
) %>% 
  st_drop_geometry()

##bring in alderman data and restrictiveness scores

alderman_restrictiveness_scores <- read_csv("../input/alderman_restrictiveness_score.csv") 
alderman_panel <- read_csv("../input/alderman_panel.csv") 

alderman_panel <- alderman_panel %>% 
  filter(month(year_month_date) == 6) %>% 
  mutate(year = year(year_month_date)) %>% 
  select(year, ward, alderman, finance_chair, zoning_chair, budget_chair)


alderman_scores <- alderman_panel %>% 
  left_join(
    alderman_restrictiveness_scores, 
    by = c("alderman")
    ) 


joined_panel_switch <- left_join(
  joined_panel_switch,
  alderman_scores, 
  by = c("ward", "year")
  ) 


##bring in block-group level controls

bg_controls <-  read_csv("../input/block_group_controls.csv") %>% 
  rename(block_group_id = GEOID) %>% 
  mutate(block_group_id = as.character(block_group_id)) 

# Create the block group ID
joined_panel_switch <- joined_panel_switch %>%
  mutate(block_group_id = substr(block_id, 1, 12))

final_regression_panel <- left_join(
  joined_panel_switch,
  bg_controls,
  by = c("block_group_id", "year")
) %>% 
  select(-block_group_id)

# Save the final regression panel
write_csv(final_regression_panel, "../output/permit_regression_panel_blocks_unbalanced.csv")

balanced_panel <- final_regression_panel %>%
  group_by(block_id) %>%
  filter(all(!is.na(avg_reported_cost))) %>%
  ungroup()

write_csv(balanced_panel, "../output/permit_regression_panel_blocks_balanced.csv")


## effective sample size 

# # 1. Get a list of all unique block_ids that are ever treated
# switching_block_ids <- final_regression_panel %>%
#   filter(ward_switch == 1) %>%
#   distinct(block_id) %>%
#   pull(block_id) # pull() extracts the column into a simple vector
# # 
# # # 2. Filter the main panel to just these treated blocks and run the diagnostic
# effective_sample_summary <- final_regression_panel %>%
#   filter(!is.na(avg_reported_cost)) %>%
#   filter(block_id %in% switching_block_ids) %>%
#   group_by(block_id) %>%
#   summarise(
#     has_pre_period_data = any(year(as.Date(month)) < 2014, na.rm = TRUE),
#     has_post_period_data = any(year(as.Date(month)) >= 2015, na.rm = TRUE)
#   ) %>%
#   # Keep only blocks that have data on both sides of the switch
#   filter(has_pre_period_data & has_post_period_data)
# 
# 
# effective_sample <- final_regression_panel %>% 
#   filter(!is.na(avg_reported_cost)) %>%
#   filter(block_id %in% switching_block_ids) %>%
#   mutate(year = year(as.Date(month))) %>%
#   group_by(block_id, year) %>% 
#   summarise(
#     n_permits = sum(n_permits, na.rm = TRUE),
#     avg_processing_time = mean(avg_processing_time, na.rm = TRUE),
#     avg_reported_cost = mean(avg_reported_cost, na.rm = TRUE),
#     avg_total_fee = mean(avg_total_fee, na.rm = TRUE),
#   ) %>% 
#   ungroup() %>% 
#   arrange(desc(n_permits))

# n_useful_blocks <- nrow(effective_sample_summary)
# 
# print(paste("Number of switching blocks you can actually use for identification:", n_useful_blocks))




