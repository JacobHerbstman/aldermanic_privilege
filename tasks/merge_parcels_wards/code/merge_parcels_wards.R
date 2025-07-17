#This script merges newly built parcels with their wards over time to create a panel with parcel-level variation in wards

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

parcels <- st_read("../input/year_built_sample.shp")

ward_panel <- st_read("../input/ward_panel.shp")


## harmonize geometries
parcels <- parcels %>%
  st_transform(st_crs(ward_panel))
##harmonize year variable name
parcels <- parcels %>%
  rename(year_built = sa_yr_blt, 
         year = taxyear) %>% 
  filter(year > 0)

# 
# parcels <- parcels %>% 
#   sample_n(1000)  # Sample 1000 parcels for testing

years <- sort(unique(parcels$year))
merged_list <- list()

## loop through each year and perform spatial join  
for(yr in years) {
  # Filter parcels for this year
  parcels_yr <- parcels %>% filter(year == yr)
  
  # Filter ward_panel for this year
  ward_yr <- ward_panel %>% filter(year == yr)
  
  # Spatial join for this year
  merged_yr <- st_join(parcels_yr, ward_yr, join = st_within)
  
  merged_list[[as.character(yr)]] <- merged_yr
  
  print(yr)
}

## append at the end all years
merged_parcels <- do.call(rbind, merged_list)
# fix weird rownames
rownames(merged_parcels) <- NULL

## get rid of unmatched wards (couple thousand)
merged_parcels <- merged_parcels %>%
  arrange(attom_id, year.x, ward) %>% 
  filter(!is.na(ward)) %>% 
  select(-year.y) %>% 
  rename(year = year.x) 

st_write(merged_parcels, 
         "../output/matched_parcels_wards.shp", 
         delete_dsn = TRUE)




## identify parcels that switch wards

# merged_parcels <- merged_parcels %>%
#   group_by(attom_id) %>%
#   mutate(
#     # Check if ward in 2014 differs from ward in 2015
#     ward_changed = {
#       ward_2014 <- ward[year == 2014][1]
#       ward_2016 <- ward[year == 2016][1]
#       if(is.na(ward_2014) | is.na(ward_2016)) {
#         NA
#       } else {
#         ward_2014 != ward_2016
#       }
#     },
#     
#     # Create the indicator: 1 if changed and year >= 2015, 0 otherwise
#     ward_change = case_when(
#       is.na(ward_changed) ~ NA_real_,
#       ward_changed == TRUE & year >= 2015 ~ 1,
#       TRUE ~ 0
#     )
#   ) %>%
#   select(-ward_changed) %>%
#   ungroup()
# 
# ward_switchers <- merged_parcels %>%
#   group_by(attom_id) %>%
#   filter(any(ward_change == 1, na.rm = TRUE)) %>%
#   ungroup()
# 
# st_write(ward_switchers, 
#          "../output/ward_switchers.shp", 
#          delete_dsn = TRUE)
