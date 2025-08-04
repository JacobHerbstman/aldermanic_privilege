#This script cleans the processed attom historical files from link_assessor_history

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

data <- st_read_parquet("../input/attom_historical_with_address.shp")
  
# Much faster - just takes the first row per group
unique_spatial_data <- data %>%
  group_by(attom_id) %>%
  slice(1) %>%
  ungroup()

## historical parcel data 
parcel_data_historical <- read_parquet("../input/chicago_attom_history.parquet") %>% 
  filter(sa_yr_blt >= 2006) %>%
  filter(sa_mail_city == "CHICAGO") ## chicago only
  # slice_sample(n = 100000, replace = FALSE) 


## merge geometry and parcel info
merged_parcels <- unique_spatial_data %>% 
  inner_join(parcel_data_historical, by = "attom_id")

gc()


# fix inconsistencies in year built by using majority vote
merged_parcels <- merged_parcels %>%
  group_by(attom_id) %>%
  mutate(
    # Find the most common year_built for each attom_id
    majority_year_built = {
      year_counts <- table(sa_yr_blt)
      as.numeric(names(year_counts)[which.max(year_counts)])
    },
    # Replace all sa_yr_blt values with the majority vote
    sa_yr_blt = majority_year_built
  ) %>%
  select(-majority_year_built) %>%  # Remove the helper column
  ungroup()

# ## just keep essential columns
# merged_parcels <- merged_parcels %>% 
#   dplyr::select(
#     attom_id, 
#     sa_yr_blt, 
#     sa_lotsize,
#     sa_sqft,
#     geometry
#   )


st_write(merged_parcels, "../output/year_built_sample.gpkg", delete_dsn = TRUE)
             

