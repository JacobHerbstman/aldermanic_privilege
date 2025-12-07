# process_single_day.R
# Usage: Rscript process_single_day.R <input_parquet> <output_csv>

source("../../setup_environment/code/packages.R")

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file <- args[2]

# Read and process
data <- read_parquet(input_file) %>% 
  janitor::clean_names() %>% 
  filter(city == "Chicago") %>% 
  mutate(
    available_date = as.Date(str_sub(available_at, 1, 10)), 
    file_date = as.Date(str_sub(scraped_timestamp, 1, 10))
  ) %>% 
  select(
    id, property_id, rent_price, building_type, beds, baths, sqft, 
    availability_status, doorman, furnished, gym, laundry, pool, 
    year_built, latitude, longitude, available_date, file_date
  )

# Write output
write_csv(data, output_file)



# processing renthub chicago data

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
# 
# source("../../setup_environment/code/packages.R")
# 
# data <- read_parquet("../input/2025-11-11--data_01c09c08-0107-1cbd-0042-fa07047ca0a6_507_6_0.snappy.parquet", show_col_types = FALSE) %>% 
#   janitor::clean_names()
# 
# data <- data %>% 
#   filter(city == "Chicago") %>% 
#   mutate(available_date = as.Date(str_sub(available_at, 1, 10)), 
#          file_date = as.Date(str_sub(scraped_timestamp, 1, 10))) %>% 
#   select(id, property_id, rent_price, building_type, beds, baths, sqft, 
#          availability_status, doorman, furnished, gym, laundry, pool, 
#          year_built,  latitude, longitude, available_date, file_date) 