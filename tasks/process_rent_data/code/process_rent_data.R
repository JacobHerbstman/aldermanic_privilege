# process_single_day.R
# Usage: Rscript process_single_day.R <input_parquet> <output_csv>

source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/process_rent_data/code")
# input_file <- "../input/2025-11-11--data_example.parquet"
# output_file <- "../temp/2025-11-11.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(input_file, output_file)
}

if (length(args) != 2) {
  stop("FATAL: Script requires 2 args: <input_file> <output_file>", call. = FALSE)
}
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
