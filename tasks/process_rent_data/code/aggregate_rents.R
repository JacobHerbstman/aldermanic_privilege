# aggregate_rents.R
# Aggregates individual daily CSVs from ../temp into one time-series Parquet file.
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# 1. Setup
temp_dir <- "../temp"
output_file <- "../output/chicago_rent_panel.parquet"

# 2. Load Data
# We use arrow::open_dataset which is incredibly fast and memory-efficient
message("Opening dataset from ", temp_dir)

# NOTE: We must keep these as strings in the schema because the raw CSVs 
# still contain "Y"/"N". We will convert them to integers in the pipeline below.
schema_defs <- arrow::schema(
  id = arrow::string(),
  property_id = arrow::string(),
  rent_price = arrow::float64(),
  building_type = arrow::string(),
  beds = arrow::float64(),
  baths = arrow::float64(),
  sqft = arrow::float64(),
  availability_status = arrow::string(),
  doorman = arrow::string(),   # Read as string first
  furnished = arrow::string(), # Read as string first
  gym = arrow::string(),       # Read as string first
  laundry = arrow::string(),   # Read as string first
  pool = arrow::string(),      # Read as string first
  year_built = arrow::float64(),
  latitude = arrow::float64(),
  longitude = arrow::float64(),
  available_date = arrow::date32(),
  file_date = arrow::date32()
)

ds <- arrow::open_dataset(
  temp_dir, 
  format = "csv",
  schema = schema_defs, 
  skip = 1
)

# 3. Aggregation & Transformation
# We use mutate() before collect(). Arrow handles this conversion efficiently.
rent_panel <- ds %>%
  mutate(
    # Convert Y/N to 1/0 integers (1L/0L ensures integer type for max memory savings)
    doorman   = ifelse(doorman == "Y", 1L, 0L),
    furnished = ifelse(furnished == "Y", 1L, 0L),
    gym       = ifelse(gym == "Y", 1L, 0L),
    laundry   = ifelse(laundry == "Y", 1L, 0L),
    pool      = ifelse(pool == "Y", 1L, 0L)
  ) %>%
  select(everything()) %>% 
  collect() %>%
  janitor::clean_names()

message(sprintf("Aggregated %d rows from daily files.", nrow(rent_panel)))

# 4. Final Polish (Optional)
rent_panel <- rent_panel %>%
  arrange(file_date, id) %>%
  distinct() # Safety deduplication

# 5. Save
write_parquet(rent_panel, output_file)
message("Saved aggregated panel to ", output_file)