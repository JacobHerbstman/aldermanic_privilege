# process_improvements.R
# Creates a PIN x tax_year panel of residential property characteristics
# Outputs a PANEL (not cross-section) for temporal matching to sales
#
# Uses DuckDB for robust parsing of large CSV with some malformed rows

source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD DATA USING DUCKDB (handles malformed rows gracefully)
# =============================================================================
message("Loading full residential improvements data using DuckDB...")
message("(This may take a few minutes for ~15M rows)")

# Connect to DuckDB
con <- dbConnect(duckdb())

# Read CSV with DuckDB's flexible parser - ignores malformed rows
message("Reading CSV...")
dbExecute(con, "
  CREATE TABLE improvements AS
  SELECT
    pin,
    year AS tax_year,
    township_code,
    char_yrblt AS year_built,
    char_bldg_sf AS building_sqft,
    char_land_sf AS land_sqft,
    char_rooms AS num_rooms,
    char_beds AS num_bedrooms,
    char_fbath AS num_full_baths,
    char_hbath AS num_half_baths,
    char_frpl AS num_fireplaces,
    char_apts AS num_apartments_raw,
    char_gar1_size AS garage_size_raw
  FROM read_csv('../input/residential_improvement_characteristics_full.csv',
                ignore_errors = true,
                auto_detect = true,
                max_line_size = 10000000)
")

# Get row count
n_loaded <- dbGetQuery(con, "SELECT COUNT(*) as n FROM improvements")$n
message(sprintf("Loaded %s rows", format(n_loaded, big.mark = ",")))

# Filter to Chicago townships
message("Filtering to Chicago townships...")
dbExecute(con, "
  DELETE FROM improvements
  WHERE township_code NOT IN ('70', '71', '72', '73', '74', '75', '76', '77')
")

n_chicago <- dbGetQuery(con, "SELECT COUNT(*) as n FROM improvements")$n
message(sprintf("Chicago properties: %s rows", format(n_chicago, big.mark = ",")))

# Pull into R as data.table
message("Converting to data.table...")
data <- as.data.table(dbGetQuery(con, "SELECT * FROM improvements"))
dbDisconnect(con, shutdown = TRUE)

# =============================================================================
# 2. CLEAN AND CONVERT COLUMNS
# =============================================================================
message("Cleaning columns...")

data[, `:=`(
    pin = as.character(pin),
    tax_year = as.integer(tax_year),
    year_built = as.integer(year_built),
    building_sqft = as.numeric(building_sqft),
    land_sqft = as.numeric(land_sqft),
    num_rooms = as.numeric(num_rooms),
    num_bedrooms = as.numeric(num_bedrooms),
    num_full_baths = as.numeric(num_full_baths),
    num_half_baths = as.numeric(num_half_baths),
    num_fireplaces = as.numeric(num_fireplaces)
)]

# Convert spelled-out apartment counts to integers
data[, num_apartments := fcase(
    is.na(num_apartments_raw), NA_integer_,
    tolower(trimws(num_apartments_raw)) %in% c("none", "zero"), 0L,
    tolower(trimws(num_apartments_raw)) == "one", 1L,
    tolower(trimws(num_apartments_raw)) == "two", 2L,
    tolower(trimws(num_apartments_raw)) == "three", 3L,
    tolower(trimws(num_apartments_raw)) == "four", 4L,
    tolower(trimws(num_apartments_raw)) == "five", 5L,
    tolower(trimws(num_apartments_raw)) == "six", 6L,
    default = suppressWarnings(as.integer(num_apartments_raw))
)]

# Parse garage size from strings like "2.5 cars"
data[, garage_size := as.numeric(gsub("[^0-9.]", "", garage_size_raw))]

# Drop raw columns
data[, c("num_apartments_raw", "garage_size_raw", "township_code") := NULL]

# =============================================================================
# 3. HANDLE DUPLICATES WITHIN PIN-YEAR
# =============================================================================
# Some PINs have multiple records per tax_year (e.g., multiple buildings/cards)
# Keep the one with largest building_sqft as the "main" structure

message("Handling duplicates within PIN-year...")
n_before <- nrow(data)

# Count duplicates for diagnostics
dupe_check <- data[, .N, by = .(pin, tax_year)][N > 1]
message(sprintf("PIN-years with multiple records: %s", format(nrow(dupe_check), big.mark = ",")))

# Keep largest building per PIN-year
data <- data[order(pin, tax_year, -building_sqft)]
data <- data[, .SD[1], by = .(pin, tax_year)]

n_after <- nrow(data)
message(sprintf(
    "Rows after deduplication: %s (dropped %s)",
    format(n_after, big.mark = ","),
    format(n_before - n_after, big.mark = ",")
))

# =============================================================================
# 4. SELECT COLUMNS FOR PANEL
# =============================================================================
message("Selecting hedonic variables...")

improvements_panel <- data[, .(
    pin,
    tax_year,
    year_built,
    building_sqft,
    land_sqft,
    num_rooms,
    num_bedrooms,
    num_full_baths,
    num_half_baths,
    num_fireplaces,
    num_apartments,
    garage_size
)]

# =============================================================================
# 5. DIAGNOSTICS
# =============================================================================
message("\n=== PANEL DIAGNOSTICS ===")

message(sprintf("\nTotal rows: %s", format(nrow(improvements_panel), big.mark = ",")))
message(sprintf("Unique PINs: %s", format(uniqueN(improvements_panel$pin), big.mark = ",")))
message(sprintf(
    "Tax years covered: %d to %d",
    min(improvements_panel$tax_year, na.rm = TRUE),
    max(improvements_panel$tax_year, na.rm = TRUE)
))

# Records per PIN distribution
records_per_pin <- improvements_panel[, .N, by = pin]
message(sprintf(
    "\nRecords per PIN: median = %d, mean = %.1f, max = %d",
    median(records_per_pin$N),
    mean(records_per_pin$N),
    max(records_per_pin$N)
))

# Coverage of key variables
message("\nVariable coverage (% non-missing):")
coverage <- improvements_panel[, .(
    building_sqft = mean(!is.na(building_sqft)) * 100,
    land_sqft = mean(!is.na(land_sqft)) * 100,
    year_built = mean(!is.na(year_built)) * 100,
    num_bedrooms = mean(!is.na(num_bedrooms)) * 100,
    num_full_baths = mean(!is.na(num_full_baths)) * 100,
    garage_size = mean(!is.na(garage_size)) * 100
)]
print(coverage)

# Tax year coverage
message("\nObservations by tax year:")
tax_year_counts <- improvements_panel[, .N, by = tax_year][order(tax_year)]
print(tax_year_counts)

# Year built distribution (time-invariant but useful to see)
message("\nYear built distribution:")
decade_counts <- improvements_panel[!is.na(year_built),
    .(n_properties = uniqueN(pin)),
    by = .(decade = floor(year_built / 10) * 10)
][order(decade)]
print(decade_counts)

# =============================================================================
# 6. SORT AND SAVE AS PARQUET
# =============================================================================
message("\nSaving panel...")

# Sort by pin and tax_year - CRITICAL for efficient rolling joins in Step 3
setkey(improvements_panel, pin, tax_year)

write_parquet(improvements_panel, "../output/residential_improvements_panel.parquet")

message(sprintf(
    "Saved: ../output/residential_improvements_panel.parquet (%s rows)",
    format(nrow(improvements_panel), big.mark = ",")
))

message("\nDone!")
