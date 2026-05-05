## This code creates a comprehensive Ward-Year Panel (2000-2023) with Controls
## Sources: 2000 Decennial, 2010 Decennial, and Annual ACS 5-Year Estimates

## --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_ward_controls/code")

source("../../setup_environment/code/packages.R")
library(tigris) # Required for the geometry fix

# 1. SETUP & INPUTS
# -----------------------------------------------------------------------------
if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("Error: CENSUS_API_KEY not found in .Renviron")
}
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Set tigris cache to avoid re-downloading good files
options(tigris_use_cache = TRUE)

census_metadata <- tibble(
  source = character(),
  product = character(),
  year = integer(),
  survey = character(),
  sumfile = character(),
  geography = character(),
  state = character(),
  county = character(),
  variables = character(),
  geometry = logical(),
  rows = integer(),
  geographies = integer(),
  downloaded_at_utc = character()
)
census_metadata_timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Load Ward Panel (CRS 3435)
ward_panel <- st_read("../input/ward_panel.gpkg") %>% st_transform(3435)

# 2. VARIABLE DICTIONARIES
# -----------------------------------------------------------------------------

# A. ACS Variables (2013+) & 2013 Proxy for 2010-2012 Econ
vars_acs <- c(
  tot_pop       = "B01003_001",
  tot_hhs       = "B11001_001",
  tot_units     = "B25003_001",
  owner_occ     = "B25003_002",
  renter_occ    = "B25003_003",
  pop_white     = "B02001_002",
  pop_black     = "B02001_003",
  pop_hisp      = "B03003_003",
  median_income = "B19013_001",
  agg_value     = "B25079_001",
  agg_rent      = "B25065_001",
  pop_25plus    = "B15003_001",
  educ_bach     = "B15003_022",
  educ_mast     = "B15003_023",
  educ_prof     = "B15003_024",
  educ_doc      = "B15003_025"
)

# B. 2000 Decennial (SF3) - Has Econ Data
vars_2000 <- c(
  tot_pop       = "P001001",
  tot_hhs       = "P010001",
  tot_units     = "H001001",
  owner_occ     = "H007002",
  renter_occ    = "H007003",
  pop_white     = "P006002",
  pop_black     = "P006003",
  pop_hisp      = "P007010",
  median_income = "P053001",
  agg_value     = "H081001",
  agg_rent      = "H065001",
  pop_25plus    = "P037001"
)

# C. 2010 Decennial (SF1) - Counts Only (No Econ)
vars_2010_sf1 <- c(
  tot_pop       = "P001001",
  tot_hhs       = "P018001",
  tot_units     = "H001001",
  owner_occ     = "H004002",
  renter_occ    = "H004003",
  pop_white     = "P003002",
  pop_black     = "P003003",
  pop_hisp      = "P004003"
)

# 3. PRE-FETCH STATIC DATASETS (Regimes 1 & 2)
# -----------------------------------------------------------------------------

# --- REGIME 1: 2000 DECENNIAL (2000-2009) ---
message("Fetching 2000 Decennial Data...")
data_2000_raw <- get_decennial(
  geography = "block group", variables = vars_2000,
  state = "IL", county = "Cook", year = 2000, sumfile = "sf3", geometry = TRUE
)
census_metadata <- bind_rows(
  census_metadata,
  tibble(
    source = "tidycensus::get_decennial",
    product = "decennial",
    year = 2000L,
    survey = NA_character_,
    sumfile = "sf3",
    geography = "block group",
    state = "IL",
    county = "Cook",
    variables = paste(unname(vars_2000), collapse = ";"),
    geometry = TRUE,
    rows = nrow(data_2000_raw),
    geographies = n_distinct(data_2000_raw$GEOID),
    downloaded_at_utc = census_metadata_timestamp
  )
)
data_2000 <- data_2000_raw %>%
  st_transform(3435) %>%
  select(GEOID, variable, value, geometry) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(educ_bach_plus = 0) # Placeholder

# --- REGIME 2: 2010 HYBRID (2010-2012) ---
message("Building 2010 Hybrid Dataset...")

# A. Get Geometry using tigris (Avoids 'zip file' error)
geo_2010 <- tigris::block_groups(state = "IL", county = "Cook", year = 2010, cb = FALSE) %>%
  st_transform(3435) %>%
  select(GEOID = GEOID10, geometry)
census_metadata <- bind_rows(
  census_metadata,
  tibble(
    source = "tigris::block_groups",
    product = "tiger_line",
    year = 2010L,
    survey = NA_character_,
    sumfile = NA_character_,
    geography = "block group",
    state = "IL",
    county = "Cook",
    variables = NA_character_,
    geometry = TRUE,
    rows = nrow(geo_2010),
    geographies = n_distinct(geo_2010$GEOID),
    downloaded_at_utc = census_metadata_timestamp
  )
)

# B. Get 2010 Demographics (SF1)
data_2010_sf1_raw <- get_decennial(
  geography = "block group", variables = vars_2010_sf1,
  state = "IL", county = "Cook", year = 2010, geometry = FALSE
)
census_metadata <- bind_rows(
  census_metadata,
  tibble(
    source = "tidycensus::get_decennial",
    product = "decennial",
    year = 2010L,
    survey = NA_character_,
    sumfile = "sf1",
    geography = "block group",
    state = "IL",
    county = "Cook",
    variables = paste(unname(vars_2010_sf1), collapse = ";"),
    geometry = FALSE,
    rows = nrow(data_2010_sf1_raw),
    geographies = n_distinct(data_2010_sf1_raw$GEOID),
    downloaded_at_utc = census_metadata_timestamp
  )
)
data_2010_sf1 <- data_2010_sf1_raw %>%
  select(GEOID, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

# C. Get 2013 ACS Economics (Proxy for 2010-2012 Econ)
data_2013_econ_raw <- get_acs(
  geography = "block group", variables = vars_acs,
  state = "IL", county = "Cook", year = 2013, survey = "acs5", geometry = FALSE
)
census_metadata <- bind_rows(
  census_metadata,
  tibble(
    source = "tidycensus::get_acs",
    product = "acs",
    year = 2013L,
    survey = "acs5",
    sumfile = NA_character_,
    geography = "block group",
    state = "IL",
    county = "Cook",
    variables = paste(unname(vars_acs), collapse = ";"),
    geometry = FALSE,
    rows = nrow(data_2013_econ_raw),
    geographies = n_distinct(data_2013_econ_raw$GEOID),
    downloaded_at_utc = census_metadata_timestamp
  )
)
data_2013_econ <- data_2013_econ_raw %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    educ_bach_plus = rowSums(across(c(educ_bach, educ_mast, educ_prof, educ_doc)), na.rm = TRUE)
  ) %>%
  select(GEOID, median_income, agg_value, agg_rent, educ_bach_plus, pop_25plus)

# D. Merge to create the Hybrid 2010 Dataset
data_2010_hybrid <- geo_2010 %>%
  left_join(data_2010_sf1, by = "GEOID") %>%
  left_join(data_2013_econ, by = "GEOID")

# --- REGIME 3 PREP: 2020 GEOMETRY ---
message("Fetching 2020 Geometry...")
geo_2020 <- tigris::block_groups(state = "IL", county = "Cook", year = 2020, cb = FALSE) %>%
  st_transform(3435) %>%
  select(GEOID, geometry)
census_metadata <- bind_rows(
  census_metadata,
  tibble(
    source = "tigris::block_groups",
    product = "tiger_line",
    year = 2020L,
    survey = NA_character_,
    sumfile = NA_character_,
    geography = "block group",
    state = "IL",
    county = "Cook",
    variables = NA_character_,
    geometry = TRUE,
    rows = nrow(geo_2020),
    geographies = n_distinct(geo_2020$GEOID),
    downloaded_at_utc = census_metadata_timestamp
  )
)


# 4. THE PANEL CONSTRUCTION LOOP
# -----------------------------------------------------------------------------
years <- 2000:2023
final_panel_list <- list()

message(glue("Starting Panel Construction ({min(years)}-{max(years)})..."))

for (y in years) {
  message(glue("Processing Year: {y}"))

  # --- A. Select Data Source ---
  if (y <= 2009) {
    # Regime 1: 2000 Decennial
    current_bgs <- data_2000
  } else if (y >= 2010 & y <= 2012) {
    # Regime 2: 2010 Hybrid (Decennial Counts + 2013 ACS Econ)
    current_bgs <- data_2010_hybrid
  } else {
    # Regime 3: Annual ACS (2013+)
    current_data_raw <- get_acs(
      geography = "block group", variables = vars_acs,
      state = "IL", county = "Cook", year = y, survey = "acs5", geometry = FALSE
    )
    census_metadata <- bind_rows(
      census_metadata,
      tibble(
        source = "tidycensus::get_acs",
        product = "acs",
        year = as.integer(y),
        survey = "acs5",
        sumfile = NA_character_,
        geography = "block group",
        state = "IL",
        county = "Cook",
        variables = paste(unname(vars_acs), collapse = ";"),
        geometry = FALSE,
        rows = nrow(current_data_raw),
        geographies = n_distinct(current_data_raw$GEOID),
        downloaded_at_utc = census_metadata_timestamp
      )
    )
    current_data <- current_data_raw %>%
      select(GEOID, variable, estimate) %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      mutate(
        educ_bach_plus = rowSums(across(c(educ_bach, educ_mast, educ_prof, educ_doc)), na.rm = TRUE)
      )

    # Attach correct geometry
    if (y < 2020) {
      current_bgs <- geo_2010 %>% left_join(current_data, by = "GEOID")
    } else {
      current_bgs <- geo_2020 %>% left_join(current_data, by = "GEOID")
    }
  }

  # --- B. Spatial Join to Ward ---
  # Pick Ward Map
  current_wards <- ward_panel %>% filter(year == y)
  if (nrow(current_wards) == 0) next

  # Centroid Join
  bg_centroids <- st_centroid(current_bgs)
  joined_data <- st_join(bg_centroids, current_wards, join = st_within) %>%
    st_drop_geometry() %>%
    mutate(year = y) %>%
    filter(!is.na(ward))

  final_panel_list[[as.character(y)]] <- joined_data
}

# 5. AGGREGATE TO WARD LEVEL
# -----------------------------------------------------------------------------
message("Aggregating to Ward-Year Level...")

final_bg_panel <- bind_rows(final_panel_list)

ward_controls <- final_bg_panel %>%
  group_by(ward, year) %>%
  summarize(
    # --- Universe Sums ---
    pop_total = sum(tot_pop, na.rm = TRUE),
    hh_total = sum(tot_hhs, na.rm = TRUE),
    hu_total = sum(tot_units, na.rm = TRUE),

    # --- Demographics (Weighted Shares) ---
    share_black = sum(pop_black, na.rm = TRUE) / sum(tot_pop, na.rm = TRUE),
    share_hisp = sum(pop_hisp, na.rm = TRUE) / sum(tot_pop, na.rm = TRUE),
    share_white = sum(pop_white, na.rm = TRUE) / sum(tot_pop, na.rm = TRUE),
    homeownership_rate = sum(owner_occ, na.rm = TRUE) / sum(tot_units, na.rm = TRUE),
    share_bach_plus = sum(educ_bach_plus, na.rm = TRUE) / sum(pop_25plus, na.rm = TRUE),

    # --- Economics (Weighted Means/Medians) ---
    # Median income: household-weighted average of block group medians
    median_hh_income = weighted.mean(median_income, tot_hhs, na.rm = TRUE),
    avg_home_value = sum(agg_value, na.rm = TRUE) / sum(owner_occ, na.rm = TRUE),
    avg_rent = sum(agg_rent, na.rm = TRUE) / sum(renter_occ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(pop_total > 0) %>%
  arrange(ward, year)

# 6. SAVE
# -----------------------------------------------------------------------------
# Save block group level controls (before aggregation)
write_csv(final_bg_panel, "../output/block_group_controls_2000_2023.csv")
message("Block Group Panel saved to: ../output/block_group_controls_2000_2023.csv")

# Save ward-level aggregated controls
write_csv(ward_controls, "../output/ward_controls_2000_2023.csv")
message("Ward Panel saved to: ../output/ward_controls_2000_2023.csv")

write_csv(
  census_metadata %>% arrange(year, product, survey, sumfile, source),
  "../output/ward_controls_census_metadata.csv"
)
message("Census metadata saved to: ../output/ward_controls_census_metadata.csv")

message("Done! Both Block Group and Ward Panels Created.")
