## This code creates a comprehensive ward-year panel with controls.
## Sources: 2000 Decennial, 2010 Decennial, and Annual ACS 5-Year Estimates

## --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_ward_controls/code")
# start_year <- 2006
# end_year <- 2022

source("../../setup_environment/code/packages.R")
library(tigris) # Required for the geometry fix

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_year, end_year)
}
if (length(cli_args) != 2) {
  stop("Script requires 2 arguments: <start_year> <end_year>.", call. = FALSE)
}
start_year <- suppressWarnings(as.integer(cli_args[1]))
end_year <- suppressWarnings(as.integer(cli_args[2]))
if (!is.finite(start_year) || !is.finite(end_year) || start_year > end_year) {
  stop("start_year and end_year must be valid integers with start_year <= end_year.", call. = FALSE)
}

# 1. SETUP & INPUTS
# -----------------------------------------------------------------------------
if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("Error: CENSUS_API_KEY not found in .Renviron")
}
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Set tigris cache to avoid re-downloading good files
options(tigris_use_cache = TRUE)

# Load Ward Panel (CRS 3435)
ward_panel <- st_read("../input/ward_panel.gpkg") %>% st_transform(3435)

# 2. VARIABLE DICTIONARIES
# -----------------------------------------------------------------------------

# A. ACS Variables (2013+) & 2013 Proxy for 2010-2012 Economics
vars_acs <- c(
  tot_pop       = "B01003_001",
  tot_hhs       = "B11001_001",
  tot_units     = "B25003_001",
  owner_occ     = "B25003_002",
  renter_occ    = "B25003_003",
  pop_white     = "B03002_003",
  pop_black     = "B03002_004",
  pop_hisp      = "B03002_012",
  median_income = "B19013_001",
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
  tot_units     = "H007001",
  owner_occ     = "H007002",
  renter_occ    = "H007003",
  pop_white     = "P007003",
  pop_black     = "P007004",
  pop_hisp      = "P007010",
  median_income = "P053001",
  pop_25plus    = "P037001",
  educ_bach_m   = "P037015",
  educ_mast_m   = "P037016",
  educ_prof_m   = "P037017",
  educ_doc_m    = "P037018",
  educ_bach_f   = "P037032",
  educ_mast_f   = "P037033",
  educ_prof_f   = "P037034",
  educ_doc_f    = "P037035"
)

# C. 2010 Decennial (SF1) - Counts Only (No Econ)
vars_2010_sf1 <- c(
  tot_pop       = "P001001",
  tot_hhs       = "P018001",
  tot_units     = "H004001",
  owner_mortgage = "H004002",
  owner_free_clear = "H004003",
  renter_occ    = "H004004",
  pop_white     = "P005003",
  pop_black     = "P005004",
  pop_hisp      = "P005010"
)

# 3. PRE-FETCH STATIC DATASETS (Regimes 1 & 2)
# -----------------------------------------------------------------------------

# --- REGIME 1: 2000 DECENNIAL (2000-2009) ---
message("Fetching 2000 Decennial Data...")
data_2000_raw <- get_decennial(
  geography = "block group", variables = vars_2000,
  state = "IL", county = "Cook", year = 2000, sumfile = "sf3", geometry = TRUE
)
data_2000 <- data_2000_raw %>%
  st_transform(3435) %>%
  select(GEOID, variable, value, geometry) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    educ_bach_plus = rowSums(
      across(c(
        educ_bach_m, educ_mast_m, educ_prof_m, educ_doc_m,
        educ_bach_f, educ_mast_f, educ_prof_f, educ_doc_f
      )),
      na.rm = TRUE
    )
  )

if (any(data_2000$owner_occ + data_2000$renter_occ != data_2000$tot_units, na.rm = TRUE)) {
  stop("2000 tenure components do not sum to occupied housing units.", call. = FALSE)
}

# --- REGIME 2: 2010 HYBRID (2010-2012) ---
message("Building 2010 Hybrid Dataset...")

# A. Get Geometry using tigris (Avoids 'zip file' error)
geo_2010 <- tigris::block_groups(state = "IL", county = "Cook", year = 2010, cb = FALSE) %>%
  st_transform(3435) %>%
  select(GEOID = GEOID10, geometry)

# B. Get 2010 Demographics (SF1)
data_2010_sf1_raw <- get_decennial(
  geography = "block group", variables = vars_2010_sf1,
  state = "IL", county = "Cook", year = 2010, geometry = FALSE
)
data_2010_sf1 <- data_2010_sf1_raw %>%
  select(GEOID, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(owner_occ = owner_mortgage + owner_free_clear)

if (any(data_2010_sf1$owner_occ + data_2010_sf1$renter_occ != data_2010_sf1$tot_units, na.rm = TRUE)) {
  stop("2010 tenure components do not sum to occupied housing units.", call. = FALSE)
}

# C. Get 2013 ACS Economics (Proxy for 2010-2012 Econ)
data_2013_econ_raw <- get_acs(
  geography = "block group", variables = vars_acs,
  state = "IL", county = "Cook", year = 2013, survey = "acs5", geometry = FALSE
)
data_2013_econ <- data_2013_econ_raw %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    educ_bach_plus = rowSums(across(c(educ_bach, educ_mast, educ_prof, educ_doc)), na.rm = TRUE)
  ) %>%
  select(GEOID, median_income, educ_bach_plus, pop_25plus)

# D. Merge to create the Hybrid 2010 Dataset
data_2010_hybrid <- geo_2010 %>%
  left_join(data_2010_sf1, by = "GEOID", relationship = "one-to-one") %>%
  left_join(data_2013_econ, by = "GEOID", relationship = "one-to-one")

# --- REGIME 3 PREP: 2020 GEOMETRY ---
message("Fetching 2020 Geometry...")
geo_2020 <- tigris::block_groups(state = "IL", county = "Cook", year = 2020, cb = FALSE) %>%
  st_transform(3435) %>%
  select(GEOID, geometry)


# 4. THE PANEL CONSTRUCTION LOOP
# -----------------------------------------------------------------------------
years <- start_year:end_year
final_panel_list <- list()

assign_block_groups_to_wards <- function(current_bgs, current_wards, current_year) {
  count_columns <- c(
    "tot_pop", "tot_hhs", "tot_units", "owner_occ", "renter_occ",
    "pop_white", "pop_black", "pop_hisp", "pop_25plus", "educ_bach_plus"
  )

  if (anyDuplicated(current_bgs$GEOID) > 0) {
    stop(sprintf("Block-group data has duplicate GEOID values before ward assignment in %s.", current_year), call. = FALSE)
  }
  if (anyDuplicated(current_wards$ward) > 0) {
    stop(sprintf("Ward map has duplicate ward geometries in %s.", current_year), call. = FALSE)
  }
  if (is.na(st_crs(current_bgs)) || st_crs(current_bgs)$epsg != 3435 ||
      is.na(st_crs(current_wards)) || st_crs(current_wards)$epsg != 3435) {
    stop(sprintf("Block groups and wards must be EPSG:3435 before ward assignment in %s.", current_year), call. = FALSE)
  }
  empty_bg <- st_is_empty(current_bgs)
  if (any(empty_bg)) {
    message(sprintf("Dropping %d empty block-group geometries before ward assignment in %s.", sum(empty_bg), current_year))
    current_bgs <- current_bgs[!empty_bg, ]
  }
  if (any(st_is_empty(current_wards))) {
    stop(sprintf("Empty ward geometry found before ward assignment in %s.", current_year), call. = FALSE)
  }

  current_bgs <- current_bgs %>%
    st_make_valid() %>%
    mutate(block_group_area = as.numeric(st_area(geometry)))
  current_wards <- current_wards %>%
    select(ward) %>%
    st_make_valid()

  chicago_coverage <- suppressWarnings(
    st_intersection(
      current_bgs %>% select(GEOID, block_group_area),
      st_union(current_wards)
    )
  ) %>%
    mutate(covered_area = as.numeric(st_area(geometry))) %>%
    st_drop_geometry() %>%
    group_by(GEOID) %>%
    summarize(
      covered_area_share = sum(covered_area) / first(block_group_area),
      .groups = "drop"
    )
  if (any(!is.finite(chicago_coverage$covered_area_share)) ||
      any(chicago_coverage$covered_area_share > 1.001)) {
    stop(sprintf("Invalid block-group coverage shares in %s.", current_year), call. = FALSE)
  }

  assigned_data <- suppressWarnings(
    st_intersection(
      current_bgs %>%
        select(GEOID, all_of(count_columns), median_income, block_group_area),
      current_wards
    )
  ) %>%
    mutate(intersection_area = as.numeric(st_area(geometry))) %>%
    filter(intersection_area > 0) %>%
    mutate(raw_area_share = intersection_area / block_group_area) %>%
    st_drop_geometry() %>%
    group_by(GEOID, ward) %>%
    summarize(
      across(all_of(count_columns), first),
      median_income = first(median_income),
      raw_area_share = sum(raw_area_share),
      .groups = "drop"
    ) %>%
    left_join(
      chicago_coverage,
      by = "GEOID",
      relationship = "many-to-one"
    ) %>%
    group_by(GEOID) %>%
    mutate(
      area_share = raw_area_share * covered_area_share / sum(raw_area_share)
    ) %>%
    ungroup() %>%
    mutate(across(all_of(count_columns), ~ .x * area_share)) %>%
    mutate(year = current_year) %>%
    select(-raw_area_share, -covered_area_share, -area_share)

  assigned_data
}

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
    current_data <- current_data_raw %>%
      select(GEOID, variable, estimate) %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      mutate(
        educ_bach_plus = rowSums(across(c(educ_bach, educ_mast, educ_prof, educ_doc)), na.rm = TRUE)
      )

    # Attach correct geometry
    if (y < 2020) {
      current_bgs <- geo_2010 %>% left_join(current_data, by = "GEOID", relationship = "one-to-one")
    } else {
      current_bgs <- geo_2020 %>% left_join(current_data, by = "GEOID", relationship = "one-to-one")
    }
  }

  # --- B. Allocate block groups to wards by polygon overlap ---
  current_wards <- ward_panel %>% filter(year == y)
  if (nrow(current_wards) == 0) next

  final_panel_list[[as.character(y)]] <- assign_block_groups_to_wards(current_bgs, current_wards, y)
}

# 5. AGGREGATE TO WARD LEVEL
# -----------------------------------------------------------------------------
message("Aggregating to Ward-Year Level...")

final_bg_panel <- bind_rows(final_panel_list)

duplicate_bg_years <- final_bg_panel %>%
  count(GEOID, ward, year, name = "n") %>%
  filter(n > 1)
if (nrow(duplicate_bg_years) > 0) {
  stop("Block group controls contain duplicate GEOID-ward-year assignments.", call. = FALSE)
}

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
    .groups = "drop"
  ) %>%
  filter(pop_total > 0) %>%
  arrange(ward, year)

if (any(!is.finite(ward_controls$median_hh_income)) ||
    any(!is.finite(ward_controls$homeownership_rate)) ||
    any(!is.finite(ward_controls$share_bach_plus))) {
  stop("Ward controls contain non-finite socioeconomic measures.", call. = FALSE)
}
if (any(ward_controls$homeownership_rate < 0 | ward_controls$homeownership_rate > 1) ||
    any(ward_controls$share_bach_plus < 0 | ward_controls$share_bach_plus > 1) ||
    any(ward_controls$share_black < 0 | ward_controls$share_hisp < 0 | ward_controls$share_white < 0) ||
    any(ward_controls$share_black + ward_controls$share_hisp + ward_controls$share_white > 1 + 1e-8)) {
  stop("Ward controls contain invalid demographic shares.", call. = FALSE)
}

write_csv(
  ward_controls,
  sprintf("../output/ward_controls_%d_%d.csv", start_year, end_year)
)

message("Done! Ward Panel Created.")
