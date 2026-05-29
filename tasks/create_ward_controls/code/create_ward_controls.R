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

# B. Get 2010 Demographics (SF1)
data_2010_sf1_raw <- get_decennial(
  geography = "block group", variables = vars_2010_sf1,
  state = "IL", county = "Cook", year = 2010, geometry = FALSE
)
data_2010_sf1 <- data_2010_sf1_raw %>%
  select(GEOID, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

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
  select(GEOID, median_income, agg_value, agg_rent, educ_bach_plus, pop_25plus)

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
years <- 2000:2023
final_panel_list <- list()

resolve_ambiguous_assignment <- function(bg_row_id_value, current_bgs, current_wards, candidate_assignments, current_year) {
  bg <- current_bgs %>% filter(bg_row_id == bg_row_id_value)
  candidate_wards <- candidate_assignments %>%
    filter(bg_row_id == bg_row_id_value) %>%
    pull(ward) %>%
    unique() %>%
    sort()

  intersections <- suppressWarnings(
    st_intersection(
      current_wards %>% filter(ward %in% candidate_wards) %>% select(ward),
      st_geometry(bg)
    )
  )
  intersections$intersection_area_sqft <- as.numeric(st_area(intersections))

  areas <- tibble(candidate_ward = candidate_wards) %>%
    left_join(
      intersections %>%
        st_drop_geometry() %>%
        group_by(candidate_ward = ward) %>%
        summarize(intersection_area_sqft = sum(intersection_area_sqft, na.rm = TRUE), .groups = "drop"),
      by = "candidate_ward",
      relationship = "one-to-one"
    ) %>%
    mutate(intersection_area_sqft = coalesce(intersection_area_sqft, 0))

  max_area <- max(areas$intersection_area_sqft, na.rm = TRUE)
  if (!is.finite(max_area) || max_area <= 0) {
    stop(
      sprintf("Ambiguous ward assignment for GEOID %s in %s has no positive polygon overlap.", bg$GEOID[1], current_year),
      call. = FALSE
    )
  }

  selected_rows <- areas %>%
    filter(abs(intersection_area_sqft - max_area) <= 1e-6)
  if (nrow(selected_rows) > 1) {
    stop(
      sprintf("Ambiguous ward assignment for GEOID %s in %s has tied polygon overlaps.", bg$GEOID[1], current_year),
      call. = FALSE
    )
  }
  selected_ward <- selected_rows$candidate_ward[1]

  areas %>%
    mutate(
      year = current_year,
      GEOID = bg$GEOID[1],
      candidate_wards = paste(candidate_wards, collapse = ";"),
      selected_ward = selected_ward,
      resolution_reason = "centroid matched multiple wards; selected largest block-group polygon overlap"
    ) %>%
    select(
      year,
      GEOID,
      candidate_ward,
      candidate_wards,
      intersection_area_sqft,
      selected_ward,
      resolution_reason
    )
}

assign_block_groups_to_wards <- function(current_bgs, current_wards, current_year) {
  current_bgs <- current_bgs %>% mutate(bg_row_id = row_number())
  current_wards <- current_wards %>% select(ward)

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


  candidate_assignments <- st_join(
    st_centroid(current_bgs),
    current_wards,
    join = st_within,
    left = FALSE
  ) %>%
    st_drop_geometry() %>%
    select(bg_row_id, GEOID, ward)

  if (nrow(candidate_assignments) == 0) {
    return(tibble())
  }

  duplicate_candidates <- candidate_assignments %>%
    count(bg_row_id, GEOID, name = "candidate_count") %>%
    filter(candidate_count > 1)

  assignment_diagnostics <- tibble(GEOID = character(), selected_ward = integer())
  if (nrow(duplicate_candidates) > 0) {
    assignment_diagnostics <- map_dfr(
      duplicate_candidates$bg_row_id,
      resolve_ambiguous_assignment,
      current_bgs = current_bgs,
      current_wards = current_wards,
      candidate_assignments = candidate_assignments,
      current_year = current_year
    )
  }

  simple_assignments <- candidate_assignments %>%
    anti_join(duplicate_candidates, by = c("bg_row_id", "GEOID")) %>%
    distinct(bg_row_id, GEOID, ward)

  resolved_assignments <- assignment_diagnostics %>%
    distinct(GEOID, selected_ward) %>%
    inner_join(
      duplicate_candidates %>% select(bg_row_id, GEOID),
      by = "GEOID",
      relationship = "one-to-one"
    ) %>%
    transmute(bg_row_id, GEOID, ward = selected_ward)

  assignments <- bind_rows(simple_assignments, resolved_assignments) %>%
    arrange(bg_row_id)

  duplicate_assignments <- assignments %>%
    count(bg_row_id, GEOID, name = "n") %>%
    filter(n > 1)
  if (nrow(duplicate_assignments) > 0) {
    stop(sprintf("Block-group ward assignment still has duplicates in %s.", current_year), call. = FALSE)
  }

  assigned_data <- current_bgs %>%
    st_drop_geometry() %>%
    left_join(
      assignments %>% select(bg_row_id, ward),
      by = "bg_row_id",
      relationship = "one-to-one"
    ) %>%
    filter(!is.na(ward)) %>%
    mutate(year = current_year) %>%
    select(-bg_row_id)

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

  # --- B. Assign block groups to wards by centroid ---
  current_wards <- ward_panel %>% filter(year == y)
  if (nrow(current_wards) == 0) next

  final_panel_list[[as.character(y)]] <- assign_block_groups_to_wards(current_bgs, current_wards, y)
}

# 5. AGGREGATE TO WARD LEVEL
# -----------------------------------------------------------------------------
message("Aggregating to Ward-Year Level...")

final_bg_panel <- bind_rows(final_panel_list)

duplicate_bg_years <- final_bg_panel %>%
  count(GEOID, year, name = "n") %>%
  filter(n > 1)
if (nrow(duplicate_bg_years) > 0) {
  stop("Block group controls contain duplicate GEOID-year assignments.", call. = FALSE)
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
    avg_home_value = sum(agg_value, na.rm = TRUE) / sum(owner_occ, na.rm = TRUE),
    avg_rent = sum(agg_rent, na.rm = TRUE) / sum(renter_occ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(pop_total > 0) %>%
  arrange(ward, year)

write_csv(ward_controls, "../output/ward_controls_2000_2023.csv")
message("Ward Panel saved to: ../output/ward_controls_2000_2023.csv")

message("Done! Ward Panel Created.")
