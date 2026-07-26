# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/ward_control_area_weighting/code")

source("../../../setup_environment/code/packages.R")
library(tigris)

if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("CENSUS_API_KEY not found in .Renviron.", call. = FALSE)
}
census_api_key(Sys.getenv("CENSUS_API_KEY"))
options(tigris_use_cache = TRUE)

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  st_transform(3435)

vars_acs <- c(
  tot_pop = "B01003_001",
  tot_hhs = "B11001_001",
  tot_units = "B25003_001",
  owner_occ = "B25003_002",
  renter_occ = "B25003_003",
  pop_white = "B03002_003",
  pop_black = "B03002_004",
  pop_hisp = "B03002_012",
  median_income = "B19013_001",
  pop_25plus = "B15003_001",
  educ_bach = "B15003_022",
  educ_mast = "B15003_023",
  educ_prof = "B15003_024",
  educ_doc = "B15003_025"
)

vars_2000 <- c(
  tot_pop = "P001001",
  tot_hhs = "P010001",
  tot_units = "H007001",
  owner_occ = "H007002",
  renter_occ = "H007003",
  pop_white = "P007003",
  pop_black = "P007004",
  pop_hisp = "P007010",
  median_income = "P053001",
  pop_25plus = "P037001",
  educ_bach_m = "P037015",
  educ_mast_m = "P037016",
  educ_prof_m = "P037017",
  educ_doc_m = "P037018",
  educ_bach_f = "P037032",
  educ_mast_f = "P037033",
  educ_prof_f = "P037034",
  educ_doc_f = "P037035"
)

vars_2010_sf1 <- c(
  tot_pop = "P001001",
  tot_hhs = "P018001",
  tot_units = "H004001",
  owner_mortgage = "H004002",
  owner_free_clear = "H004003",
  renter_occ = "H004004",
  pop_white = "P005003",
  pop_black = "P005004",
  pop_hisp = "P005010"
)

message("Fetching 2000 block-group data")
data_2000 <- get_decennial(
  geography = "block group",
  variables = vars_2000,
  state = "IL",
  county = "Cook",
  year = 2000,
  sumfile = "sf3",
  geometry = TRUE
) %>%
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

message("Fetching 2010 block-group data")
geo_2010 <- tigris::block_groups(
  state = "IL", county = "Cook", year = 2010, cb = FALSE
) %>%
  st_transform(3435) %>%
  select(GEOID = GEOID10, geometry)

data_2010_sf1 <- get_decennial(
  geography = "block group",
  variables = vars_2010_sf1,
  state = "IL",
  county = "Cook",
  year = 2010,
  geometry = FALSE
) %>%
  select(GEOID, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(owner_occ = owner_mortgage + owner_free_clear)

data_2013_econ <- get_acs(
  geography = "block group",
  variables = vars_acs,
  state = "IL",
  county = "Cook",
  year = 2013,
  survey = "acs5",
  geometry = FALSE
) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    educ_bach_plus = rowSums(
      across(c(educ_bach, educ_mast, educ_prof, educ_doc)),
      na.rm = TRUE
    )
  ) %>%
  select(GEOID, median_income, educ_bach_plus, pop_25plus)

data_2010 <- geo_2010 %>%
  left_join(data_2010_sf1, by = "GEOID", relationship = "one-to-one") %>%
  left_join(data_2013_econ, by = "GEOID", relationship = "one-to-one")

geo_2020 <- tigris::block_groups(
  state = "IL", county = "Cook", year = 2020, cb = FALSE
) %>%
  st_transform(3435) %>%
  select(GEOID, geometry)

allocate_block_groups <- function(block_groups, wards, year_value) {
  count_columns <- c(
    "tot_pop", "tot_hhs", "tot_units", "owner_occ", "renter_occ",
    "pop_white", "pop_black", "pop_hisp", "pop_25plus", "educ_bach_plus"
  )

  if (anyDuplicated(block_groups$GEOID) > 0 || anyDuplicated(wards$ward) > 0) {
    stop(sprintf("Duplicate block-group or ward keys in %s.", year_value), call. = FALSE)
  }

  block_groups <- block_groups %>%
    st_make_valid() %>%
    filter(!st_is_empty(.)) %>%
    mutate(block_group_area = as.numeric(st_area(geometry)))
  wards <- wards %>%
    select(ward) %>%
    st_make_valid()

  chicago_coverage <- suppressWarnings(
    st_intersection(
      block_groups %>% select(GEOID, block_group_area),
      st_union(wards)
    )
  ) %>%
    mutate(covered_area = as.numeric(st_area(geometry))) %>%
    st_drop_geometry() %>%
    group_by(GEOID) %>%
    summarise(
      covered_area_share = sum(covered_area) / first(block_group_area),
      .groups = "drop"
    )

  intersections <- suppressWarnings(
    st_intersection(
      block_groups %>%
        select(GEOID, all_of(count_columns), median_income, block_group_area),
      wards
    )
  ) %>%
    mutate(intersection_area = as.numeric(st_area(geometry))) %>%
    filter(intersection_area > 0) %>%
    mutate(area_share = intersection_area / block_group_area) %>%
    st_drop_geometry() %>%
    group_by(GEOID, ward) %>%
    summarise(
      across(all_of(count_columns), first),
      median_income = first(median_income),
      raw_area_share = sum(area_share),
      .groups = "drop"
    ) %>%
    left_join(chicago_coverage, by = "GEOID", relationship = "many-to-one") %>%
    group_by(GEOID) %>%
    mutate(
      raw_area_share_total = sum(raw_area_share),
      area_share = raw_area_share * covered_area_share / raw_area_share_total
    ) %>%
    ungroup()

  block_group_coverage <- intersections %>%
    group_by(GEOID) %>%
    summarise(
      ward_count = n(),
      covered_area_share = sum(area_share),
      raw_area_share = sum(raw_area_share),
      .groups = "drop"
    )

  if (any(block_group_coverage$covered_area_share > 1.001) ||
      any(!is.finite(block_group_coverage$covered_area_share))) {
    stop(sprintf("Invalid block-group coverage shares in %s.", year_value), call. = FALSE)
  }

  ward_controls <- intersections %>%
    mutate(
      across(all_of(count_columns), ~ .x * area_share),
      income_households = if_else(
        is.finite(median_income),
        tot_hhs,
        0
      ),
      income_total = if_else(
        is.finite(median_income),
        median_income * tot_hhs,
        0
      )
    ) %>%
    group_by(ward) %>%
    summarise(
      pop_total = sum(tot_pop, na.rm = TRUE),
      hh_total = sum(tot_hhs, na.rm = TRUE),
      hu_total = sum(tot_units, na.rm = TRUE),
      share_black = sum(pop_black, na.rm = TRUE) / pop_total,
      share_hisp = sum(pop_hisp, na.rm = TRUE) / pop_total,
      share_white = sum(pop_white, na.rm = TRUE) / pop_total,
      homeownership_rate = sum(owner_occ, na.rm = TRUE) / hu_total,
      share_bach_plus = sum(educ_bach_plus, na.rm = TRUE) /
        sum(pop_25plus, na.rm = TRUE),
      median_hh_income = sum(income_total, na.rm = TRUE) /
        sum(income_households, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year = year_value, .after = ward)

  coverage <- tibble(
    year = year_value,
    block_groups_intersecting_chicago = nrow(block_group_coverage),
    block_groups_split_across_wards = sum(block_group_coverage$ward_count > 1),
    mean_covered_area_share = mean(block_group_coverage$covered_area_share),
    median_covered_area_share = median(block_group_coverage$covered_area_share),
    maximum_overlap_share = max(
      block_group_coverage$raw_area_share - block_group_coverage$covered_area_share
    )
  )

  list(controls = ward_controls, coverage = coverage)
}

controls_by_year <- list()
coverage_by_year <- list()

for (year_value in 2000:2023) {
  message(sprintf("Area-weighting block groups for %s", year_value))

  if (year_value <= 2009) {
    current_bgs <- data_2000
  } else if (year_value <= 2012) {
    current_bgs <- data_2010
  } else {
    current_data <- get_acs(
      geography = "block group",
      variables = vars_acs,
      state = "IL",
      county = "Cook",
      year = year_value,
      survey = "acs5",
      geometry = FALSE
    ) %>%
      select(GEOID, variable, estimate) %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      mutate(
        educ_bach_plus = rowSums(
          across(c(educ_bach, educ_mast, educ_prof, educ_doc)),
          na.rm = TRUE
        )
      )

    current_bgs <- if (year_value < 2020) {
      geo_2010 %>%
        left_join(current_data, by = "GEOID", relationship = "one-to-one")
    } else {
      geo_2020 %>%
        left_join(current_data, by = "GEOID", relationship = "one-to-one")
    }
  }

  current_wards <- ward_panel %>% filter(year == year_value)
  if (nrow(current_wards) == 0) {
    next
  }

  allocated <- allocate_block_groups(current_bgs, current_wards, year_value)
  controls_by_year[[as.character(year_value)]] <- allocated$controls
  coverage_by_year[[as.character(year_value)]] <- allocated$coverage
}

ward_controls <- bind_rows(controls_by_year) %>%
  arrange(ward, year)

if (anyDuplicated(ward_controls[c("ward", "year")]) > 0) {
  stop("Area-weighted controls are not unique by ward and year.", call. = FALSE)
}
if (nrow(ward_controls) != 50 * 24) {
  stop("Area-weighted controls do not contain all 50 wards for 2000-2023.", call. = FALSE)
}
if (any(!is.finite(as.matrix(ward_controls %>% select(-ward, -year))))) {
  stop("Area-weighted controls contain non-finite values.", call. = FALSE)
}
if (any(ward_controls$homeownership_rate < 0 | ward_controls$homeownership_rate > 1) ||
    any(ward_controls$share_bach_plus < 0 | ward_controls$share_bach_plus > 1) ||
    any(ward_controls$share_black < 0 | ward_controls$share_hisp < 0 |
      ward_controls$share_white < 0) ||
    any(ward_controls$share_black + ward_controls$share_hisp +
      ward_controls$share_white > 1 + 1e-8)) {
  stop("Area-weighted controls contain invalid shares.", call. = FALSE)
}

write_csv(ward_controls, "../output/ward_controls_area_weighted_2000_2023.csv")
write_csv(bind_rows(coverage_by_year), "../output/area_weighting_coverage.csv")
