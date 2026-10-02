# setwd("tasks/audits/outcomes_through_2026/code")
# Exploratory: ward controls for the 2023 ward map, in effect from May 15, 2023. The latest ACS five-year block-group
# estimates in tasks/create_ward_controls are for 2022; they are allocated to the 2023-map wards by polygon overlap
# exactly as tasks/create_ward_controls/code/create_ward_controls.R allocates each year's estimates to that year's map
# (its allocation function and aggregation are copied below), and used for 2023-2026.
acs_year <- 2022L
map_2023_years <- 2023:2026

source("../../../setup_environment/code/packages.R")

acs <- read_csv("../input/census_acs.csv", show_col_types = FALSE,
  col_types = cols(GEOID = col_character(), .default = col_guess()))
geo_2020 <- st_read("../input/census_block_groups.gpkg", layer = "year2020", quiet = TRUE) %>% rename(geometry = geom)
wards_2023 <- st_read("../output/ward_panel_through_2026.gpkg", quiet = TRUE) %>%
  filter(year == 2024) %>%
  st_transform(3435) %>%
  rename(geometry = geom)
stopifnot(nrow(wards_2023) == 50)

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

y <- acs_year
current_data_raw <- acs %>% filter(source_year == y) %>% select(-source_year)
current_data <- current_data_raw %>%
      select(GEOID, variable, estimate) %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      mutate(
        educ_bach_plus = rowSums(across(c(educ_bach, educ_mast, educ_prof, educ_doc)), na.rm = TRUE)
      )

current_bgs <- geo_2020 %>% left_join(current_data, by = "GEOID", relationship = "one-to-one")

# Check: the same allocation on the 2015 map reproduces the production 2022 ward controls.
aggregate_controls <- function(final_bg_panel) {
  ward_controls <- final_bg_panel %>%
    group_by(ward, year) %>%
    summarize(
      pop_total = sum(tot_pop, na.rm = TRUE),
      hh_total = sum(tot_hhs, na.rm = TRUE),
      hu_total = sum(tot_units, na.rm = TRUE),
      share_black = sum(pop_black, na.rm = TRUE) / sum(tot_pop, na.rm = TRUE),
      share_hisp = sum(pop_hisp, na.rm = TRUE) / sum(tot_pop, na.rm = TRUE),
      share_white = sum(pop_white, na.rm = TRUE) / sum(tot_pop, na.rm = TRUE),
      homeownership_rate = sum(owner_occ, na.rm = TRUE) / sum(tot_units, na.rm = TRUE),
      share_bach_plus = sum(educ_bach_plus, na.rm = TRUE) / sum(pop_25plus, na.rm = TRUE),
      # Median income: household-weighted average of block group medians.
      median_hh_income = weighted.mean(median_income, tot_hhs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(pop_total > 0) %>%
    arrange(ward, year)
  stopifnot(all(is.finite(ward_controls$median_hh_income)), all(is.finite(ward_controls$homeownership_rate)),
    all(is.finite(ward_controls$share_bach_plus)), all(between(ward_controls$homeownership_rate, 0, 1)),
    all(between(ward_controls$share_bach_plus, 0, 1)),
    all(ward_controls$share_black + ward_controls$share_hisp + ward_controls$share_white <= 1 + 1e-8))
  ward_controls
}
wards_2015 <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  filter(year == acs_year) %>%
  st_transform(3435) %>%
  rename(geometry = geom)
check <- aggregate_controls(assign_block_groups_to_wards(current_bgs, wards_2015, acs_year)) %>%
  inner_join(read_csv("../input/ward_controls_2006_2022.csv", show_col_types = FALSE) %>% filter(year == acs_year),
    by = c("ward", "year"), suffix = c("", "_production"), relationship = "one-to-one")
stopifnot(nrow(check) == 50, isTRUE(all.equal(check$share_white, check$share_white_production)),
  isTRUE(all.equal(check$median_hh_income, check$median_hh_income_production)),
  isTRUE(all.equal(check$homeownership_rate, check$homeownership_rate_production)))

ward_controls <- aggregate_controls(assign_block_groups_to_wards(current_bgs, wards_2023, acs_year))

stopifnot(nrow(ward_controls) == 50)
ward_controls <- ward_controls %>%
  select(-year) %>%
  tidyr::crossing(year = map_2023_years) %>%
  mutate(era = "post_2023")
write_csv(ward_controls, "../output/ward_controls_2023_map.csv")
