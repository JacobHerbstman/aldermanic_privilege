# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_analysis_data/code")
# adjacent_year_window <- 1
# coincident_distance_ft <- 1
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(adjacent_year_window, coincident_distance_ft)
stopifnot(length(args) == 2L)
adjacent_year_window <- as.integer(args[1])
coincident_distance_ft <- as.numeric(args[2])

projects <- read_csv("../output/construction_regressors.csv", col_types = cols(
  project_id = "c", component_pins = "c", ward_pair = "c", segment_id = "c", zoning_group = "c", zoning_source = "c", zoning_note = "c", zoning_year = "i", .default = col_guess()))
validated <- read_csv("../input/historical_zoning_project_construction_year.csv", col_types = cols(pin = "c")) |>
  transmute(component_pin = pin, validated_year = as.integer(construction_year),
    validated_group = construction_zone_group, longitude, latitude)
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(validated$component_pin))

# Use construction-year zoning already established for a component parcel.
components <- projects |> select(project_id, component_pins, construction_year) |>
  separate_longer_delim(component_pins, delim = "/") |>
  left_join(validated, by = c("component_pins" = "component_pin"), relationship = "many-to-one") |>
  mutate(exact = construction_year == validated_year, adjacent = abs(construction_year - validated_year) <= adjacent_year_window) |>
  group_by(project_id) |> summarise(
    exact_count = n_distinct(validated_group[exact], na.rm = TRUE),
    adjacent_count = n_distinct(validated_group[adjacent], na.rm = TRUE),
    exact_group = paste(sort(unique(validated_group[exact])), collapse = ";"),
    adjacent_group = paste(sort(unique(validated_group[adjacent])), collapse = ";"),
    adjacent_first_year = if (any(adjacent, na.rm = TRUE)) min(validated_year[adjacent %in% TRUE]) else NA_integer_,
    adjacent_last_year = if (any(adjacent, na.rm = TRUE)) max(validated_year[adjacent %in% TRUE]) else NA_integer_,
    .groups = "drop")

# Compare official zoning maps where a component has no construction-year record.
zone_group <- function(code) {
  code <- str_to_upper(code)
  case_when(
    is.na(code) | str_trim(code) == "" ~ NA_character_,
    str_detect(code, "^RS-?") ~ "Single-Family Residential",
    str_detect(code, "^(RT|RM)-?") ~ "Multi-Family Residential",
    str_detect(code, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    str_detect(code, "^C-?[1-7]-") ~ "Commercial",
    str_detect(code, "^M-?[1-7]-") ~ "Industrial",
    str_detect(code, "^(DX|DR|DS|DC)-") ~ "Downtown",
    str_starts(code, "PD") ~ "Planned Development",
    str_starts(code, "PMD") ~ "Planned Manufacturing",
    str_starts(code, "POS") ~ "Open Space",
    TRUE ~ "Other")
}
zoning_2006 <- st_read("../input/historical_zoning_2006_candidate.gpkg", quiet = TRUE) |>
  transmute(group_2006 = candidate_zone_group_2006) |> st_transform(3435)
zoning_2012 <- st_read("/vsizip/../input/zoning_nov2012.zip/Zoning_nov2012.shp", quiet = TRUE) |>
  transmute(group_2012 = zone_group(ZONE_CLASS)) |> st_transform(3435)
zoning_2014 <- st_read("/vsizip/../input/zoning_sep2014.zip/Zoning.shp", quiet = TRUE) |>
  transmute(group_2014 = zone_group(ZONE_CLASS)) |> st_transform(3435)
zoning_2016 <- st_read("/vsizip/../input/zoning_jan2016.zip/zoning_2016_01.shp", quiet = TRUE) |>
  transmute(group_2016 = zone_group(ZONE_CLASS)) |> st_transform(3435)
zoning_2025 <- st_read("../input/zoning_sep2025.geojson", quiet = TRUE) |>
  transmute(group_2025 = zone_group(zone_class), ordinance_date = as.Date(ordinance_1)) |> st_transform(3435)
points <- st_as_sf(projects, coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE) |>
  st_join(zoning_2006, largest = TRUE) |> st_join(zoning_2012, largest = TRUE) |>
  st_join(zoning_2014, largest = TRUE) |> st_join(zoning_2016, largest = TRUE) |>
  st_join(zoning_2025, largest = TRUE)
stopifnot(nrow(points) == nrow(projects), !anyDuplicated(points$project_id))
validated_points <- st_as_sf(validated, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
nearest <- st_nearest_feature(points, validated_points)
points$nearest_year <- validated$validated_year[nearest]
points$nearest_group <- validated$validated_group[nearest]
points$nearest_distance <- as.numeric(st_distance(points, validated_points[nearest, ], by_element = TRUE))

zoning <- st_drop_geometry(points) |>
  left_join(components, by = "project_id", relationship = "one-to-one") |>
  mutate(construction_date = as.Date(construction_date),
    adjacent_first_date = as.Date(if_else(is.na(adjacent_first_year), NA_character_, paste0(adjacent_first_year, "-06-15"))),
    adjacent_last_date = as.Date(if_else(is.na(adjacent_last_year), NA_character_, paste0(adjacent_last_year, "-06-15"))),
    nearest_date = as.Date(paste0(nearest_year, "-06-15")),
    adjacent_event = coalesce(ordinance_date > pmin(construction_date, adjacent_first_date) &
      ordinance_date <= pmax(construction_date, adjacent_last_date), FALSE),
    nearest_event = coalesce(ordinance_date > pmin(construction_date, nearest_date) &
      ordinance_date <= pmax(construction_date, nearest_date), FALSE),
    coincident_group = if_else(nearest_distance <= coincident_distance_ft & abs(construction_year - nearest_year) <= adjacent_year_window &
      !nearest_event, nearest_group, NA_character_),
    stable_group = case_when(
      construction_year == 2006 ~ group_2006,
      construction_year <= 2012 & group_2006 == group_2012 ~ group_2006,
      construction_year <= 2014 & group_2012 == group_2014 ~ group_2012,
      construction_year == 2015 & group_2014 == group_2016 ~ group_2014,
      construction_year >= 2016 & group_2016 == group_2025 ~ group_2016),
    preceding_group = case_when(construction_year <= 2012 ~ group_2006,
      construction_year <= 2014 ~ group_2012, construction_year == 2015 ~ group_2014,
      construction_year >= 2016 ~ group_2016),
    later_group = if_else(construction_year <= 2012 & is.na(preceding_group) &
      group_2012 == group_2014 & group_2014 == group_2016 &
      (is.na(ordinance_date) | ordinance_date > construction_date), group_2012, NA_character_),
    zoning_assignment_source = case_when(
      !is.na(zoning_group) & zoning_year == construction_year ~ paste0("recorded_corrected_year:", zoning_source),
      exact_count == 1 ~ "validated_component_exact_year",
      exact_count == 0 & adjacent_count == 1 & !adjacent_event ~ "validated_component_adjacent_year",
      !is.na(coincident_group) ~ "coincident_validated_project",
      !is.na(stable_group) ~ "stable_official_snapshot_interval",
      !is.na(ordinance_date) & ordinance_date <= construction_date ~ "current_polygon_last_event_preconstruction",
      !is.na(preceding_group) ~ "preceding_official_snapshot",
      !is.na(later_group) ~ "stable_later_snapshots_missing_2006_polygon",
      TRUE ~ "unresolved_snapshot_change"),
    construction_zone_group = case_when(
      str_starts(zoning_assignment_source, "recorded_corrected_year:") ~ zoning_group,
      zoning_assignment_source == "validated_component_exact_year" ~ exact_group,
      zoning_assignment_source == "validated_component_adjacent_year" ~ adjacent_group,
      zoning_assignment_source == "coincident_validated_project" ~ coincident_group,
      zoning_assignment_source == "stable_official_snapshot_interval" ~ stable_group,
      zoning_assignment_source == "current_polygon_last_event_preconstruction" ~ group_2025,
      zoning_assignment_source == "preceding_official_snapshot" ~ preceding_group,
      zoning_assignment_source == "stable_later_snapshots_missing_2006_polygon" ~ later_group)) |>
  select(project_id, source_family, construction_year, within_500ft, construction_zone_group, zoning_assignment_source)

projects <- projects |>
  left_join(zoning |> select(project_id, zone_group = construction_zone_group, zoning_assignment_source),
    by = "project_id", relationship = "one-to-one") |>
  mutate(density_eligible = allow_far & allow_dupac & is.finite(density_far) & density_far > 0 &
    is.finite(density_dupac) & density_dupac > 0)
stopifnot(!anyNA(projects$density_eligible),
  !any(projects$within_500ft & (projects$allow_far | projects$allow_dupac) & (is.na(projects$zone_group) | projects$zone_group == "")))
SaveData(projects |> arrange(construction_year, project_id), "project_id", "../output/new_construction_analysis_data.csv", na = "")
