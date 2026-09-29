# setwd("tasks/explore_alderman_measures/code")
# Exploratory: do builders and applicants locate on the more lenient side of ward boundaries? For each boundary and
# pair of aldermen serving on its two sides at the same time (a cell), count what lies within 500 ft (and 250 ft) of the
# boundary on each side, and relate a side's count to how much stricter its alderman is than the other side's on each
# measure in alderman_measures.csv (compare_alderman_measures.R), in standard deviations across aldermen. Poisson with
# cell fixed effects: the coefficient is the log difference in counts between the sides per standard deviation.
# Counts: new residential buildings of 2006-2022 (tasks/new_construction_analysis_data), placed with the aldermen
# serving on the construction date; zoning map amendments of 2010-2026, placed with the aldermen serving on the
# introduction date and the nearest other ward on the map then in force.
# Placebo lines: the same comparison at lines 500, 750 and 1,000 ft inside either ward, where both sides have the same
# alderman; the side nearer the real boundary is given the other ward's alderman.
# A second stall measure uses only applications more than 500 ft from any other ward, so none of the applications
# counted at the real boundaries feed into it (those around the placebo lines can). Beyond 1,000 ft too few
# applications remain to separate aldermen from noise. It follows
# tasks/estimate_alderman_zoning_measures: applications introduced before the current term, adjusted for introduction
# year, kind of change and months left in the term, averaged by alderman and shrunk by empirical Bayes.
half_widths_ft <- c(500, 250)
offsets_ft <- c(-1000, -750, -500, 0, 500, 750, 1000)
away_from_boundary_ft <- 500
current_term_start <- as.Date("2023-05-15")
term_starts <- as.Date(c("2011-05-16", "2015-05-18", "2019-05-20", "2023-05-15", "2027-05-17"))
months_left_breaks <- c(0, 6, 12, 24, 48)

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
sf_use_s2(FALSE)

# Zoning map amendments: distance to and identity of the nearest other ward on the map in force.
ward_maps <- bind_rows(
  st_read("../input/Wards_2014.geojson", quiet = TRUE) |> filter(ward != "OUT") |> transmute(map_year = 2003L, ward),
  st_read("../input/Wards_2015.geojson", quiet = TRUE) |> transmute(map_year = 2015L, ward),
  st_read("../input/Wards_2024.geojson", quiet = TRUE) |> transmute(map_year = 2024L, ward)
) |>
  st_transform(3435) |>
  st_make_valid() |>
  mutate(ward = as.integer(ward))
located <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  select(matter_id, introduction_date, filed_by_alderman, direction, outcome) |>
  inner_join(read_csv("../input/zoning_amendment_wards.csv", show_col_types = FALSE) |>
    select(matter_id, longitude, latitude, map_year, ward, alderman), by = "matter_id", relationship = "one-to-one") |>
  filter(is.finite(longitude), is.finite(latitude), !is.na(ward), !is.na(alderman)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3435)
located$neighbor_ward <- NA_integer_
located$distance_ft <- NA_real_
for (i in seq_len(nrow(ward_maps))) {
  rows <- which(located$map_year == ward_maps$map_year[i] & located$ward == ward_maps$ward[i])
  if (length(rows) == 0) next
  others <- ward_maps[ward_maps$map_year == ward_maps$map_year[i] & ward_maps$ward != ward_maps$ward[i], ]
  nearest <- st_nearest_feature(located[rows, ], others)
  located$neighbor_ward[rows] <- others$ward[nearest]
  located$distance_ft[rows] <- as.numeric(st_distance(located[rows, ], others[nearest, ], by_element = TRUE))
}
located <- st_drop_geometry(located)

# Stall rates from applications away from boundaries.
away <- located |>
  filter(!filed_by_alderman, outcome != "pending", introduction_date < current_term_start,
    distance_ft > away_from_boundary_ft) |>
  mutate(stalled = as.numeric(outcome == "stalled"), introduction_year = as.integer(format(introduction_date, "%Y")),
    months_left_in_term = cut(as.numeric(term_starts[findInterval(introduction_date, term_starts) + 1L] -
      introduction_date) / 30.44, months_left_breaks))
away$adjusted <- stats::residuals(fixest::feols(stalled ~ 1 | introduction_year + direction + months_left_in_term,
  data = away, notes = FALSE))
stall_away <- away |>
  summarise(applications = n(), effect = mean(adjusted), .by = alderman) |>
  mutate(variance = stats::var(away$adjusted) / applications)
signal_variance <- max(stats::var(stall_away$effect) - mean(stall_away$variance), 0)
stall_away <- mutate(stall_away, stall_rate_away = signal_variance / (signal_variance + variance) * effect)
SaveData(select(stall_away, alderman, applications, stall_rate_away), "alderman",
  "../output/stall_rate_away_from_boundaries.csv")

measures <- read_csv("../output/alderman_measures.csv", show_col_types = FALSE) |>
  select(-applications_decided, -processing_time_index_2014) |>
  left_join(select(stall_away, alderman, stall_rate_away), by = "alderman", relationship = "one-to-one") |>
  tidyr::pivot_longer(-alderman, names_to = "measure", values_to = "value") |>
  filter(is.finite(value)) |>
  mutate(value = (value - mean(value)) / sd(value), .by = measure)

# One row per building or amendment within 1,500 ft of a boundary: its cell, the cell's first (lower-numbered) and
# second ward's aldermen, and its signed distance, positive inside the first ward.
buildings <- read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_select = c(construction_year, distance_to_boundary_ft, ward, neighbor_ward, ward_pair, joint_service,
    alderman_own, alderman_neighbor, multifamily, dwelling_units)) |>
  filter(construction_year >= 2006, construction_year <= 2022, !is.na(joint_service)) |>
  transmute(cell = joint_service, ward_pair,
    first_alderman = if_else(ward < neighbor_ward, alderman_own, alderman_neighbor),
    second_alderman = if_else(ward < neighbor_ward, alderman_neighbor, alderman_own),
    signed_ft = if_else(ward < neighbor_ward, 1, -1) * distance_to_boundary_ft, buildings = 1,
    multifamily_buildings = as.numeric(multifamily & coalesce(dwelling_units, 0) >= 2),
    dwelling_units = coalesce(dwelling_units, 0))
terms <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE)
amendments <- located |>
  filter(distance_ft < 1500) |>
  left_join(terms |> rename(neighbor_ward = ward, other = alderman),
    by = join_by(neighbor_ward, between(introduction_date, start_date, end_date)), relationship = "many-to-one") |>
  filter(!is.na(other)) |>
  transmute(cell = paste(map_year, pmin(ward, neighbor_ward), pmax(ward, neighbor_ward), pmin(alderman, other),
    pmax(alderman, other), sep = ":"), ward_pair = paste(map_year, pmin(ward, neighbor_ward), pmax(ward, neighbor_ward)),
    first_alderman = if_else(ward < neighbor_ward, alderman, other),
    second_alderman = if_else(ward < neighbor_ward, other, alderman),
    signed_ft = if_else(ward < neighbor_ward, 1, -1) * distance_ft,
    zoning_applications = as.numeric(!filed_by_alderman),
    upzoning_applications = as.numeric(!filed_by_alderman & direction == "up"),
    alderman_downzonings = as.numeric(filed_by_alderman & direction == "down"))

# Counts on the two sides of a line at offset_ft (0 is the boundary), within half_width_ft of it. The side beyond the
# line, away from the second ward, takes the first ward's alderman.
side_counts <- function(rows, outcomes, offset_ft, half_width_ft) {
  window <- rows |>
    filter(abs(signed_ft - offset_ft) < half_width_ft) |>
    mutate(side = if_else(signed_ft >= offset_ft, first_alderman, second_alderman))
  cells <- distinct(window, cell, ward_pair, first_alderman, second_alderman)
  stopifnot(!anyDuplicated(cells$cell))
  bind_rows(transmute(cells, cell, ward_pair, side = first_alderman, other = second_alderman),
    transmute(cells, cell, ward_pair, side = second_alderman, other = first_alderman)) |>
    left_join(summarise(window, across(all_of(outcomes), sum), .by = c(cell, side)), by = c("cell", "side"),
      relationship = "one-to-one") |>
    mutate(across(all_of(outcomes), \(x) coalesce(x, 0)))
}
panels <- list(
  list(outcomes = c("buildings", "multifamily_buildings", "dwelling_units"), rows = buildings),
  list(outcomes = c("zoning_applications", "upzoning_applications", "alderman_downzonings"), rows = amendments))
results <- bind_rows(lapply(panels, function(panel) {
  tidyr::expand_grid(offset_ft = offsets_ft, within_ft = half_widths_ft) |>
    mutate(result = purrr::map2(offset_ft, within_ft, function(offset, width) {
      counts <- side_counts(panel$rows, panel$outcomes, offset, width)
      tidyr::expand_grid(outcome = panel$outcomes, measure = unique(measures$measure)) |>
        mutate(result = purrr::map2(outcome, measure, function(y, m) {
          scored <- filter(measures, measure == m)
          data <- counts |>
            inner_join(select(scored, side = alderman, side_value = value), by = "side", relationship = "many-to-one") |>
            inner_join(select(scored, other = alderman, other_value = value), by = "other",
              relationship = "many-to-one") |>
            mutate(stricter_by = side_value - other_value)
          fit <- fixest::fepois(stats::as.formula(paste(y, "~ stricter_by | cell")), data = data, cluster = ~ward_pair,
            notes = FALSE, warn = FALSE)
          tibble(estimate = coef(fit)[["stricter_by"]], std_error = fixest::se(fit)[["stricter_by"]],
            p_value = fixest::pvalue(fit)[["stricter_by"]], ward_pairs = n_distinct(data$ward_pair[data[[y]] > 0]),
            cells = n_distinct(data$cell[data[[y]] > 0]),
            count = sum(data[[y]]))
        })) |>
        tidyr::unnest(result)
    })) |>
    tidyr::unnest(result)
}))
SaveData(select(results, outcome, measure, within_ft, offset_ft, everything()),
  c("outcome", "measure", "within_ft", "offset_ft"), "../output/boundary_side_counts.csv")
