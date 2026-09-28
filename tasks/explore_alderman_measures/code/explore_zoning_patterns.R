# setwd("tasks/explore_alderman_measures/code")
# Exploratory patterns in the zoning map amendments of 2010-2026 (tasks/clean_zoning_map_amendments, wards and
# aldermen from tasks/assign_zoning_amendment_wards): citywide trends, outcomes by ward demographics and by distance to
# the ward boundary, and how much of the variation in outcomes the serving alderman explains beyond the ward.
# Applications are amendments not filed by an alderman; stalls are known only for those introduced before the current
# council term. Terms begin on the dates below; applications introduced in a term's last months lapse more often.
current_term_start <- as.Date("2023-05-15")
term_starts <- as.Date(c("2011-05-16", "2015-05-18", "2019-05-20", "2023-05-15", "2027-05-17"))
months_left_breaks <- c(0, 6, 12, 24, 48)
distance_breaks_ft <- c(0, 250, 500, 1000, 2000, Inf)

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
sf_use_s2(FALSE)

amendments <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  inner_join(read_csv("../input/zoning_amendment_wards.csv", show_col_types = FALSE) |>
    select(matter_id, longitude, latitude, map_year, ward, alderman), by = "matter_id", relationship = "one-to-one") |>
  mutate(introduction_year = as.integer(format(introduction_date, "%Y")),
    next_term_start = term_starts[findInterval(introduction_date, term_starts) + 1L],
    months_left_in_term = cut(as.numeric(next_term_start - introduction_date) / 30.44, months_left_breaks),
    stalled = if_else(introduction_date < current_term_start & outcome != "pending", as.numeric(outcome == "stalled"),
      NA_real_),
    log_days_to_passage = if_else(outcome == "passed" & days_to_passage > 0, log(pmax(days_to_passage, 1)), NA_real_),
    upzoning = as.numeric(direction == "up"), downzoning = as.numeric(direction == "down"),
    to_planned_development = as.numeric(direction == "to_planned_development"),
    far_increase = if_else(is.finite(from_far) & is.finite(to_far), to_far - from_far, NA_real_),
    ward_in_map = paste(map_year, ward))
applications <- filter(amendments, !filed_by_alderman)

# 1. Citywide trends by year of introduction, and outcomes by who filed and the kind of change.
trends <- amendments |>
  summarise(amendments = n(), applications = sum(!filed_by_alderman), filed_by_aldermen = sum(filed_by_alderman),
    alderman_downzonings = sum(filed_by_alderman & direction == "down"),
    application_stall_rate = mean(stalled[!filed_by_alderman], na.rm = TRUE),
    median_days_to_passage = median(days_to_passage[!filed_by_alderman & outcome == "passed"], na.rm = TRUE),
    application_upzoning_share = mean(upzoning[!filed_by_alderman & direction != "unknown"]),
    application_planned_development_share = mean(to_planned_development[!filed_by_alderman & direction != "unknown"]),
    .by = introduction_year) |>
  arrange(introduction_year)
SaveData(trends, "introduction_year", "../output/zoning_trends_by_year.csv")
by_direction <- amendments |>
  summarise(amendments = n(), stall_rate = mean(stalled, na.rm = TRUE), passed_share = mean(outcome == "passed"),
    withdrawn_share = mean(outcome == "withdrawn"),
    median_days_to_passage = median(days_to_passage[outcome == "passed"], na.rm = TRUE),
    .by = c(filed_by_alderman, direction)) |>
  arrange(filed_by_alderman, direction)
SaveData(by_direction, c("filed_by_alderman", "direction"), "../output/zoning_outcomes_by_direction.csv")

# 2. Ward demographics (the index's ward controls for 2006-2022 permits, averaged by ward, year and map; map version 1
# is the 2003 map and 2 the 2015 map) and outcomes, with year, kind-of-change and months-left-in-term fixed effects for
# stalls and days; ward shares are in tenths and income in $10,000s.
ward_years <- read_csv("../input/permits_for_uncertainty_index.csv", show_col_types = FALSE,
  col_select = c(ward, year, map_version, share_white, share_black, share_hisp, median_hh_income, homeownership_rate)) |>
  summarise(across(c(share_white, share_black, share_hisp, median_hh_income, homeownership_rate), \(x) mean(x, na.rm = TRUE)),
    .by = c(ward, year, map_version)) |>
  mutate(map_year = c(2003L, 2015L)[map_version])
stopifnot(all(ward_years$map_version %in% 1:2))
with_wards <- applications |>
  left_join(select(ward_years, -map_version), by = c("ward", introduction_year = "year", "map_year"),
    relationship = "many-to-one") |>
  mutate(across(c(share_white, share_black, share_hisp, homeownership_rate), \(x) 10 * x),
    median_hh_income = median_hh_income / 1e4)
demographic_fits <- tidyr::expand_grid(outcome = c("stalled", "log_days_to_passage", "upzoning", "far_increase"),
    ward_characteristic = c("share_white", "share_black", "share_hisp", "median_hh_income", "homeownership_rate")) |>
  mutate(result = purrr::map2(outcome, ward_characteristic, function(y, x) {
    fe <- if (y %in% c("stalled", "log_days_to_passage")) "introduction_year + direction + months_left_in_term" else
      "introduction_year"
    fit <- fixest::feols(stats::as.formula(sprintf("%s ~ %s | %s", y, x, fe)), data = with_wards, cluster = ~ward_in_map,
      notes = FALSE, warn = FALSE)
    tibble(estimate = unname(coef(fit)[x]), std_error = unname(fixest::se(fit)[x]),
      p_value = unname(fixest::pvalue(fit)[x]), applications = stats::nobs(fit))
  })) |>
  tidyr::unnest(result)
SaveData(demographic_fits, c("outcome", "ward_characteristic"), "../output/zoning_outcomes_by_ward_demographics.csv")

# 3. Distance to the nearest other ward, on the map in force at introduction. Aldermen may favor projects whose
# effects fall on their own constituents, so outcomes may differ near ward boundaries.
ward_maps <- bind_rows(
  st_read("../input/Wards_2014.geojson", quiet = TRUE) |> filter(ward != "OUT") |> transmute(map_year = 2003L, ward),
  st_read("../input/Wards_2015.geojson", quiet = TRUE) |> transmute(map_year = 2015L, ward),
  st_read("../input/Wards_2024.geojson", quiet = TRUE) |> transmute(map_year = 2024L, ward)
) |>
  st_transform(3435) |>
  st_make_valid() |>
  mutate(ward = as.integer(ward))
located <- applications |>
  filter(is.finite(longitude), is.finite(latitude), !is.na(ward)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3435)
located$distance_to_boundary_ft <- NA_real_
for (i in seq_len(nrow(ward_maps))) {
  rows <- which(located$map_year == ward_maps$map_year[i] & located$ward == ward_maps$ward[i])
  if (length(rows) == 0) next
  others <- st_union(ward_maps[ward_maps$map_year == ward_maps$map_year[i] & ward_maps$ward != ward_maps$ward[i], ])
  located$distance_to_boundary_ft[rows] <- as.numeric(st_distance(located[rows, ], others))
}
by_distance <- st_drop_geometry(located) |>
  mutate(distance_band_ft = cut(distance_to_boundary_ft, distance_breaks_ft, right = FALSE)) |>
  summarise(applications = n(), stall_rate = mean(stalled, na.rm = TRUE),
    median_days_to_passage = median(days_to_passage[outcome == "passed"], na.rm = TRUE),
    upzoning_share = mean(upzoning[direction != "unknown"]), mean_far_increase = mean(far_increase, na.rm = TRUE),
    .by = distance_band_ft) |>
  arrange(distance_band_ft)
SaveData(by_distance, "distance_band_ft", "../output/zoning_outcomes_by_boundary_distance.csv")

# 4. How much the serving alderman explains beyond the ward: fit with ward-on-map fixed effects, then add alderman
# fixed effects (identified where a ward changed aldermen), comparing R-squared and testing the alderman terms.
alderman_share <- purrr::map_dfr(c("stalled", "log_days_to_passage", "upzoning", "far_increase"), function(y) {
  data <- filter(applications, is.finite(.data[[y]]), !is.na(alderman), !is.na(ward))
  base <- "introduction_year + direction + months_left_in_term"
  if (y %in% c("upzoning", "far_increase")) base <- "introduction_year + months_left_in_term"
  ward_only <- fixest::feols(stats::as.formula(sprintf("%s ~ 1 | %s + ward_in_map", y, base)), data = data, notes = FALSE)
  with_alderman <- fixest::feols(stats::as.formula(sprintf("%s ~ 1 | %s + ward_in_map + alderman", y, base)), data = data,
    notes = FALSE)
  # Identified fixed effects: levels less the references fixest sets, so aldermen who are the only alderman of their
  # ward on a map add nothing (with several fixed effects the count of references is approximate).
  identified <- function(fit) {
    effects <- fixest::fixef(fit, notes = FALSE)
    sum(lengths(effects)) - sum(attr(effects, "references"))
  }
  rss_restricted <- sum(stats::residuals(ward_only)^2)
  rss_full <- sum(stats::residuals(with_alderman)^2)
  df_full <- stats::nobs(with_alderman) - identified(with_alderman)
  added_terms <- identified(with_alderman) - identified(ward_only)
  f <- ((rss_restricted - rss_full) / added_terms) / (rss_full / df_full)
  tibble(outcome = y, applications = nrow(data), wards_on_maps = length(unique(data$ward_in_map)),
    aldermen = length(unique(data$alderman)), added_alderman_terms = added_terms,
    r2_ward_only = fixest::r2(ward_only, "r2"), r2_with_alderman = fixest::r2(with_alderman, "r2"),
    f_statistic = f, p_value = stats::pf(f, added_terms, df_full, lower.tail = FALSE))
})
SaveData(alderman_share, "outcome", "../output/alderman_share_beyond_ward.csv")
