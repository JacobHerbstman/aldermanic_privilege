# setwd("tasks/audits/alderman_turnover_volumes/code")
# New construction permitted in each ward of the 2024 map in the current council term against the same territory in
# the previous term, one row per ward. Permits (tasks/clean_building_permits, applications of 2006--2022 and
# 2023--September 2026) are placed in the 2024 ward containing them and counted by issue month, since the source lists
# issued permits only: the previous term from May 20, 2019 to May 14, 2023 and the current term from May 15, 2023
# through August 2026, the last full month issued. For each ward, new construction permits and their
# reported cost per year in each term, and the ward's change relative to the city's (1 means the ward grew as fast as
# the city). The alderman is the one holding the ward at the end of the current window
# (create_alderman_data/adjudication/alderman_terms.csv).
previous_term <- as.Date(c("2019-05-20", "2023-05-14"))
current_term <- as.Date(c("2023-05-15", "2026-08-31"))

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

query <- "SELECT id, permit_type, reported_cost, issue_date_ym, geom FROM building_permits_clean
  WHERE permit_type = 'PERMIT - NEW CONSTRUCTION'"
permits <- bind_rows(
  st_read("../input/building_permits_clean_2006_2022.gpkg", query = query, quiet = TRUE),
  st_read("../input/building_permits_clean_2023_2026.gpkg", query = query, quiet = TRUE)) |>
  mutate(issued = as.Date(issue_date_ym)) |>
  filter(issued >= previous_term[1], issued <= current_term[2])
stopifnot(!anyDuplicated(permits$id), st_crs(permits)$epsg == 3435)
wards <- st_read("../input/Wards_2024.geojson", quiet = TRUE) |>
  st_transform(3435) |>
  transmute(ward = as.integer(ward))
placed <- st_join(permits, wards, join = st_within) |>
  st_drop_geometry() |>
  filter(!is.na(ward)) |>
  mutate(term = if_else(issued < current_term[1], "previous", "current"))

years <- c(previous = as.numeric(diff(previous_term) + 1), current = as.numeric(diff(current_term) + 1)) / 365.25
by_ward <- placed |>
  summarise(permits = n(), cost = sum(reported_cost, na.rm = TRUE), .by = c(ward, term)) |>
  tidyr::pivot_wider(names_from = term, values_from = c(permits, cost), values_fill = 0) |>
  mutate(permits_per_year_previous = permits_previous / years[["previous"]],
    permits_per_year_current = permits_current / years[["current"]],
    cost_per_year_previous = cost_previous / years[["previous"]],
    cost_per_year_current = cost_current / years[["current"]])
city_permits <- sum(by_ward$permits_per_year_current) / sum(by_ward$permits_per_year_previous)
city_cost <- sum(by_ward$cost_per_year_current) / sum(by_ward$cost_per_year_previous)
aldermen <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE) |>
  filter(start_date <= current_term[2], end_date >= current_term[2]) |>
  select(ward, alderman)
current_term_permits <- tibble(ward = 1:50) |>
  left_join(aldermen, by = "ward", relationship = "one-to-one") |>
  left_join(by_ward, by = "ward", relationship = "one-to-one") |>
  mutate(across(c(permits_previous, permits_current, cost_previous, cost_current, permits_per_year_previous,
    permits_per_year_current, cost_per_year_previous, cost_per_year_current), ~ coalesce(.x, 0)),
    permits_change_vs_city = (permits_per_year_current / permits_per_year_previous) / city_permits,
    cost_change_vs_city = (cost_per_year_current / cost_per_year_previous) / city_cost) |>
  arrange(desc(permits_per_year_current))
SaveData(current_term_permits, "ward", "../output/current_term_permits_by_ward.csv")
