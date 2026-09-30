# setwd("tasks/audits/alderman_turnover_volumes/code")
# Zoning map amendments and building permits by ward and council term, 2003--2023, one row per ward and term: the
# alderman who held the ward for most of the term (create_alderman_data/adjudication/alderman_terms.csv), its side of
# the city, and counts of applications (amendments not filed by an alderman, by the ward their site lies in), the
# alderman's own amendments and own downzonings (by the ward they were filed from), high-discretion permits, new
# construction permits, and new residential buildings with their dwelling units (and the sum of each building's units
# squared, for the counting noise of units, which come in buildings of very different sizes). Buildings are those of
# tasks/prepare_permit_construction (every building of a new construction permit issued 2006--2022 and those the
# Assessor records without one), dated by their first permit's issue date or the year built, in the ward containing
# them on the map in force then; the 14 without a unit count are counted as buildings only. Amendments are the
# Journals' before 2011 and eLMS's from 2011 (tasks/audits/zoning_stalls_and_delays); permits are the high-discretion
# permits of 2006--2022 with their wards (tasks/data_for_alderman_uncertainty_index), dated by the month of
# application, so the first and last terms are covered in part. Wards follow the map in force: the 2003 map for the
# terms of 2003--2015, the 2015 map for those of 2015--2023 (the permits' map_version 1 and 2). A ward's side is that
# of the community area most of its permits lie in (sources/community_area_sides.csv, the conventional nine sides).
council_terms <- as.Date(c("2003-05-05", "2007-05-21", "2011-05-16", "2015-05-18", "2019-05-20", "2023-05-15"))
elms_start <- as.Date("2011-01-01")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

term_of <- function(date) findInterval(date, council_terms)
ward_terms <- tidyr::expand_grid(term = seq_len(length(council_terms) - 1), ward = 1:50) |>
  mutate(term_start = council_terms[term], map_version = if_else(term_start < as.Date("2015-05-18"), 1, 2))

terms <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE)
# For each council term, every alderman who held each ward during it, and the days held.
seats <- bind_rows(lapply(seq_len(length(council_terms) - 1), function(t) {
  filter(ward_terms, term == t) |>
    inner_join(terms, by = "ward", relationship = "one-to-many") |>
    mutate(days = as.numeric(pmin(end_date, council_terms[t + 1] - 1) - pmax(start_date, term_start)) + 1) |>
    filter(days > 0)
})) |>
  arrange(term, ward, desc(days)) |>
  summarise(alderman = first(alderman), aldermen = n(), .by = c(term, ward))
stopifnot(nrow(seats) == nrow(ward_terms))

amendments <- read_csv("../input/zoning_amendments.csv", show_col_types = FALSE) |>
  filter(source == "journals" & introduction_date < elms_start | source == "elms" & introduction_date >= elms_start,
    introduction_date >= min(council_terms), introduction_date < max(council_terms), !is.na(ward)) |>
  mutate(term = term_of(introduction_date))
permits <- read_csv("../input/permits_for_uncertainty_index.csv", show_col_types = FALSE,
  col_select = c(id, ward, ca_id, month, map_version, permit_type_clean)) |>
  mutate(term = term_of(as.Date(paste("01", month), "%d %b %Y")))
stopifnot(!anyNA(permits$term), all(permits$term >= 1))
buildings <- read_csv("../input/permit_construction.csv", show_col_types = FALSE,
  col_select = c(building_id, construction_date, ward, dwelling_units)) |>
  mutate(term = term_of(construction_date)) |>
  filter(term >= 1, term < length(council_terms))
stopifnot(!anyDuplicated(buildings$building_id), !anyNA(buildings$ward))

sides <- read_csv("../input/community_area_sides.csv", show_col_types = FALSE)
ward_sides <- permits |>
  count(map_version, ward, ca_id) |>
  slice_max(n, n = 1, with_ties = FALSE, by = c(map_version, ward)) |>
  inner_join(sides, by = c(ca_id = "community_area_number"), relationship = "many-to-one") |>
  select(map_version, ward, side)
stopifnot(nrow(ward_sides) == 100)

counts <- bind_rows(
  amendments |> filter(filer == "applicant") |> count(term, ward) |> mutate(measure = "applications"),
  amendments |> filter(filer == "alderman") |> count(term, ward) |> mutate(measure = "own_amendments"),
  amendments |> filter(filer == "alderman", direction == "down") |> count(term, ward) |>
    mutate(measure = "own_downzonings"),
  permits |> count(term, ward) |> mutate(measure = "permits"),
  permits |> filter(permit_type_clean == "new_construction") |> count(term, ward) |>
    mutate(measure = "new_construction")
) |>
  tidyr::pivot_wider(names_from = measure, values_from = n) |>
  full_join(buildings |> summarise(new_buildings = n(), new_units = sum(dwelling_units, na.rm = TRUE),
    new_units_squared = sum(dwelling_units^2, na.rm = TRUE), .by = c(term, ward)),
    by = c("term", "ward"), relationship = "one-to-one")
ward_term_counts <- ward_terms |>
  left_join(seats, by = c("term", "ward"), relationship = "one-to-one") |>
  left_join(ward_sides, by = c("map_version", "ward"), relationship = "many-to-one") |>
  left_join(counts, by = c("term", "ward"), relationship = "one-to-one") |>
  mutate(across(c(applications, own_amendments, own_downzonings, permits, new_construction, new_buildings, new_units,
    new_units_squared), ~ coalesce(.x, 0))) |>
  select(term_start, ward, map_version, side, alderman, aldermen, applications, own_amendments, own_downzonings,
    permits, new_construction, new_buildings, new_units, new_units_squared)
SaveData(ward_term_counts, c("term_start", "ward"), "../output/ward_term_counts.csv")
