# setwd("tasks/assign_zoning_amendment_wards/code")
# The ward and alderman of each zoning map amendment: the geocoded title address placed on the ward map in force on
# the amendment's introduction date (canonical_boundary_year_from_date: the 2003 map until May 18, 2015, the 2015 map
# until May 15, 2023, then the 2023 map), and the alderman serving that ward that day (hand-built terms in
# tasks/create_alderman_data, recorded through June 24, 2025). An amendment filed by an alderman without a geocoded
# address takes the filing ward. For alderman-filed amendments with both, the two wards are compared.
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")
sf_use_s2(FALSE)

amendments <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  select(matter_id, introduction_date, filed_by_alderman, filing_ward)
queries <- read_csv("../output/zoning_amendment_address_queries.csv", show_col_types = FALSE)
# Addresses without a match (No_Match, Tie) come back with three fields rather than eight, which readr reports as
# parsing problems; the check below stops on any other short row.
responses <- suppressWarnings(read_csv("../output/census_geocoder_responses.csv", col_names = c("address_id",
    "input_address", "match", "match_type", "matched_address", "coordinates", "tiger_line_id", "side"),
  col_types = cols(.default = col_character())))
short_rows <- readr::problems(responses)$row
stopifnot(all(responses$match[short_rows] %in% c("No_Match", "Tie")))
stopifnot(!anyDuplicated(responses$address_id), setequal(as.integer(responses$address_id),
  queries$address_id[!is.na(queries$address_id)]))
geocodes <- responses |>
  transmute(address_id = as.integer(address_id), geocode_match = match, geocode_match_type = match_type,
    matched_address, longitude = as.numeric(sub(",.*", "", coordinates)),
    latitude = as.numeric(sub(".*,", "", coordinates)))

points <- queries |>
  left_join(geocodes, by = "address_id", relationship = "many-to-one") |>
  inner_join(amendments, by = "matter_id", relationship = "one-to-one") |>
  mutate(map_year = canonical_boundary_year_from_date(introduction_date),
    geocoded = geocode_match %in% "Match" & is.finite(longitude) & is.finite(latitude))
stopifnot(nrow(points) == nrow(amendments), !anyNA(points$map_year), all(points$map_year >= 2003L))

# Ward maps, as the paper's ward panel reads them (the 2003 map's OUT polygons are not wards).
ward_maps <- bind_rows(
  st_read("../input/Wards_2014.geojson", quiet = TRUE) |> filter(ward != "OUT") |> transmute(map_year = 2003L, ward),
  st_read("../input/Wards_2015.geojson", quiet = TRUE) |> transmute(map_year = 2015L, ward),
  st_read("../input/Wards_2024.geojson", quiet = TRUE) |> transmute(map_year = 2024L, ward)
) |>
  st_transform(3435) |>
  st_make_valid() |>
  mutate(ward = as.integer(ward))
located <- points |>
  filter(geocoded) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3435)
located_wards <- bind_rows(lapply(c(2003L, 2015L, 2024L), function(year) {
  joined <- st_join(filter(located, map_year == year), filter(ward_maps, map_year == year) |> select(geocoded_ward = ward),
    join = st_within)
  st_drop_geometry(joined) |> select(matter_id, geocoded_ward)
}))
stopifnot(!anyDuplicated(located_wards$matter_id))

terms <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE)
wards <- points |>
  left_join(located_wards, by = "matter_id", relationship = "one-to-one") |>
  mutate(
    ward = coalesce(geocoded_ward, filing_ward),
    ward_source = case_when(!is.na(geocoded_ward) ~ "geocoded_address",
      !is.na(filing_ward) ~ "filing_ward",
      geocoded ~ "geocoded_outside_wards",
      !is.na(address_query) ~ "address_not_matched",
      TRUE ~ "no_address_in_title"),
    filing_ward_agrees = if_else(filed_by_alderman & !is.na(geocoded_ward), geocoded_ward == filing_ward, NA))
serving <- wards |>
  filter(!is.na(ward)) |>
  select(matter_id, ward, introduction_date) |>
  inner_join(terms, by = join_by(ward, between(introduction_date, start_date, end_date)), relationship = "many-to-one") |>
  select(matter_id, alderman)
stopifnot(!anyDuplicated(serving$matter_id))

wards <- wards |>
  left_join(serving, by = "matter_id", relationship = "one-to-one") |>
  select(matter_id, introduction_date, address_query, geocode_match, geocode_match_type, matched_address, map_year,
    ward, ward_source, filing_ward_agrees, alderman)
SaveData(wards, "matter_id", "../output/zoning_amendment_wards.csv")
