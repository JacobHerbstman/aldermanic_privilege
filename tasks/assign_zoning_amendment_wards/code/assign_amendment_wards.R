# setwd("tasks/assign_zoning_amendment_wards/code")
# The ward and alderman of each zoning map amendment: the geocoded title address placed on the ward map in force on
# the amendment's introduction date (canonical_boundary_year_from_date: the 2003 map until May 18, 2015, the 2015 map
# until May 15, 2023, then the map drawn for the 2023 elections, in Wards_2024.geojson), and the alderman serving
# that ward that day (hand-built terms in tasks/create_alderman_data, recorded through September 27, 2026). An
# amendment filed by an alderman without a geocoded address takes the filing ward. For alderman-filed amendments
# with both, the two wards are compared.
# The Census geocoder sometimes returns the house number on the opposite side of the city from the one the title gives
# ("5145 N CALIFORNIA AVE" as 5145 S California Ave) or in another city ("1601 W DIVISION ST" in Chicago Heights).
# Such a match is rejected (geocode_rejected), and the title's address is placed instead on the City's 2013 street
# centerlines (tasks/download_chicago_gis_layers): on the segment with the title's direction, street name and a
# house-number range on the side of the number's parity, at the number's position along it, centerline_offset_feet
# toward that side (location_source). A match that changes a direction the street cannot have ("11231 W WESTERN
# AVE" as 11231 S Western Ave) or matches another address the title gives ("... AKA 1611 W IRVING PARK RD") is kept.
centerline_offset_feet <- 30
street_types <- "AVE|AV|ST|RD|BLVD|DR|PL|CT|PKWY|PKY|TER|HWY|LN|WAY|SQ|CIR"
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

opposite <- c(N = "S", S = "N", E = "W", W = "E")
compact <- function(x) str_remove_all(x, "[^A-Z0-9]")
points <- queries |>
  left_join(geocodes, by = "address_id", relationship = "many-to-one") |>
  inner_join(amendments, by = "matter_id", relationship = "one-to-one") |>
  mutate(map_year = canonical_boundary_year_from_date(introduction_date),
    matched = str_match(matched_address, paste0("^[0-9]+\\s+(?:([NSEW])\\s+)?(.+?)(?:\\s+(?:", street_types,
      "))?,\\s*([^,]+),")),
    matched_direction = matched[, 2], matched_street = compact(matched[, 3]), matched_city = matched[, 4],
    title_directions = map2(address_query, matched_street, function(query, street) {
      found <- str_match_all(coalesce(query, ""), paste0("\\b([NSEW])\\.?\\s+([A-Z0-9 .']+?)(?=\\s+(?:",
        street_types, ")\\b|\\s*$|\\s*[,/;(]|\\s+A\\.?K\\.?A)"))[[1]]
      found[compact(found[, 3]) == street, 2]
    }),
    geocode_rejected = case_when(
      !geocode_match %in% "Match" ~ NA_character_,
      matched_city != "CHICAGO" ~ "outside_chicago",
      map2_lgl(matched_direction, title_directions, function(direction, given) !is.na(direction) &&
        length(given) > 0 && all(given == opposite[direction])) ~ "opposite_side"))
stopifnot(nrow(points) == nrow(amendments), !anyNA(points$map_year), all(points$map_year >= 2003L))

# The rejected matches placed on the centerlines.
centerlines <- st_read("/vsizip/../input/street_centerlines_2013.zip/Transportation.shp", quiet = TRUE) |>
  st_transform(3435) |>
  transmute(direction = PRE_DIR, street = compact(STREET_NAM), left_low = pmin(L_F_ADD, L_T_ADD),
    left_high = pmax(L_F_ADD, L_T_ADD), right_low = pmin(R_F_ADD, R_T_ADD), right_high = pmax(R_F_ADD, R_T_ADD),
    L_F_ADD, L_T_ADD, R_F_ADD, R_T_ADD)
rejected <- points |>
  filter(!is.na(geocode_rejected)) |>
  mutate(title = str_match(address_query, paste0("^([0-9]+)\\s+([NSEW])\\.?\\s+(.+?)\\s+(?:", street_types,
    ")\\b")),
    number = as.integer(title[, 2]), direction = title[, 3], street = compact(title[, 4])) |>
  distinct(address_id, number, direction, street)
segments <- bind_rows(lapply(seq_len(nrow(rejected)), function(i) {
    filter(centerlines, direction == rejected$direction[i], street == rejected$street[i]) |>
      mutate(address_id = rejected$address_id[i], number = rejected$number[i])
  })) |>
  mutate(side = case_when(number %% 2 == L_F_ADD %% 2 & number >= left_low & number <= left_high ~ "left",
      number %% 2 == R_F_ADD %% 2 & number >= right_low & number <= right_high ~ "right")) |>
  filter(!is.na(side)) |>
  add_count(address_id)
stopifnot(all(segments$n == 1))
along <- with(segments, if_else(side == "left", (number - L_F_ADD) / (L_T_ADD - L_F_ADD),
  (number - R_F_ADD) / (R_T_ADD - R_F_ADD)))
along <- coalesce(if_else(is.finite(along), along, 0.5), 0.5)
lines <- st_geometry(st_as_sf(segments))
at <- st_coordinates(st_line_interpolate(lines, along, normalized = TRUE))
ahead <- st_coordinates(st_line_interpolate(lines, pmin(along + 0.01, 1), normalized = TRUE)) -
  st_coordinates(st_line_interpolate(lines, pmax(along - 0.01, 0), normalized = TRUE))
toward <- if_else(segments$side == "left", 1, -1) * centerline_offset_feet / sqrt(rowSums(ahead^2))
centerline_points <- st_as_sf(tibble(address_id = segments$address_id, x = at[, 1] - ahead[, 2] * toward,
  y = at[, 2] + ahead[, 1] * toward), coords = c("x", "y"), crs = 3435) |>
  st_transform(4326)
centerline_points <- tibble(address_id = centerline_points$address_id,
  centerline_longitude = st_coordinates(centerline_points)[, 1],
  centerline_latitude = st_coordinates(centerline_points)[, 2])
points <- points |>
  left_join(centerline_points, by = "address_id", relationship = "many-to-one") |>
  mutate(location_source = case_when(is.na(geocode_rejected) & geocode_match %in% "Match" ~ "census_geocoder",
      !is.na(centerline_longitude) ~ "street_centerlines"),
    longitude = if_else(location_source %in% "street_centerlines", centerline_longitude, longitude),
    latitude = if_else(location_source %in% "street_centerlines", centerline_latitude, latitude),
    geocoded = !is.na(location_source) & is.finite(longitude) & is.finite(latitude))

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
    ward_source = case_when(!is.na(geocoded_ward) ~ if_else(location_source == "street_centerlines",
        "centerline_address", "geocoded_address"),
      !is.na(filing_ward) ~ "filing_ward",
      geocoded ~ "geocoded_outside_wards",
    !is.na(geocode_rejected) ~ "geocode_rejected",
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
  mutate(longitude = if_else(geocoded, longitude, NA_real_), latitude = if_else(geocoded, latitude, NA_real_)) |>
  select(matter_id, introduction_date, address_query, geocode_match, geocode_match_type, matched_address,
    geocode_rejected, location_source, longitude, latitude, map_year, ward, ward_source, filing_ward_agrees, alderman)
SaveData(wards, "matter_id", "../output/zoning_amendment_wards.csv")
