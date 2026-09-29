# setwd("tasks/audits/zoning_record_validation/code")
# The places of the Journals' zoning map amendments (tasks/place_journal_zoning_amendments), placed from their
# boundaries, against the zoning districts they created (tasks/download_chicago_gis_layers). A district on the City's
# zoning map, as in force now or in November 2012, carries the application number and passage date of the amendment
# that set it, so a passed ordinance (tasks/link_journal_zoning_outcomes) whose number matches a district passed
# within passage_days_tolerance days of the Journal's meeting (eLMS and the map date some passages a day or two
# later), and whose district class is one the ordinance creates, is the same amendment, and its districts are its
# parcel. The class must agree because the map records the odd district under another amendment's number (map
# number 16849 of June 30, 2009 is a B3-5 district in the 5th Ward; the Journal's ordinance 16849 of that day
# rezones a parcel on map sheet 11-G to RT-4). One row per such ordinance whose introduction is placed: the distance
# from the placed point to the parcel (0 inside it), the ward of a point inside the parcel on the ward map the
# placement used (the wards of 1998 before May 5, 2003, then the 2003 map), the number of wards the parcel touches, and
# whether the placed ward agrees.
passage_days_tolerance <- 3
ward_map_2003_start <- as.Date("2003-05-05")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

districts <- bind_rows(
  st_read("../input/zoning_districts_current.geojson", quiet = TRUE) |>
    st_transform(3435) |>
    transmute(map = "current", number = ordinance, passed = as.Date(substr(ordinance_1, 1, 10)), zone_class),
  st_read("/vsizip/../input/zoning_districts_2012.zip/Zoning_nov2012.shp", quiet = TRUE) |>
    st_transform(3435) |>
    transmute(map = "2012", number = ORDINANCE_, passed = as.Date(ORDINANCE1), zone_class = ZONE_CLASS)) |>
  st_make_valid() |>
  filter(!is.na(number), !is.na(passed)) |>
  mutate(application_number = str_replace(number, "^A(?=[0-9])", "A-"), district_class = str_remove(zone_class, " .*$"))
# One parcel per amendment: the union of its districts on either map.
parcels <- districts |>
  group_by(application_number, passed, district_class) |>
  summarise(maps = paste(sort(unique(map)), collapse = ";"), .groups = "drop")

# One row per ordinance and district class it creates.
ordinance_classes <- read_csv("../input/journal_ordinance_links.csv", col_types = cols(.default = col_character())) |>
  filter(action == "passed", !is.na(introduction), !is.na(application_number), !is.na(to_districts)) |>
  transmute(ordinance, introduction, application_number, district_class = to_districts,
    earliest = as.Date(meeting_date) - passage_days_tolerance,
    latest = as.Date(meeting_date) + passage_days_tolerance) |>
  separate_longer_delim(district_class, ";")
matched <- ordinance_classes |>
  inner_join(st_drop_geometry(parcels) |> mutate(parcel = row_number()),
    by = join_by(application_number, district_class, between(y$passed, x$earliest, x$latest)),
    relationship = "many-to-one") |>
  summarise(maps = paste(sort(unique(unlist(str_split(maps, ";")))), collapse = ";"), parcels = list(parcel),
    .by = c(ordinance, introduction, application_number))
parcel_shapes <- st_sfc(lapply(matched$parcels, function(p) st_union(st_geometry(parcels)[p])[[1]]), crs = 3435)

places <- read_csv("../input/journal_amendment_places.csv", col_types = cols(.default = col_character())) |>
  filter(!is.na(longitude)) |>
  mutate(introduction = paste0(file, "#", position),
    map = if_else(as.Date(introduction_date) < ward_map_2003_start, "1998", "2003"))
checks <- matched |>
  mutate(shape = parcel_shapes) |>
  inner_join(select(places, introduction, map, place_source, longitude, latitude, placed_ward), by = "introduction",
    relationship = "many-to-one")
shapes <- st_sfc(checks$shape, crs = 3435)
placed_points <- st_as_sf(checks, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)

ward_maps <- bind_rows(
  st_read("../input/Chicago_Wards_1998.geojson", quiet = TRUE) |> st_transform(3435) |>
    transmute(map = "1998", ward = as.integer(WARD)),
  st_read("../input/Wards_2014.geojson", quiet = TRUE) |> filter(ward != "OUT") |> st_transform(3435) |>
    transmute(map = "2003", ward = as.integer(ward))) |>
  st_make_valid()
parcel_points <- st_point_on_surface(shapes)
in_map <- function(geometry, m) st_intersects(geometry, filter(ward_maps, map == m))
checks <- checks |>
  mutate(parcel_ward = vapply(seq_along(shapes), function(i) {
      w <- in_map(parcel_points[i], map[i])[[1]]
      if (length(w) == 1) filter(ward_maps, map == .env$map[i])$ward[w] else NA_integer_
    }, integer(1)),
    parcel_wards = vapply(seq_along(shapes), function(i) lengths(in_map(shapes[i], map[i])), integer(1)),
    distance_feet = round(as.numeric(st_distance(placed_points, shapes, by_element = TRUE)), 1),
    placed_ward = as.integer(placed_ward),
    ward_agrees = if_else(is.na(parcel_ward) | is.na(placed_ward), NA, parcel_ward == placed_ward)) |>
  select(ordinance, introduction, application_number, map, maps, place_source, distance_feet, parcel_ward,
    parcel_wards, placed_ward, ward_agrees)
SaveData(checks, "ordinance", "../output/journal_places_vs_zoning_map.csv")
