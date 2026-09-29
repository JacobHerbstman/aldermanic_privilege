# setwd("tasks/explore_land_prices/code")
# Exploratory: market sales of land in Chicago, 2006-2022, placed at their ward boundaries. Two kinds of sale:
#   vacant: a vacant parcel (Assessor class 100 in the year of sale);
#   teardown: any other parcel whose building was demolished soon after, so that the price is mostly for the land:
#     the last sale in the two years before the application for a demolition permit naming the parcel. Demolitions
#     of a garage, shed, porch, roof tank or other structure alone are not teardowns, nor are emergency demolitions,
#     which the City orders for derelict buildings whose sales are distressed. A teardown is redeveloped when a
#     new-construction permit names the parcel within three years of the demolition; for demolitions after 2019
#     the permits (through 2022) do not cover the three years, and redevelopment is left unknown.
# Either sale is of the parcel alone, passes the county's three sale-quality flags, is for more than $10,000, and is not
# by or to a government body or land bank (the City's and the Land Bank's lot programs price lots administratively).
# Each sale gets its parcel's centroid (tasks/download_parcel_centroids), its 2021 lot area
# (tasks/download_parcel_polygons_2021; lots under 1,000 square feet are dropped), the zoning group in effect on the
# sale date (by the rule of tasks/new_construction_analysis_data), and, on the ward map in force on the sale date, its
# ward, the nearest other ward and its distance to it, and the aldermen serving both wards that day.
start_year <- 2006L
end_year <- 2022L
teardown_window_days <- 730
redevelopment_window_days <- 1095
permits_end <- as.Date("2022-12-31")
# Lots under this area in 2021 are slivers or parts of a parcel, where price per square foot means little.
min_lot_sqft <- 1000
# The 2015 ward map took effect with the council term that began on this date.
map_2015_start <- as.Date("2015-05-18")
government_party <- "CITY OF CHICAGO|LAND BANK|COUNTY OF COOK|COOK COUNTY|STATE OF ILLINOIS|CHICAGO HOUSING AUTH|BOARD OF EDUCATION"
building_words <- "RESIDEN|DWELLING|HOUSE|BUILDING|BLDG|FLAT|COTTAGE|UNIT|COMMERCIAL|FAMILY|APARTMENT|STRUCTURE|STORE"
accessory_words <- "GARAGE|SHED|PORCH|DECK|TANK|CANOPY|SIGN|FENCE|CHIMNEY|STAIR|INTERIOR"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
sf_use_s2(FALSE)

market_sales <- read_csv("../input/parcel_sales_city.csv", show_col_types = FALSE,
  col_types = cols(pin = "c", class = "c", sale_price = "c", sale_document_num = "c", row_id = "c", .default = col_guess())) |>
  mutate(pin = str_pad(gsub("[^0-9]", "", pin), 14, pad = "0"), pin10 = substr(pin, 1, 10),
    sale_date = as.Date(sale_date), price = as.numeric(gsub("[$,]", "", sale_price)),
    seller = toupper(coalesce(sale_seller_name, "")), buyer = toupper(coalesce(sale_buyer_name, ""))) |>
  filter(year >= start_year, year <= end_year, num_parcels_sale == 1, !sale_filter_same_sale_within_365,
    !sale_filter_less_than_10k, !sale_filter_deed_type, price > 10000, !grepl(government_party, seller),
    !grepl(government_party, buyer)) |>
  select(row_id, pin10, class, sale_date, year, price, sale_type)

# Demolitions of a building, one row per parcel the permit names, dated by the application.
demolitions <- read_csv("../input/building_permits_2006_2022.csv", show_col_types = FALSE,
  col_select = c(id, permit_type, application_start_date, issue_date, work_description, pin_list),
  col_types = cols(.default = col_character())) |>
  filter(grepl("WRECK|DEMOL", permit_type), !is.na(pin_list), pin_list != "") |>
  mutate(description = toupper(coalesce(work_description, "")),
    demolition_date = as.Date(substr(coalesce(application_start_date, issue_date), 1, 10))) |>
  filter(!grepl("EMERGENCY", description),
    grepl(building_words, description) | (grepl("STORY", description) & !grepl(accessory_words, description))) |>
  tidyr::separate_longer_delim(pin_list, delim = "|") |>
  transmute(pin10 = substr(str_pad(gsub("[^0-9]", "", pin_list), 10, pad = "0"), 1, 10), demolition_date) |>
  filter(!is.na(demolition_date)) |>
  distinct()
# Each sale is matched to the parcel's next demolition; of several sales before one demolition, the last counts.
teardowns <- market_sales |>
  filter(class != "100") |>
  inner_join(demolitions, by = join_by(pin10, closest(sale_date <= demolition_date)), relationship = "many-to-one") |>
  filter(as.numeric(demolition_date - sale_date) <= teardown_window_days) |>
  slice_max(sale_date, n = 1, with_ties = FALSE, by = c(pin10, demolition_date)) |>
  select(row_id, pin10, demolition_date)
# New construction on the parcel after the demolition, by the first permit application on or after it.
new_construction <- read_csv("../input/building_permits_2006_2022.csv", show_col_types = FALSE,
  col_select = c(permit_type, application_start_date, issue_date, pin_list), col_types = cols(.default = col_character())) |>
  filter(permit_type == "PERMIT - NEW CONSTRUCTION", !is.na(pin_list), pin_list != "") |>
  mutate(construction_date = as.Date(substr(coalesce(application_start_date, issue_date), 1, 10))) |>
  tidyr::separate_longer_delim(pin_list, delim = "|") |>
  transmute(pin10 = substr(str_pad(gsub("[^0-9]", "", pin_list), 10, pad = "0"), 1, 10), construction_date) |>
  filter(!is.na(construction_date)) |>
  distinct()
teardowns <- teardowns |>
  left_join(new_construction, by = join_by(pin10, closest(demolition_date <= construction_date)),
    relationship = "many-to-one") |>
  mutate(redeveloped = case_when(
    coalesce(as.numeric(construction_date - demolition_date) <= redevelopment_window_days, FALSE) ~ TRUE,
    demolition_date + redevelopment_window_days > permits_end ~ NA,
    TRUE ~ FALSE)) |>
  select(row_id, redeveloped)

sales <- bind_rows(
  market_sales |> filter(class == "100") |> mutate(sale_kind = "vacant", redeveloped = NA),
  market_sales |> inner_join(teardowns, by = "row_id", relationship = "one-to-one") |> mutate(sale_kind = "teardown")
) |>
  select(row_id, sale_kind, redeveloped, pin10, sale_date, year, price, sale_type)
stopifnot(!anyDuplicated(sales$row_id))

located <- sales |>
  inner_join(read_csv("../input/parcel_centroids.csv", show_col_types = FALSE, col_types = cols(pin10 = "c")) |>
    select(pin10, x_3435, y_3435), by = "pin10", relationship = "many-to-one") |>
  inner_join(read_csv("../input/parcel_lot_areas_2021.csv", show_col_types = FALSE, col_types = cols(pin10 = "c")) |>
    select(pin10, lot_sqft), by = "pin10", relationship = "many-to-one") |>
  filter(lot_sqft >= min_lot_sqft) |>
  mutate(map_year = if_else(sale_date < map_2015_start, 2003L, 2015L)) |>
  st_as_sf(coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)

# Zoning in effect on the sale date, as for construction dates in tasks/new_construction_analysis_data: the current
# polygon when its last amendment precedes the sale, and otherwise the latest snapshot before the sale.
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
zoned <- located |>
  select(row_id) |>
  st_join(st_read("../input/historical_zoning_2006_candidate.gpkg", quiet = TRUE) |>
    transmute(group_2006 = candidate_zone_group_2006) |> st_transform(3435), largest = TRUE) |>
  st_join(st_read("/vsizip/../input/zoning_nov2012.zip/Zoning_nov2012.shp", quiet = TRUE) |>
    transmute(group_2012 = zone_group(ZONE_CLASS)) |> st_transform(3435), largest = TRUE) |>
  st_join(st_read("/vsizip/../input/zoning_sep2014.zip/Zoning.shp", quiet = TRUE) |>
    transmute(group_2014 = zone_group(ZONE_CLASS)) |> st_transform(3435), largest = TRUE) |>
  st_join(st_read("/vsizip/../input/zoning_jan2016.zip/zoning_2016_01.shp", quiet = TRUE) |>
    transmute(group_2016 = zone_group(ZONE_CLASS)) |> st_transform(3435), largest = TRUE) |>
  st_join(st_read("../input/zoning_sep2025.geojson", quiet = TRUE) |>
    transmute(group_2025 = zone_group(zone_class), ordinance_date = as.Date(ordinance_1)) |> st_transform(3435),
    largest = TRUE) |>
  st_drop_geometry()
stopifnot(!anyDuplicated(zoned$row_id))

# Ward, nearest other ward and distance to it, on the map in force.
ward_maps <- bind_rows(
  st_read("../input/Wards_2014.geojson", quiet = TRUE) |> filter(ward != "OUT") |> transmute(map_year = 2003L, ward),
  st_read("../input/Wards_2015.geojson", quiet = TRUE) |> transmute(map_year = 2015L, ward)
) |>
  st_transform(3435) |>
  st_make_valid() |>
  mutate(ward = as.integer(ward))
located$ward <- NA_integer_
located$neighbor_ward <- NA_integer_
located$distance_ft <- NA_real_
for (m in c(2003L, 2015L)) {
  rows <- which(located$map_year == m)
  wards <- ward_maps[ward_maps$map_year == m, ]
  inside <- st_intersects(located[rows, ], wards)
  located$ward[rows] <- vapply(inside, \(w) if (length(w) == 1) wards$ward[w] else NA_integer_, integer(1))
  for (w in unique(na.omit(located$ward[rows]))) {
    in_ward <- rows[which(located$ward[rows] == w)]
    others <- wards[wards$ward != w, ]
    nearest <- st_nearest_feature(located[in_ward, ], others)
    located$neighbor_ward[in_ward] <- others$ward[nearest]
    located$distance_ft[in_ward] <- as.numeric(st_distance(located[in_ward, ], others[nearest, ], by_element = TRUE))
  }
}

terms <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE)
land_sales <- st_drop_geometry(located) |>
  filter(!is.na(ward)) |>
  left_join(zoned, by = "row_id", relationship = "one-to-one") |>
  mutate(preceding_group = case_when(year <= 2012 ~ group_2006, year <= 2014 ~ group_2012, year == 2015 ~ group_2014,
      TRUE ~ group_2016),
    zone_group = case_when(!is.na(group_2025) & coalesce(ordinance_date <= sale_date, FALSE) ~ group_2025,
      !is.na(preceding_group) ~ preceding_group,
      year <= 2012 & group_2012 == group_2014 & group_2014 == group_2016 ~ group_2012)) |>
  left_join(terms, by = join_by(ward, between(sale_date, start_date, end_date)),
    relationship = "many-to-one") |>
  select(-start_date, -end_date) |>
  left_join(rename(terms, neighbor_ward = ward, neighbor_alderman = alderman),
    by = join_by(neighbor_ward, between(sale_date, start_date, end_date)), relationship = "many-to-one") |>
  transmute(row_id, sale_kind, redeveloped, pin10, sale_date, year, price, sale_type, lot_sqft, log_price_per_sqft = log(price / lot_sqft),
    zone_group, x_3435, y_3435, map_year, ward, neighbor_ward, distance_ft, alderman, neighbor_alderman)
SaveData(land_sales, "row_id", "../output/land_sales.csv")
