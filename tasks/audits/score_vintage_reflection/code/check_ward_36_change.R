# setwd("tasks/audits/score_vintage_reflection/code")

source("../../../setup_environment/code/packages.R")
library(sf)

ward_2014 <- sf::st_read("../../../../data_raw/Wards_2014.geojson", quiet = TRUE) |>
  sf::st_transform(3435) |>
  dplyr::filter(as.character(ward) == "36") |>
  sf::st_make_valid() |>
  sf::st_union()
ward_2015 <- sf::st_read("../../../../data_raw/Wards_2015.geojson", quiet = TRUE) |>
  sf::st_transform(3435) |>
  dplyr::filter(as.character(ward) == "36") |>
  sf::st_make_valid() |>
  sf::st_union()

overlap <- sf::st_intersection(ward_2014, ward_2015)
area_2014 <- as.numeric(sf::st_area(ward_2014))
area_2015 <- as.numeric(sf::st_area(ward_2015))
overlap_area <- as.numeric(sf::st_area(overlap))
centroid_distance <- as.numeric(sf::st_distance(
  sf::st_centroid(ward_2014),
  sf::st_centroid(ward_2015)
))

readr::write_csv(
  tibble::tibble(
    ward = 36L,
    area_2014_sqft = area_2014,
    area_2015_sqft = area_2015,
    overlap_sqft = overlap_area,
    share_2014_area_retained = overlap_area / area_2014,
    share_2015_area_from_old_ward = overlap_area / area_2015,
    jaccard_overlap = overlap_area / (area_2014 + area_2015 - overlap_area),
    centroid_shift_ft = centroid_distance
  ),
  "../output/ward_36_geography_change.csv"
)
