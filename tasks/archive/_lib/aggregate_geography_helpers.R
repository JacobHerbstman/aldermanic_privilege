coord_key_from_latlon <- function(latitude, longitude) {
  sprintf("%.7f|%.7f", as.numeric(latitude), as.numeric(longitude))
}

load_aggregate_geographies <- function(ward_panel_input, community_area_input) {
  ward_panel <- st_read(ward_panel_input, quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(3435) %>%
    mutate(ward = as.integer(ward))

  community_areas <- st_read(community_area_input, quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(3435) %>%
    transmute(
      community_area = as.integer(area_numbe),
      community_name = as.character(community)
    )

  list(
    ward_2003 = ward_panel %>% filter(year == 2014) %>% select(ward),
    ward_2015 = ward_panel %>% filter(year == 2015) %>% select(ward),
    ward_2024 = ward_panel %>% filter(year == 2025) %>% select(ward),
    community_areas = community_areas
  )
}

build_coord_geography_lookup <- function(coords_tbl, ward_panel_input, community_area_input) {
  geographies <- load_aggregate_geographies(ward_panel_input, community_area_input)

  points <- coords_tbl %>%
    arrange(coord_key) %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    filter(is.finite(latitude), is.finite(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(3435)

  community_lookup <- st_join(
    points,
    geographies$community_areas,
    join = st_within,
    left = TRUE
  ) %>%
    st_drop_geometry() %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    select(coord_key, community_area, community_name)

  ward_2003_lookup <- st_join(
    points,
    geographies$ward_2003,
    join = st_within,
    left = TRUE
  ) %>%
    st_drop_geometry() %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    select(coord_key, ward_2003 = ward)

  ward_2015_lookup <- st_join(
    points,
    geographies$ward_2015,
    join = st_within,
    left = TRUE
  ) %>%
    st_drop_geometry() %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    select(coord_key, ward_2015 = ward)

  ward_2024_lookup <- st_join(
    points,
    geographies$ward_2024,
    join = st_within,
    left = TRUE
  ) %>%
    st_drop_geometry() %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    select(coord_key, ward_2024 = ward)

  coords_tbl %>%
    arrange(coord_key) %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    left_join(community_lookup, by = "coord_key") %>%
    left_join(ward_2003_lookup, by = "coord_key") %>%
    left_join(ward_2015_lookup, by = "coord_key") %>%
    left_join(ward_2024_lookup, by = "coord_key")
}

load_block_geographies <- function(census_blocks_2010_input, census_blocks_2020_input) {
  blocks_2010 <- read_csv(census_blocks_2010_input, show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_make_valid() %>%
    st_transform(3435) %>%
    transmute(block_id_2010 = as.character(GEOID10))

  blocks_2020 <- read_csv(census_blocks_2020_input, show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_make_valid() %>%
    st_transform(3435) %>%
    transmute(block_id_2020 = as.character(GEOID20))

  list(
    blocks_2010 = blocks_2010,
    blocks_2020 = blocks_2020
  )
}

build_coord_block_lookup <- function(coords_tbl, census_blocks_2010_input, census_blocks_2020_input) {
  blocks <- load_block_geographies(census_blocks_2010_input, census_blocks_2020_input)

  points <- coords_tbl %>%
    arrange(coord_key) %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    filter(is.finite(latitude), is.finite(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(3435)

  block_2010_lookup <- st_join(
    points,
    blocks$blocks_2010,
    join = st_within,
    left = TRUE
  ) %>%
    st_drop_geometry() %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    select(coord_key, block_id_2010)

  block_2020_lookup <- st_join(
    points,
    blocks$blocks_2020,
    join = st_within,
    left = TRUE
  ) %>%
    st_drop_geometry() %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    select(coord_key, block_id_2020)

  coords_tbl %>%
    arrange(coord_key) %>%
    distinct(coord_key, .keep_all = TRUE) %>%
    left_join(block_2010_lookup, by = "coord_key") %>%
    left_join(block_2020_lookup, by = "coord_key")
}

assign_ward_from_year <- function(year, ward_2003, ward_2015, ward_2024) {
  case_when(
    year <= 2014 ~ ward_2003,
    year <= 2023 ~ ward_2015,
    TRUE ~ ward_2024
  )
}

load_map_geography <- function(geography_level, ward_panel_input, community_area_input, map_year = NULL) {
  geographies <- load_aggregate_geographies(ward_panel_input, community_area_input)

  if (geography_level == "ward") {
    if (is.null(map_year)) {
      stop("ward maps require map_year", call. = FALSE)
    }

    if (map_year <= 2014) {
      return(geographies$ward_2003 %>% mutate(geography_id = ward, geography_name = paste("Ward", ward)))
    }
    if (map_year <= 2023) {
      return(geographies$ward_2015 %>% mutate(geography_id = ward, geography_name = paste("Ward", ward)))
    }
    return(geographies$ward_2024 %>% mutate(geography_id = ward, geography_name = paste("Ward", ward)))
  }

  geographies$community_areas %>%
    mutate(
      geography_id = community_area,
      geography_name = community_name
    )
}
