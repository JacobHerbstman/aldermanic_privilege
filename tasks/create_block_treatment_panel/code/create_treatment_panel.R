# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_block_treatment_panel/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

suppressMessages(sf_use_s2(FALSE))

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(
    month_date = as.Date(paste("01", month), format = "%d %b %Y"),
    year = year(month_date)
  ) %>%
  filter(month(month_date) == 6, year %in% c(2014L, 2015L)) %>%
  select(year, ward, alderman)
if (anyDuplicated(alderman_panel[c("year", "ward")]) > 0) {
  stop("Alderman panel must be unique by ward-year after June filtering.", call. = FALSE)
}

blocks_2010 <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_transform(st_crs(ward_panel)) %>%
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id)) %>%
  distinct(block_id, .keep_all = TRUE)

ward_map_2014 <- aggregate_ward_map(
  ward_panel,
  canonical_map_year_for_era("2003_2014")
)
ward_map_2015 <- aggregate_ward_map(
  ward_panel,
  canonical_map_year_for_era("2015_2023")
)

block_areas <- tibble(
  block_id = blocks_2010$block_id,
  block_area = as.numeric(st_area(blocks_2010))
)

intersections_2014 <- suppressWarnings(
  st_intersection(
    blocks_2010 %>% select(block_id),
    ward_map_2014 %>% select(ward)
  )
)
if (nrow(intersections_2014) == 0) {
  stop("No 2010 Census blocks intersect the 2014 ward map.", call. = FALSE)
}

assignments_2014 <- intersections_2014 %>%
  mutate(intersection_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  left_join(block_areas, by = "block_id", relationship = "many-to-one") %>%
  group_by(block_id) %>%
  arrange(desc(intersection_area), ward, .by_group = TRUE) %>%
  summarise(
    ward_origin = first(as.integer(ward)),
    ward_origin_share = first(intersection_area) / first(block_area),
    ward_origin_n_wards = n_distinct(ward),
    .groups = "drop"
  )

intersections_2015 <- suppressWarnings(
  st_intersection(
    blocks_2010 %>% select(block_id),
    ward_map_2015 %>% select(ward)
  )
)
if (nrow(intersections_2015) == 0) {
  stop("No 2010 Census blocks intersect the 2015 ward map.", call. = FALSE)
}

assignments_2015 <- intersections_2015 %>%
  mutate(intersection_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  left_join(block_areas, by = "block_id", relationship = "many-to-one") %>%
  group_by(block_id) %>%
  arrange(desc(intersection_area), ward, .by_group = TRUE) %>%
  summarise(
    ward_dest = first(as.integer(ward)),
    ward_dest_share = first(intersection_area) / first(block_area),
    ward_dest_n_wards = n_distinct(ward),
    .groups = "drop"
  )

ward_turnover_2015 <- alderman_panel %>%
  pivot_wider(names_from = year, values_from = alderman, names_prefix = "alderman_") %>%
  mutate(ward_had_turnover = alderman_2014 != alderman_2015) %>%
  select(ward, ward_had_turnover)
if (anyDuplicated(ward_turnover_2015$ward) > 0) {
  stop("The 2015 ward-turnover lookup must be unique by ward.", call. = FALSE)
}

block_treatment_pre_scores <- tibble(block_id = blocks_2010$block_id) %>%
  left_join(assignments_2014, by = "block_id", relationship = "one-to-one") %>%
  left_join(assignments_2015, by = "block_id", relationship = "one-to-one") %>%
  left_join(
    ward_turnover_2015,
    by = c("ward_origin" = "ward"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    block_vintage = "2010",
    switched = ward_origin != ward_dest & !is.na(ward_origin) & !is.na(ward_dest),
    has_complete_ward_assignment = !is.na(ward_origin) & !is.na(ward_dest),
    valid = replace_na(has_complete_ward_assignment, FALSE),
    cohort = "2015",
    min_assignment_share = pmin(ward_origin_share, ward_dest_share, na.rm = TRUE)
  ) %>%
  select(
    block_id,
    block_vintage,
    ward_origin,
    ward_dest,
    ward_origin_share,
    ward_dest_share,
    ward_origin_n_wards,
    ward_dest_n_wards,
    switched,
    ward_had_turnover,
    valid,
    has_complete_ward_assignment,
    cohort,
    min_assignment_share
  )

write_csv(block_treatment_pre_scores, "../output/block_treatment_pre_scores.csv")
