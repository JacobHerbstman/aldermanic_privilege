# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_block_assignment_review/code")
# min_overlap_sqft <- 100

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(min_overlap_sqft)
}
if (length(cli_args) != 1) {
  stop("Script requires one argument: <min_overlap_sqft>.", call. = FALSE)
}
min_overlap_sqft <- as.numeric(cli_args[1])

sf_use_s2(FALSE)

zoning <- st_read("../input/zoning_districts_2025.geojson", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(3435)

blocks <- read_csv(
  "../input/census_blocks_2010.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  distinct(GEOID10, .keep_all = TRUE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  transmute(census_block_id = GEOID10)

ordinance_geometry <- bind_rows(
  zoning %>%
    filter(clerk_docn %in% c("O2012-6644", "O2016-1620")) %>%
    mutate(
      matter_id = clerk_docn,
      assignment_source = "city_zoning_clerk_document",
      evidence = paste0("City zoning polygon linked to ", clerk_docn)
    ),
  zoning %>%
    filter(ordinance == "A8390") %>%
    mutate(
      matter_id = "O2018-3281",
      assignment_source = "city_zoning_ordinance",
      evidence = "City T-district polygon linked to ordinance A8390"
    ),
  zoning %>%
    filter(ordinance == "MA181") %>%
    mutate(
      matter_id = "SO2014-2421",
      assignment_source = "city_zoning_ordinance",
      evidence = "City zoning polygons linked to 606 Trail ordinance MA181"
    ),
  zoning %>%
    filter(ordinance == "A7964") %>%
    mutate(
      matter_id = "SO2013-8665",
      assignment_source = "city_zoning_ordinance",
      evidence = "City PD 733 polygon linked to ordinance A7964"
    ),
  zoning %>%
    filter(pd_num == "1169") %>%
    mutate(
      matter_id = "SO2016-7343",
      assignment_source = "city_zoning_pd_boundary",
      evidence = "City PD 1169 boundary retained by the 2017 amendment"
    )
) %>%
  select(matter_id, assignment_source, evidence)

geometry_blocks <- st_intersection(blocks, ordinance_geometry) %>%
  mutate(overlap_sqft = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  group_by(matter_id, census_block_id, assignment_source, evidence) %>%
  summarise(overlap_sqft = sum(overlap_sqft), .groups = "drop") %>%
  filter(overlap_sqft >= min_overlap_sqft) %>%
  group_by(matter_id) %>%
  mutate(is_representative = overlap_sqft == max(overlap_sqft)) %>%
  ungroup()

direct_reviews <- tribble(
  ~matter_id, ~census_block_id, ~is_representative, ~assignment_source, ~evidence, ~overlap_sqft,
  "O2011-845", "170314601001031", FALSE, "ordinance_legal_description", "South Works legal boundary reaches the block east of S Green Bay Avenue", NA_real_,
  "O2011-845", "170314601001032", TRUE, "ordinance_legal_description", "South Works legal boundary includes the block bounded by E 86th Street, E 87th Street, and S Burley Avenue", NA_real_,
  "O2011-9720", "170313302002017", TRUE, "ordinance_legal_description_and_parcel", "Official boundary and 1615 S Clark parcels fall in this block", NA_real_,
  "SO2011-8028", "170318330002020", TRUE, "ordinance_common_address_parcel", "Official 1241 W Fulton Market address, PIN 17083200030000", NA_real_
)

output <- bind_rows(geometry_blocks, direct_reviews) %>%
  arrange(matter_id, census_block_id)

if (anyDuplicated(output[c("matter_id", "census_block_id")]) > 0) {
  stop("Reviewed assignments are not unique by matter and block.", call. = FALSE)
}
if (!setequal(
  output$matter_id,
  c(
    "O2011-845", "O2011-9720", "O2012-6644", "O2016-1620", "O2018-3281",
    "SO2011-8028", "SO2013-8665", "SO2014-2421", "SO2016-7343"
  )
)) {
  stop("Reviewed assignments do not cover the nine target matters.", call. = FALSE)
}
if (any(count(filter(output, is_representative), matter_id)$n != 1)) {
  stop("Each reviewed matter must have exactly one representative block.", call. = FALSE)
}

write_csv(output, "../output/reviewed_rezoning_block_assignments.csv")
