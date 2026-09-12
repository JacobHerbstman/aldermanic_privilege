# setwd("tasks/construction_project_permits/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/permit_unit_patterns.R")


# Build construction permit history

# Keep all dates in the pinned permit source for construction verification.
# Study-period restrictions belong in the consuming analyses.
crs_projected <- 3435

building_permits <- read_csv(
  "../input/building_permits_full.csv",
  col_types = cols(id = col_character(), application_start_date = col_character(), issue_date = col_character()),
  show_col_types = FALSE,
  guess_max = Inf
)

if (anyNA(building_permits$id) || anyDuplicated(building_permits$id) ||
    nrow(readr::problems(building_permits)) > 0L) {
  stop("Permit source records must parse successfully and have unique IDs.")
}

building_permits_clean <- building_permits %>%
  janitor::clean_names() %>%
  mutate(across(
    .cols = matches("cost|fee|paid|waived|subtotal"),
    .fns = ~ as.numeric(gsub("[^0-9.-]", "", .x))
  ))

chicago_lat_min <- 41
chicago_lat_max <- 43
chicago_lon_min <- -89
chicago_lon_max <- -87
dominant_hotspot_latitude <- 42.00853640087
dominant_hotspot_longitude <- -87.91442843927
dominant_hotspot_tolerance <- 1e-6

needs_conversion_mask <- (
  is.na(building_permits_clean$latitude) | is.na(building_permits_clean$longitude)
) &
  is.finite(building_permits_clean$xcoordinate) &
  is.finite(building_permits_clean$ycoordinate)
permits_to_convert <- building_permits_clean[needs_conversion_mask, ]

converted_sf <- st_as_sf(
    permits_to_convert,
    coords = c("xcoordinate", "ycoordinate"),
    crs = crs_projected,
    remove = FALSE
  ) %>%
  st_transform(crs = 4326)

new_coords <- st_coordinates(converted_sf)
building_permits_clean$longitude[needs_conversion_mask] <- new_coords[, "X"]
building_permits_clean$latitude[needs_conversion_mask] <- new_coords[, "Y"]

building_permits_clean <- building_permits_clean %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::filter(!is.na(longitude)) %>%
  dplyr::filter(latitude >= chicago_lat_min) %>%
  dplyr::filter(latitude <= chicago_lat_max) %>%
  dplyr::filter(longitude >= chicago_lon_min) %>%
  dplyr::filter(longitude <= chicago_lon_max) %>%
  dplyr::filter(
    !(
      abs(latitude - dominant_hotspot_latitude) < dominant_hotspot_tolerance &
        abs(longitude - dominant_hotspot_longitude) < dominant_hotspot_tolerance
    )
  )

building_permits_clean <- building_permits_clean %>%
  dplyr::mutate(issue_date = as.Date(substr(issue_date, 1, 10))) %>%
  dplyr::mutate(application_start_date = as.Date(substr(application_start_date, 1, 10))) %>%
  dplyr::mutate(issue_date_ym = zoo::as.yearmon(issue_date)) %>%
  dplyr::mutate(application_start_date_ym = zoo::as.yearmon(application_start_date)) %>%
  arrange(application_start_date) %>%
  rename(pin = pin_list) %>%
  dplyr::select(id, pin, ward, application_start_date_ym, issue_date_ym, everything())


high_discretion_permits <- c(
  "PERMIT - NEW CONSTRUCTION",
  "PERMIT - RENOVATION/ALTERATION",
  "PERMIT - WRECKING/DEMOLITION",
  "PERMIT - PORCH CONSTRUCTION",
  "PERMIT - REINSTATE REVOKED PMT"
)

minor_permits <- c(
  "PERMIT - EASY PERMIT PROCESS",
  "PERMIT – EXPRESS PERMIT PROGRAM",
  "PERMIT - SIGNS",
  "PERMIT - SCAFFOLDING"
)

building_permits_clean2 <- building_permits_clean %>%
  mutate(high_discretion = ifelse(permit_type %in% high_discretion_permits, 1, 0)) %>%
  mutate(minor_permit = ifelse(permit_type %in% minor_permits, 1, 0))


building_permits_clean2 <- building_permits_clean2 %>%
  dplyr::filter(processing_time >= 0)

building_permits_final <- building_permits_clean2 %>%
  mutate(
    permit_issued = case_when(
      permit_status %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING") ~ 1,
      permit_status %in% c("EXPIRED", "CANCELLED", "REVOKED", "SUSPENDED") ~ 0,
      TRUE ~ NA_integer_
    ),
    corporate_applicant = as.integer(
      if_any(
        starts_with("contact_") & ends_with("_name"),
        ~ str_detect(
          str_to_upper(coalesce(.x, "")),
          "IN|SERVICE|CO|LLC|INC|CORP|LTD|LLP|PC|ASSOCIATE|GROUP|COMPANY|CONSTRUCTION|DEVELOPMENT|PROPERTY|PROPERTIES"
        )
      )
    )
  ) %>%
  dplyr::select(
    id, pin, ward, application_start_date_ym, issue_date_ym,
    processing_time, reported_cost, total_fee,
    high_discretion, permit_issued, corporate_applicant,
    everything(), -contains("contact_")
  )


building_permits_sf <- st_as_sf(
  building_permits_final,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(crs_projected) %>%
  mutate(across(c(application_start_date_ym, issue_date_ym), as.Date))

SaveData(building_permits_sf, character(), "../output/building_permits_for_verification.gpkg", layer = "building_permits_clean", delete_layer = TRUE, quiet = TRUE)

# Build new construction permit evidence

normalize_pin <- function(x) {
  digits <- str_replace_all(str_squish(as.character(x)), "[^0-9]", "")
  if_else(str_length(digits) == 14, digits, NA_character_)
}

finite_min <- function(x) {
  value <- suppressWarnings(min(x[is.finite(x)], na.rm = TRUE))
  if (is.infinite(value)) NA_real_ else value
}

finite_max <- function(x) {
  value <- suppressWarnings(max(x[is.finite(x)], na.rm = TRUE))
  if (is.infinite(value)) NA_real_ else value
}

extract_unit_mentions <- function(permit_id, permit_number, work_description) {
  text <- str_to_upper(coalesce(work_description, ""))
  locations <- str_locate_all(
    text,
    paste0(
    "\\b[0-9]{1,4}\\s*(?:TOTAL\\s+)?(?:DWELLING\\s+|RESIDENTIAL\\s+|APARTMENT\\s+|EFFICIENCY\\s+)?(?:UNITS?|D\\.?U\\.?)\\b",
      "|", attached_house_count_pattern
    )
  )[[1]]

  if (nrow(locations) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    permit_id = permit_id,
    permit_number = permit_number,
    mention_order = seq_len(nrow(locations)),
    unit_mention = str_sub(text, locations[, "start"], locations[, "end"]),
    unit_count = suppressWarnings(as.numeric(str_extract(unit_mention, "[0-9]{1,4}"))),
    mention_context = purrr::map2_chr(
      locations[, "start"],
      locations[, "end"],
      ~ str_squish(str_sub(text, max(1, .x - 90), min(str_length(text), .y + 90)))
    )
  )
}

residential <- readr::read_csv(
  "../input/residential_project_candidate_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    tieback_lineage_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

tieback_members <- readr::read_csv(
  "../input/residential_tieback_members_full.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    tieback_group = readr::col_character(),
    .default = readr::col_guess()
  )
)

tieback_groups <- readr::read_csv(
  "../input/residential_tieback_groups_full.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    tieback_group = readr::col_character(),
    tieback_lineage_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

residential_non_tieback_components <- residential %>%
  filter(review_category != "tieback") %>%
  transmute(
    source_family = "residential",
    project_id = paste0("residential_", pin),
    component_pin = pin,
    candidate_year_min = as.integer(year_built),
    candidate_year_max = as.integer(year_built),
    candidate_units_min = as.numeric(num_apartments),
    candidate_units_max = as.numeric(num_apartments),
    candidate_building_sqft_min = as.numeric(building_sqft),
    candidate_building_sqft_max = as.numeric(building_sqft),
    candidate_land_sqft_min = as.numeric(land_sqft),
    candidate_land_sqft_max = as.numeric(land_sqft),
    review_category,
    within_1500ft
  )

residential_tieback_components <- tieback_members %>%
  left_join(
    tieback_groups %>% select(tieback_group, tieback_lineage_id),
    by = "tieback_group",
    relationship = "many-to-one"
  ) %>%
  group_by(tieback_lineage_id, pin) %>%
  summarise(
    candidate_year_min = finite_min(year_built),
    candidate_year_max = finite_max(year_built),
    candidate_units_min = finite_min(num_apartments),
    candidate_units_max = finite_max(num_apartments),
    candidate_building_sqft_min = finite_min(building_sqft),
    candidate_building_sqft_max = finite_max(building_sqft),
    candidate_land_sqft_min = finite_min(land_sqft),
    candidate_land_sqft_max = finite_max(land_sqft),
    within_1500ft = any(within_1500ft %in% TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    source_family = "residential",
    project_id = tieback_lineage_id,
    component_pin = pin,
    candidate_year_min = as.integer(candidate_year_min),
    candidate_year_max = as.integer(candidate_year_max),
    candidate_units_min,
    candidate_units_max,
    candidate_building_sqft_min,
    candidate_building_sqft_max,
    candidate_land_sqft_min,
    candidate_land_sqft_max,
    review_category = "tieback",
    within_1500ft
  )

commercial_components <- readr::read_csv(
  "../input/commercial_entity_component_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  distinct(project_family_id, component_pin)

commercial_members <- readr::read_csv(
  "../input/commercial_production_family_members.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    keypin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  group_by(project_family_id) %>%
  summarise(
    candidate_year_min = finite_min(production_yearbuilt),
    candidate_year_max = finite_max(production_yearbuilt),
    candidate_units_min = finite_min(production_units),
    candidate_units_max = finite_max(production_units),
    candidate_building_sqft_min = finite_min(production_bldgsf),
    candidate_building_sqft_max = finite_max(production_bldgsf),
    candidate_land_sqft_min = finite_min(production_landsf),
    candidate_land_sqft_max = finite_max(production_landsf),
    within_1500ft = any(within_1500ft %in% TRUE),
    .groups = "drop"
  )

commercial_project_components <- commercial_components %>%
  inner_join(commercial_members, by = "project_family_id", relationship = "many-to-one") %>%
  transmute(
    source_family = "commercial",
    project_id = project_family_id,
    component_pin,
    candidate_year_min = as.integer(candidate_year_min),
    candidate_year_max = as.integer(candidate_year_max),
    candidate_units_min,
    candidate_units_max,
    candidate_building_sqft_min,
    candidate_building_sqft_max,
    candidate_land_sqft_min,
    candidate_land_sqft_max,
    review_category = "commercial_entity",
    within_1500ft
  )

project_components <- bind_rows(
  residential_non_tieback_components,
  residential_tieback_components,
  commercial_project_components
) %>%
  mutate(
    component_pin = normalize_pin(component_pin),
    pin10 = str_sub(component_pin, 1, 10)
  ) %>%
  filter(!is.na(component_pin)) %>%
  distinct(source_family, project_id, component_pin, .keep_all = TRUE) %>%
  arrange(source_family, project_id, component_pin)

if (anyDuplicated(project_components[c("source_family", "project_id", "component_pin")]) > 0) {
  stop("Project-component evidence keys are not unique.", call. = FALSE)
}

permits_sf <- sf::st_read("../output/building_permits_for_verification.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435) %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    !is.na(application_start_date),
    !is.na(issue_date)
  )

permit_coordinates <- sf::st_coordinates(permits_sf)
permits <- permits_sf %>%
  sf::st_drop_geometry() %>%
  mutate(
    permit_id = as.character(id),
    permit_number = as.character(permit),
    permit_x_3435 = permit_coordinates[, "X"],
    permit_y_3435 = permit_coordinates[, "Y"],
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    application_year = lubridate::year(application_date),
    issue_year = lubridate::year(issue_date),
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    referenced_permit_numbers = purrr::map2_chr(
      str_extract_all(
        str_to_upper(coalesce(work_description, "")),
        "(?<![0-9])10[0-9]{7}(?![0-9])"
      ),
      permit_number,
      ~ paste(sort(setdiff(unique(.x), .y)), collapse = "/")
    )
  ) %>%
  select(
    permit_id,
    permit_number,
    pin,
    application_date,
    issue_date,
    application_year,
    issue_year,
    permit_status,
    permit_address,
    referenced_permit_numbers,
    permit_x_3435,
    permit_y_3435,
    work_description
  ) %>%
  separate_rows(pin, sep = "\\s*\\|\\s*") %>%
  mutate(pin10 = str_replace_all(pin, "[^0-9]", "")) %>%
  filter(str_detect(pin10, "^[0-9]{10}$")) %>%
  distinct(permit_id, pin10, .keep_all = TRUE)

permits_by_pin <- split(permits, permits$pin10)

exact_permit_matches <- purrr::map_dfr(
  seq_len(nrow(project_components)),
  function(i) {
    candidate <- project_components[i, ]
    matched <- permits_by_pin[[candidate$pin10]]
    if (is.null(matched) || nrow(matched) == 0) {
      return(tibble::tibble())
    }
    bind_cols(
      candidate[rep(1, nrow(matched)), ],
      matched %>% select(-pin10),
      tibble::tibble(match_method = "exact_component_pin10")
    )
  }
) %>%
  mutate(
    plausible_application_window =
      application_year >= candidate_year_min - 6 &
      application_year <= candidate_year_max + 2,
    plausible_issue_window =
      is.na(issue_year) |
      (issue_year >= candidate_year_min - 4 & issue_year <= candidate_year_max + 2)
  ) %>%
  distinct(source_family, project_id, component_pin, permit_id, .keep_all = TRUE) %>%
  arrange(source_family, project_id, application_date, permit_id)

permit_unit_mentions_base <- exact_permit_matches %>%
  distinct(permit_id, permit_number, work_description) %>%
  purrr::pmap_dfr(extract_unit_mentions)

unit_mentions_by_permit <- split(permit_unit_mentions_base, permit_unit_mentions_base$permit_id)
project_permit_links <- exact_permit_matches %>%
  select(source_family, project_id, component_pin, permit_id) %>%
  distinct()

permit_unit_mentions <- purrr::map_dfr(
  seq_len(nrow(project_permit_links)),
  function(i) {
    link <- project_permit_links[i, ]
    mentions <- unit_mentions_by_permit[[link$permit_id]]
    if (is.null(mentions) || nrow(mentions) == 0) {
      return(tibble::tibble())
    }
    bind_cols(
      link[rep(1, nrow(mentions)), ],
      mentions %>% select(-permit_id)
    )
  }
) %>%
  select(
    source_family,
    project_id,
    component_pin,
    permit_id,
    permit_number,
    mention_order,
    unit_mention,
    unit_count,
    mention_context
  ) %>%
  arrange(source_family, project_id, permit_id, mention_order)

SaveData(exact_permit_matches, c("project_id", "component_pin", "permit_id"), "../output/new_construction_exact_permit_matches.csv")
SaveData(permit_unit_mentions, c("project_id", "component_pin", "permit_id", "mention_order"), "../output/new_construction_permit_unit_mentions.csv")
SaveData(project_components, c("project_id", "component_pin"), "../output/new_construction_project_components.csv")
