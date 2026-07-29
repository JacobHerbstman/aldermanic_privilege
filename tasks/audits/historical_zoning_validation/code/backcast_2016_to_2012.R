# setwd("tasks/audits/historical_zoning_validation/code")

library(dplyr)
library(readr)
library(sf)
library(stringr)

snapshot_zone_group <- function(zone_code) {
  zone_code <- str_to_upper(as.character(zone_code))
  case_when(
    str_detect(zone_code, "^RS-?") ~ "Single-Family Residential",
    str_detect(zone_code, "^(RT|RM)-?") ~ "Multi-Family Residential",
    str_detect(zone_code, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    str_detect(zone_code, "^C-?[1-7]-") ~ "Commercial",
    str_detect(zone_code, "^M-?[1-7]-") ~ "Industrial",
    str_starts(zone_code, "DX-") | str_starts(zone_code, "DR-") |
      str_starts(zone_code, "DS-") | str_starts(zone_code, "DC-") ~ "Downtown",
    str_starts(zone_code, "PD") ~ "Planned Development",
    str_starts(zone_code, "PMD") ~ "Planned Manufacturing",
    str_starts(zone_code, "POS") ~ "Open Space",
    TRUE ~ "Other"
  )
}

normalize_application <- function(value) {
  value <- str_to_upper(as.character(value))
  value <- str_replace_all(value, "[^A-Z0-9]", "")
  if_else(is.na(value) | value == "", NA_character_, value)
}

split_groups <- function(value) {
  if (is.na(value) || !nzchar(str_trim(value))) return(character())
  sort(unique(str_trim(str_split(value, ";", simplify = FALSE)[[1]])))
}

zoning_2016 <- st_read(
  "/vsizip/../input/zoning_jan2016.zip/zoning_2016_01.shp",
  quiet = TRUE
) %>%
  st_transform(3435)
ordinance_date_text <- as.character(zoning_2016$ORDINANCE1)
ordinance_date_text[str_starts(ordinance_date_text, "-")] <- NA_character_
zoning_2016 <- zoning_2016 %>%
  transmute(
    zone_group_2016 = snapshot_zone_group(ZONE_CLASS),
    application_key_2016 = normalize_application(ORDINANCE_),
    ordinance_date_2016 = as.Date(ordinance_date_text),
    geometry
  ) %>%
  st_make_valid()

events <- read_csv(
  "../output/historical_zoning_matter_parsing.csv",
  col_types = cols(.default = col_character())
) %>%
  transmute(
    event_date = as.Date(matter_passed_date),
    application_key = normalize_application(application_key),
    source_groups = from_groups,
    destination_groups = to_groups,
    matter_id,
    matter_title
  ) %>%
  filter(
    event_date > as.Date("2012-10-31"),
    event_date <= as.Date("2015-11-18"),
    !is.na(application_key)
  )

zoning_2016$candidate_zone_group_2012 <- zoning_2016$zone_group_2016
zoning_2016$backcast_status <- "anchor_predates_2012_cutoff_or_undated"
zoning_2016$matched_matter_id <- NA_character_

post_cutoff_rows <- which(
  !is.na(zoning_2016$ordinance_date_2016) &
    zoning_2016$ordinance_date_2016 > as.Date("2012-10-31")
)
for (row in post_cutoff_rows) {
  candidates <- which(
    events$event_date == zoning_2016$ordinance_date_2016[row] &
      events$application_key == zoning_2016$application_key_2016[row]
  )
  destination_mismatch <- FALSE
  if (length(candidates) > 0) {
    destination_candidates <- candidates[vapply(
      events$destination_groups[candidates],
      function(value) zoning_2016$zone_group_2016[row] %in% split_groups(value),
      logical(1)
    )]
    if (length(destination_candidates) > 0) {
      candidates <- destination_candidates
    } else {
      candidates <- integer()
      destination_mismatch <- TRUE
    }
  }

  if (length(candidates) == 0) {
    zoning_2016$candidate_zone_group_2012[row] <- NA_character_
    zoning_2016$backcast_status[row] <- if_else(
      destination_mismatch,
      "recorded_event_destination_disagrees",
      "event_not_found"
    )
    next
  }
  if (length(candidates) > 1) {
    zoning_2016$candidate_zone_group_2012[row] <- NA_character_
    zoning_2016$backcast_status[row] <- "event_ambiguous"
    next
  }

  event_row <- candidates[[1]]
  origins <- split_groups(events$source_groups[event_row])
  zoning_2016$matched_matter_id[row] <- events$matter_id[event_row]
  if (length(origins) == 1) {
    zoning_2016$candidate_zone_group_2012[row] <- origins[[1]]
    zoning_2016$backcast_status[row] <- "unique_immediate_prior_group"
  } else if (length(origins) > 1) {
    zoning_2016$candidate_zone_group_2012[row] <- NA_character_
    zoning_2016$backcast_status[row] <- "multiple_immediate_prior_groups"
  } else {
    zoning_2016$candidate_zone_group_2012[row] <- NA_character_
    zoning_2016$backcast_status[row] <- "immediate_prior_group_not_parsed"
  }
}

zoning_2012 <- st_read(
  "/vsizip/../input/zoning_nov2012.zip/Zoning_nov2012.shp",
  quiet = TRUE
) %>%
  st_transform(3435) %>%
  transmute(zone_group_2012 = snapshot_zone_group(ZONE_CLASS), geometry) %>%
  st_make_valid()

polygon_points <- zoning_2016 %>%
  mutate(anchor_polygon_id = row_number()) %>%
  st_point_on_surface()
polygon_matches <- st_intersects(polygon_points, zoning_2012)

polygon_check <- polygon_points %>%
  st_drop_geometry() %>%
  mutate(
    official_group_count = vapply(
      polygon_matches,
      function(rows) length(unique(zoning_2012$zone_group_2012[rows])),
      integer(1)
    ),
    zone_group_2012 = vapply(
      polygon_matches,
      function(rows) paste(sort(unique(zoning_2012$zone_group_2012[rows])), collapse = ";"),
      character(1)
    ),
    backcast_resolved = !is.na(candidate_zone_group_2012),
    backcast_agrees = backcast_resolved & vapply(
      seq_along(polygon_matches),
      function(row) candidate_zone_group_2012[row] %in%
        zoning_2012$zone_group_2012[polygon_matches[[row]]],
      logical(1)
    )
  )

polygon_check %>%
  write_csv("../output/historical_zoning_2016_to_2012_backcast_polygon_check.csv")

polygon_check %>%
  filter(backcast_status == "unique_immediate_prior_group") %>%
  summarize(
    anchor_polygons = nrow(polygon_check),
    uniquely_reversed_polygons = n(),
    uniquely_reversed_correct = sum(backcast_agrees),
    uniquely_reversed_agreement_share = mean(backcast_agrees)
  ) %>%
  write_csv("../output/historical_zoning_2016_to_2012_backcast_polygon_summary.csv")

projects <- st_read("../input/parcels_with_geometry.gpkg", quiet = TRUE) %>%
  filter(construction_year >= 2006, construction_year <= 2022) %>%
  select(pin, unitscount, dist_to_boundary)

project_count <- nrow(projects)
projects <- suppressWarnings(
  st_join(
    projects,
    zoning_2016 %>%
      select(
        zone_group_2016,
        candidate_zone_group_2012,
        application_key_2016,
        ordinance_date_2016,
        backcast_status,
        matched_matter_id
      ),
    left = TRUE,
    largest = TRUE
  )
)
projects <- suppressWarnings(
  st_join(projects, zoning_2012, left = TRUE, largest = TRUE)
)
if (nrow(projects) != project_count) {
  stop("A zoning join changed the density-project row count.", call. = FALSE)
}

projects <- projects %>%
  st_drop_geometry() %>%
  mutate(
    within_500ft = dist_to_boundary <= 500,
    multifamily = unitscount > 1,
    backcast_resolved = !is.na(candidate_zone_group_2012),
    true_group_change = zone_group_2012 != zone_group_2016,
    backcast_agrees = backcast_resolved &
      candidate_zone_group_2012 == zone_group_2012
  )

projects %>%
  write_csv("../output/historical_zoning_2016_to_2012_backcast_project_check.csv")

projects %>%
  filter(!backcast_agrees) %>%
  write_csv("../output/historical_zoning_2016_to_2012_backcast_mismatches.csv")

samples <- list(
  all_density_points = rep(TRUE, nrow(projects)),
  within_500ft = projects$within_500ft,
  multifamily_within_500ft = projects$within_500ft & projects$multifamily
)

summary_rows <- lapply(names(samples), function(sample_name) {
  sample_data <- projects %>%
    slice(which(samples[[sample_name]]))
  changed_data <- sample_data %>%
    filter(true_group_change)
  reversed_data <- sample_data %>%
    filter(backcast_status == "unique_immediate_prior_group")
  tibble(
    sample = sample_name,
    projects = nrow(sample_data),
    resolved_projects = sum(sample_data$backcast_resolved),
    resolved_share = mean(sample_data$backcast_resolved),
    agreement_projects = sum(sample_data$backcast_agrees),
    agreement_share_all = mean(sample_data$backcast_agrees),
    agreement_share_resolved = mean(
      sample_data$backcast_agrees[sample_data$backcast_resolved]
    ),
    true_changed_projects = nrow(changed_data),
    changed_projects_resolved = sum(changed_data$backcast_resolved),
    changed_projects_correct = sum(changed_data$backcast_agrees),
    changed_agreement_share_resolved = mean(
      changed_data$backcast_agrees[changed_data$backcast_resolved]
    ),
    uniquely_reversed_projects = nrow(reversed_data),
    uniquely_reversed_correct = sum(reversed_data$backcast_agrees),
    uniquely_reversed_agreement_share = mean(reversed_data$backcast_agrees)
  )
})

bind_rows(summary_rows) %>%
  write_csv("../output/historical_zoning_2016_to_2012_backcast_summary.csv")
