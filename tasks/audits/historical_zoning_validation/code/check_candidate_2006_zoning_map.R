# setwd("tasks/audits/historical_zoning_validation/code")

library(dplyr)
library(readr)
library(sf)

zoning <- st_read("../output/historical_zoning_2006_candidate.gpkg", quiet = TRUE) %>%
  select(
    map_zone_group_2006 = candidate_zone_group_2006,
    map_reconstruction_status = reconstruction_status,
    map_reconstruction_resolved = reconstruction_resolved,
    map_broad_group_changed = broad_group_changed_since_2006
  )

projects <- st_read("../input/parcels_with_geometry.gpkg", quiet = TRUE) %>%
  filter(construction_year >= 2006, construction_year <= 2022) %>%
  select(pin, construction_year, unitscount, dist_to_boundary)

if (anyDuplicated(projects$pin)) {
  stop("Density-project PINs are not unique.", call. = FALSE)
}

projects <- suppressWarnings(
  st_join(projects, zoning, left = TRUE, largest = TRUE)
) %>%
  st_drop_geometry() %>%
  mutate(
    within_500ft = dist_to_boundary <= 500,
    multifamily = unitscount > 1
  )

project_reconstruction <- read_csv(
  "../output/historical_zoning_2006_project_map.csv",
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    project_zone_group_2006 = candidate_zone_group_2006,
    project_reconstruction_status = reconstruction_status
  )

projects <- projects %>%
  left_join(project_reconstruction, by = "pin", relationship = "one-to-one") %>%
  mutate(
    jointly_resolved = !is.na(map_zone_group_2006) & !is.na(project_zone_group_2006),
    implementations_agree = jointly_resolved &
      map_zone_group_2006 == project_zone_group_2006
  )

projects %>%
  write_csv("../output/historical_zoning_2006_candidate_project_check.csv")

samples <- list(
  all_density_points = rep(TRUE, nrow(projects)),
  within_500ft = projects$within_500ft,
  multifamily_within_500ft = projects$within_500ft & projects$multifamily
)

summary_rows <- lapply(names(samples), function(sample_name) {
  sample_data <- projects %>%
    slice(which(samples[[sample_name]]))
  tibble(
    sample = sample_name,
    projects = nrow(sample_data),
    map_resolved = sum(!is.na(sample_data$map_zone_group_2006)),
    map_resolved_share = mean(!is.na(sample_data$map_zone_group_2006)),
    map_changed_broad_group = sum(sample_data$map_broad_group_changed, na.rm = TRUE),
    project_method_resolved = sum(!is.na(sample_data$project_zone_group_2006)),
    jointly_resolved = sum(sample_data$jointly_resolved),
    jointly_resolved_agree = sum(sample_data$implementations_agree),
    jointly_resolved_disagree = sum(
      sample_data$jointly_resolved & !sample_data$implementations_agree
    )
  )
})

bind_rows(summary_rows) %>%
  write_csv("../output/historical_zoning_2006_candidate_project_summary.csv")
