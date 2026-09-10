# setwd("tasks/new_construction_analysis_data/code")
source("../../setup_environment/code/packages.R")

projects <- readr::read_csv("../output/construction_regressors.csv",
  col_types = readr::cols(project_id = "c", component_pins = "c", ward_pair = "c", segment_id = "c", .default = readr::col_guess()))
zoning <- readr::read_csv("../input/preferred_new_construction_zoning.csv",
  col_types = readr::cols(project_id = "c", construction_year = "i",
    construction_zone_group = "c", zoning_assignment_source = "c", .default = readr::col_skip()))
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(zoning$project_id),
  all(projects$project_id %in% zoning$project_id))
projects <- projects |>
  left_join(zoning |> select(project_id, zoning_year = construction_year,
    zone_group = construction_zone_group, zoning_assignment_source),
    by = "project_id", relationship = "one-to-one")
stopifnot(all(projects$construction_year == projects$zoning_year),
  !any(projects$within_500ft & (projects$allow_far | projects$allow_dupac) &
    (is.na(projects$zone_group) | projects$zone_group == "")))
readr::write_csv(projects |> select(-zoning_year) |> arrange(construction_year, project_id),
  "../output/new_construction_analysis_data.csv", na = "")
