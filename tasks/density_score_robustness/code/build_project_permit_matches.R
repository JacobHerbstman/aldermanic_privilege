# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_score_robustness/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
projects <- read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = cols(project_id = "c", component_pins = "c", .default = col_guess()))
recorded <- read_csv("../adjudication/project_permit_matches.csv", col_types = cols(.default = "c"))
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(recorded[c("project_id", "permit_id")]))
permits <- st_read("../input/building_permits_for_verification.gpkg", quiet = TRUE) |> st_transform(3435) |>
  filter(permit_type == "PERMIT - NEW CONSTRUCTION", !is.na(issue_date), !is.na(application_start_date)) |>
  mutate(permit_id = as.character(id), application_year = lubridate::year(application_start_date),
    issue_year = lubridate::year(issue_date))
stopifnot(!anyDuplicated(permits$permit_id))
permit_pins <- st_drop_geometry(permits) |> select(permit_id, pin) |>
  separate_rows(pin, sep = "\\s*\\|\\s*") |>
  mutate(pin10 = str_replace_all(pin, "[^0-9]", "")) |>
  filter(str_detect(pin10, "^[0-9]{10}$")) |> distinct(pin10, permit_id)
by_pin <- split(permit_pins$permit_id, permit_pins$pin10)
components <- projects |> select(project_id, component_pins) |>
  separate_rows(component_pins, sep = "/") |> mutate(pin10 = substr(component_pins, 1, 10)) |>
  distinct(project_id, pin10)
links <- list()
for (i in seq_len(nrow(components))) {
  ids <- by_pin[[components$pin10[i]]]
  if (length(ids)) links[[length(links) + 1L]] <- tibble(project_id = components$project_id[i], permit_id = ids, match_source = "exact_parcel_number")
}
# Only use a saved project polygon with the same ID and construction year.
polygons <- st_read("../input/preferred_project_year_geometry.gpkg", quiet = TRUE) |> st_transform(3435) |>
  inner_join(projects |> select(project_id, construction_year), by = "project_id", relationship = "many-to-one") |>
  filter(target_year == construction_year)
inside <- st_intersects(polygons, permits)
for (i in seq_len(nrow(polygons))) {
  if (length(inside[[i]])) links[[length(links) + 1L]] <- tibble(project_id = polygons$project_id[i],
    permit_id = permits$permit_id[inside[[i]]], match_source = "inside_construction_year_parcel")
}
additions <- bind_rows(links) |>
  left_join(projects |> select(project_id, construction_year), by = "project_id", relationship = "many-to-one") |>
  left_join(st_drop_geometry(permits) |> select(permit_id, application_year, issue_year), by = "permit_id", relationship = "many-to-one") |>
  filter(between(construction_year - application_year, -2, 6), between(construction_year - issue_year, -2, 4)) |>
  select(project_id, permit_id, match_source)
recorded <- recorded |> semi_join(projects, by = "project_id") |> mutate(match_source = "recorded_match")
result <- bind_rows(recorded, additions) |> group_by(project_id, permit_id) |>
  summarise(match_source = paste(sort(unique(match_source)), collapse = "/"), .groups = "drop") |>
  arrange(project_id, permit_id)
stopifnot(!anyDuplicated(result[c("project_id", "permit_id")]), !anyNA(result))
SaveData(result, c("project_id", "permit_id"), "../output/project_permit_matches.csv")
