# setwd("tasks/audits/zoning_stalls_and_delays/code")
# Large projects compared with the most similar large projects in other aldermen's wards, one row per large project.
# A large project is an application in eLMS (combine_amendments.R) whose form (tasks/extract_zoning_application_forms)
# states at least large_units dwelling units or a building of at least large_height_ft feet. Its matches are the
# match_count large projects of other aldermen nearest to it among those of the same kind and place: the kind is a
# planned development (a change to a planned development, or an amendment whose districts are unknown, mostly planned
# developments' own amendments) or a change of district; the place is within downtown_miles of the Loop (State and
# Madison) or not. Nearness is the difference in log dwelling units (log height for projects stating no units, matched
# among those) plus the difference in years of introduction divided by years_per_doubling. For each project and its
# matches: days from introduction to passage, and whether it passed within a year, known once it passed or was
# followed for a year (eLMS is followed to its last recorded action). days_vs_matches is the project's log days to
# passage less the mean of its passed matches'.
large_units <- 100
large_height_ft <- 150
match_count <- 5
downtown_miles <- 2
years_per_doubling <- 4
loop <- c(longitude = -87.6278, latitude = 41.8819)

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

elms <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE)
elms_end <- max(c(elms$introduction_date, elms$final_action_date), na.rm = TRUE)
forms <- read_csv("../input/application_form_fields.csv", show_col_types = FALSE) |>
  select(record_number, dwelling_units, height_ft)
locations <- read_csv("../input/zoning_amendment_wards.csv", show_col_types = FALSE) |>
  filter(!is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3435)
loop_point <- st_transform(st_sfc(st_point(loop), crs = 4326), 3435)
locations <- tibble(matter_id = locations$matter_id,
  miles_to_loop = as.numeric(st_distance(locations, loop_point)) / 5280)

projects <- read_csv("../output/zoning_amendments.csv", show_col_types = FALSE) |>
  filter(source == "elms", filer == "applicant", !is.na(alderman)) |>
  inner_join(select(elms, matter_id, record_number, raw_days = days_to_passage), by = c(amendment = "matter_id"),
    relationship = "one-to-one") |>
  left_join(forms, by = "record_number", relationship = "one-to-one") |>
  inner_join(locations, by = c(amendment = "matter_id"), relationship = "one-to-one") |>
  filter(coalesce(dwelling_units, 0) >= large_units | coalesce(height_ft, 0) >= large_height_ft) |>
  mutate(planned_development = direction %in% c("to_planned_development", "unknown"),
    downtown = miles_to_loop <= downtown_miles,
    size = if_else(!is.na(dwelling_units) & dwelling_units > 0, log(dwelling_units), log(height_ft)),
    size_measure = if_else(!is.na(dwelling_units) & dwelling_units > 0, "units", "height"),
    year = as.numeric(format(introduction_date, "%Y")),
    days = if_else(outcome == "passed" & raw_days > 0, raw_days, NA_real_),
    passed_within_year = case_when(!is.na(days) ~ days <= 365,
      as.numeric(elms_end - introduction_date) >= 365 ~ FALSE))
stopifnot(!anyDuplicated(projects$record_number), !anyNA(projects$size))

matched <- bind_rows(lapply(seq_len(nrow(projects)), function(i) {
  project <- projects[i, ]
  projects |>
    filter(alderman != project$alderman, planned_development == project$planned_development,
      downtown == project$downtown, size_measure == project$size_measure) |>
    mutate(distance = abs(size - project$size) + abs(year - project$year) / years_per_doubling) |>
    slice_min(distance, n = match_count, with_ties = FALSE) |>
    summarise(matches = paste(record_number, collapse = ";"),
      matched_median_days = median(days, na.rm = TRUE), matched_mean_log_days = mean(log(days), na.rm = TRUE),
      matched_passed_within_year = mean(passed_within_year, na.rm = TRUE), matched_distance = max(distance)) |>
    mutate(record_number = project$record_number)
}))
large_project_matches <- projects |>
  inner_join(matched, by = "record_number", relationship = "one-to-one") |>
  transmute(record_number, alderman, ward, introduction_date, planned_development, downtown,
    miles_to_loop, dwelling_units, height_ft, outcome, days, passed_within_year, matches, matched_median_days,
    matched_passed_within_year, days_vs_matches = log(days) - matched_mean_log_days, matched_distance) |>
  arrange(alderman, introduction_date)
SaveData(large_project_matches, "record_number", "../output/large_project_matches.csv")
