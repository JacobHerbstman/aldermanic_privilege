# setwd("tasks/new_construction_cleaning/code")

library(dplyr)
library(readr)
library(tidyr)

components <- read_csv("../output/new_construction_project_components.csv",
  col_types = cols(source_family = col_character(), project_id = col_character(),
    component_pin = col_character(), pin10 = col_character(),
    candidate_year_min = col_double(), candidate_year_max = col_double(),
    within_1500ft = col_logical(), .default = col_skip())) |>
  filter(within_1500ft, is.finite(candidate_year_min), is.finite(candidate_year_max)) |>
  mutate(first_year = pmax(2006L, as.integer(candidate_year_min)),
         last_year = pmin(2022L, as.integer(candidate_year_max))) |>
  filter(first_year <= last_year)

# A candidate can still have several possible construction years at this stage.
requests <- components |>
  mutate(target_year = Map(seq.int, first_year, last_year)) |>
  unnest_longer(target_year) |>
  distinct(source_family, project_id, component_pin, pin10, target_year) |>
  arrange(target_year, pin10, source_family, project_id, component_pin)
stopifnot(nrow(requests) > 0L, !anyNA(requests),
          all(grepl("^[0-9]{14}$", requests$component_pin)),
          all(requests$pin10 == substr(requests$component_pin, 1, 10)),
          !anyDuplicated(requests[c("source_family", "project_id", "component_pin", "target_year")]))
write_csv(requests, "../output/historical_project_parcel_requests.csv")
