# setwd("tasks/working_paper_release_audit/code")

library(dplyr)
library(readr)
library(stringr)
source("../../shared/code/normalize_chicago_address.R")

review <- read_csv("../input/preferred_historical_address_geocodes.csv",
  col_types = cols(component_pin = col_character(), .default = col_guess())) |>
  filter(census_status == "street_address_mismatch") |>
  select(request_id, project_id, component_pin, target_year, selected_address,
    selected_address_year, matched_address, query_street_address, matched_street_address,
    census_status)
city <- read_csv("../input/preferred_chicago_address_geocodes.csv", show_col_types = FALSE) |>
  select(request_id, chicago_status, chicago_matched_address)
stopifnot(!anyDuplicated(review$request_id), !anyDuplicated(city$request_id))
review <- left_join(review, city, by = "request_id", relationship = "one-to-one")
permits <- data.table::fread("../input/building_permits_full.csv", colClasses = "character",
  select = c("id", "permit_", "permit_type", "application_start_date", "issue_date",
    "street_number", "street_direction", "street_name", "work_description", "pin_list")) |>
  as_tibble() |>
  mutate(permit_address = geocode_street_address(paste(street_number, street_direction, street_name))) |>
  arrange(issue_date, id)
stopifnot(!anyNA(permits$id), !anyDuplicated(permits$id))
review <- review |>
  mutate(query_address_permits = 0L, alternative_address_permits = 0L,
    alternative_permit_ids = NA_character_, alternative_permit_dates = NA_character_,
    alternative_permit_descriptions = NA_character_, alternative_permit_pin_lists = NA_character_)
for (i in seq_len(nrow(review))) {
  review$query_address_permits[i] <- sum(permits$permit_address == review$query_street_address[i], na.rm = TRUE)
  evidence <- permits |> filter(permit_address == review$matched_street_address[i])
  review$alternative_address_permits[i] <- nrow(evidence)
  if (nrow(evidence)) {
    review$alternative_permit_ids[i] <- paste(evidence$permit_, collapse = " / ")
    review$alternative_permit_dates[i] <- paste(evidence$issue_date, collapse = " / ")
    review$alternative_permit_descriptions[i] <- paste(evidence$work_description, collapse = " / ")
    review$alternative_permit_pin_lists[i] <- paste(evidence$pin_list, collapse = " / ")
  }
}
# Address corroboration alone does not establish the historical parent/successor PIN link.
write_csv(review, "../output/geocoder_mismatch_review.csv")
