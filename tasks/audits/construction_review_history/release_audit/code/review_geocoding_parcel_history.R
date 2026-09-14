# setwd("tasks/working_paper_release_audit/code")

library(dplyr)
library(readr)

requests <- read_csv("../input/preferred_address_geocode_requests.csv",
  col_types = cols(component_pin = col_character(), .default = col_guess()))
queries <- read_csv("../input/geocoding_history_queries.csv", col_types = cols(pin = col_character()))
history <- read_csv("../input/geocoding_parcel_history.csv",
  col_types = cols(pin = col_character(), year = col_integer(), lon = col_double(), lat = col_double(),
    row_id = col_character(), .default = col_skip()))
stopifnot(!anyDuplicated(requests$request_id), !anyDuplicated(queries$pin),
          !anyDuplicated(history[c("pin", "year")]), !anyNA(history[c("pin", "year", "row_id")]),
          nrow(anti_join(requests, queries, by = c("component_pin" = "pin"))) == 0L)
review <- requests |>
  select(request_id, project_id, component_pin, target_year, selected_address) |>
  mutate(history_year = NA_integer_, history_year_gap = NA_integer_, history_row_id = NA_character_,
    history_x_3435 = NA_real_, history_y_3435 = NA_real_, history_status = "no_finite_historical_coordinate")
for (i in seq_len(nrow(review))) {
  candidates <- history |>
    filter(pin == review$component_pin[i], is.finite(lon), is.finite(lat)) |>
    arrange(abs(year - review$target_year[i]), year)
  if (!nrow(candidates)) next
  selected <- candidates[1, ]
  point <- selected |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |> sf::st_transform(3435)
  review$history_year[i] <- selected$year
  review$history_year_gap[i] <- abs(selected$year - review$target_year[i])
  review$history_row_id[i] <- selected$row_id
  review$history_x_3435[i] <- sf::st_coordinates(point)[1, "X"]
  review$history_y_3435[i] <- sf::st_coordinates(point)[1, "Y"]
  review$history_status[i] <- "exact_pin_history_available_for_review"
}
# This evaluates a proposed priority; production does not consume these selected points.
write_csv(review, "../output/geocoding_parcel_history_review.csv")
