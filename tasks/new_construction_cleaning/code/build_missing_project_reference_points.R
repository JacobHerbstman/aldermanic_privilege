# setwd("tasks/new_construction_cleaning/code")

library(dplyr)
library(readr)

missing <- read_csv("../output/historical_project_parcel_coverage.csv",
  col_types = cols(component_pin = col_character(), pin10 = col_character(), .default = col_guess())) |>
  filter(coverage_status == "missing") |>
  mutate(request_id = row_number(), .before = 1)
queries <- read_csv("../input/predecessor_history_queries.csv", col_types = cols(pin = col_character()))
history <- read_csv("../input/predecessor_parcel_history.csv",
  col_types = cols(pin = col_character(), pin10 = col_character(), row_id = col_character(), .default = col_double()))
stopifnot(!anyDuplicated(missing[c("source_family", "project_id", "component_pin", "target_year")]),
          !anyDuplicated(queries), !anyNA(queries),
          nrow(anti_join(missing, queries, by = c("component_pin" = "pin"))) == 0L,
          nrow(anti_join(history, queries, by = "pin")) == 0L,
          !anyNA(history[c("pin", "year", "row_id")]),
          !anyDuplicated(history[c("pin", "year")]), !anyDuplicated(history$row_id))
history <- filter(history, is.finite(x_3435), is.finite(y_3435))
history_rows <- split(seq_len(nrow(history)), history$pin)
selected_rows <- rep(NA_integer_, nrow(missing))
for (i in seq_len(nrow(missing))) {
  rows <- history_rows[[missing$component_pin[i]]]
  if (length(rows) == 0L) next
  years <- history$year[rows]
  # Closest assessment year; prefer the earlier year when equally close.
  selected_rows[i] <- rows[order(abs(years - missing$target_year[i]),
                                years > missing$target_year[i], -years)[1]]
}
selected <- history[selected_rows, ]
reference_points <- missing |>
  mutate(history_reference_year = selected$year,
    history_reference_year_gap = selected$year - target_year,
    history_row_id = selected$row_id,
    reference_source = if_else(is.na(selected_rows), "unresolved", "nearest_exact_pin_history"),
    reference_x_3435 = selected$x_3435, reference_y_3435 = selected$y_3435)
# Missing history remains explicit; no address or permit proxy is substituted.
write_csv(reference_points, "../output/missing_project_reference_points.csv")
