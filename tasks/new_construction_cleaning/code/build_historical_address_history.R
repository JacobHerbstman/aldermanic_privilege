# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
source("../../shared/code/normalize_chicago_address.R")

targets <- read_csv(
  "../input/historical_parcel_requests.csv", show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>% select(pin, construction_year)
if (anyNA(targets$pin) || anyDuplicated(targets$pin)) stop("Historical address requests must be unique.")

historical_addresses <- read_csv(
  "../input/density_historical_address_records.csv", show_col_types = FALSE,
  col_types = cols(pin = col_character(), pin10 = col_character(), .default = col_guess())
)
if (anyNA(historical_addresses$pin) || anyNA(historical_addresses$year) ||
    anyDuplicated(historical_addresses[c("pin", "year")])) {
  stop("Historical addresses must be unique by PIN-year.")
}
if (nrow(anti_join(historical_addresses, targets, by = "pin")) > 0) {
  stop("Historical address records include PINs outside the recorded request scope.")
}
historical_addresses <- historical_addresses %>%
  inner_join(targets, by = "pin", relationship = "many-to-one") %>%
  mutate(
    address_normalized = normalize_address(property_address),
    year_gap = year - construction_year,
    absolute_year_gap = abs(year_gap)
  ) %>% filter(!is.na(address_normalized))

nearest_address_rows <- historical_addresses %>%
  group_by(pin) %>%
  filter(absolute_year_gap == min(absolute_year_gap)) %>%
  arrange(desc(year <= construction_year), desc(year), .by_group = TRUE) %>%
  mutate(nearest_address_count = n_distinct(address_normalized)) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    pin,
    selected_address = property_address,
    selected_address_normalized = address_normalized,
    selected_address_year = year,
    selected_address_year_gap = year_gap,
    nearest_address_count
  )

address_history_summary <- historical_addresses %>%
  group_by(pin) %>%
  summarise(
    address_history_rows = n(),
    distinct_historical_addresses = n_distinct(address_normalized),
    .groups = "drop"
  )

selected_history <- targets %>%
  left_join(nearest_address_rows, by = "pin", relationship = "one-to-one") %>%
  left_join(address_history_summary, by = "pin", relationship = "one-to-one") %>%
  mutate(
    address_selection_status = case_when(
      is.na(selected_address_normalized) ~ "no_historical_property_address",
      nearest_address_count > 1 ~ "ambiguous_nearest_year_address",
      TRUE ~ "selected_nearest_year_address"
    )
  )

# Apply recorded address corrections before any consumer requests a geocode.
corrections <- read_csv("../input/historical_address_corrections.csv",
  col_types = cols(.default = col_character()))
stopifnot(!anyNA(corrections), !anyDuplicated(corrections[c("pin", "original_address")]),
  nrow(anti_join(corrections, selected_history,
    by = c("pin", "original_address" = "selected_address_normalized"))) == 0L)
selected_history <- selected_history %>%
  left_join(corrections %>% select(pin, original_address, corrected_address, address_correction_reason = reason),
    by = c("pin", "selected_address_normalized" = "original_address"), relationship = "one-to-one") %>%
  mutate(source_selected_address = selected_address,
    selected_address = coalesce(corrected_address, selected_address),
    selected_address_normalized = normalize_address(selected_address)) %>%
  select(-corrected_address)
SaveData(selected_history, c("pin"), "../output/density_parcel_address_selected_history.csv")
