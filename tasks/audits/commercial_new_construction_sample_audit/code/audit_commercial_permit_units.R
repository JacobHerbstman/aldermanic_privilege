# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

extract_unit_count <- function(description) {
  unit_matches <- stringr::str_match_all(
    stringr::str_to_upper(dplyr::coalesce(description, "")),
    "\\b([0-9]{1,4})\\s*(?:TOTAL\\s+)?(?:DWELLING\\s+|RESIDENTIAL\\s+|APARTMENT\\s+|EFFICIENCY\\s+)?UNITS?\\b"
  )[[1]]
  du_matches <- stringr::str_match_all(
    stringr::str_to_upper(dplyr::coalesce(description, "")),
    "\\b([0-9]{1,4})\\s*D\\.?U\\.?\\b"
  )[[1]]
  counts <- suppressWarnings(as.numeric(c(unit_matches[, 2], du_matches[, 2])))
  if (length(counts) == 0 || all(is.na(counts))) NA_real_ else max(counts, na.rm = TRUE)
}

commercial <- readr::read_csv(
  "../output/commercial_500ft_sample_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(in_main_model_inputs) %>%
  select(
    pin,
    address = production_address,
    construction_year,
    production_units,
    production_bldgsf,
    production_landsf
  )

permit_matches <- readr::read_csv(
  "../output/commercial_new_construction_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), permit_id = readr::col_character(), .default = readr::col_guess())
) %>%
  inner_join(commercial %>% select(pin), by = "pin", relationship = "many-to-one") %>%
  mutate(
    permit_units = purrr::map_dbl(work_description, extract_unit_count),
    permit_application_year = as.integer(permit_application_year)
  ) %>%
  filter(!is.na(permit_units))

permit_summary <- permit_matches %>%
  group_by(pin) %>%
  summarise(
    unit_permits = n_distinct(permit_id),
    permit_unit_counts = paste(sort(unique(permit_units)), collapse = "/"),
    unambiguous_permit_units = if_else(n_distinct(permit_units) == 1, first(permit_units), NA_real_),
    permit_ids = paste(sort(unique(permit_id)), collapse = "/"),
    permit_addresses = paste(sort(unique(permit_address)), collapse = " / "),
    .groups = "drop"
  )

review <- commercial %>%
  left_join(permit_summary, by = "pin", relationship = "one-to-one") %>%
  mutate(
    exact_pin_unit_match = !is.na(unambiguous_permit_units),
    units_disagree = exact_pin_unit_match & production_units != unambiguous_permit_units
  )

readr::write_csv(
  tibble::tribble(
    ~metric, ~value,
    "commercial_rows_in_main_model", nrow(review),
    "rows_with_exact_component_pin_unit_permit", sum(review$exact_pin_unit_match),
    "rows_where_production_units_disagree_with_exact_pin_permit", sum(review$units_disagree)
  ),
  "../output/commercial_permit_unit_summary.csv"
)
readr::write_csv(review %>% arrange(desc(units_disagree), pin), "../output/commercial_permit_unit_review.csv")
readr::write_csv(permit_matches %>% arrange(pin, permit_application_year, permit_id), "../output/commercial_permit_unit_matches.csv")
