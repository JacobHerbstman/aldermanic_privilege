# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")

unlocated <- read_csv(
  "../output/density_historical_parcel_unmatched.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), source_class = col_character(), parcel_class = col_character(),
    .default = col_guess()
  )
) %>%
  filter(!current_coordinates_complete) %>%
  transmute(
    pin,
    construction_year,
    target_type = "unlocated_original_pin"
  )

lineage_pins <- read_csv(
  "../output/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = cols(member_pins = col_character(), .default = col_guess())
) %>%
  select(project_key, member_pins, reported_construction_year) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  transmute(
    pin = member_pins,
    project_key,
    reported_construction_year
  )

lineage <- read_csv(
  "../input/density_historical_recovered_exact_rows.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  filter(dist_to_boundary_m <= 304.8) %>%
  select(pin, construction_year) %>%
  left_join(lineage_pins, by = "pin", relationship = "one-to-one") %>%
  transmute(
    pin,
    construction_year,
    target_type = "recovered_1000ft_lineage_pin",
    project_key
  )

if (any(is.na(lineage$project_key))) {
  stop("A recovered 1,000-foot PIN is missing from project lineage.", call. = FALSE)
}

targets <- bind_rows(
  unlocated %>% mutate(project_key = NA_character_),
  lineage
) %>%
  group_by(pin) %>%
  summarise(
    construction_year_count = n_distinct(construction_year),
    construction_year = min(construction_year),
    target_types = paste(sort(unique(target_type)), collapse = ";"),
    project_keys = paste(sort(unique(na.omit(project_key))), collapse = ";"),
    .groups = "drop"
  )

if (any(targets$construction_year_count != 1)) {
  stop("Address-audit target PINs have conflicting construction years.", call. = FALSE)
}
if (anyDuplicated(targets$pin) > 0) {
  stop("Address-audit targets are not unique by original PIN.", call. = FALSE)
}

targets %>%
  select(-construction_year_count) %>%
  arrange(pin) %>%
  write_csv("../output/density_parcel_address_targets.csv")
