# setwd("tasks/new_construction_cleaning/code")
# max_building_gap <- 0.10

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(max_building_gap)
if (length(args) != 1L) stop("Expected the maximum building-area gap.")
max_building_gap <- as.numeric(args[1])
if (!is.finite(max_building_gap) || max_building_gap < 0) stop("Invalid building-area gap.")

normalize_address <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("[.,#]", " ") %>%
    str_replace_all("\\bNORTH\\b", "N") %>%
    str_replace_all("\\bSOUTH\\b", "S") %>%
    str_replace_all("\\bEAST\\b", "E") %>%
    str_replace_all("\\bWEST\\b", "W") %>%
    str_replace_all("\\bAVENUE\\b", "AVE") %>%
    str_replace_all("\\bSTREET\\b", "ST") %>%
    str_replace_all("\\bROAD\\b", "RD") %>%
    str_replace_all("\\bBOULEVARD\\b", "BLVD") %>%
    str_replace_all("\\bDRIVE\\b", "DR") %>%
    str_replace_all("\\bPLACE\\b", "PL") %>%
    str_replace_all("\\bCOURT\\b", "CT") %>%
    str_replace_all("\\bPARKWAY\\b", "PKWY") %>%
    str_replace_all("\\bTERRACE\\b", "TER") %>%
    str_replace_all("\\bHIGHWAY\\b", "HWY") %>%
    str_replace_all("\\bLANE\\b", "LN") %>%
    str_squish() %>%
    na_if("")
}

selected_history <- read_csv(
  "../output/density_parcel_address_selected_history.csv", show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)
if (anyNA(selected_history$pin) || anyDuplicated(selected_history$pin)) {
  stop("Selected historical addresses must be unique by PIN.")
}

current_addresses <- fread(
  "../input/parcel_addresses_2025_chicago.csv",
  select = c("pin", "prop_address_full")
) %>%
  as_tibble() %>%
  transmute(
    pin = as.character(pin),
    current_property_address = as.character(prop_address_full),
    address_normalized = normalize_address(prop_address_full)
  )

if (anyDuplicated(current_addresses$pin) > 0) {
  stop("Current parcel addresses are not unique by PIN.", call. = FALSE)
}

lineage_all <- read_csv(
  "../output/density_project_lineage.csv", show_col_types = FALSE,
  col_types = cols(
    member_pins = col_character(), same_year_current_member_pins = col_character(),
    nearest_current_project_pins = col_character(), .default = col_guess()
  )
)
if (anyNA(lineage_all$project_key) || anyDuplicated(lineage_all$project_key)) {
  stop("Historical project keys must be unique.")
}
lineage_pin_lookup <- lineage_all %>%
  select(project_key, member_pins) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  transmute(project_key, pin = member_pins)
if (anyDuplicated(lineage_pin_lookup$pin)) stop("Each historical PIN must belong to one project.")

recovered_project_counts <- read_csv(
  "../output/density_historical_coordinate_candidates.csv", show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  filter(coordinate_source == "historical_parcel") %>%
  select(pin) %>%
  left_join(lineage_pin_lookup, by = "pin", relationship = "one-to-one") %>%
  count(project_key, name = "recovered_audit_pin_count")
if (anyNA(recovered_project_counts$project_key)) stop("A recovered PIN is missing from project lineage.")
lineage <- lineage_all %>%
  inner_join(recovered_project_counts, by = "project_key", relationship = "one-to-one")

project_target_addresses <- lineage %>%
  select(project_key, member_pins) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  rename(pin = member_pins) %>%
  left_join(
    selected_history %>%
      select(pin, selected_address_normalized, address_selection_status),
    by = "pin",
    relationship = "many-to-one"
  ) %>%
  group_by(project_key) %>%
  summarise(
    target_address_count = n_distinct(
      selected_address_normalized[address_selection_status == "selected_nearest_year_address"],
      na.rm = TRUE
    ),
    target_addresses = paste(
      sort(unique(na.omit(selected_address_normalized[
        address_selection_status == "selected_nearest_year_address"
      ]))),
      collapse = ";"
    ),
    .groups = "drop"
  )

candidate_addresses <- lineage %>%
  select(
    project_key,
    same_year_current_member_pins,
    nearest_current_project_pins
  ) %>%
  pivot_longer(
    cols = -project_key,
    names_to = "candidate_source",
    values_to = "candidate_pin_text"
  ) %>%
  mutate(candidate_pin = str_extract_all(candidate_pin_text, "[0-9]{14}")) %>%
  unnest_longer(candidate_pin) %>%
  filter(!is.na(candidate_pin)) %>%
  distinct(project_key, candidate_pin) %>%
  left_join(
    current_addresses %>% select(candidate_pin = pin, candidate_address = address_normalized),
    by = "candidate_pin",
    relationship = "many-to-one"
  ) %>%
  group_by(project_key) %>%
  summarise(
    candidate_pin_count = n_distinct(candidate_pin),
    candidate_address_count = n_distinct(candidate_address, na.rm = TRUE),
    candidate_addresses = paste(sort(unique(na.omit(candidate_address))), collapse = ";"),
    .groups = "drop"
  )

nearest_project_addresses <- lineage %>%
  select(project_key, nearest_current_project_pins) %>%
  mutate(candidate_pin = str_extract_all(nearest_current_project_pins, "[0-9]{14}")) %>%
  unnest_longer(candidate_pin) %>%
  filter(!is.na(candidate_pin)) %>%
  distinct(project_key, candidate_pin) %>%
  left_join(
    current_addresses %>% select(candidate_pin = pin, nearest_project_address = address_normalized),
    by = "candidate_pin",
    relationship = "many-to-one"
  ) %>%
  group_by(project_key) %>%
  summarise(
    nearest_project_addresses = paste(
      sort(unique(na.omit(nearest_project_address))),
      collapse = ";"
    ),
    .groups = "drop"
  )

lineage_evidence <- lineage %>%
  left_join(project_target_addresses, by = "project_key", relationship = "one-to-one") %>%
  left_join(candidate_addresses, by = "project_key", relationship = "one-to-one") %>%
  left_join(nearest_project_addresses, by = "project_key", relationship = "one-to-one") %>%
  mutate(
    across(
      c(target_address_count, candidate_pin_count, candidate_address_count),
      ~ coalesce(.x, 0L)
    ),
    target_addresses = coalesce(target_addresses, ""),
    candidate_addresses = coalesce(candidate_addresses, ""),
    nearest_project_addresses = coalesce(nearest_project_addresses, ""),
    exact_address_match = map2_lgl(
      str_split(target_addresses, fixed(";")),
      str_split(candidate_addresses, fixed(";")),
      ~ length(intersect(.x[.x != ""], .y[.y != ""])) > 0
    ),
    exact_nearest_project_address_match = map2_lgl(
      str_split(target_addresses, fixed(";")),
      str_split(nearest_project_addresses, fixed(";")),
      ~ length(intersect(.x[.x != ""], .y[.y != ""])) > 0
    ),
    address_lineage_evidence = case_when(
      exact_address_match ~ "exact_address_supports_duplicate",
      target_address_count > 0 & candidate_address_count > 0 ~
        "addresses_differ_not_dispositive",
      target_address_count == 0 ~ "missing_historical_project_address",
      TRUE ~ "missing_candidate_project_address"
    ),
    nearest_project_building_gap = abs(
      nearest_current_project_building_area - areabuilding
    ) / pmax(nearest_current_project_building_area, areabuilding),
    nearest_project_unit_gap = abs(nearest_current_project_units - unitscount),
    address_duplicate_confirmation =
      exact_nearest_project_address_match &
      nearest_project_building_gap <= max_building_gap &
      nearest_project_unit_gap == 0,
    address_audit_recommendation = case_when(
      address_duplicate_confirmation ~
        "exclude_address_confirmed_duplicate",
      recommended_action == "hold_out_pending_resolution" ~
        "continue_hold_out_pending_resolution",
      TRUE ~ recommended_action
    )
  )

SaveData(lineage_evidence, character(), "../output/density_parcel_address_lineage_evidence.csv")
