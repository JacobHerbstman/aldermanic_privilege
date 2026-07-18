# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

bandwidth_m <- 152.4
demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

corrected_zone_group_from_code <- function(z) {
  z <- str_to_upper(as.character(z))
  case_when(
    str_starts(z, "RS-") ~ "Single-Family Residential",
    str_starts(z, "RT-") | str_starts(z, "RM-") ~
      "Multi-Family Residential",
    str_detect(z, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    str_detect(z, "^C-?[1-7]-") ~ "Commercial",
    str_detect(z, "^M-?[1-7]-") ~ "Industrial",
    str_starts(z, "DX-") | str_starts(z, "DR-") |
      str_starts(z, "DS-") | str_starts(z, "DC-") ~ "Downtown",
    str_starts(z, "PD") ~ "Planned Development",
    str_starts(z, "PMD") ~ "Planned Manufacturing",
    str_starts(z, "POS") ~ "Open Space",
    TRUE ~ "Other"
  )
}

lineage <- read_csv(
  "../output/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = cols(member_pins = col_character(), .default = col_guess())
) %>%
  left_join(
    read_csv(
      "../output/density_parcel_address_lineage_evidence.csv",
      show_col_types = FALSE,
      col_types = cols(member_pins = col_character(), .default = col_guess())
    ) %>%
      select(project_key, address_audit_recommendation),
    by = "project_key",
    relationship = "one-to-one"
  ) %>%
  mutate(
    final_recovery =
      recommended_action == "candidate_for_recovery" &
      address_audit_recommendation != "exclude_address_confirmed_duplicate"
  )

lineage_pins <- lineage %>%
  select(project_key, member_pins, final_recovery) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  rename(pin = member_pins)

if (anyDuplicated(lineage_pins$pin) > 0) {
  stop("A historical PIN belongs to more than one lineage group.", call. = FALSE)
}

recovered <- read_csv(
  "../input/density_historical_recovered_exact_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    .default = col_guess()
  )
) %>%
  left_join(lineage_pins, by = "pin", relationship = "one-to-one") %>%
  filter(final_recovery) %>%
  mutate(sample_source = "recovered_historical_parcel_coordinate")

if (any(is.na(recovered$project_key))) {
  stop("A recovered exact-year row is missing from final project lineage.", call. = FALSE)
}

address_recovered <- read_csv(
  "../output/density_parcel_address_recovered_model_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    source_class = col_character(), .default = col_guess()
  )
)

production <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    .default = col_guess()
  )
) %>%
  mutate(
    sample_source = "production_current_2025_coordinate",
    project_key = NA_character_
  )

parcels <- bind_rows(production, recovered, address_recovered) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    stale_zone_code = str_to_upper(as.character(zone_code)),
    stale_zone_group_active = zone_group_from_code(stale_zone_code),
    stale_zone_group_corrected = corrected_zone_group_from_code(stale_zone_code),
    stale_planned_district = str_detect(stale_zone_code, "^(PD|PMD|POS)"),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0)
  )

if (anyDuplicated(parcels$pin) > 0) {
  stop("The final lineage sample contains duplicate PINs.", call. = FALSE)
}

production_geometry <- st_read(
  "../input/parcels_with_geometry.gpkg",
  quiet = TRUE
) %>%
  mutate(pin = as.character(pin)) %>%
  select(pin) %>%
  semi_join(production %>% select(pin), by = "pin")
st_geometry(production_geometry) <- "geometry"

if (anyDuplicated(production_geometry$pin) > 0) {
  stop("Production parcel geometry is not unique by PIN.", call. = FALSE)
}

recovered_geometry <- st_as_sf(
  recovered %>%
    select(pin, longitude, latitude),
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435) %>%
  select(pin)

address_geometry <- st_as_sf(
  address_recovered %>%
    select(pin, address_x_crs_3435, address_y_crs_3435),
  coords = c("address_x_crs_3435", "address_y_crs_3435"),
  crs = 3435,
  remove = FALSE
) %>%
  select(pin)

all_geometry <- bind_rows(
  production_geometry,
  recovered_geometry,
  address_geometry
)

if (nrow(all_geometry) != nrow(parcels) || anyDuplicated(all_geometry$pin) > 0) {
  stop("Final lineage geometry does not match the final lineage sample.", call. = FALSE)
}
if (any(st_is_empty(all_geometry))) {
  stop("The final lineage sample contains an empty geometry.", call. = FALSE)
}

fresh_zoning <- st_read(
  "../input/zoning_data_clean.gpkg",
  quiet = TRUE
) %>%
  select(fresh_zone_code = zone_code) %>%
  st_make_valid() %>%
  st_transform(3435)

fresh_zoning_lookup <- all_geometry %>%
  st_transform(st_crs(fresh_zoning)) %>%
  st_join(fresh_zoning, left = TRUE, largest = TRUE) %>%
  st_drop_geometry()

if (
  nrow(fresh_zoning_lookup) != nrow(parcels) ||
  anyDuplicated(fresh_zoning_lookup$pin) > 0
) {
  stop("Fresh zoning join changed final lineage row cardinality.", call. = FALSE)
}

parcels <- parcels %>%
  left_join(fresh_zoning_lookup, by = "pin", relationship = "one-to-one") %>%
  mutate(
    fresh_zone_code = str_to_upper(as.character(fresh_zone_code)),
    fresh_zone_group_active = zone_group_from_code(fresh_zone_code),
    fresh_zone_group_corrected = corrected_zone_group_from_code(fresh_zone_code)
  )

base_sample <- parcels %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    dist_to_boundary_m <= bandwidth_m,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(segment_id),
    segment_id != ""
  )

model_specs <- tribble(
  ~specification, ~zone_variable, ~zoning_rule,
  "stale_zoning_active_groups", "stale_zone_group_active", "stale_all",
  "stale_zoning_corrected_groups", "stale_zone_group_corrected", "stale_all",
  "drop_stale_planned_active_groups", "stale_zone_group_active", "drop_stale_planned",
  "drop_stale_planned_corrected_groups", "stale_zone_group_corrected", "drop_stale_planned",
  "fresh_rebuild_active_groups", "fresh_zone_group_active", "fresh_available",
  "fresh_rebuild_corrected_groups", "fresh_zone_group_corrected", "fresh_available"
)

model_rows <- list()
attrition_rows <- list()

for (spec_i in seq_len(nrow(model_specs))) {
  specification <- model_specs$specification[spec_i]
  zone_variable <- model_specs$zone_variable[spec_i]
  zoning_rule <- model_specs$zoning_rule[spec_i]

  specification_sample <- switch(
    zoning_rule,
    stale_all = base_sample %>% filter(!is.na(stale_zone_code)),
    drop_stale_planned = base_sample %>%
      filter(!is.na(stale_zone_code), !stale_planned_district),
    fresh_available = base_sample %>% filter(!is.na(fresh_zone_code))
  )

  for (construction_sample in c("all", "multifamily")) {
    construction_rows <- if (construction_sample == "all") {
      specification_sample %>% filter(unitscount > 0)
    } else {
      specification_sample %>% filter(unitscount > 1)
    }

    attrition_rows[[length(attrition_rows) + 1L]] <- tibble(
      specification,
      construction_sample,
      n_before_fixed_effect_drops = nrow(construction_rows),
      recovered_rows = sum(
        construction_rows$sample_source !=
          "production_current_2025_coordinate"
      ),
      planned_district_rows = sum(
        construction_rows$stale_planned_district,
        na.rm = TRUE
      ),
      fresh_zone_missing = sum(is.na(construction_rows$fresh_zone_code)),
      ward_pairs_before_fixed_effect_drops =
        n_distinct(construction_rows$ward_pair)
    )

    for (outcome in c("density_far", "density_dupac")) {
      outcome_rows <- construction_rows %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
        mutate(outcome_value = log(.data[[outcome]]))

      for (treatment in c("continuous", "binary")) {
        treatment_variable <- if (treatment == "continuous") {
          "strictness_own"
        } else {
          "side"
        }

        for (cluster_level in c("ward_pair", "segment")) {
          cluster_variable <- if (cluster_level == "ward_pair") {
            "ward_pair"
          } else {
            "segment_id"
          }

          model <- feols(
            as.formula(paste0(
              "outcome_value ~ ", treatment_variable,
              " + lenient_dist + strict_dist + ",
              paste(demographic_controls, collapse = " + "),
              " | ", zone_variable, " + segment_id + construction_year"
            )),
            data = outcome_rows,
            cluster = as.formula(paste0("~", cluster_variable)),
            notes = FALSE
          )

          coefficient_table <- coeftable(model)
          used_rows <- obs(model)
          model_rows[[length(model_rows) + 1L]] <- tibble(
            specification,
            construction_sample,
            outcome,
            treatment,
            cluster_level,
            estimate = unname(
              coefficient_table[treatment_variable, "Estimate"]
            ),
            se = unname(
              coefficient_table[treatment_variable, "Std. Error"]
            ),
            p_value = unname(
              coefficient_table[treatment_variable, "Pr(>|t|)"]
            ),
            n = nobs(model),
            recovered_rows = sum(
              outcome_rows$sample_source[used_rows] !=
                "production_current_2025_coordinate"
            ),
            ward_pairs = n_distinct(outcome_rows$ward_pair[used_rows]),
            clusters = n_distinct(outcome_rows[[cluster_variable]][used_rows])
          )
        }
      }
    }
  }
}

bind_rows(model_rows) %>%
  arrange(construction_sample, outcome, treatment, specification) %>%
  write_csv("../output/density_lineage_zoning_models.csv")

bind_rows(attrition_rows) %>%
  distinct() %>%
  arrange(construction_sample, specification) %>%
  write_csv("../output/density_lineage_zoning_attrition.csv")

base_sample %>%
  mutate(
    stale_zoning_status = case_when(
      is.na(stale_zone_code) ~ "missing",
      stale_planned_district ~ "PD/PMD/POS",
      TRUE ~ "included district"
    ),
    fresh_zoning_status = if_else(
      is.na(fresh_zone_code),
      "missing from rebuilt source",
      "included district"
    )
  ) %>%
  count(
    sample_source,
    stale_zoning_status,
    fresh_zoning_status,
    stale_zone_group_active,
    stale_zone_group_corrected,
    fresh_zone_group_active,
    fresh_zone_group_corrected,
    name = "n"
  ) %>%
  arrange(sample_source, stale_zoning_status, fresh_zoning_status) %>%
  write_csv("../output/density_lineage_zoning_transitions.csv")

base_sample %>%
  filter(stale_planned_district | is.na(fresh_zone_code)) %>%
  select(
    pin,
    construction_year,
    unitscount,
    density_far,
    density_dupac,
    sample_source,
    ward_pair,
    signed_distance_m,
    stale_zone_code,
    fresh_zone_code
  ) %>%
  arrange(sample_source, construction_year, pin) %>%
  write_csv("../output/density_lineage_zoning_excluded_rows.csv")
