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
confirmed_sum_pins <- c(
  "11303200330000",
  "13141270250000",
  "19083090620000",
  "20211200290000",
  "20231160220000",
  "25194060030000"
)

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

recovered_geometry <- st_as_sf(
  recovered %>% select(pin, longitude, latitude),
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

zoning <- st_read("../input/zoning_data_clean.gpkg", quiet = TRUE) %>%
  transmute(fresh_zone_code = zone_code) %>%
  st_make_valid() %>%
  st_transform(3435)

zoning_lookup <- all_geometry %>%
  st_transform(st_crs(zoning)) %>%
  st_join(zoning, left = TRUE, largest = TRUE) %>%
  st_drop_geometry()

if (nrow(zoning_lookup) != nrow(parcels) || anyDuplicated(zoning_lookup$pin) > 0) {
  stop("Zoning join changed final lineage row cardinality.", call. = FALSE)
}

confirmed_cards <- read_csv(
  "../input/density_residential_card_affected_rows.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  filter(pin %in% confirmed_sum_pins) %>%
  select(pin, total_building_sqft_cards, total_units_cards)

if (
  nrow(confirmed_cards) != length(confirmed_sum_pins) ||
  anyDuplicated(confirmed_cards$pin) > 0 ||
  !setequal(confirmed_cards$pin, confirmed_sum_pins)
) {
  stop("The six confirmed card corrections are incomplete.", call. = FALSE)
}

analysis_sample <- parcels %>%
  left_join(zoning_lookup, by = "pin", relationship = "one-to-one") %>%
  left_join(confirmed_cards, by = "pin", relationship = "many-to-one") %>%
  mutate(
    zone_group = zone_group_from_code(fresh_zone_code),
    confirmed_sum_applied = pin %in% confirmed_sum_pins,
    density_far_confirmed = if_else(
      confirmed_sum_applied,
      total_building_sqft_cards / arealotsf,
      density_far
    ),
    density_dupac_confirmed = if_else(
      confirmed_sum_applied,
      43560 * total_units_cards / arealotsf,
      density_dupac
    ),
    unitscount_confirmed = if_else(
      confirmed_sum_applied,
      total_units_cards,
      unitscount
    )
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    dist_to_boundary_m <= bandwidth_m,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(fresh_zone_code),
    !is.na(segment_id),
    segment_id != ""
  )

if (sum(analysis_sample$confirmed_sum_applied) != length(confirmed_sum_pins)) {
  stop("Not all confirmed card corrections enter the final 500ft sample.", call. = FALSE)
}

analysis_sample %>%
  filter(confirmed_sum_applied) %>%
  transmute(
    pin,
    construction_year,
    ward_pair,
    segment_id,
    original_units = unitscount,
    corrected_units = unitscount_confirmed,
    original_building_sqft = areabuilding,
    corrected_building_sqft = total_building_sqft_cards,
    original_far = density_far,
    corrected_far = density_far_confirmed,
    original_dupac = density_dupac,
    corrected_dupac = density_dupac_confirmed
  ) %>%
  arrange(pin) %>%
  write_csv("../output/density_lineage_confirmed_card_rows.csv")

model_rows <- list()
for (card_specification in c("lineage_baseline", "six_confirmed_card_sums")) {
  model_data <- analysis_sample %>%
    mutate(
      model_far = if (card_specification == "six_confirmed_card_sums") {
        density_far_confirmed
      } else {
        density_far
      },
      model_dupac = if (card_specification == "six_confirmed_card_sums") {
        density_dupac_confirmed
      } else {
        density_dupac
      },
      model_units = if (card_specification == "six_confirmed_card_sums") {
        unitscount_confirmed
      } else {
        unitscount
      }
    )

  for (construction_sample in c(
    "all",
    "multifamily",
    "multifamily_fixed_baseline_sample"
  )) {
    construction_rows <- switch(
      construction_sample,
      all = model_data %>% filter(model_units > 0),
      multifamily = model_data %>% filter(model_units > 1),
      multifamily_fixed_baseline_sample = model_data %>% filter(unitscount > 1)
    )

    for (outcome in c("model_far", "model_dupac")) {
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
              " | zone_group + segment_id + construction_year"
            )),
            data = outcome_rows,
            cluster = as.formula(paste0("~", cluster_variable)),
            notes = FALSE
          )

          coefficient_table <- coeftable(model)
          used_rows <- obs(model)
          model_rows[[length(model_rows) + 1L]] <- tibble(
            card_specification,
            construction_sample,
            outcome = if_else(
              outcome == "model_far",
              "density_far",
              "density_dupac"
            ),
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
            corrected_rows = sum(
              outcome_rows$confirmed_sum_applied[used_rows]
            ),
            recovered_rows = sum(
              outcome_rows$sample_source[used_rows] !=
                "production_current_2025_coordinate"
            ),
            clusters = n_distinct(
              outcome_rows[[cluster_variable]][used_rows]
            )
          )
        }
      }
    }
  }
}

bind_rows(model_rows) %>%
  arrange(
    construction_sample,
    outcome,
    treatment,
    cluster_level,
    card_specification
  ) %>%
  write_csv("../output/density_lineage_confirmed_card_models.csv")
