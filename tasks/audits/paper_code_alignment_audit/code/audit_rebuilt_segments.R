# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

old_classification <- read_csv(
  "../../../border_segment_creation/output/segment_classification.csv",
  show_col_types = FALSE
)
fresh_classification <- read_csv(
  "../temp/segment_rebuild/tasks/border_segment_creation/output/segment_classification.csv",
  show_col_types = FALSE
)

old_assignments <- read_csv(
  "../../../assign_segment_ids/output/parcel_segment_ids.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
)
fresh_assignments <- read_csv(
  "../temp/segment_rebuild/tasks/assign_segment_ids/output/parcel_segment_ids.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
)

if (anyDuplicated(old_assignments$pin) > 0 || anyDuplicated(fresh_assignments$pin) > 0) {
  stop("Old or rebuilt segment assignment is not unique by PIN.", call. = FALSE)
}

assignment_comparison <- old_assignments %>%
  full_join(
    fresh_assignments,
    by = "pin",
    suffix = c("_old", "_fresh"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    old_present = !is.na(segment_reason_old),
    fresh_present = !is.na(segment_reason_fresh),
    segment_changed = coalesce(segment_id_old, "") != coalesce(segment_id_fresh, ""),
    reason_changed = coalesce(segment_reason_old, "") != coalesce(segment_reason_fresh, ""),
    distance_changed = case_when(
      is.na(dist_to_segment_m_old) & is.na(dist_to_segment_m_fresh) ~ FALSE,
      is.na(dist_to_segment_m_old) | is.na(dist_to_segment_m_fresh) ~ TRUE,
      TRUE ~ abs(dist_to_segment_m_old - dist_to_segment_m_fresh) > 1e-8
    )
  )

assignment_changes <- assignment_comparison %>%
  filter(!old_present | !fresh_present | segment_changed | reason_changed | distance_changed) %>%
  select(
    pin,
    segment_id_old,
    segment_id_fresh,
    dist_to_segment_m_old,
    dist_to_segment_m_fresh,
    segment_reason_old,
    segment_reason_fresh,
    old_present,
    fresh_present,
    segment_changed,
    reason_changed,
    distance_changed
  )

if (nrow(assignment_changes) == 0) {
  assignment_changes <- tibble(
    pin = character(), segment_id_old = character(), segment_id_fresh = character(),
    dist_to_segment_m_old = double(), dist_to_segment_m_fresh = double(),
    segment_reason_old = character(), segment_reason_fresh = character(),
    old_present = logical(), fresh_present = logical(), segment_changed = logical(),
    reason_changed = logical(), distance_changed = logical()
  )
}
write_csv(assignment_changes, "../output/segment_assignment_changes.csv")

classification_comparison <- old_classification %>%
  select(
    segment_id,
    era,
    ward_pair_id,
    segment_number,
    segment_length_ft,
    centroid_lat,
    centroid_lon,
    analysis_segment_id,
    valid_segment,
    invalid_reason
  ) %>%
  full_join(
    fresh_classification %>%
      select(
        segment_id,
        era,
        ward_pair_id,
        segment_number,
        segment_length_ft,
        centroid_lat,
        centroid_lon,
        analysis_segment_id,
        valid_segment,
        invalid_reason
      ),
    by = "segment_id",
    suffix = c("_old", "_fresh"),
    relationship = "one-to-one"
  )

common_segments <- classification_comparison %>%
  filter(!is.na(era_old), !is.na(era_fresh))

summary_rows <- bind_rows(
  tibble(
    component = "segment_classification",
    metric = c(
      "old_rows", "fresh_rows", "common_segment_ids", "old_only_segment_ids",
      "fresh_only_segment_ids", "changed_length_rows", "changed_centroid_rows",
      "changed_analysis_segment_rows", "changed_validity_rows"
    ),
    value = c(
      nrow(old_classification),
      nrow(fresh_classification),
      nrow(common_segments),
      sum(!is.na(classification_comparison$era_old) & is.na(classification_comparison$era_fresh)),
      sum(is.na(classification_comparison$era_old) & !is.na(classification_comparison$era_fresh)),
      sum(abs(common_segments$segment_length_ft_old - common_segments$segment_length_ft_fresh) > 1e-8, na.rm = TRUE),
      sum(
        abs(common_segments$centroid_lat_old - common_segments$centroid_lat_fresh) > 1e-10 |
          abs(common_segments$centroid_lon_old - common_segments$centroid_lon_fresh) > 1e-10,
        na.rm = TRUE
      ),
      sum(coalesce(common_segments$analysis_segment_id_old, "") != coalesce(common_segments$analysis_segment_id_fresh, "")),
      sum(
        coalesce(common_segments$valid_segment_old, FALSE) != coalesce(common_segments$valid_segment_fresh, FALSE) |
          coalesce(common_segments$invalid_reason_old, "") != coalesce(common_segments$invalid_reason_fresh, "")
      )
    )
  ),
  tibble(
    component = "parcel_segment_assignment",
    metric = c(
      "old_rows", "fresh_rows", "common_pins", "old_only_pins", "fresh_only_pins",
      "changed_segment_rows", "changed_reason_rows", "changed_distance_rows"
    ),
    value = c(
      nrow(old_assignments),
      nrow(fresh_assignments),
      sum(assignment_comparison$old_present & assignment_comparison$fresh_present),
      sum(assignment_comparison$old_present & !assignment_comparison$fresh_present),
      sum(!assignment_comparison$old_present & assignment_comparison$fresh_present),
      sum(assignment_comparison$segment_changed),
      sum(assignment_comparison$reason_changed),
      sum(assignment_comparison$distance_changed)
    )
  )
)

write_csv(summary_rows, "../output/segment_rebuild_summary.csv")

parcels <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  select(
    -any_of(c(
      "segment_id", "dist_to_segment_m", "segment_reason", "analysis_segment_id",
      "valid_segment", "invalid_reason", "segment_length_ft", "segment_lt500ft", "segment_lt1000ft"
    ))
  ) %>%
  left_join(fresh_assignments, by = "pin", relationship = "one-to-one") %>%
  mutate(
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    stale_zone_group = zone_group_from_code(zone_code),
    side = as.integer(signed_distance_m > 0),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  )

geometry_parcels <- st_read(
  "../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg",
  quiet = TRUE
) %>%
  mutate(pin = as.character(pin))
clean_zoning <- st_read(
  "../../../zoning_data_cleaning/output/zoning_data_clean.gpkg",
  quiet = TRUE
) %>%
  select(fresh_zone_code = zone_code)
if (st_crs(clean_zoning) != st_crs(geometry_parcels)) {
  clean_zoning <- st_transform(clean_zoning, st_crs(geometry_parcels))
}
fresh_zoning_lookup <- geometry_parcels %>%
  select(pin) %>%
  st_join(clean_zoning, left = TRUE, largest = TRUE) %>%
  st_drop_geometry()

parcels <- parcels %>%
  left_join(fresh_zoning_lookup, by = "pin", relationship = "many-to-one") %>%
  mutate(fresh_zone_group = zone_group_from_code(fresh_zone_code))

demographic_controls <- c(
  "share_white_own", "share_black_own", "median_hh_income_own",
  "share_bach_plus_own", "homeownership_rate_own"
)
model_rows <- list()

for (sample_label in c("All Construction", "Multifamily")) {
  for (outcome_var in c("density_far", "density_dupac")) {
    base_dat <- parcels %>%
      filter(
        construction_year >= 2006,
        construction_year <= 2022,
        arealotsf > 1,
        areabuilding > 1,
        if (sample_label == "All Construction") unitscount > 0 else unitscount > 1,
        dist_to_boundary_m <= 152.4,
        !is.na(ward_pair),
        is.finite(signed_distance_m),
        !is.na(segment_id),
        segment_id != "",
        is.finite(.data[[outcome_var]]),
        .data[[outcome_var]] > 0
      ) %>%
      mutate(outcome_value = log(.data[[outcome_var]]))

    for (zoning_spec in c("stale_zoning", "fresh_clean_zoning")) {
      model_dat <- if (zoning_spec == "stale_zoning") {
        base_dat %>% filter(!is.na(zone_code))
      } else {
        base_dat %>% filter(!is.na(fresh_zone_code))
      }
      zoning_fe <- if (zoning_spec == "stale_zoning") "stale_zone_group" else "fresh_zone_group"

      for (treatment in c("continuous", "binary")) {
        treatment_var <- if (treatment == "continuous") "strictness_own" else "side"
        formula <- as.formula(paste0(
          "outcome_value ~ ", treatment_var, " + lenient_dist + strict_dist + ",
          paste(demographic_controls, collapse = " + "),
          " | ", zoning_fe, " + segment_id + construction_year"
        ))
        model <- feols(formula, data = model_dat, cluster = ~ward_pair)
        table <- coeftable(model)
        model_rows[[length(model_rows) + 1L]] <- tibble(
          sample = sample_label,
          outcome = if_else(outcome_var == "density_far", "FAR", "DUPAC"),
          treatment = treatment,
          zoning = zoning_spec,
          estimate = unname(table[treatment_var, "Estimate"]),
          se = unname(table[treatment_var, "Std. Error"]),
          p_value = unname(table[treatment_var, "Pr(>|t|)"]),
          n = nobs(model),
          ward_pairs = n_distinct(model_dat$ward_pair[obs(model)]),
          segments = n_distinct(model_dat$segment_id[obs(model)])
        )
      }
    }
  }
}

write_csv(bind_rows(model_rows), "../output/density_segment_rebuild_models.csv")
