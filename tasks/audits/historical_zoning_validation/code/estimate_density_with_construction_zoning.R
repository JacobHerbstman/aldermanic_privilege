# setwd("tasks/audits/historical_zoning_validation/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    .default = col_guess()
  )
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = as.integer(construction_year),
    current_zone_group = zone_group_from_code(
      str_replace(str_to_upper(zone_code), "^(RS|RT|RM)(?=[0-9])", "\\1-")
    ),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2
  )

construction_zoning <- read_csv(
  "../output/historical_zoning_project_construction_year.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    construction_zone_group = col_character(),
    construction_zone_group_supported = col_logical(),
    construction_zoning_status = col_character()
  ),
  col_select = c(
    pin,
    construction_zone_group,
    construction_zone_group_supported,
    construction_zoning_status
  )
)

if (anyDuplicated(parcels$pin)) {
  stop("Density input is not unique by PIN.", call. = FALSE)
}
if (anyDuplicated(construction_zoning$pin)) {
  stop("Construction-year zoning is not unique by PIN.", call. = FALSE)
}

parcels <- parcels %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(segment_id),
    segment_id != ""
  ) %>%
  left_join(
    construction_zoning,
    by = "pin",
    relationship = "one-to-one"
  )

if (anyNA(parcels$construction_zone_group)) {
  stop("Construction-year zoning does not cover the 500-foot density sample.", call. = FALSE)
}
if (!all(parcels$construction_zone_group_supported)) {
  stop("The density sample contains unsupported construction-year zoning.", call. = FALSE)
}

model_rows <- list()
for (zoning_version in c("Current zoning", "Construction-year zoning")) {
  parcels$zone_group <- if (zoning_version == "Current zoning") {
    parcels$current_zone_group
  } else {
    parcels$construction_zone_group
  }

  for (sample_name in c("All construction", "Multifamily")) {
    sample <- if (sample_name == "All construction") {
      parcels %>% filter(unitscount > 0)
    } else {
      parcels %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac")) {
      sample_outcome <- sample %>%
        filter(
          !is.na(zone_group),
          is.finite(.data[[outcome]]),
          .data[[outcome]] > 0
        )

      for (treatment in c("Binary", "Continuous")) {
        treatment_variable <- if (treatment == "Binary") {
          "side"
        } else {
          "continuous_score_difference"
        }
        model <- feols(
          as.formula(paste0(
            "log(", outcome, ") ~ ",
            paste(
              c(
                treatment_variable,
                "pair_average_score",
                "lenient_dist",
                "strict_dist",
                demographic_controls
              ),
              collapse = " + "
            ),
            " | zone_group + segment_id + construction_year"
          )),
          data = sample_outcome,
          cluster = ~ward_pair
        )
        coefficient_table <- coeftable(model)
        model_rows[[length(model_rows) + 1L]] <- tibble(
          zoning = zoning_version,
          sample = sample_name,
          outcome = outcome,
          treatment = treatment,
          estimate = unname(coefficient_table[treatment_variable, "Estimate"]),
          standard_error = unname(coefficient_table[treatment_variable, "Std. Error"]),
          p_value = unname(coefficient_table[treatment_variable, "Pr(>|t|)"]),
          observations = nobs(model),
          ward_pairs = n_distinct(sample_outcome$ward_pair)
        )
      }
    }
  }
}

bind_rows(model_rows) %>%
  write_csv(
    "../output/historical_zoning_density_model_comparison.csv",
    na = ""
  )

bind_rows(
  parcels %>%
    mutate(sample = "All construction") %>%
    filter(unitscount > 0),
  parcels %>%
    mutate(sample = "Multifamily") %>%
    filter(unitscount > 1)
) %>%
  summarise(
    projects = n(),
    current_and_construction_groups_differ = sum(
      current_zone_group != construction_zone_group,
      na.rm = TRUE
    ),
    share_different = mean(
      current_zone_group != construction_zone_group,
      na.rm = TRUE
    ),
    .by = sample
  ) %>%
  write_csv(
    "../output/historical_zoning_density_sample_comparison.csv",
    na = ""
  )
