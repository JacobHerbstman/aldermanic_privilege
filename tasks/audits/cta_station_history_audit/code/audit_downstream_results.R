# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/cta_station_history_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

scores <- read_csv(
  "../output/cta_history_score_alderman_changes.csv",
  show_col_types = FALSE
)

density_scores <- scores %>%
  filter(cutoff_year == 2022) %>%
  select(alderman, static_score = current_file_score, historical_score)

parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  select(-strictness_own, -strictness_neighbor, -sign, -signed_distance, -signed_distance_m) %>%
  left_join(
    density_scores,
    by = c("alderman_own" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(static_own = static_score, historical_own = historical_score) %>%
  left_join(
    density_scores,
    by = c("alderman_neighbor" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(static_neighbor = static_score, historical_neighbor = historical_score) %>%
  mutate(
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code)
  )

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

density_results <- list()
for (score_version in c("static", "historical")) {
  density_data <- parcels %>%
    mutate(
      strictness_own = .data[[paste0(score_version, "_own")]],
      strictness_neighbor = .data[[paste0(score_version, "_neighbor")]],
      sign = case_when(
        strictness_own > strictness_neighbor ~ 1,
        strictness_own < strictness_neighbor ~ -1,
        TRUE ~ NA_real_
      ),
      signed_distance_m = dist_to_boundary_m * sign,
      lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
      strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
      side = as.integer(signed_distance_m > 0)
    ) %>%
    filter(
      arealotsf > 1,
      areabuilding > 1,
      between(construction_year, 2006, 2022),
      dist_to_boundary_m <= 152.4,
      !is.na(ward_pair),
      is.finite(signed_distance_m),
      !is.na(zone_code),
      !is.na(segment_id),
      segment_id != ""
    )

  for (sample_name in c("all", "multifamily")) {
    sample_data <- if (sample_name == "all") {
      density_data %>% filter(unitscount > 0)
    } else {
      density_data %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac")) {
      model_data <- sample_data %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0)

      continuous_model <- feols(
        as.formula(paste0(
          "log(", outcome, ") ~ strictness_own + lenient_dist + strict_dist + ",
          paste(demographic_controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = model_data,
        cluster = ~ward_pair
      )
      binary_model <- feols(
        as.formula(paste0(
          "log(", outcome, ") ~ side + lenient_dist + strict_dist + ",
          paste(demographic_controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = model_data,
        cluster = ~ward_pair
      )

      continuous_table <- coeftable(continuous_model)
      binary_table <- coeftable(binary_model)
      density_results[[length(density_results) + 1L]] <- tibble(
        analysis = "density",
        score_version,
        sample = sample_name,
        outcome,
        treatment = c("continuous", "binary"),
        estimate = c(
          continuous_table["strictness_own", "Estimate"],
          binary_table["side", "Estimate"]
        ),
        se = c(
          continuous_table["strictness_own", "Std. Error"],
          binary_table["side", "Std. Error"]
        ),
        p_value = c(
          continuous_table["strictness_own", "Pr(>|t|)"],
          binary_table["side", "Pr(>|t|)"]
        ),
        observations = nobs(continuous_model)
      )
    }
  }
}

permit_scores <- scores %>%
  filter(cutoff_year == 2014) %>%
  select(alderman, static_score = current_file_score, historical_score)

permit_panel <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  as_tibble() %>%
  left_join(
    permit_scores,
    by = c("alderman_origin_2014" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(static_origin = static_score, historical_origin = historical_score) %>%
  left_join(
    permit_scores,
    by = c("alderman_dest_2014" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(static_dest = static_score, historical_dest = historical_score)

permit_results <- list()
for (score_version in c("static", "historical")) {
  for (outcome in c("n_high_discretion_application", "n_low_discretion_nosigns_application")) {
    permit_data <- permit_panel %>%
      mutate(
        strictness_change = .data[[paste0(score_version, "_dest")]] -
          .data[[paste0(score_version, "_origin")]],
        model_outcome = .data[[outcome]]
      ) %>%
      filter(
        dist_m <= 152.4,
        between(relative_year, -5, 5),
        !is.na(strictness_change),
        !is.na(ward_pair_id),
        ward_pair_id != ""
      )

    pre_period_controls <- permit_data %>%
      filter(relative_year < 0) %>%
      group_by(block_id) %>%
      summarise(
        pre_period_permit_volume = sum(model_outcome, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

    permit_data <- permit_data %>%
      left_join(pre_period_controls, by = "block_id", relationship = "many-to-one") %>%
      mutate(post_treat = as.integer(relative_year >= 0) * strictness_change)

    model <- fepois(
      model_outcome ~ post_treat +
        pre_period_permit_volume:factor(year) +
        no_pre_period_permits:factor(year) |
        block_id + ward_pair_id^year,
      data = permit_data,
      cluster = ~ward_pair_id,
      notes = FALSE
    )
    model_table <- coeftable(model)
    permit_results[[length(permit_results) + 1L]] <- tibble(
      analysis = "permit_event_study",
      score_version,
      sample = "itt",
      outcome,
      treatment = "continuous",
      estimate = model_table["post_treat", "Estimate"],
      se = model_table["post_treat", "Std. Error"],
      p_value = model_table["post_treat", "Pr(>|z|)"],
      observations = nobs(model)
    )
  }
}

bind_rows(density_results, permit_results) %>%
  write_csv("../output/cta_history_downstream_estimate_comparison.csv")
