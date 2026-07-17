# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

lineage_pins <- read_csv(
  "../output/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = cols(
    member_pins = col_character(), nearest_current_pin = col_character(),
    nearest_current_parcel_class = col_character(), .default = col_guess()
  )
) %>%
  select(
    project_key,
    member_pins,
    lineage_status,
    recommended_action,
    nearest_current_pin,
    nearest_current_parcel_class
  ) %>%
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
  inner_join(lineage_pins, by = "pin", relationship = "one-to-one") %>%
  mutate(
    sample_source = case_when(
      recommended_action == "candidate_for_recovery" ~ "lineage_candidate",
      recommended_action == "exclude_as_already_represented" ~ "successor_duplicate",
      TRUE ~ "unresolved_lineage"
    ),
    current_parcel_class = nearest_current_parcel_class
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
    sample_source = "production_surviving_pin",
    project_key = paste0("production_", pin)
  )

current_classes <- read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), class = col_character(), .default = col_skip()
  )
) %>%
  transmute(pin, current_parcel_class = class)

if (anyDuplicated(current_classes$pin) > 0) {
  stop("Current parcel classes are not unique by PIN.", call. = FALSE)
}

production <- production %>%
  left_join(current_classes, by = "pin", relationship = "many-to-one")

all_rows <- bind_rows(production, recovered) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code),
    side = if_else(signed_distance_m > 0, "more_stringent", "less_stringent"),
    condominium_successor = current_parcel_class == "299"
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != "",
    unitscount > 0
  )

corrected_rows <- all_rows %>%
  filter(sample_source %in% c("production_surviving_pin", "lineage_candidate"))

side_density_rows <- list()
for (construction_sample in c("all", "multifamily")) {
  sample_rows <- if (construction_sample == "all") {
    corrected_rows
  } else {
    corrected_rows %>% filter(unitscount > 1)
  }

  for (outcome in c("density_far", "density_dupac")) {
    outcome_rows <- sample_rows %>%
      filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
      mutate(log_outcome = log(.data[[outcome]]))

    residual_model <- feols(
      as.formula(paste0(
        "log_outcome ~ ",
        paste(demographic_controls, collapse = " + "),
        " | zone_group + segment_id + construction_year"
      )),
      data = outcome_rows
    )
    used_rows <- obs(residual_model)
    outcome_rows <- outcome_rows[used_rows, , drop = FALSE] %>%
      mutate(residualized_log_outcome = as.numeric(resid(residual_model)))

    side_density_rows[[length(side_density_rows) + 1L]] <- outcome_rows %>%
      group_by(sample_source, side) %>%
      summarise(
        construction_sample = construction_sample,
        outcome = outcome,
        rows = n(),
        projects = n_distinct(project_key),
        ward_pairs = n_distinct(ward_pair),
        mean_level = mean(.data[[outcome]]),
        p25_level = quantile(.data[[outcome]], 0.25),
        median_level = median(.data[[outcome]]),
        p75_level = quantile(.data[[outcome]], 0.75),
        mean_log = mean(log_outcome),
        median_log = median(log_outcome),
        mean_residualized_log = mean(residualized_log_outcome),
        median_residualized_log = median(residualized_log_outcome),
        mean_alderman_score = mean(strictness_own),
        mean_boundary_distance_ft = mean(abs(signed_distance)),
        .groups = "drop"
      )
  }
}

write_csv(
  bind_rows(side_density_rows) %>%
    select(
      construction_sample,
      outcome,
      sample_source,
      side,
      everything()
    ) %>%
    arrange(construction_sample, outcome, sample_source, side),
  "../output/density_project_lineage_side_density_summary.csv"
)

candidate_multifamily <- corrected_rows %>%
  filter(sample_source == "lineage_candidate", unitscount > 1)

write_csv(
  candidate_multifamily %>%
    group_by(ward_pair, side) %>%
    summarise(
      rows = n(),
      projects = n_distinct(project_key),
      first_construction_year = min(construction_year),
      last_construction_year = max(construction_year),
      mean_far = mean(density_far),
      median_far = median(density_far),
      mean_dupac = mean(density_dupac),
      median_dupac = median(density_dupac),
      condominium_successor_share = mean(condominium_successor),
      .groups = "drop"
    ) %>%
    arrange(desc(rows), ward_pair, side),
  "../output/density_project_lineage_ward_pair_density.csv"
)

condo_rows <- list()
for (construction_sample in c("all", "multifamily")) {
  sample_rows <- if (construction_sample == "all") {
    all_rows
  } else {
    all_rows %>% filter(unitscount > 1)
  }

  condo_rows[[length(condo_rows) + 1L]] <- sample_rows %>%
    group_by(sample_source, condominium_successor) %>%
    summarise(
      construction_sample = construction_sample,
      rows = n(),
      projects = n_distinct(project_key),
      .groups = "drop"
    ) %>%
    group_by(sample_source) %>%
    mutate(
      row_share_within_source = rows / sum(rows),
      project_share_within_source = projects / sum(projects)
    ) %>%
    ungroup()
}

write_csv(
  bind_rows(condo_rows) %>%
    select(
      construction_sample,
      sample_source,
      condominium_successor,
      rows,
      projects,
      row_share_within_source,
      project_share_within_source
    ) %>%
    arrange(construction_sample, sample_source, desc(condominium_successor)),
  "../output/density_project_lineage_condo_summary.csv"
)

fit_density_model <- function(data, outcome, treatment) {
  treatment_variable <- if (treatment == "continuous") "strictness_own" else "side_indicator"
  model <- feols(
    as.formula(paste0(
      "log_outcome ~ ", treatment_variable,
      " + lenient_dist + strict_dist + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = data,
    cluster = ~ward_pair
  )
  coefficient_table <- coeftable(model)
  c(
    estimate = unname(coefficient_table[treatment_variable, "Estimate"]),
    se = unname(coefficient_table[treatment_variable, "Std. Error"]),
    p_value = unname(coefficient_table[treatment_variable, "Pr(>|t|)"]),
    n = nobs(model)
  )
}

model_results <- read_csv(
  "../output/density_project_lineage_models.csv",
  show_col_types = FALSE
)

influence_rows <- list()
group_influence_rows <- list()
influence_pair_groups <- list(
  largest_recovery_pairs = c("1_26", "1_32"),
  dominant_three_pairs = c("1_26", "1_32", "3_4"),
  dominant_four_pairs = c("1_26", "1_32", "3_4", "2_25")
)
for (construction_sample in c("all", "multifamily")) {
  sample_rows <- if (construction_sample == "all") {
    corrected_rows
  } else {
    corrected_rows %>% filter(unitscount > 1)
  }

  for (outcome in c("density_far", "density_dupac")) {
    outcome_rows <- sample_rows %>%
      filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
      mutate(
        log_outcome = log(.data[[outcome]]),
        side_indicator = as.integer(signed_distance_m > 0),
        lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
        strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
      )

    production_rows <- outcome_rows %>%
      filter(sample_source == "production_surviving_pin")
    recovered_pairs <- outcome_rows %>%
      filter(sample_source == "lineage_candidate") %>%
      distinct(ward_pair) %>%
      pull(ward_pair) %>%
      sort()

    for (treatment in c("continuous", "binary")) {
      full_result <- fit_density_model(outcome_rows, outcome, treatment)
      production_result <- fit_density_model(production_rows, outcome, treatment)

      expected_full <- model_results %>%
        filter(
          analysis_sample == "lineage_182_pin_recovery",
          .data$construction_sample == !!construction_sample,
          .data$outcome == !!outcome,
          .data$treatment == !!treatment
        )
      if (nrow(expected_full) != 1 ||
          abs(full_result["estimate"] - expected_full$estimate) > 1e-8 ||
          abs(full_result["se"] - expected_full$se) > 1e-8) {
        stop("Influence baseline does not match the lineage model table.", call. = FALSE)
      }

      for (ward_pair_i in recovered_pairs) {
        recovered_pair_rows <- outcome_rows %>%
          filter(
            sample_source == "lineage_candidate",
            ward_pair == ward_pair_i
          )

        without_recovered_pair <- outcome_rows %>%
          filter(!(
            sample_source == "lineage_candidate" &
              ward_pair == ward_pair_i
          ))
        only_recovered_pair <- bind_rows(production_rows, recovered_pair_rows)
        without_entire_pair <- outcome_rows %>%
          filter(ward_pair != ward_pair_i)

        without_recovered_result <- fit_density_model(
          without_recovered_pair,
          outcome,
          treatment
        )
        only_recovered_result <- fit_density_model(
          only_recovered_pair,
          outcome,
          treatment
        )
        without_entire_result <- fit_density_model(
          without_entire_pair,
          outcome,
          treatment
        )

        influence_rows[[length(influence_rows) + 1L]] <- tibble(
          construction_sample,
          outcome,
          treatment,
          ward_pair = ward_pair_i,
          recovered_rows = nrow(recovered_pair_rows),
          recovered_projects = n_distinct(recovered_pair_rows$project_key),
          recovered_less_stringent = sum(
            recovered_pair_rows$side == "less_stringent"
          ),
          recovered_more_stringent = sum(
            recovered_pair_rows$side == "more_stringent"
          ),
          production_estimate = production_result["estimate"],
          full_lineage_estimate = full_result["estimate"],
          full_lineage_se = full_result["se"],
          total_lineage_shift = full_result["estimate"] - production_result["estimate"],
          estimate_without_recovered_pair = without_recovered_result["estimate"],
          recovered_pair_contribution =
            full_result["estimate"] - without_recovered_result["estimate"],
          estimate_with_only_recovered_pair = only_recovered_result["estimate"],
          single_pair_shift_from_production =
            only_recovered_result["estimate"] - production_result["estimate"],
          estimate_without_entire_pair = without_entire_result["estimate"],
          leave_entire_pair_change =
            without_entire_result["estimate"] - full_result["estimate"]
        )
      }

      for (pair_group_name in names(influence_pair_groups)) {
        pair_group_values <- intersect(
          influence_pair_groups[[pair_group_name]],
          recovered_pairs
        )
        group_recovered_rows <- outcome_rows %>%
          filter(
            sample_source == "lineage_candidate",
            ward_pair %in% pair_group_values
          )
        without_group_recovered <- outcome_rows %>%
          filter(!(
            sample_source == "lineage_candidate" &
              ward_pair %in% pair_group_values
          ))
        only_group_recovered <- bind_rows(production_rows, group_recovered_rows)
        without_entire_group <- outcome_rows %>%
          filter(!ward_pair %in% pair_group_values)

        without_group_recovered_result <- fit_density_model(
          without_group_recovered,
          outcome,
          treatment
        )
        only_group_recovered_result <- fit_density_model(
          only_group_recovered,
          outcome,
          treatment
        )
        without_entire_group_result <- fit_density_model(
          without_entire_group,
          outcome,
          treatment
        )

        group_influence_rows[[length(group_influence_rows) + 1L]] <- tibble(
          construction_sample,
          outcome,
          treatment,
          pair_group = pair_group_name,
          ward_pairs = paste(pair_group_values, collapse = ";"),
          recovered_rows = nrow(group_recovered_rows),
          recovered_projects = n_distinct(group_recovered_rows$project_key),
          recovered_less_stringent = sum(
            group_recovered_rows$side == "less_stringent"
          ),
          recovered_more_stringent = sum(
            group_recovered_rows$side == "more_stringent"
          ),
          production_estimate = production_result["estimate"],
          production_se = production_result["se"],
          full_lineage_estimate = full_result["estimate"],
          full_lineage_se = full_result["se"],
          total_lineage_shift = full_result["estimate"] - production_result["estimate"],
          estimate_without_group_recovered =
            without_group_recovered_result["estimate"],
          se_without_group_recovered = without_group_recovered_result["se"],
          group_recovered_contribution =
            full_result["estimate"] - without_group_recovered_result["estimate"],
          estimate_with_only_group_recovered =
            only_group_recovered_result["estimate"],
          se_with_only_group_recovered = only_group_recovered_result["se"],
          group_shift_from_production =
            only_group_recovered_result["estimate"] - production_result["estimate"],
          estimate_without_entire_group = without_entire_group_result["estimate"],
          se_without_entire_group = without_entire_group_result["se"],
          leave_entire_group_change =
            without_entire_group_result["estimate"] - full_result["estimate"]
        )
      }
    }
  }
}

ward_pair_influence <- bind_rows(influence_rows) %>%
  arrange(
    construction_sample,
    outcome,
    treatment,
    desc(abs(recovered_pair_contribution))
  )
write_csv(
  ward_pair_influence,
  "../output/density_project_lineage_ward_pair_influence.csv"
)

write_csv(
  bind_rows(group_influence_rows) %>%
    arrange(construction_sample, outcome, treatment, pair_group),
  "../output/density_project_lineage_ward_pair_group_influence.csv"
)

top_pairs <- candidate_multifamily %>%
  count(ward_pair, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(ward_pair)

pair_counts <- candidate_multifamily %>%
  filter(ward_pair %in% top_pairs) %>%
  count(ward_pair, side) %>%
  mutate(ward_pair = fct_reorder(ward_pair, n, .fun = sum))

plot_side_counts <- ggplot(
  pair_counts,
  aes(x = ward_pair, y = n, fill = side)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("less_stringent" = "#1f77b4", "more_stringent" = "#d62728"),
    labels = c("Less stringent", "More stringent")
  ) +
  labs(
    title = "Recovered Multifamily Rows by Ward Pair",
    x = NULL,
    y = "Recovered PIN rows",
    fill = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

condo_plot_data <- all_rows %>%
  filter(
    unitscount > 1,
    sample_source %in% c("production_surviving_pin", "lineage_candidate")
  ) %>%
  group_by(sample_source) %>%
  summarise(
    row_share_within_source = mean(condominium_successor),
    .groups = "drop"
  ) %>%
  mutate(
    sample_source = recode(
      sample_source,
      production_surviving_pin = "Production",
      lineage_candidate = "Recovered"
    )
  )

plot_condo <- ggplot(
  condo_plot_data,
  aes(x = sample_source, y = row_share_within_source, fill = sample_source)
) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = scales::percent(row_share_within_source, accuracy = 0.1)),
    vjust = -0.3,
    size = 3.3
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    labels = scales::percent_format()
  ) +
  scale_fill_manual(values = c("Production" = "#666666", "Recovered" = "#2a9d8f")) +
  labs(
    title = "Current Condominium PIN",
    subtitle = "Multifamily construction rows",
    x = NULL,
    y = "Share with class 299 successor"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

density_plot_data <- corrected_rows %>%
  filter(unitscount > 1) %>%
  transmute(
    sample_source = recode(
      sample_source,
      production_surviving_pin = "Production",
      lineage_candidate = "Recovered"
    ),
    side = recode(
      side,
      less_stringent = "Less stringent",
      more_stringent = "More stringent"
    ),
    `Log(FAR)` = log(density_far),
    `Log(DUPAC)` = log(density_dupac)
  ) %>%
  pivot_longer(
    cols = c(`Log(FAR)`, `Log(DUPAC)`),
    names_to = "outcome",
    values_to = "value"
  )

plot_density <- ggplot(
  density_plot_data,
  aes(x = side, y = value, fill = sample_source)
) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.65,
    outlier.alpha = 0.15
  ) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_fill_manual(values = c("Production" = "#999999", "Recovered" = "#2a9d8f")) +
  labs(
    title = "Multifamily Density Distribution",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

influence_plot_data <- ward_pair_influence %>%
  filter(
    construction_sample == "multifamily",
    treatment == "continuous"
  ) %>%
  group_by(outcome) %>%
  slice_max(abs(recovered_pair_contribution), n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    outcome = recode(
      outcome,
      density_far = "FAR",
      density_dupac = "DUPAC"
    ),
    ward_pair = fct_reorder(ward_pair, abs(recovered_pair_contribution))
  )

plot_influence <- ggplot(
  influence_plot_data,
  aes(x = recovered_pair_contribution, y = ward_pair)
) +
  geom_vline(xintercept = 0, color = "gray55", linetype = "dashed") +
  geom_point(color = "#264653", size = 2) +
  facet_wrap(~outcome, scales = "free_y") +
  labs(
    title = "Contribution of Recovered Rows",
    subtitle = "Change in the continuous coefficient attributable to each pair",
    x = "Full estimate minus estimate without recovered rows",
    y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

diagnostic_plot <- (plot_side_counts | plot_condo) /
  (plot_density | plot_influence) +
  plot_annotation(title = "Why Historical PIN Recovery Changes the Density RD") &
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "../output/density_project_lineage_diagnostics.png",
  plot = diagnostic_plot,
  width = 13,
  height = 9,
  dpi = 220
)
