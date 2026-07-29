# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

bandwidths_ft <- seq(200, 1000, by = 50)
controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L, spec_id == "controls_drop_income") %>%
  transmute(
    alderman_key = str_squish(str_to_lower(alderman)),
    score
  )

if (anyDuplicated(scores$alderman_key) > 0) {
  stop("The selected score is not unique by alderman.", call. = FALSE)
}

parcels <- read_csv(
  "../input/density_placebo_1000ft_sample.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    source_class = col_character(),
    .default = col_guess()
  ),
  guess_max = Inf
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    alderman_own_key = str_squish(str_to_lower(alderman_own)),
    alderman_neighbor_key = str_squish(str_to_lower(alderman_neighbor)),
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(fresh_zone_code)
  ) %>%
  left_join(
    scores %>% rename(alderman_own_key = alderman_key, score_own = score),
    by = "alderman_own_key",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_neighbor_key = alderman_key, score_neighbor = score),
    by = "alderman_neighbor_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    side = as.integer(score_own > score_neighbor),
    signed_distance = if_else(
      side == 1L,
      abs(dist_to_boundary_m),
      -abs(dist_to_boundary_m)
    ),
    lenient_dist = abs(signed_distance) * as.integer(signed_distance <= 0),
    strict_dist = abs(signed_distance) * as.integer(signed_distance > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 0,
    construction_year >= 2006L,
    construction_year <= 2022L,
    !is.na(ward_pair),
    is.finite(signed_distance),
    !is.na(fresh_zone_code),
    !is.na(segment_id),
    segment_id != ""
  )

if (anyNA(parcels$score_own) || anyNA(parcels$score_neighbor)) {
  stop("An extended-sample alderman is missing the selected score.", call. = FALSE)
}
if (anyDuplicated(parcels$pin) > 0) {
  stop("The extended density sample contains duplicate PINs.", call. = FALSE)
}

model_rows <- list()
for (construction_sample in c("all", "multifamily")) {
  construction_rows <- if (construction_sample == "all") {
    parcels
  } else {
    parcels %>% filter(unitscount > 1)
  }

  for (bandwidth_ft in bandwidths_ft) {
    for (outcome in c("density_far", "density_dupac")) {
      model_data <- construction_rows %>%
        filter(
          dist_to_boundary_m <= bandwidth_ft * FT_TO_M,
          is.finite(.data[[outcome]]),
          .data[[outcome]] > 0
        ) %>%
        mutate(outcome_value = log(.data[[outcome]]))

      model <- feols(
        as.formula(paste0(
          "outcome_value ~ score_own + lenient_dist + strict_dist + ",
          paste(controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = model_data,
        cluster = ~ward_pair,
        notes = FALSE
      )
      coefficient_table <- coeftable(model)
      used_rows <- obs(model)
      estimate <- unname(coefficient_table["score_own", "Estimate"])
      standard_error <- unname(coefficient_table["score_own", "Std. Error"])

      model_rows[[length(model_rows) + 1L]] <- tibble(
        construction_sample,
        bandwidth_ft,
        outcome,
        estimate,
        standard_error,
        p_value = unname(coefficient_table["score_own", "Pr(>|t|)"]),
        ci_low = estimate - qnorm(0.975) * standard_error,
        ci_high = estimate + qnorm(0.975) * standard_error,
        observations = nobs(model),
        ward_pairs = n_distinct(model_data$ward_pair[used_rows])
      )
    }
  }
}

results <- bind_rows(model_rows) %>%
  arrange(construction_sample, outcome, bandwidth_ft)

expected_500ft <- read_csv(
  "../input/density_score_sensitivity_models.csv",
  show_col_types = FALSE
) %>%
  filter(
    spec_id == "controls_drop_income",
    treatment == "continuous",
    outcome %in% c("density_far", "density_dupac")
  ) %>%
  select(
    construction_sample,
    outcome,
    expected_estimate = estimate,
    expected_se = se
  )

validation_500ft <- results %>%
  filter(bandwidth_ft == 500L) %>%
  inner_join(
    expected_500ft,
    by = c("construction_sample", "outcome"),
    relationship = "one-to-one"
  )

if (
  nrow(validation_500ft) != 4L ||
  any(abs(validation_500ft$estimate - validation_500ft$expected_estimate) > 1e-8) ||
  any(abs(validation_500ft$standard_error - validation_500ft$expected_se) > 1e-8)
) {
  stop("The extended-sample 500-foot estimates do not reproduce Table 2.", call. = FALSE)
}

write_csv(
  results %>% filter(construction_sample == "multifamily"),
  "../output/drop_income_multifamily_continuous_bandwidth_path.csv"
)
write_csv(
  results %>% filter(construction_sample == "all"),
  "../output/drop_income_all_construction_continuous_bandwidth_path.csv"
)

plot_specs <- tribble(
  ~construction_sample, ~sample_label, ~output_stem,
  "all", "All Construction", "drop_income_all_construction_continuous_bandwidth_path",
  "multifamily", "Multifamily", "drop_income_multifamily_continuous_bandwidth_path"
)

for (plot_i in seq_len(nrow(plot_specs))) {
  sample_i <- plot_specs$construction_sample[plot_i]
  sample_label_i <- plot_specs$sample_label[plot_i]
  plot_data <- results %>%
    filter(construction_sample == sample_i) %>%
    mutate(
      outcome_label = factor(
        recode(
          outcome,
          density_far = paste0(sample_label_i, ": Log(FAR)"),
          density_dupac = paste0(sample_label_i, ": Log(DUPAC)")
        ),
        levels = c(
          paste0(sample_label_i, ": Log(FAR)"),
          paste0(sample_label_i, ": Log(DUPAC)")
        )
      )
    )

  bandwidth_plot <- ggplot(plot_data, aes(bandwidth_ft, estimate)) +
    geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high),
      fill = "#9ecae1",
      alpha = 0.35,
      color = NA
    ) +
    geom_line(color = "#176b87", linewidth = 0.8) +
    geom_point(color = "#176b87", size = 1.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = 500, linetype = "dashed", color = "gray45", linewidth = 0.4) +
    facet_wrap(~outcome_label, ncol = 1, scales = "free_y") +
    scale_x_continuous(
      breaks = seq(200, 1000, by = 100),
      limits = c(200, 1000)
    ) +
    labs(
      x = "Bandwidth (feet)",
      y = "Coefficient on stringency index (1 SD)",
      caption = "Shaded regions are 95% confidence intervals. Dashed line marks the 500-foot specification."
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0, size = 8.5)
    )

  ggsave(
    file.path("../output", paste0(plot_specs$output_stem[plot_i], ".pdf")),
    bandwidth_plot,
    width = 8.2,
    height = 7.2,
    bg = "white"
  )
  ggsave(
    file.path("../output", paste0(plot_specs$output_stem[plot_i], ".png")),
    bandwidth_plot,
    width = 8.2,
    height = 7.2,
    dpi = 240,
    bg = "white"
  )
}
