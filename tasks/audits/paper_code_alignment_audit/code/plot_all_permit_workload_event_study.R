# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

scores <- read_csv(
  "../output/permit_zero_day_score_aldermen.csv",
  show_col_types = FALSE
) %>%
  filter(cutoff == 2014, variant == "positive_log_all_permit_volume") %>%
  select(alderman, score)

if (anyDuplicated(scores$alderman) > 0) {
  stop("The all-permit workload score is not unique by alderman.", call. = FALSE)
}

panel <- read_parquet(
  "../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet"
) %>%
  select(
    -any_of(c(
      "strictness_origin",
      "strictness_dest",
      "strictness_change",
      "strictness_origin_frozen",
      "strictness_dest_frozen",
      "strictness_change_frozen"
    ))
  ) %>%
  left_join(
    scores %>% rename(alderman_origin_2014 = alderman, strictness_origin = score),
    by = "alderman_origin_2014",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_dest_2014 = alderman, strictness_destination = score),
    by = "alderman_dest_2014",
    relationship = "many-to-one"
  ) %>%
  mutate(strictness_change = strictness_destination - strictness_origin) %>%
  filter(
    dist_m <= 152.4,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(strictness_change),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  )

outcome_specs <- tribble(
  ~outcome, ~outcome_group, ~title, ~filename,
  "n_high_discretion_application", "high", "High-discretion permits by application date",
  "permit_all_workload_event_high.png",
  "n_low_discretion_nosigns_application", "low", "Low-discretion permits by application date",
  "permit_all_workload_event_low.png"
)

event_rows <- list()
summary_rows <- list()
plots <- vector("list", nrow(outcome_specs))

for (outcome_i in seq_len(nrow(outcome_specs))) {
  outcome_name <- outcome_specs$outcome[outcome_i]
  outcome_group <- outcome_specs$outcome_group[outcome_i]

  model_data <- panel %>%
    mutate(outcome = .data[[outcome_name]])
  pre_period_controls <- model_data %>%
    filter(relative_year < 0L) %>%
    group_by(block_id) %>%
    summarise(
      pre_period_permit_volume = sum(outcome, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0L))

  if (anyDuplicated(pre_period_controls$block_id) > 0) {
    stop("Pre-period controls are not unique by census block.", call. = FALSE)
  }

  model_data <- model_data %>%
    left_join(
      pre_period_controls,
      by = "block_id",
      relationship = "many-to-one"
    )

  event_model <- fepois(
    outcome ~ i(relative_year, strictness_change, ref = -1) +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair_id^year,
    data = model_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  event_estimates <- iplot(event_model, .plot = FALSE)[[1]] %>%
    as_tibble() %>%
    transmute(
      outcome_group,
      event_time = as.integer(x),
      estimate_log = estimate,
      std_error_log = if_else(
        is_ref,
        0,
        (ci_high - estimate_log) / qnorm(0.975)
      ),
      effect = expm1(estimate_log),
      ci_low = expm1(ci_low),
      ci_high = expm1(ci_high),
      estimate_name_raw = estimate_names_raw,
      is_reference = is_ref
    ) %>%
    filter(event_time >= -5L, event_time <= 5L)

  lead_terms <- event_estimates %>%
    filter(event_time <= -2L, !is_reference) %>%
    pull(estimate_name_raw)
  pretrend_p_value <- wald(event_model, lead_terms, print = FALSE)$p

  pooled_data <- model_data %>%
    mutate(post_treat = as.integer(relative_year >= 0L) * strictness_change)
  pooled_model <- fepois(
    outcome ~ post_treat +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair_id^year,
    data = pooled_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  pooled_estimate <- coef(pooled_model)[["post_treat"]]
  pooled_se <- se(pooled_model)[["post_treat"]]
  pooled_p <- 2 * pnorm(-abs(pooled_estimate / pooled_se))
  pooled_stars <- case_when(
    pooled_p <= 0.01 ~ "***",
    pooled_p <= 0.05 ~ "**",
    pooled_p <= 0.10 ~ "*",
    TRUE ~ ""
  )

  expected_result <- read_csv(
    "../output/permit_zero_day_score_downstream_models.csv",
    show_col_types = FALSE
  ) %>%
    filter(
      analysis == "permit_event_study",
      cutoff == 2014,
      variant == "positive_log_all_permit_volume",
      outcome == outcome_name
    )
  if (
    nrow(expected_result) != 1L ||
    abs(pooled_estimate - expected_result$coefficient) > 1e-8 ||
    abs(pooled_se - expected_result$standard_error) > 1e-8
  ) {
    stop(sprintf("All-workload event plot/model mismatch for %s.", outcome_name), call. = FALSE)
  }

  plots[[outcome_i]] <- ggplot(
    event_estimates,
    aes(event_time, effect)
  ) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_vline(
      xintercept = -0.5,
      linetype = "dashed",
      color = "gray60",
      linewidth = 0.4
    ) +
    geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high),
      fill = "#B8D8CF",
      color = NA
    ) +
    geom_line(color = "#176B58", linewidth = 0.9) +
    geom_point(color = "#176B58", size = 2.2) +
    scale_x_continuous(breaks = -5:5) +
    labs(
      title = outcome_specs$title[outcome_i],
      subtitle = sprintf(
        paste0(
          "Pooled estimate = %.3f%s (SE %.3f)\n",
          "Assigned-change ITT; score estimated through 2014; clustered by ward pair"
        ),
        pooled_estimate,
        pooled_stars,
        pooled_se
      ),
      x = "Years relative to the 2015 ward remap",
      y = "Effect of a 1 SD increase in assigned stringency",
      caption = paste(
        "PPML census-block event study with ward-pair-by-year comparisons.",
        "500ft sample; pre-period permit controls."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0),
      plot.margin = margin(10, 20, 10, 10)
    )

  ggsave(
    file.path("../output", outcome_specs$filename[outcome_i]),
    plots[[outcome_i]],
    width = 7.6,
    height = 5.2,
    dpi = 220,
    bg = "white"
  )

  event_rows[[outcome_i]] <- event_estimates %>%
    select(
      outcome_group,
      event_time,
      estimate_log,
      std_error_log,
      effect,
      ci_low,
      ci_high
    )
  summary_rows[[outcome_i]] <- tibble(
    outcome_group,
    outcome = outcome_name,
    pooled_estimate,
    pooled_se,
    pooled_p,
    pooled_effect = expm1(pooled_estimate),
    observations = nobs(pooled_model),
    blocks = n_distinct(model_data$block_id),
    ward_pairs = n_distinct(model_data$ward_pair_id),
    pretrend_p_value
  )
}

combined_plot <- plots[[1]] / plots[[2]]
ggsave(
  "../output/permit_all_workload_event_2panel.png",
  combined_plot,
  width = 7.8,
  height = 10.4,
  dpi = 220,
  bg = "white"
)

write_csv(
  bind_rows(event_rows),
  "../output/permit_all_workload_event_study_estimates.csv"
)
write_csv(
  bind_rows(summary_rows),
  "../output/permit_all_workload_event_study_summary.csv"
)
