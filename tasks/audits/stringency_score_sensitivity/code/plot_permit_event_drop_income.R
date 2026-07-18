# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2014L, spec_id == "controls_drop_income") %>%
  select(alderman, score)

if (anyDuplicated(scores$alderman) > 0) {
  stop("The through-2014 no-income score is not unique by alderman.", call. = FALSE)
}

panel <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
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

if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("Permit event-study data must be unique by block and year.", call. = FALSE)
}

outcome_specs <- tribble(
  ~outcome, ~outcome_group, ~sample, ~title, ~filename,
  "n_high_discretion_application", "high", "itt", "High-discretion permit applications", "permit_drop_income_event_high.png",
  "n_low_discretion_nosigns_application", "low", "itt", "Low-discretion permit applications", "permit_drop_income_event_low.png",
  "n_high_discretion_application", "high", "stable", "High-discretion permit applications: stable incumbents", "permit_drop_income_event_high_stable.png"
)

event_rows <- list()
summary_rows <- list()
plots <- vector("list", nrow(outcome_specs))

for (outcome_i in seq_len(nrow(outcome_specs))) {
  outcome_name <- outcome_specs$outcome[outcome_i]
  outcome_group <- outcome_specs$outcome_group[outcome_i]
  sample_name <- outcome_specs$sample[outcome_i]

  model_data <- panel %>%
    filter(sample_name == "itt" | stable_both == 1L) %>%
    mutate(outcome = .data[[outcome_name]])
  pre_period_controls <- model_data %>%
    filter(relative_year < 0L) %>%
    group_by(block_id) %>%
    summarise(pre_period_permit_volume = sum(outcome, na.rm = TRUE), .groups = "drop") %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0L))

  if (anyDuplicated(pre_period_controls$block_id) > 0) {
    stop("Pre-period controls are not unique by census block.", call. = FALSE)
  }
  model_data <- model_data %>%
    left_join(pre_period_controls, by = "block_id", relationship = "many-to-one")

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
      sample = sample_name,
      event_time = as.integer(x),
      estimate_log = estimate,
      std_error_log = if_else(is_ref, 0, (ci_high - estimate_log) / qnorm(0.975)),
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
  pooled_p <- pvalue(pooled_model)[["post_treat"]]
  pooled_stars <- case_when(
    pooled_p <= 0.01 ~ "***",
    pooled_p <= 0.05 ~ "**",
    pooled_p <= 0.10 ~ "*",
    TRUE ~ ""
  )

  plots[[outcome_i]] <- ggplot(event_estimates, aes(event_time, effect)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#B8D8CF", color = NA) +
    geom_line(color = "#176B58", linewidth = 0.9) +
    geom_point(color = "#176B58", size = 2.2) +
    scale_x_continuous(breaks = -5:5) +
    labs(
      title = outcome_specs$title[outcome_i],
      subtitle = sprintf(
        paste0(
          "Pooled estimate = %.3f%s (SE %.3f)\n",
          "Assigned-change ITT; score estimated through 2014 without median income"
        ),
        pooled_estimate,
        pooled_stars,
        pooled_se
      ),
      x = "Years relative to the 2015 ward remap",
      y = "Effect of a 1 SD increase in assigned stringency",
      caption = paste0(
        "PPML census-block event study with ward-pair-by-year comparisons.\n",
        "500ft application-date sample; pre-period controls; ward-pair clustering."
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
    select(outcome_group, event_time, estimate_log, std_error_log, effect, ci_low, ci_high)
  summary_rows[[outcome_i]] <- tibble(
    outcome_group,
    sample = sample_name,
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
  "../output/permit_drop_income_event_2panel.png",
  combined_plot,
  width = 7.8,
  height = 10.4,
  dpi = 220,
  bg = "white"
)
write_csv(bind_rows(event_rows), "../output/permit_drop_income_event_estimates.csv")
summary_results <- bind_rows(summary_rows)
write_csv(summary_results, "../output/permit_drop_income_event_summary.csv")

did_rows <- summary_results %>%
  filter(sample == "itt") %>%
  arrange(match(outcome_group, c("high", "low"))) %>%
  mutate(
    stars = case_when(
      pooled_p <= 0.01 ~ "***",
      pooled_p <= 0.05 ~ "**",
      pooled_p <= 0.10 ~ "*",
      TRUE ~ ""
    )
  )
if (nrow(did_rows) != 2L) {
  stop("The shadow permit DID table requires high- and low-discretion ITT rows.", call. = FALSE)
}
writeLines(
  c(
    "\\begingroup",
    "\\centering",
    "\\small",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & (1) & (2) \\\\",
    " & High-discretion & Low-discretion \\\\",
    "\\midrule",
    sprintf(
      "Post $\\times$ Assigned stringency $\\Delta$ & %.4f%s & %.4f%s \\\\",
      did_rows$pooled_estimate[1],
      did_rows$stars[1],
      did_rows$pooled_estimate[2],
      did_rows$stars[2]
    ),
    sprintf(" & (%.4f) & (%.4f) \\\\", did_rows$pooled_se[1], did_rows$pooled_se[2]),
    "\\midrule",
    "Block fixed effects & Yes & Yes \\\\",
    "Ward-pair $\\times$ year fixed effects & Yes & Yes \\\\",
    "Pre-period permit controls $\\times$ year & Yes & Yes \\\\",
    sprintf(
      "Observations & %s & %s \\\\",
      format(did_rows$observations[1], big.mark = ",", scientific = FALSE),
      format(did_rows$observations[2], big.mark = ",", scientific = FALSE)
    ),
    sprintf(
      "Census blocks & %s & %s \\\\",
      format(did_rows$blocks[1], big.mark = ",", scientific = FALSE),
      format(did_rows$blocks[2], big.mark = ",", scientific = FALSE)
    ),
    sprintf("Ward pairs & %d & %d \\\\", did_rows$ward_pairs[1], did_rows$ward_pairs[2]),
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\endgroup"
  ),
  "../output/permit_drop_income_did_table.tex"
)
