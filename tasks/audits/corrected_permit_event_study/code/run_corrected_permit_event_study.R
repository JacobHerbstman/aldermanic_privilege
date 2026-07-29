# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")
# specification <- "application_stable"
# cluster_level <- "wardpair"
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"

source("../../../setup_environment/code/packages.R")

grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(specification, cluster_level, bandwidth, bandwidth_label)
}
if (length(cli_args) != 4) {
  stop("Script requires specification, cluster level, bandwidth, and label.", call. = FALSE)
}

specification <- cli_args[1]
cluster_level <- cli_args[2]
bandwidth <- as.numeric(cli_args[3])
bandwidth_label <- cli_args[4]

valid_specifications <- c(
  "issue_stable", "low_issue_stable", "application_stable", "issue_itt", "application_itt",
  "issue_stable_pre2015", "issue_itt_pre2015", "application_itt_pre2015",
  "application_itt_pre2015_prevolume", "low_application_itt_pre2015",
  "low_application_itt_pre2015_prevolume",
  "issue_realized_pre2015", "low_issue_realized_pre2015", "application_realized_pre2015",
  "issue_realized_pre2015_prevolume", "application_realized_pre2015_prevolume",
  "triple_issue_stable"
)
if (!specification %in% valid_specifications) {
  stop("Unknown corrected permit specification.", call. = FALSE)
}
if (!cluster_level %in% c("block", "wardpair")) {
  stop("Cluster level must be block or wardpair.", call. = FALSE)
}
if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("Bandwidth must be positive.", call. = FALSE)
}

application_timing <- grepl("application", specification, fixed = TRUE)
triple_difference <- grepl("^triple_", specification)
stable_sample <- grepl("stable", specification, fixed = TRUE)
pre_2015_score <- grepl("pre2015", specification, fixed = TRUE)
realized_treatment <- grepl("realized", specification, fixed = TRUE)
low_discretion_placebo <- grepl("^low_", specification)
prevolume_controls <- grepl("prevolume", specification, fixed = TRUE)

high_outcome <- if (application_timing) "n_high_discretion_application" else "n_high_discretion_issue"
low_outcome <- if (application_timing) "n_low_discretion_nosigns_application" else "n_low_discretion_nosigns_issue"
dose_variable <- if (pre_2015_score && realized_treatment) {
  "realized_change_2014"
} else if (pre_2015_score) {
  "assigned_change_2014"
} else {
  "assigned_change_2022"
}
timing_label <- if (application_timing) "application date" else "issue date"
sample_label <- if (realized_treatment) {
  "realized 2015 alderman change"
} else if (stable_sample) {
  "stable aldermen on both sides"
} else {
  "assigned-change ITT"
}
score_label <- if (pre_2015_score) "score estimated through 2014" else "through-2022 score"
cluster_label <- if (cluster_level == "block") "census block" else "ward pair"
cluster_formula <- if (cluster_level == "block") ~block_id else ~ward_pair_id
control_terms <- character(0)
control_subtitle <- ""

data <- read_parquet("../output/corrected_permit_block_year_panel.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(.data[[dose_variable]])
  ) %>%
  mutate(strictness_change = .data[[dose_variable]])
if (stable_sample) {
  data <- data %>% filter(stable_both)
}

if (triple_difference) {
  data <- bind_rows(
    data %>% transmute(
      across(everything()), permit_group = "high", outcome = .data[[high_outcome]]
    ),
    data %>% transmute(
      across(everything()), permit_group = "low", outcome = .data[[low_outcome]]
    )
  ) %>%
    mutate(
      high_discretion = as.integer(permit_group == "high"),
      treatment_dose = strictness_change * high_discretion,
      block_year = paste(block_id, year, sep = "_"),
      block_permit_group = paste(block_id, permit_group, sep = "_"),
      pair_year_group = paste(ward_pair_id, year, permit_group, sep = "_")
    )
  event_formula <- outcome ~ i(relative_year, treatment_dose, ref = -1) |
    block_year + block_permit_group + pair_year_group
  outcome_label <- "High- relative to low-discretion permits"
} else {
  selected_outcome <- if (low_discretion_placebo) low_outcome else high_outcome
  data <- data %>% mutate(outcome = .data[[selected_outcome]])
  if (prevolume_controls) {
    pre_period_controls <- data %>%
      filter(relative_year < 0) %>%
      group_by(block_id) %>%
      summarise(
        pre_period_permit_volume = sum(outcome, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

    if (anyDuplicated(pre_period_controls$block_id) > 0) {
      stop("Pre-period permit controls are not unique by block.", call. = FALSE)
    }

    data <- data %>%
      left_join(pre_period_controls, by = "block_id", relationship = "many-to-one")
    control_terms <- "pre_period_permit_volume:factor(year) + no_pre_period_permits:factor(year)"
    control_subtitle <- "\nPre-period permit volume and zero-permit indicator interacted with year"
  }
  event_right_hand_side <- paste(
    c("i(relative_year, strictness_change, ref = -1)", control_terms),
    collapse = " + "
  )
  event_formula <- as.formula(sprintf(
    "outcome ~ %s | block_id + ward_pair_id^year",
    event_right_hand_side
  ))
  outcome_label <- if (low_discretion_placebo) {
    "Low-discretion permits"
  } else {
    "High-discretion permits"
  }
}

event_model <- fepois(
  event_formula,
  data = data,
  cluster = cluster_formula,
  notes = FALSE
)

event_estimates <- iplot(event_model, .plot = FALSE)[[1]] %>%
  as_tibble() %>%
  transmute(
    event_time = as.integer(x),
    estimate_log = estimate,
    std_error_log = if_else(is_ref, 0, (ci_high - estimate_log) / qnorm(0.975)),
    estimate = expm1(estimate_log),
    ci_low = expm1(ci_low),
    ci_high = expm1(ci_high),
    estimate_name_raw = estimate_names_raw,
    is_reference = is_ref,
    estimate_label = sprintf("%.3f", estimate)
  ) %>%
  filter(event_time >= -5L, event_time <= 5L)

lead_terms <- event_estimates %>%
  filter(event_time <= -2L, !is_reference) %>%
  pull(estimate_name_raw)
pretrend_p_value <- wald(event_model, lead_terms, print = FALSE)$p

pooled_results <- list()
for (window_start in c(0L, 2L, 3L)) {
  pooled_data <- data %>%
    mutate(
      medium_post = as.integer(relative_year >= window_start) * strictness_change,
      early_post = as.integer(relative_year >= 0L & relative_year < window_start) * strictness_change
    )
  if (triple_difference) {
    pooled_data <- pooled_data %>%
      mutate(
        medium_post = medium_post * high_discretion,
        early_post = early_post * high_discretion
      )
    fixed_effects <- "block_year + block_permit_group + pair_year_group"
  } else {
    fixed_effects <- "block_id + ward_pair_id^year"
  }
  post_terms <- if (window_start == 0L) "medium_post" else "medium_post + early_post"
  right_hand_side <- paste(c(post_terms, control_terms), collapse = " + ")
  pooled_model <- fepois(
    as.formula(sprintf("outcome ~ %s | %s", right_hand_side, fixed_effects)),
    data = pooled_data,
    cluster = cluster_formula,
    notes = FALSE
  )
  estimate_log <- coef(pooled_model)[["medium_post"]]
  std_error_log <- se(pooled_model)[["medium_post"]]
  pooled_results[[as.character(window_start)]] <- tibble(
    window = sprintf("%d-5", window_start),
    estimate_log,
    std_error_log,
    effect = expm1(estimate_log),
    p_value = 2 * pnorm(-abs(estimate_log / std_error_log)),
    observations = nobs(pooled_model)
  )
}
pooled_results <- bind_rows(pooled_results)
full_post <- pooled_results %>% filter(window == "0-5")
pooled_stars <- case_when(
  full_post$p_value <= 0.01 ~ "***",
  full_post$p_value <= 0.05 ~ "**",
  full_post$p_value <= 0.10 ~ "*",
  TRUE ~ ""
)

plot <- ggplot(event_estimates, aes(event_time, estimate)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#B8D8CF", color = NA) +
  geom_line(color = "#176B58", linewidth = 0.9) +
  geom_point(color = "#176B58", size = 2.2) +
  scale_x_continuous(breaks = -5:5) +
  labs(
    title = sprintf("%s by %s", outcome_label, timing_label),
    subtitle = sprintf(
      "Pooled estimate = %.3f%s (SE %.3f)\n%s; %s; clustered by %s%s",
      full_post$estimate_log, pooled_stars, full_post$std_error_log,
      sample_label, score_label, cluster_label, control_subtitle
    ),
    x = "Years relative to the 2015 ward remap",
    y = if_else(
      realized_treatment,
      "Effect of a 1 SD increase in realized stringency",
      "Effect of a 1 SD increase in assigned stringency"
    ),
    caption = sprintf(
      "PPML census-block event study with local ward-pair-by-year comparisons. %s sample.",
      bandwidth_label
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
  sprintf(
    "../output/corrected_permit_event_study_%s_clust_%s_%s.pdf",
    specification, cluster_level, bandwidth_label
  ),
  plot, width = 7.6, height = 5.2, bg = "white"
)
ggsave(
  sprintf(
    "../output/corrected_permit_event_study_%s_clust_%s_%s.png",
    specification, cluster_level, bandwidth_label
  ),
  plot, width = 7.6, height = 5.2, dpi = 180, bg = "white"
)

write_csv(
  event_estimates %>% select(event_time, estimate, std_error_log, ci_low, ci_high),
  sprintf(
    "../output/corrected_permit_event_study_%s_clust_%s_%s_estimates.csv",
    specification, cluster_level, bandwidth_label
  )
)
write_csv(
  pooled_results %>%
    mutate(
      specification,
      cluster_level,
      bandwidth_m = bandwidth,
      blocks = n_distinct(data$block_id),
      switched_blocks = n_distinct(data$block_id[data$switched]),
      ward_pairs = n_distinct(data$ward_pair_id),
      pretrend_p_value
    ) %>%
    select(
      specification, cluster_level, window, estimate_log, std_error_log,
      effect, p_value, observations, blocks, switched_blocks, ward_pairs,
      pretrend_p_value, bandwidth_m
    ),
  sprintf(
    "../output/corrected_permit_event_study_%s_clust_%s_%s_summary.csv",
    specification, cluster_level, bandwidth_label
  )
)
