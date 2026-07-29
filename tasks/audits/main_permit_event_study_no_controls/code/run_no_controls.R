# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/main_permit_event_study_no_controls/code")
# specification <- "main"

source("../../../setup_environment/code/packages.R")

grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(specification)
}
if (length(cli_args) != 1) {
  stop("Script requires one specification.", call. = FALSE)
}

specification <- cli_args[1]
if (!specification %in% c("main", "low_discretion", "stable_incumbent")) {
  stop("Specification must be main, low_discretion, or stable_incumbent.", call. = FALSE)
}

outcome_var <- if (specification == "low_discretion") {
  "n_low_discretion_nosigns_application"
} else {
  "n_high_discretion_application"
}
outcome_label <- if (specification == "low_discretion") {
  "Low-discretion permits by application year"
} else {
  "High-discretion permits by application year"
}
output_prefix <- case_when(
  specification == "main" ~ "main_permit_event_study_no_controls_500ft",
  specification == "low_discretion" ~ "appendix_low_discretion_event_study_no_controls_500ft",
  TRUE ~ "appendix_stable_incumbent_event_study_no_controls_500ft"
)

data <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    dist_m <= 152.4,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  mutate(
    outcome = .data[[outcome_var]],
    strictness_change = strictness_change_frozen
  )
if (specification == "stable_incumbent") {
  data <- data %>% filter(stable_both)
}

if (anyDuplicated(data[c("block_id", "year")]) > 0) {
  stop("Permit event-study data must be unique by block and year.", call. = FALSE)
}

event_model <- fepois(
  outcome ~ i(relative_year, strictness_change, ref = -1) |
    block_id + ward_pair_id^year,
  data = data,
  cluster = ~ward_pair_id,
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
    is_reference = is_ref
  ) %>%
  filter(event_time >= -5L, event_time <= 5L)

lead_terms <- event_estimates %>%
  filter(event_time <= -2L, !is_reference) %>%
  pull(estimate_name_raw)
pretrend_p_value <- wald(event_model, lead_terms, print = FALSE)$p

pooled_data <- data %>%
  mutate(post_treat = as.integer(relative_year >= 0L) * strictness_change)

pooled_model <- fepois(
  outcome ~ post_treat | block_id + ward_pair_id^year,
  data = pooled_data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

pooled_estimate <- coef(pooled_model)[["post_treat"]]
pooled_std_error <- se(pooled_model)[["post_treat"]]
pooled_p_value <- pvalue(pooled_model)[["post_treat"]]
pooled_stars <- case_when(
  pooled_p_value <= 0.01 ~ "***",
  pooled_p_value <= 0.05 ~ "**",
  pooled_p_value <= 0.10 ~ "*",
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
    title = outcome_label,
    subtitle = sprintf(
      "Pooled estimate = %.3f%s (SE %.3f)",
      pooled_estimate,
      pooled_stars,
      pooled_std_error
    ),
    x = "Years relative to the 2015 ward remap",
    y = "Effect of a 1 SD increase in assigned stringency"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  sprintf("../output/%s.pdf", output_prefix),
  plot,
  width = 7.6,
  height = 5.2,
  bg = "white"
)
ggsave(
  sprintf("../output/%s.png", output_prefix),
  plot,
  width = 7.6,
  height = 5.2,
  dpi = 200,
  bg = "white"
)

write_csv(
  event_estimates %>%
    select(event_time, estimate_log, std_error_log, estimate, ci_low, ci_high),
  sprintf("../output/%s_estimates.csv", output_prefix)
)
write_csv(
  tibble(
    estimate_log = pooled_estimate,
    std_error_log = pooled_std_error,
    effect = expm1(pooled_estimate),
    p_value = pooled_p_value,
    pretrend_p_value = pretrend_p_value,
    observations = nobs(pooled_model),
    blocks = n_distinct(data$block_id),
    ward_pairs = n_distinct(data$ward_pair_id)
  ),
  sprintf("../output/%s_summary.csv", output_prefix)
)
