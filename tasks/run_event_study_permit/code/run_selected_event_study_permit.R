# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# outcome_family <- "high_discretion"
# sample <- "itt"
# bandwidth <- 152.4
# bandwidth_label <- "500ft"
# min_period <- -5
# max_period <- 5

source("../../setup_environment/code/packages.R")

grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(outcome_family, sample, bandwidth, bandwidth_label, min_period, max_period)
}
if (length(cli_args) != 6) {
  stop("Script requires outcome family, sample, bandwidth, bandwidth label, minimum period, and maximum period.", call. = FALSE)
}

outcome_family <- cli_args[1]
sample <- cli_args[2]
bandwidth <- as.numeric(cli_args[3])
bandwidth_label <- cli_args[4]
min_period <- as.integer(cli_args[5])
max_period <- as.integer(cli_args[6])

if (!outcome_family %in% c("high_discretion", "low_discretion_nosigns")) {
  stop("outcome_family must be high_discretion or low_discretion_nosigns.", call. = FALSE)
}
if (!sample %in% c("itt", "stable")) {
  stop("sample must be itt or stable.", call. = FALSE)
}
if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (!is.finite(min_period) || !is.finite(max_period) || min_period >= max_period) {
  stop("min_period and max_period must define an increasing event window.", call. = FALSE)
}

outcome_var <- if (outcome_family == "high_discretion") {
  "n_high_discretion_application"
} else {
  "n_low_discretion_nosigns_application"
}
outcome_label <- if (outcome_family == "high_discretion") {
  "High-discretion permits by application year"
} else {
  "Low-discretion permits by application year"
}

data <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= min_period,
    relative_year <= max_period,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  mutate(
    outcome = .data[[outcome_var]],
    strictness_change = strictness_change_frozen
  )
if (sample == "stable") {
  data <- data %>% filter(stable_both)
}

if (anyDuplicated(data[c("block_id", "year")]) > 0) {
  stop("Permit event-study data must be unique by block and year.", call. = FALSE)
}

pre_period_controls <- data %>%
  filter(relative_year < 0) %>%
  group_by(block_id) %>%
  summarise(
    pre_period_permit_volume = sum(outcome, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

if (anyDuplicated(pre_period_controls$block_id) > 0) {
  stop("Pre-period permit controls must be unique by block.", call. = FALSE)
}

data <- data %>%
  left_join(pre_period_controls, by = "block_id", relationship = "many-to-one")

event_model <- fepois(
  outcome ~ i(relative_year, strictness_change, ref = -1) +
    pre_period_permit_volume:factor(year) +
    no_pre_period_permits:factor(year) |
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
    estimate = expm1(estimate_log),
    ci_low = expm1(ci_low),
    ci_high = expm1(ci_high)
  ) %>%
  filter(event_time >= min_period, event_time <= max_period)

pooled_data <- data %>%
  mutate(post_treat = as.integer(relative_year >= 0) * strictness_change)

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
  scale_x_continuous(breaks = seq(min_period, max_period)) +
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
  sprintf(
    "../output/event_study_permit_2015_%s_application_frozen2014_%s_preperiod_controls_%s_clust_ward_pair.pdf",
    outcome_family,
    sample,
    bandwidth_label
  ),
  plot,
  width = 7.6,
  height = 5.2,
  bg = "white"
)
