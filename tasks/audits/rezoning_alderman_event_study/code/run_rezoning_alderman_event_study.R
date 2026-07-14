# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rezoning_alderman_event_study/code")
# outcome <- "any_rezoning"
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"
# min_event_year <- -4
# max_event_year <- 5
# score_window <- "through2022"

source("../../../setup_environment/code/packages.R")

grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(outcome, bandwidth, bandwidth_label, min_event_year, max_event_year, score_window)
}
if (length(cli_args) != 6) {
  stop("Script requires outcome, bandwidth, label, event-year limits, and score window.", call. = FALSE)
}
outcome <- cli_args[1]
bandwidth <- as.numeric(cli_args[2])
bandwidth_label <- cli_args[3]
min_event_year <- as.integer(cli_args[4])
max_event_year <- as.integer(cli_args[5])
score_window <- cli_args[6]

valid_outcomes <- c("any_rezoning", "any_upzone", "far_change_total", "mean_far_change", "upzone_share")
if (!outcome %in% valid_outcomes) {
  stop("Unknown rezoning outcome.", call. = FALSE)
}
if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("Bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("Bandwidth label contains invalid characters.", call. = FALSE)
}
if (!is.finite(min_event_year) || !is.finite(max_event_year) || min_event_year >= -1L || max_event_year < 0L) {
  stop("Event window must contain leads, the omitted year -1, and post-periods.", call. = FALSE)
}
if (!score_window %in% c("through2022", "pre2015")) {
  stop("Score window must be through2022 or pre2015.", call. = FALSE)
}

score_label <- if (score_window == "pre2015") {
  "Stringency estimated using 2006-2014 permits"
} else {
  "Stringency estimated through 2022"
}
output_stem <- if (score_window == "pre2015") {
  sprintf("pre2015_%s_%s", outcome, bandwidth_label)
} else {
  sprintf("%s_%s", outcome, bandwidth_label)
}

conditional_outcome <- outcome %in% c("mean_far_change", "upzone_share")
outcome_label <- c(
  any_rezoning = "Probability of any rezoning",
  any_upzone = "Probability of any upzoning",
  far_change_total = "Total FAR change",
  mean_far_change = "Mean FAR change conditional on rezoning",
  upzone_share = "Upzoning share conditional on rezoning"
)[[outcome]]

panel_path <- if (score_window == "pre2015") {
  "../output/rezoning_alderman_block_year_panel_pre2015.parquet"
} else {
  "../output/rezoning_alderman_block_year_panel.parquet"
}
data <- read_parquet(panel_path) %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= min_event_year,
    relative_year <= max_event_year,
    !is.na(ward_pair_id),
    ward_pair_id != "",
    !is.na(.data[[outcome]])
  )

fixed_effects <- if (conditional_outcome) {
  "ward_pair_id^year"
} else {
  "block_id + ward_pair_id^year"
}

model <- feols(
  as.formula(sprintf(
    "%s ~ i(relative_year, strictness_change, ref = -1) | %s",
    outcome,
    fixed_effects
  )),
  data = data,
  cluster = ~block_id,
  notes = FALSE
)

iplot_data <- iplot(model, .plot = FALSE)[[1]] %>%
  as_tibble() %>%
  transmute(
    event_time = as.integer(x),
    estimate,
    ci_low,
    ci_high,
    std_error = if_else(is_ref, 0, (ci_high - estimate) / qnorm(0.975)),
    estimate_name_raw = estimate_names_raw,
    is_reference = is_ref,
    estimate_label = sprintf("%.3f", estimate)
  ) %>%
  filter(event_time >= min_event_year, event_time <= max_event_year)

lead_terms <- iplot_data %>%
  filter(event_time <= -2L, !is_reference) %>%
  pull(estimate_name_raw)
pretrend <- if (length(lead_terms) > 0L) {
  wald(model, lead_terms)
} else {
  NULL
}

plot <- ggplot(iplot_data, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, color = "gray45", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#B8D8CF", color = NA) +
  geom_line(color = "#176B58", linewidth = 0.9) +
  geom_point(color = "#176B58", size = 2.2) +
  geom_text(aes(label = estimate_label), vjust = -0.8, size = 3, color = "#24413A") +
  scale_x_continuous(breaks = seq(min_event_year, max_event_year)) +
  labs(
    title = "Rezoning after reassignment to a more stringent alderman",
    subtitle = sprintf(
      "Coefficient for a one standard deviation increase in aldermanic stringency. %s.",
      score_label
    ),
    x = "Years relative to 2015 ward reassignment",
    y = outcome_label
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 18, 10, 10)
  )

ggsave(
  sprintf("../output/rezoning_alderman_event_study_%s.pdf", output_stem),
  plot,
  width = 7.4,
  height = 4.8,
  bg = "white"
)

write_csv(
  iplot_data %>% select(event_time, estimate, std_error, ci_low, ci_high),
  sprintf("../output/rezoning_alderman_event_study_%s_estimates.csv", output_stem)
)
write_csv(
  tibble(
    outcome,
    score_window,
    conditional_outcome,
    fixed_effects,
    observations = nobs(model),
    blocks = n_distinct(data$block_id),
    rezoning_block_years = sum(data$any_rezoning == 1L),
    min_event_year,
    max_event_year,
    pretrend_p_value = if (is.null(pretrend)) NA_real_ else pretrend$p
  ),
  sprintf("../output/rezoning_alderman_event_study_%s_summary.csv", output_stem)
)
