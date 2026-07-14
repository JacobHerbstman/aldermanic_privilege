# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")
# cluster_level <- "wardpair"
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"

source("../../../setup_environment/code/packages.R")

grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(cluster_level, bandwidth, bandwidth_label)
}
if (length(cli_args) != 3) {
  stop("Script requires cluster level, bandwidth, and label.", call. = FALSE)
}

cluster_level <- cli_args[1]
bandwidth <- as.numeric(cli_args[2])
bandwidth_label <- cli_args[3]
if (!cluster_level %in% c("block", "wardpair")) {
  stop("Cluster level must be block or wardpair.", call. = FALSE)
}
if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("Bandwidth must be positive.", call. = FALSE)
}

cluster_formula <- if (cluster_level == "block") ~block_id else ~ward_pair_id
cluster_label <- if (cluster_level == "block") "census block" else "ward pair"

data <- read_parquet("../output/corrected_permit_block_year_panel.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(assigned_change_2022),
    !is.na(realized_change_2022)
  )

block_data <- data %>%
  distinct(block_id, ward_pair_id, assigned_change_2022, realized_change_2022)
first_stage <- feols(
  realized_change_2022 ~ assigned_change_2022 | ward_pair_id,
  data = block_data,
  cluster = cluster_formula,
  notes = FALSE
)
first_stage_estimate <- coef(first_stage)[["assigned_change_2022"]]
first_stage_se <- se(first_stage)[["assigned_change_2022"]]
first_stage_f <- (first_stage_estimate / first_stage_se)^2

event_times <- c(-5L, -4L, -3L, -2L, 0L, 1L, 2L, 3L, 4L, 5L)
realized_terms <- character(length(event_times))
assigned_terms <- character(length(event_times))
for (i in seq_along(event_times)) {
  event_time <- event_times[i]
  suffix <- if (event_time < 0L) paste0("m", abs(event_time)) else paste0("p", event_time)
  realized_terms[i] <- paste0("realized_", suffix)
  assigned_terms[i] <- paste0("assigned_", suffix)
  data[[realized_terms[i]]] <- as.integer(data$relative_year == event_time) * data$realized_change_2022
  data[[assigned_terms[i]]] <- as.integer(data$relative_year == event_time) * data$assigned_change_2022
}

event_model <- feols(
  as.formula(sprintf(
    "n_high_discretion_issue ~ 1 | block_id + ward_pair_id^year | %s ~ %s",
    paste(realized_terms, collapse = " + "),
    paste(assigned_terms, collapse = " + ")
  )),
  data = data,
  cluster = cluster_formula,
  notes = FALSE
)

event_estimates <- tibble(
  event_time = event_times,
  estimate = unname(coef(event_model)[paste0("fit_", realized_terms)]),
  std_error = unname(se(event_model)[paste0("fit_", realized_terms)])
) %>%
  mutate(
    ci_low = estimate - qnorm(0.975) * std_error,
    ci_high = estimate + qnorm(0.975) * std_error
  ) %>%
  bind_rows(tibble(event_time = -1L, estimate = 0, std_error = 0, ci_low = 0, ci_high = 0)) %>%
  arrange(event_time) %>%
  mutate(estimate_label = sprintf("%.3f", estimate))
pretrend_p_value <- wald(
  event_model,
  paste0("fit_", realized_terms[event_times <= -2L]),
  print = FALSE
)$p

pooled_results <- list()
for (window_start in c(0L, 2L, 3L)) {
  pooled_data <- data %>%
    mutate(
      realized_medium = as.integer(relative_year >= window_start) * realized_change_2022,
      assigned_medium = as.integer(relative_year >= window_start) * assigned_change_2022,
      realized_early = as.integer(relative_year >= 0L & relative_year < window_start) * realized_change_2022,
      assigned_early = as.integer(relative_year >= 0L & relative_year < window_start) * assigned_change_2022
    )
  if (window_start == 0L) {
    iv_formula <- n_high_discretion_issue ~ 1 | block_id + ward_pair_id^year |
      realized_medium ~ assigned_medium
  } else {
    iv_formula <- n_high_discretion_issue ~ 1 | block_id + ward_pair_id^year |
      realized_medium + realized_early ~ assigned_medium + assigned_early
  }
  pooled_model <- feols(
    iv_formula,
    data = pooled_data,
    cluster = cluster_formula,
    notes = FALSE
  )
  estimate <- coef(pooled_model)[["fit_realized_medium"]]
  std_error <- se(pooled_model)[["fit_realized_medium"]]
  pooled_results[[as.character(window_start)]] <- tibble(
    window = sprintf("%d-5", window_start),
    estimate,
    std_error,
    p_value = 2 * pnorm(-abs(estimate / std_error)),
    observations = nobs(pooled_model)
  )
}
pooled_results <- bind_rows(pooled_results)
full_post <- pooled_results %>% filter(window == "0-5")

plot <- ggplot(event_estimates, aes(event_time, estimate)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#D7C7A4", color = NA) +
  geom_line(color = "#755C24", linewidth = 0.9) +
  geom_point(color = "#755C24", size = 2.2) +
  geom_text(aes(label = estimate_label), vjust = -0.8, size = 3, color = "#493A19") +
  scale_x_continuous(breaks = -5:5) +
  labs(
    title = "Exploratory IV: issued high-discretion permits",
    subtitle = sprintf(
      "2014 assigned change instruments for realized 2015 change; clustered by %s\nPooled years 0-5 = %.3f (SE %.3f, p = %.3f); first-stage F = %.1f",
      cluster_label, full_post$estimate, full_post$std_error, full_post$p_value, first_stage_f
    ),
    x = "Years relative to the 2015 ward remap",
    y = "Change in permits per block-year",
    caption = "Linear 2SLS with block and ward-pair-by-year fixed effects. Exclusion restriction is not established."
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
  sprintf("../output/corrected_permit_iv_issue_clust_%s_%s.pdf", cluster_level, bandwidth_label),
  plot, width = 7.8, height = 5.2, bg = "white"
)
ggsave(
  sprintf("../output/corrected_permit_iv_issue_clust_%s_%s.png", cluster_level, bandwidth_label),
  plot, width = 7.8, height = 5.2, dpi = 180, bg = "white"
)

write_csv(
  event_estimates %>% select(event_time, estimate, std_error, ci_low, ci_high),
  sprintf("../output/corrected_permit_iv_issue_clust_%s_%s_estimates.csv", cluster_level, bandwidth_label)
)
write_csv(
  pooled_results %>%
    mutate(
      cluster_level,
      first_stage_estimate,
      first_stage_se,
      first_stage_f,
      pretrend_p_value,
      blocks = n_distinct(data$block_id),
      switched_blocks = n_distinct(data$block_id[data$switched]),
      ward_pairs = n_distinct(data$ward_pair_id),
      bandwidth_m = bandwidth
    ),
  sprintf("../output/corrected_permit_iv_issue_clust_%s_%s_summary.csv", cluster_level, bandwidth_label)
)
