# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")
# bandwidth <- 152.4
# bandwidth_label <- "500ft"
# score_window <- "pre2015"
# treatment_definition <- "realized"
# permit_timing <- "application"
# control_set <- "none"

source("../../../setup_environment/code/packages.R")

grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    bandwidth, bandwidth_label, score_window,
    treatment_definition, permit_timing, control_set
  )
}
if (length(cli_args) != 6) {
  stop("Script requires bandwidth, label, score window, treatment definition, permit timing, and control set.", call. = FALSE)
}

bandwidth <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]
score_window <- cli_args[3]
treatment_definition <- cli_args[4]
permit_timing <- cli_args[5]
control_set <- cli_args[6]

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("Bandwidth must be positive.", call. = FALSE)
}
if (!score_window %in% c("through2022", "pre2015")) {
  stop("Score window must be through2022 or pre2015.", call. = FALSE)
}
if (!treatment_definition %in% c("assigned", "realized")) {
  stop("Treatment definition must be assigned or realized.", call. = FALSE)
}
if (!permit_timing %in% c("issue", "application")) {
  stop("Permit timing must be issue or application.", call. = FALSE)
}
if (!control_set %in% c("none", "prevolume")) {
  stop("Control set must be none or prevolume.", call. = FALSE)
}

dose_variable <- if (score_window == "pre2015" && treatment_definition == "realized") {
  "realized_change_2014"
} else if (score_window == "pre2015") {
  "assigned_change_2014"
} else if (treatment_definition == "realized") {
  "realized_change_2022"
} else {
  "assigned_change_2022"
}
outcome_variable <- if (permit_timing == "application") {
  "n_high_discretion_application"
} else {
  "n_high_discretion_issue"
}
score_label <- if (score_window == "pre2015") {
  "stringency estimated using 2006-2014 permits"
} else {
  "stringency estimated through 2022"
}
timing_label <- if (permit_timing == "application") "application date" else "issue date"
treatment_label <- if (treatment_definition == "realized") "realized" else "assigned"
score_slug <- if (score_window == "pre2015") "pre2015" else "through2022"

data <- read_parquet("../output/corrected_permit_block_year_panel.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(.data[[dose_variable]])
  ) %>%
  mutate(strictness_change = .data[[dose_variable]])

if (control_set == "prevolume") {
  pre_period_controls <- data %>%
    filter(relative_year < 0) %>%
    group_by(block_id) %>%
    summarise(
      pre_period_permit_volume = sum(.data[[outcome_variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

  if (anyDuplicated(pre_period_controls$block_id) > 0) {
    stop("Pre-period permit controls are not unique by block.", call. = FALSE)
  }

  data <- data %>%
    left_join(pre_period_controls, by = "block_id", relationship = "many-to-one")
  control_terms <- "pre_period_permit_volume:factor(year) + no_pre_period_permits:factor(year)"
  control_label <- "pre-period permit volume and zero-permit indicator interacted with year"
  control_slug <- "prevolume_controls"
} else {
  control_terms <- character(0)
  control_label <- "no additional controls"
  control_slug <- "no_controls"
}

all_event_results <- list()
all_pooled_results <- list()

for (direction in c("stricter", "lenient")) {
  if (direction == "stricter") {
    direction_data <- data %>%
      filter(strictness_change >= 0) %>%
      mutate(treatment_dose = strictness_change)
    direction_title <- "Reassignment to a more stringent alderman"
    direction_color <- "#9B2F2F"
    direction_fill <- "#D8A0A0"
  } else {
    direction_data <- data %>%
      filter(strictness_change <= 0) %>%
      mutate(treatment_dose = -strictness_change)
    direction_title <- "Reassignment to a more lenient alderman"
    direction_color <- "#176B58"
    direction_fill <- "#B8D8CF"
  }

  event_right_hand_side <- paste(
    c("i(relative_year, treatment_dose, ref = -1)", control_terms),
    collapse = " + "
  )
  event_model <- fepois(
    as.formula(sprintf(
      "%s ~ %s | block_id + ward_pair_id^year",
      outcome_variable,
      event_right_hand_side
    )),
    data = direction_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  event_results <- iplot(event_model, .plot = FALSE)[[1]] %>%
    as_tibble() %>%
    transmute(
      direction,
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

  lead_terms <- event_results %>%
    filter(event_time <= -2L, !is_reference) %>%
    pull(estimate_name_raw)
  pretrend_p_value <- wald(event_model, lead_terms, print = FALSE)$p

  pooled_results <- list()
  for (window_start in c(0L, 2L, 3L)) {
    pooled_data <- direction_data %>%
      mutate(
        medium_post = as.integer(relative_year >= window_start) * treatment_dose,
        early_post = as.integer(relative_year >= 0L & relative_year < window_start) * treatment_dose
      )
    post_terms <- if (window_start == 0L) "medium_post" else "medium_post + early_post"
    right_hand_side <- paste(c(post_terms, control_terms), collapse = " + ")
    pooled_model <- fepois(
      as.formula(sprintf(
        "%s ~ %s | block_id + ward_pair_id^year",
        outcome_variable,
        right_hand_side
      )),
      data = pooled_data,
      cluster = ~ward_pair_id,
      notes = FALSE
    )
    estimate_log <- coef(pooled_model)[["medium_post"]]
    std_error_log <- se(pooled_model)[["medium_post"]]
    pooled_results[[as.character(window_start)]] <- tibble(
      direction,
      window = sprintf("%d-5", window_start),
      estimate_log,
      std_error_log,
      effect = expm1(estimate_log),
      p_value = 2 * pnorm(-abs(estimate_log / std_error_log)),
      pretrend_p_value,
      observations = nobs(pooled_model),
      blocks = n_distinct(direction_data$block_id),
      treated_blocks = n_distinct(direction_data$block_id[direction_data$treatment_dose > 0]),
      control_blocks = n_distinct(direction_data$block_id[direction_data$treatment_dose == 0]),
      ward_pairs = n_distinct(direction_data$ward_pair_id),
      bandwidth_m = bandwidth,
      control_set
    )
  }
  pooled_results <- bind_rows(pooled_results)
  full_post <- pooled_results %>% filter(window == "0-5")

  plot <- ggplot(event_results, aes(event_time, estimate)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = direction_fill, color = NA, alpha = 0.55) +
    geom_line(color = direction_color, linewidth = 0.9) +
    geom_point(color = direction_color, size = 2.2) +
    geom_text(aes(label = estimate_label), vjust = -0.8, size = 3, color = direction_color) +
    scale_x_continuous(breaks = -5:5) +
    labs(
      title = direction_title,
      subtitle = sprintf(
        "High-discretion permits by %s; estimated separately against stayers\n%s\n%s\nPooled years 0-5 = %.3f (SE %.3f, p = %.3f); pretrend p = %.3f",
        timing_label,
        score_label,
        control_label,
        full_post$effect,
        full_post$std_error_log,
        full_post$p_value,
        full_post$pretrend_p_value
      ),
      x = "Years relative to the 2015 ward remap",
      y = "Effect of a 1 SD directional stringency change",
      caption = sprintf(
        "PPML with block and ward-pair-by-year fixed effects; ward-pair clustered SEs. %s sample.",
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

  output_stub <- sprintf(
    "../output/corrected_permit_direction_continuous_separate_%s_%s_%s_%s_%s_%s",
    permit_timing, treatment_label, score_slug, bandwidth_label, control_slug, direction
  )
  ggsave(paste0(output_stub, ".pdf"), plot, width = 7.6, height = 5.2, bg = "white")
  ggsave(paste0(output_stub, ".png"), plot, width = 7.6, height = 5.2, dpi = 180, bg = "white")

  all_event_results[[direction]] <- event_results
  all_pooled_results[[direction]] <- pooled_results
}

write_csv(
  bind_rows(all_event_results) %>%
    select(direction, event_time, estimate, std_error_log, ci_low, ci_high),
  sprintf(
    "../output/corrected_permit_direction_continuous_separate_%s_%s_%s_%s_%s_estimates.csv",
    permit_timing, treatment_label, score_slug, bandwidth_label, control_slug
  )
)
write_csv(
  bind_rows(all_pooled_results) %>%
    mutate(permit_timing, treatment_definition, score_window),
  sprintf(
    "../output/corrected_permit_direction_continuous_separate_%s_%s_%s_%s_%s_summary.csv",
    permit_timing, treatment_label, score_slug, bandwidth_label, control_slug
  )
)
