# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/corrected_permit_event_study/code")
# treatment_type <- "indicator"
# bandwidth <- 152.4
# bandwidth_label <- "500ft"
# score_window <- "through2022"
# treatment_definition <- "assigned"
# permit_timing <- "issue"

source("../../../setup_environment/code/packages.R")

grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    treatment_type, bandwidth, bandwidth_label, score_window,
    treatment_definition, permit_timing
  )
}
if (length(cli_args) != 6) {
  stop("Script requires treatment type, bandwidth, label, score window, treatment definition, and permit timing.", call. = FALSE)
}

treatment_type <- cli_args[1]
bandwidth <- as.numeric(cli_args[2])
bandwidth_label <- cli_args[3]
score_window <- cli_args[4]
treatment_definition <- cli_args[5]
permit_timing <- cli_args[6]
if (!treatment_type %in% c("indicator", "continuous_split")) {
  stop("Treatment type must be indicator or continuous_split.", call. = FALSE)
}
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

dose_variable <- if (score_window == "pre2015" && treatment_definition == "realized") {
  "realized_change_2014"
} else if (score_window == "pre2015") {
  "assigned_change_2014"
} else if (treatment_definition == "realized") {
  "realized_change_2022"
} else {
  "assigned_change_2022"
}
score_label <- if (score_window == "pre2015") {
  "stringency estimated using 2006-2014 permits"
} else {
  "stringency estimated through 2022"
}
output_label <- if (score_window == "pre2015" && treatment_definition == "realized") {
  paste0("realized_pre2015_", bandwidth_label)
} else if (score_window == "pre2015") {
  paste0("pre2015_", bandwidth_label)
} else if (treatment_definition == "realized") {
  paste0("realized_", bandwidth_label)
} else {
  bandwidth_label
}
if (permit_timing == "application") {
  output_label <- paste0("application_", output_label)
}
outcome_variable <- if (permit_timing == "application") {
  "n_high_discretion_application"
} else {
  "n_high_discretion_issue"
}
timing_label <- if (permit_timing == "application") "application date" else "issue date"

data <- read_parquet("../output/corrected_permit_block_year_panel.parquet") %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= -5L,
    relative_year <= 5L,
    !is.na(.data[[dose_variable]])
  ) %>%
  mutate(
    strictness_change = .data[[dose_variable]],
    stricter_indicator = as.integer(strictness_change > 0),
    lenient_indicator = as.integer(strictness_change < 0),
    stricter_dose = pmax(strictness_change, 0),
    lenient_dose = pmax(-strictness_change, 0)
  )

if (treatment_type == "indicator") {
  stricter_variable <- "stricter_indicator"
  lenient_variable <- "lenient_indicator"
  subtitle_label <- "Directional indicators relative to stayers"
  y_axis_label <- "Effect of ward reassignment"
} else {
  stricter_variable <- "stricter_dose"
  lenient_variable <- "lenient_dose"
  subtitle_label <- "Separate stringency slopes by direction"
  y_axis_label <- "Effect of a 1 SD directional stringency change"
}

event_model <- fepois(
  as.formula(sprintf(
    paste0(
      outcome_variable, " ~ ",
      "i(relative_year, %s, ref = -1) + i(relative_year, %s, ref = -1) | ",
      "block_id + ward_pair_id^year"
    ),
    stricter_variable,
    lenient_variable
  )),
  data = data,
  cluster = ~ward_pair_id,
  notes = FALSE
)

event_results <- list()
for (direction in c("Moved to stricter", "Moved to more lenient")) {
  treatment_variable <- if (direction == "Moved to stricter") stricter_variable else lenient_variable
  coefficient_names <- names(coef(event_model))
  selected_names <- coefficient_names[grepl(paste0(":", treatment_variable, "$"), coefficient_names)]
  selected_event_times <- as.integer(sub("^relative_year::(-?[0-9]+):.*$", "\\1", selected_names))
  event_results[[direction]] <- tibble(
    direction,
    event_time = selected_event_times,
    estimate_log = unname(coef(event_model)[selected_names]),
    std_error_log = unname(se(event_model)[selected_names])
  ) %>%
    mutate(
      estimate = expm1(estimate_log),
      ci_low = expm1(estimate_log - qnorm(0.975) * std_error_log),
      ci_high = expm1(estimate_log + qnorm(0.975) * std_error_log)
    ) %>%
    bind_rows(tibble(
      direction,
      event_time = -1L,
      estimate_log = 0,
      std_error_log = 0,
      estimate = 0,
      ci_low = 0,
      ci_high = 0
    ))
}
event_results <- bind_rows(event_results) %>%
  arrange(direction, event_time) %>%
  mutate(estimate_label = sprintf("%.3f", estimate))

pretrend_results <- event_results %>%
  filter(event_time <= -2L) %>%
  group_by(direction) %>%
  summarise(
    pretrend_p_value = wald(
      event_model,
      names(coef(event_model))[
        grepl(
          paste0(":", if_else(
            first(direction) == "Moved to stricter",
            stricter_variable,
            lenient_variable
          ), "$"),
          names(coef(event_model))
        ) & grepl("relative_year::-[2-5]:", names(coef(event_model)))
      ],
      print = FALSE
    )$p,
    .groups = "drop"
  )

pooled_results <- list()
for (window_start in c(0L, 2L, 3L)) {
  pooled_data <- data %>%
    mutate(
      stricter_medium = as.integer(relative_year >= window_start) * .data[[stricter_variable]],
      lenient_medium = as.integer(relative_year >= window_start) * .data[[lenient_variable]],
      stricter_early = as.integer(relative_year >= 0L & relative_year < window_start) * .data[[stricter_variable]],
      lenient_early = as.integer(relative_year >= 0L & relative_year < window_start) * .data[[lenient_variable]]
    )
  right_hand_side <- if (window_start == 0L) {
    "stricter_medium + lenient_medium"
  } else {
    "stricter_medium + lenient_medium + stricter_early + lenient_early"
  }
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
  pooled_results[[paste0(window_start, "_stricter")]] <- tibble(
    direction = "Moved to stricter",
    window = sprintf("%d-5", window_start),
    estimate_log = coef(pooled_model)[["stricter_medium"]],
    std_error_log = se(pooled_model)[["stricter_medium"]]
  )
  pooled_results[[paste0(window_start, "_lenient")]] <- tibble(
    direction = "Moved to more lenient",
    window = sprintf("%d-5", window_start),
    estimate_log = coef(pooled_model)[["lenient_medium"]],
    std_error_log = se(pooled_model)[["lenient_medium"]]
  )
}
pooled_results <- bind_rows(pooled_results) %>%
  mutate(
    effect = expm1(estimate_log),
    p_value = 2 * pnorm(-abs(estimate_log / std_error_log))
  ) %>%
  left_join(pretrend_results, by = "direction", relationship = "many-to-one")

full_post <- pooled_results %>% filter(window == "0-5")
pooled_subtitle <- paste(
  sprintf(
    "%s: %.3f (SE %.3f, p = %.3f)",
    full_post$direction,
    full_post$effect,
    full_post$std_error_log,
    full_post$p_value
  ),
  collapse = "\n"
)

plot <- ggplot(event_results, aes(event_time, estimate, color = direction, fill = direction)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.16, color = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.1) +
  geom_text(
    data = event_results %>% filter(direction == "Moved to stricter"),
    aes(label = estimate_label),
    vjust = -0.8,
    size = 2.7,
    show.legend = FALSE
  ) +
  geom_text(
    data = event_results %>% filter(direction == "Moved to more lenient"),
    aes(label = estimate_label),
    vjust = 1.5,
    size = 2.7,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Moved to stricter" = "#9B2F2F", "Moved to more lenient" = "#176B58")) +
  scale_fill_manual(values = c("Moved to stricter" = "#D8A0A0", "Moved to more lenient" = "#B8D8CF")) +
  scale_x_continuous(breaks = -5:5) +
  labs(
    title = if_else(
      treatment_definition == "realized",
      "Realized alderman change by direction",
      "Assigned-change ITT by direction"
    ),
    subtitle = sprintf(
      "High-discretion permits by %s\n%s; %s\nPooled years 0-5:\n%s",
      timing_label,
      subtitle_label,
      score_label,
      pooled_subtitle
    ),
    x = "Years relative to the 2015 ward remap",
    y = y_axis_label,
    color = NULL,
    fill = NULL,
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
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  sprintf("../output/corrected_permit_direction_%s_%s.pdf", treatment_type, output_label),
  plot, width = 8.2, height = 5.5, bg = "white"
)
ggsave(
  sprintf("../output/corrected_permit_direction_%s_%s.png", treatment_type, output_label),
  plot, width = 8.2, height = 5.5, dpi = 180, bg = "white"
)

for (direction in c("Moved to stricter", "Moved to more lenient")) {
  direction_slug <- if (direction == "Moved to stricter") "stricter" else "lenient"
  direction_color <- if (direction == "Moved to stricter") "#9B2F2F" else "#176B58"
  direction_fill <- if (direction == "Moved to stricter") "#D8A0A0" else "#B8D8CF"
  direction_title <- if (direction == "Moved to stricter") {
    "Reassignment to a more stringent alderman"
  } else {
    "Reassignment to a more lenient alderman"
  }
  direction_results <- event_results %>% filter(.data$direction == .env$direction)
  direction_summary <- full_post %>% filter(.data$direction == .env$direction)

  direction_plot <- ggplot(direction_results, aes(event_time, estimate)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = direction_fill, color = NA, alpha = 0.55) +
    geom_line(color = direction_color, linewidth = 0.9) +
    geom_point(color = direction_color, size = 2.2) +
    geom_text(
      aes(label = estimate_label),
      vjust = -0.8,
      size = 3,
      color = direction_color
    ) +
    scale_x_continuous(breaks = -5:5) +
    labs(
      title = direction_title,
      subtitle = sprintf(
        "High-discretion permits by %s\n%s; %s\nPooled years 0-5 = %.3f (SE %.3f, p = %.3f); pretrend p = %.3f",
        timing_label,
        subtitle_label,
        score_label,
        direction_summary$effect,
        direction_summary$std_error_log,
        direction_summary$p_value,
        direction_summary$pretrend_p_value
      ),
      x = "Years relative to the 2015 ward remap",
      y = y_axis_label,
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

  ggsave(
    sprintf(
      "../output/corrected_permit_direction_%s_%s_%s.pdf",
      treatment_type, output_label, direction_slug
    ),
    direction_plot, width = 7.6, height = 5.2, bg = "white"
  )
  ggsave(
    sprintf(
      "../output/corrected_permit_direction_%s_%s_%s.png",
      treatment_type, output_label, direction_slug
    ),
    direction_plot, width = 7.6, height = 5.2, dpi = 180, bg = "white"
  )
}

write_csv(
  event_results %>% select(direction, event_time, estimate, std_error_log, ci_low, ci_high),
  sprintf("../output/corrected_permit_direction_%s_%s_estimates.csv", treatment_type, output_label)
)
write_csv(
  pooled_results %>%
    mutate(
      treatment_type,
      score_window,
      treatment_definition,
      permit_timing,
      blocks = n_distinct(data$block_id),
      stricter_blocks = n_distinct(data$block_id[data$strictness_change > 0]),
      lenient_blocks = n_distinct(data$block_id[data$strictness_change < 0]),
      control_blocks = n_distinct(data$block_id[data$strictness_change == 0]),
      ward_pairs = n_distinct(data$ward_pair_id),
      bandwidth_m = bandwidth
    ),
  sprintf("../output/corrected_permit_direction_%s_%s_summary.csv", treatment_type, output_label)
)
