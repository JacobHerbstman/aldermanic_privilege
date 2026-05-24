build_event_study_support_table <- function(df, event_var, time_fe_var, fe_group_var, min_period, max_period,
                                            support_mode = c("two_sided_cells", "treatment_variation"),
                                            cohort_label = NA_character_, treat_var = "treat", side_var = NULL,
                                            treatment_var = NULL, block_var = "block_id", segment_var = NULL,
                                            outcome_var = NULL, pin_var = NULL) {
  support_mode <- match.arg(support_mode)
  if (support_mode == "two_sided_cells" && is.null(side_var)) {
    stop("side_var is required for two_sided_cells support.", call. = FALSE)
  }
  if (support_mode == "treatment_variation" && is.null(treatment_var)) {
    stop("treatment_var is required for treatment_variation support.", call. = FALSE)
  }

  support_base <- df %>%
    filter(.data[[event_var]] >= min_period, .data[[event_var]] <= max_period)

  if (support_mode == "two_sided_cells") {
    cell_support <- support_base %>%
      group_by(
        event_time = .data[[event_var]],
        fe_group = .data[[fe_group_var]],
        calendar_time = .data[[time_fe_var]]
      ) %>%
      summarise(
        n_treated = sum(.data[[treat_var]] == 1, na.rm = TRUE),
        n_control = sum(.data[[treat_var]] == 0, na.rm = TRUE),
        n_sides = n_distinct(.data[[side_var]]),
        has_identifying_cell = n_treated > 0 & n_control > 0 & n_sides == 2,
        .groups = "drop"
      )
  } else {
    cell_support <- support_base %>%
      group_by(
        event_time = .data[[event_var]],
        fe_group = .data[[fe_group_var]],
        calendar_time = .data[[time_fe_var]]
      ) %>%
      summarise(
        n_treated = sum(.data[[treat_var]] == 1, na.rm = TRUE),
        n_control = sum(.data[[treat_var]] == 0, na.rm = TRUE),
        n_distinct_treatment_values = n_distinct(.data[[treatment_var]][!is.na(.data[[treatment_var]])]),
        has_identifying_cell = n_distinct_treatment_values > 1,
        .groups = "drop"
      )
  }

  event_support <- support_base %>%
    group_by(event_time = .data[[event_var]]) %>%
    summarise(
      n_obs = n(),
      n_treated = sum(.data[[treat_var]] == 1, na.rm = TRUE),
      n_control = sum(.data[[treat_var]] == 0, na.rm = TRUE),
      contributing_cohorts = if ("cohort" %in% names(support_base)) paste(sort(unique(cohort)), collapse = "|") else cohort_label,
      n_fe_groups = n_distinct(.data[[fe_group_var]]),
      n_blocks = if (!is.null(block_var) && block_var %in% names(support_base)) n_distinct(.data[[block_var]]) else NA_integer_,
      n_segments = if (!is.null(segment_var) && segment_var %in% names(support_base)) n_distinct(.data[[segment_var]][!is.na(.data[[segment_var]])]) else NA_integer_,
      n_pins = if (!is.null(pin_var) && pin_var %in% names(support_base)) n_distinct(.data[[pin_var]]) else NA_integer_,
      .groups = "drop"
    )
  if (!is.null(outcome_var) && outcome_var %in% names(support_base)) {
    event_support <- event_support %>%
      left_join(
        support_base %>%
          group_by(event_time = .data[[event_var]]) %>%
          summarise(
            total_outcome = sum(.data[[outcome_var]], na.rm = TRUE),
            n_positive_rows = sum(.data[[outcome_var]] > 0, na.rm = TRUE),
            .groups = "drop"
          ),
        by = "event_time",
        relationship = "one-to-one"
      )
  }

  cell_event_support <- cell_support %>%
    group_by(event_time) %>%
    summarise(
      n_fe_group_time_cells = n(),
      n_identifying_fe_group_time_cells = sum(has_identifying_cell, na.rm = TRUE),
      n_identifying_fe_groups = n_distinct(fe_group[has_identifying_cell]),
      .groups = "drop"
    )

  event_support %>%
    left_join(cell_event_support, by = "event_time", relationship = "one-to-one") %>%
    mutate(
      n_fe_group_time_cells = replace_na(n_fe_group_time_cells, 0L),
      n_identifying_fe_group_time_cells = replace_na(n_identifying_fe_group_time_cells, 0L),
      n_identifying_fe_groups = replace_na(n_identifying_fe_groups, 0L),
      has_treated_and_control = n_treated > 0 & n_control > 0,
      has_identifying_support = n_identifying_fe_group_time_cells > 0
    ) %>%
    arrange(event_time)
}

build_event_study_plot_data <- function(model, support_by_event_time, min_period, max_period, group_label, display_mode = c("multiply100", "exp_minus_one")) {
  display_mode <- match.arg(display_mode)

  iplot_data <- tryCatch(iplot(model, .plot = FALSE)[[1]], error = function(e) NULL)
  if (is.null(iplot_data) || nrow(iplot_data) == 0) {
    return(NULL)
  }

  supported_periods <- support_by_event_time %>%
    filter(has_identifying_support) %>%
    pull(event_time)

  out <- iplot_data %>%
    as_tibble() %>%
    transmute(
      event_time = as.integer(x),
      estimate,
      ci_low,
      ci_high,
      std_error = if_else(is_ref, 0, (ci_high - estimate) / qnorm(0.975)),
      estimate_name = estimate_names,
      estimate_name_raw = estimate_names_raw,
      is_reference = is_ref,
      group = group_label
    ) %>%
    filter(event_time >= min_period, event_time <= max_period) %>%
    filter(is_reference | event_time %in% supported_periods) %>%
    left_join(support_by_event_time, by = "event_time")

  if (display_mode == "exp_minus_one") {
    out <- out %>%
      mutate(
        estimate_display = 100 * (exp(estimate) - 1),
        ci_low_display = 100 * (exp(ci_low) - 1),
        ci_high_display = 100 * (exp(ci_high) - 1),
        display_unit = "%"
      )
  } else {
    out <- out %>%
      mutate(
        estimate_display = 100 * estimate,
        ci_low_display = 100 * ci_low,
        ci_high_display = 100 * ci_high,
        display_unit = "%"
      )
  }

  out
}

solid_event_study_band_fill <- function(color, alpha, background = "#FFFFFF") {
  color_rgb <- grDevices::col2rgb(color) / 255
  background_rgb <- grDevices::col2rgb(background) / 255
  if (ncol(background_rgb) == 1L && ncol(color_rgb) > 1L) {
    background_rgb <- background_rgb[, rep(1L, ncol(color_rgb)), drop = FALSE]
  }
  blended_rgb <- alpha * color_rgb + (1 - alpha) * background_rgb
  fill <- grDevices::rgb(blended_rgb[1, ], blended_rgb[2, ], blended_rgb[3, ])
  names(fill) <- names(color)
  fill
}

compute_event_study_pretrend <- function(model, plot_data, group_label) {
  lead_terms <- plot_data %>%
    filter(event_time <= -2, !is_reference) %>%
    pull(estimate_name_raw)

  if (length(lead_terms) == 0) {
    return(tibble(
      group = group_label,
      n_leads = 0L,
      min_lead = NA_integer_,
      max_lead = NA_integer_,
      wald_stat = NA_real_,
      p_value = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_
    ))
  }

  joint_test <- tryCatch(wald(model, lead_terms), error = function(e) NULL)
  tibble(
    group = group_label,
    n_leads = length(lead_terms),
    min_lead = min(plot_data$event_time[plot_data$event_time <= -2 & !plot_data$is_reference]),
    max_lead = max(plot_data$event_time[plot_data$event_time <= -2 & !plot_data$is_reference]),
    wald_stat = if (is.null(joint_test)) NA_real_ else joint_test$stat,
    p_value = if (is.null(joint_test)) NA_real_ else joint_test$p,
    df1 = if (is.null(joint_test)) NA_real_ else joint_test$df1,
    df2 = if (is.null(joint_test)) NA_real_ else joint_test$df2
  )
}

make_event_study_single_series_plot <- function(plot_data, plot_title, x_label, y_label, display_suffix, line_color = "#009E73") {
  axis_breaks <- sort(unique(plot_data$event_time))
  axis_labels <- as.character(axis_breaks)

  if ("event_time_label" %in% names(plot_data)) {
    label_lookup <- plot_data %>%
      distinct(event_time, event_time_label) %>%
      filter(!is.na(event_time_label))

    matched_labels <- label_lookup$event_time_label[match(axis_breaks, label_lookup$event_time)]
    axis_labels <- ifelse(is.na(matched_labels), axis_labels, matched_labels)
  }

  ggplot(plot_data, aes(x = event_time, y = estimate_display)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_low_display, ymax = ci_high_display), fill = solid_event_study_band_fill(line_color, 0.2), color = NA) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_line(color = line_color, linewidth = 1) +
    geom_point(color = line_color, size = 2.5) +
    scale_x_continuous(breaks = axis_breaks, labels = axis_labels) +
    scale_y_continuous(labels = function(x) paste0(x, display_suffix)) +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      axis.line = element_line(color = "gray40", linewidth = 0.3),
      axis.ticks = element_line(color = "gray40", linewidth = 0.3),
      axis.title = element_text(size = 10, color = "gray20"),
      axis.text = element_text(size = 9, color = "gray30"),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )
}

make_event_study_directional_plots <- function(plot_data, plot_title, x_label, y_label, display_suffix, legend_position = "bottom") {
  axis_breaks <- sort(unique(plot_data$event_time))
  axis_labels <- as.character(axis_breaks)

  if ("event_time_label" %in% names(plot_data)) {
    label_lookup <- plot_data %>%
      distinct(event_time, event_time_label) %>%
      filter(!is.na(event_time_label))

    matched_labels <- label_lookup$event_time_label[match(axis_breaks, label_lookup$event_time)]
    axis_labels <- ifelse(is.na(matched_labels), axis_labels, matched_labels)
  }

  color_values <- c(
    "Moved to Stricter" = "#c23616",
    "Moved to More Lenient" = "#7f8fa6"
  )
  band_fill_values <- solid_event_study_band_fill(color_values, 0.15)

  facet_plot <- ggplot(plot_data, aes(x = event_time, y = estimate_display, color = group, fill = group)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_ribbon(
      data = plot_data %>% filter(group == "Moved to Stricter"),
      aes(x = event_time, ymin = ci_low_display, ymax = ci_high_display),
      fill = band_fill_values["Moved to Stricter"],
      color = NA,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = plot_data %>% filter(group == "Moved to More Lenient"),
      aes(x = event_time, ymin = ci_low_display, ymax = ci_high_display),
      fill = band_fill_values["Moved to More Lenient"],
      color = NA,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = axis_breaks, labels = axis_labels) +
    scale_y_continuous(labels = function(x) paste0(x, display_suffix)) +
    facet_wrap(~group, ncol = 1) +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      axis.line = element_line(color = "gray40", linewidth = 0.3),
      axis.ticks = element_line(color = "gray40", linewidth = 0.3),
      axis.title = element_text(size = 10, color = "gray20"),
      axis.text = element_text(size = 9, color = "gray30"),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )

  combined_plot <- ggplot(plot_data, aes(x = event_time, y = estimate_display, color = group, fill = group)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_ribbon(
      data = plot_data %>% filter(group == "Moved to Stricter"),
      aes(x = event_time, ymin = ci_low_display, ymax = ci_high_display),
      fill = band_fill_values["Moved to Stricter"],
      color = NA,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = plot_data %>% filter(group == "Moved to More Lenient"),
      aes(x = event_time, ymin = ci_low_display, ymax = ci_high_display),
      fill = band_fill_values["Moved to More Lenient"],
      color = NA,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = axis_breaks, labels = axis_labels) +
    scale_y_continuous(labels = function(x) paste0(x, display_suffix)) +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      axis.line = element_line(color = "gray40", linewidth = 0.3),
      axis.ticks = element_line(color = "gray40", linewidth = 0.3),
      axis.title = element_text(size = 10, color = "gray20"),
      axis.text = element_text(size = 9, color = "gray30"),
      legend.position = legend_position,
      legend.direction = "horizontal",
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )

  list(facet = facet_plot, combined = combined_plot)
}
