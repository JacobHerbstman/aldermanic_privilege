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
  ggplot(plot_data, aes(x = event_time, y = estimate_display)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_display, ymax = ci_high_display), fill = line_color, alpha = 0.2, color = NA) +
    geom_line(color = line_color, linewidth = 1) +
    geom_point(color = line_color, size = 2.5) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
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
  color_values <- c(
    "Moved to Stricter" = "#c23616",
    "Moved to More Lenient" = "#7f8fa6"
  )

  facet_plot <- ggplot(plot_data, aes(x = event_time, y = estimate_display, color = group, fill = group)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_display, ymax = ci_high_display), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
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
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_display, ymax = ci_high_display), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
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
