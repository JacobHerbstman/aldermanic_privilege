# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

bandwidth_m <- 152.4
bins_per_side <- 5L
demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

scores <- read_csv("../input/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(cutoff == 2022L, spec_id == "controls_drop_income") %>%
  select(alderman, score)

parcels <- read_csv(
  "../input/density_score_sensitivity_sample.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess()),
  guess_max = Inf
) %>%
  left_join(
    scores %>% rename(alderman_own = alderman, score_own = score),
    by = "alderman_own",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_neighbor = alderman, score_neighbor = score),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) %>%
  mutate(
    side = as.integer(score_own > score_neighbor),
    running_distance = if_else(side == 1L, abs(dist_to_boundary_m), -abs(dist_to_boundary_m))
  ) %>%
  filter(unitscount > 1, abs(running_distance) <= bandwidth_m)

if (anyNA(parcels$score_own) || anyNA(parcels$score_neighbor)) {
  stop("The no-income score is missing for a density-sample alderman.", call. = FALSE)
}

expected_results <- read_csv(
  "../input/density_score_sensitivity_models.csv",
  show_col_types = FALSE
) %>%
  filter(
    spec_id == "controls_drop_income",
    construction_sample == "multifamily",
    outcome %in% c("density_far", "density_dupac"),
    treatment == "binary"
  )

breaks_m <- seq(-bandwidth_m, bandwidth_m, length.out = 2L * bins_per_side + 1L)
bin_width_m <- bandwidth_m / bins_per_side
colors <- c(`0` = "#2166AC", `1` = "#B2182B")

significance_stars <- function(p_value) {
  case_when(
    p_value <= 0.01 ~ "***",
    p_value <= 0.05 ~ "**",
    p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
}

coefficient_label <- function(estimate, standard_error, p_value) {
  sprintf(
    "%.3f%s (SE %.3f)",
    estimate,
    significance_stars(p_value),
    standard_error
  )
}

add_bins <- function(data, outcome_column) {
  data %>%
    mutate(
      bin_id = pmin(
        findInterval(
          running_distance,
          breaks_m,
          rightmost.closed = TRUE,
          all.inside = TRUE
        ),
        length(breaks_m) - 1L
      ),
      bin_center_m = breaks_m[bin_id] + bin_width_m / 2,
      bin_center_ft = bin_center_m * 3.28084
    ) %>%
    group_by(bin_id, bin_center_ft, side) %>%
    summarise(mean_y = mean(.data[[outcome_column]]), .groups = "drop")
}

linear_prediction_data <- function(model, plot_data) {
  prediction_data <- tibble(
    running_distance = c(
      seq(-bandwidth_m, 0, length.out = 200),
      seq(0, bandwidth_m, length.out = 200)[-1]
    )
  ) %>%
    mutate(side = as.integer(running_distance > 0))

  coefficient_names <- names(coef(model))
  design_matrix <- model.matrix(~side * running_distance, data = prediction_data)
  design_matrix <- design_matrix[, coefficient_names, drop = FALSE]
  model_vcov <- vcov(model)[coefficient_names, coefficient_names, drop = FALSE]
  critical_value <- qt(0.975, df = max(n_distinct(plot_data$ward_pair) - 1L, 1L))

  prediction_data %>%
    mutate(
      fit = as.numeric(design_matrix %*% coef(model)),
      fit_se = sqrt(pmax(rowSums((design_matrix %*% model_vcov) * design_matrix), 0)),
      ci_low = fit - critical_value * fit_se,
      ci_high = fit + critical_value * fit_se,
      running_distance_ft = running_distance * 3.28084
    )
}

adjusted_prediction_data <- function(model, plot_data) {
  prediction_data <- tibble(
    running_distance = c(
      seq(-bandwidth_m, 0, length.out = 200),
      seq(0, bandwidth_m, length.out = 200)[-1]
    )
  ) %>%
    mutate(side = as.integer(running_distance > 0))

  rd_terms <- c("side", "running_distance", "side:running_distance")
  rd_coefficients <- coef(model)[rd_terms]
  design_matrix <- cbind(
    side = prediction_data$side,
    running_distance = prediction_data$running_distance,
    `side:running_distance` = prediction_data$side * prediction_data$running_distance
  )
  model_vcov <- vcov(model)[rd_terms, rd_terms, drop = FALSE]
  critical_value <- qt(0.975, df = max(n_distinct(plot_data$ward_pair) - 1L, 1L))

  prediction_data %>%
    mutate(
      fit = as.numeric(design_matrix %*% rd_coefficients),
      fit_se = sqrt(pmax(rowSums((design_matrix %*% model_vcov) * design_matrix), 0)),
      ci_low = fit - critical_value * fit_se,
      ci_high = fit + critical_value * fit_se,
      running_distance_ft = running_distance * 3.28084
    )
}

line_plot <- function(bins, lines, title, subtitle, y_label) {
  ggplot() +
    geom_ribbon(
      data = lines,
      aes(
        x = running_distance_ft,
        ymin = ci_low,
        ymax = ci_high,
        fill = factor(side),
        group = side
      ),
      alpha = 0.10,
      color = NA
    ) +
    geom_line(
      data = lines,
      aes(x = running_distance_ft, y = fit, color = factor(side), group = side),
      linewidth = 0.8,
      alpha = 0.72
    ) +
    geom_point(
      data = bins,
      aes(x = bin_center_ft, y = mean_y, color = factor(side)),
      size = 2.5
    ) +
    geom_vline(xintercept = 0, linewidth = 0.45, linetype = "dashed", color = "grey35") +
    scale_color_manual(values = colors, guide = "none") +
    scale_fill_manual(values = colors, guide = "none") +
    scale_x_continuous(breaks = c(-500, -250, 0, 250, 500)) +
    coord_cartesian(xlim = c(-500, 500)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Distance to ward boundary (feet)",
      y = y_label
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9.5, lineheight = 1.05),
      axis.title = element_text(size = 10)
    )
}

method_plots <- list(raw = list(), adjusted = list(), residualized = list(), bins = list())
results <- list()

panel_specs <- tribble(
  ~outcome, ~title,
  "density_far", "Multifamily: Log(FAR)",
  "density_dupac", "Multifamily: Log(DUPAC)"
)

for (panel_i in seq_len(nrow(panel_specs))) {
  outcome_i <- panel_specs$outcome[panel_i]
  title_i <- panel_specs$title[panel_i]

  plot_data <- parcels %>%
    filter(is.finite(.data[[outcome_i]]), .data[[outcome_i]] > 0) %>%
    mutate(outcome_value = log(.data[[outcome_i]]))

  full_model <- feols(
    as.formula(paste0(
      "outcome_value ~ side * running_distance + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = plot_data,
    cluster = ~ward_pair,
    notes = FALSE
  )

  removed_rows <- full_model$obs_selection$obsRemoved
  kept_rows <- if (is.null(removed_rows)) {
    seq_len(nrow(plot_data))
  } else {
    setdiff(seq_len(nrow(plot_data)), abs(as.integer(removed_rows)))
  }
  plot_data <- plot_data[kept_rows, , drop = FALSE]

  full_table <- coeftable(full_model)
  full_estimate <- unname(full_table["side", "Estimate"])
  full_se <- unname(full_table["side", "Std. Error"])
  full_p <- unname(full_table["side", "Pr(>|t|)"])
  full_label <- coefficient_label(full_estimate, full_se, full_p)

  expected_result <- expected_results %>% filter(outcome == outcome_i)
  if (
    nrow(expected_result) != 1L ||
    abs(full_estimate - expected_result$estimate) > 1e-8 ||
    abs(full_se - expected_result$se) > 1e-8
  ) {
    stop(sprintf("Preferred-model mismatch for %s.", outcome_i), call. = FALSE)
  }

  raw_model <- feols(
    outcome_value ~ side * running_distance,
    data = plot_data,
    cluster = ~ward_pair,
    notes = FALSE
  )
  raw_table <- coeftable(raw_model)
  raw_estimate <- unname(raw_table["side", "Estimate"])
  raw_se <- unname(raw_table["side", "Std. Error"])
  raw_p <- unname(raw_table["side", "Pr(>|t|)"])

  method_plots$raw[[panel_i]] <- line_plot(
    add_bins(plot_data, "outcome_value"),
    linear_prediction_data(raw_model, plot_data),
    title_i,
    paste0(
      "Plotted raw jump = ", coefficient_label(raw_estimate, raw_se, raw_p),
      "\nPreferred adjusted jump = ", full_label
    ),
    str_replace(title_i, "Multifamily: ", "")
  )

  rd_terms <- c("side", "running_distance", "side:running_distance")
  rd_coefficients <- coef(full_model)[rd_terms]
  adjusted_data <- plot_data %>%
    mutate(
      adjusted_outcome = as.numeric(resid(full_model)) +
        rd_coefficients[["side"]] * side +
        rd_coefficients[["running_distance"]] * running_distance +
        rd_coefficients[["side:running_distance"]] * side * running_distance
    )

  method_plots$adjusted[[panel_i]] <- line_plot(
    add_bins(adjusted_data, "adjusted_outcome"),
    adjusted_prediction_data(full_model, plot_data),
    title_i,
    paste0("Plotted and preferred adjusted jump = ", full_label),
    "Adjusted log density (centered)"
  )

  residual_model <- feols(
    as.formula(paste0(
      "outcome_value ~ ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = plot_data,
    notes = FALSE
  )
  residualized_data <- plot_data %>%
    mutate(residualized_outcome = as.numeric(resid(residual_model)))
  display_model <- feols(
    residualized_outcome ~ side * running_distance,
    data = residualized_data,
    cluster = ~ward_pair,
    notes = FALSE
  )
  display_table <- coeftable(display_model)
  display_estimate <- unname(display_table["side", "Estimate"])
  display_se <- unname(display_table["side", "Std. Error"])
  display_p <- unname(display_table["side", "Pr(>|t|)"])

  method_plots$residualized[[panel_i]] <- line_plot(
    add_bins(residualized_data, "residualized_outcome"),
    linear_prediction_data(display_model, residualized_data),
    title_i,
    paste0(
      "Plotted residualized jump = ",
      coefficient_label(display_estimate, display_se, display_p),
      "\nPreferred adjusted jump = ", full_label
    ),
    "Residualized log density"
  )

  binned_data <- plot_data %>%
    mutate(
      bin_id = pmin(
        findInterval(
          running_distance,
          breaks_m,
          rightmost.closed = TRUE,
          all.inside = TRUE
        ),
        length(breaks_m) - 1L
      ),
      bin_center_ft = (breaks_m[bin_id] + bin_width_m / 2) * 3.28084
    )
  bin_model <- feols(
    as.formula(paste0(
      "outcome_value ~ i(bin_id, ref = 5) + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = binned_data,
    cluster = ~ward_pair,
    notes = FALSE
  )
  bin_table <- coeftable(bin_model)
  bin_estimates <- tibble(
    bin_id = seq_len(2L * bins_per_side),
    estimate = 0,
    standard_error = 0,
    p_value = NA_real_
  )
  for (bin_i in setdiff(bin_estimates$bin_id, 5L)) {
    term_i <- paste0("bin_id::", bin_i)
    bin_estimates$estimate[bin_estimates$bin_id == bin_i] <- bin_table[term_i, "Estimate"]
    bin_estimates$standard_error[bin_estimates$bin_id == bin_i] <- bin_table[term_i, "Std. Error"]
    bin_estimates$p_value[bin_estimates$bin_id == bin_i] <- bin_table[term_i, "Pr(>|t|)"]
  }
  critical_value <- qt(0.975, df = max(n_distinct(binned_data$ward_pair) - 1L, 1L))
  bin_estimates <- bin_estimates %>%
    mutate(
      bin_center_ft = (breaks_m[bin_id] + bin_width_m / 2) * 3.28084,
      side = as.integer(bin_id > bins_per_side),
      ci_low = estimate - critical_value * standard_error,
      ci_high = estimate + critical_value * standard_error
    )

  nearest_estimate <- bin_estimates$estimate[bin_estimates$bin_id == 6L]
  nearest_se <- bin_estimates$standard_error[bin_estimates$bin_id == 6L]
  nearest_p <- bin_estimates$p_value[bin_estimates$bin_id == 6L]

  method_plots$bins[[panel_i]] <- ggplot(
    bin_estimates,
    aes(x = bin_center_ft, y = estimate, color = factor(side), group = side)
  ) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.4) +
    geom_vline(xintercept = 0, linewidth = 0.45, linetype = "dashed", color = "grey35") +
    geom_line(linewidth = 0.65, alpha = 0.55) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 12, linewidth = 0.55) +
    geom_point(size = 2.5) +
    scale_color_manual(values = colors, guide = "none") +
    scale_x_continuous(breaks = c(-500, -250, 0, 250, 500)) +
    coord_cartesian(xlim = c(-500, 500)) +
    labs(
      title = title_i,
      subtitle = paste0(
        "Adjusted +50ft versus -50ft bin = ",
        coefficient_label(nearest_estimate, nearest_se, nearest_p),
        "\nPreferred local-linear jump = ", full_label
      ),
      x = "Distance to ward boundary (feet)",
      y = "Adjusted bin coefficient (-50ft bin = 0)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9.5, lineheight = 1.05),
      axis.title = element_text(size = 10)
    )

  results[[panel_i]] <- tibble(
    outcome = outcome_i,
    method = c("raw_local_linear", "full_adjusted_local_linear", "residualized_local_linear", "adjusted_nearest_bins"),
    estimate = c(raw_estimate, full_estimate, display_estimate, nearest_estimate),
    standard_error = c(raw_se, full_se, display_se, nearest_se),
    p_value = c(raw_p, full_p, display_p, nearest_p),
    n = nrow(plot_data),
    ward_pairs = n_distinct(plot_data$ward_pair)
  )
}

method_titles <- c(
  raw = "1. Raw binned outcomes and simple local-linear fits",
  adjusted = "2. Full-model covariate-adjusted bins and lines",
  residualized = "3. Residualized outcomes with local-linear fits",
  bins = "4. HDFE-adjusted bin coefficients with clustered intervals"
)
method_filenames <- c(
  raw = "density_rd_method_1_raw",
  adjusted = "density_rd_method_2_adjusted",
  residualized = "density_rd_method_3_residualized",
  bins = "density_rd_method_4_hdfe_bins"
)

method_rows <- list()
for (method_i in names(method_plots)) {
  method_rows[[method_i]] <- wrap_plots(method_plots[[method_i]], nrow = 1) +
    plot_annotation(title = method_titles[[method_i]]) &
    theme(plot.title = element_text(face = "bold", size = 15))

  ggsave(
    paste0("../output/", method_filenames[[method_i]], ".png"),
    method_rows[[method_i]],
    width = 12,
    height = 5.2,
    dpi = 220,
    bg = "white"
  )
  ggsave(
    paste0("../output/", method_filenames[[method_i]], ".pdf"),
    method_rows[[method_i]],
    width = 12,
    height = 5.2,
    device = grDevices::pdf
  )
}

comparison_plot <- wrap_plots(method_rows, ncol = 1) +
  plot_annotation(
    title = "Four Ways to Display the Multifamily Density Boundary Comparison",
    subtitle = "Same 500ft sample, no-income stringency score, outcomes, controls, fixed effects, and clustering"
  ) &
  theme(plot.title = element_text(face = "bold", size = 16))

ggsave(
  "../output/density_rd_visualization_4methods.png",
  comparison_plot,
  width = 12,
  height = 21,
  dpi = 220,
  bg = "white"
)
ggsave(
  "../output/density_rd_visualization_4methods.pdf",
  comparison_plot,
  width = 12,
  height = 21,
  device = grDevices::pdf
)

bind_rows(results) %>%
  write_csv("../output/density_rd_visualization_4methods_results.csv")
