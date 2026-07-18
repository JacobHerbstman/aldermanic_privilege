# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

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
    running_distance = if_else(
      side == 1L,
      abs(dist_to_boundary_m),
      -abs(dist_to_boundary_m)
    )
  )
model_results <- read_csv(
  "../input/density_score_sensitivity_models.csv",
  show_col_types = FALSE
)

if (anyNA(parcels$score_own) || anyNA(parcels$score_neighbor)) {
  stop("The no-income score is missing for a density-sample alderman.", call. = FALSE)
}

distance_display <- distance_display_config("ft")
plot_bandwidth_m <- 152.4
bins_per_side <- 5L
panel_specs <- tribble(
  ~outcome, ~construction_sample, ~title, ~filename,
  "density_far", "all", "All Construction: FAR", "density_drop_income_rd_all_far.png",
  "density_far", "multifamily", "Multifamily: FAR", "density_drop_income_rd_multifamily_far.png",
  "density_dupac", "all", "All Construction: DUPAC", "density_drop_income_rd_all_dupac.png",
  "density_dupac", "multifamily", "Multifamily: DUPAC", "density_drop_income_rd_multifamily_dupac.png"
)

panels <- vector("list", nrow(panel_specs))
partial_panels <- vector("list", nrow(panel_specs))
descriptive_panels <- vector("list", nrow(panel_specs))
descriptive_results <- vector("list", nrow(panel_specs))
for (panel_i in seq_len(nrow(panel_specs))) {
  outcome_i <- panel_specs$outcome[panel_i]
  sample_i <- panel_specs$construction_sample[panel_i]

  plot_data <- if (sample_i == "all") {
    parcels %>% filter(unitscount > 0)
  } else {
    parcels %>% filter(unitscount > 1)
  }
  plot_data <- plot_data %>%
    filter(is.finite(.data[[outcome_i]]), .data[[outcome_i]] > 0) %>%
    mutate(outcome_value = log(.data[[outcome_i]]))

  cutoff_model <- feols(
    as.formula(paste0(
      "outcome_value ~ side * running_distance + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = plot_data,
    cluster = ~ward_pair,
    notes = FALSE
  )
  removed_rows <- cutoff_model$obs_selection$obsRemoved
  kept_rows <- if (is.null(removed_rows)) {
    seq_len(nrow(plot_data))
  } else {
    setdiff(seq_len(nrow(plot_data)), abs(as.integer(removed_rows)))
  }
  plot_data <- plot_data[kept_rows, , drop = FALSE]

  cutoff_table <- coeftable(cutoff_model)
  cutoff_estimate <- unname(cutoff_table["side", "Estimate"])
  cutoff_se <- unname(cutoff_table["side", "Std. Error"])
  cutoff_p <- unname(cutoff_table["side", "Pr(>|t|)"])

  expected_result <- model_results %>%
    filter(
      spec_id == "controls_drop_income",
      construction_sample == sample_i,
      outcome == outcome_i,
      treatment == "binary"
    )
  if (
    nrow(expected_result) != 1L ||
    abs(cutoff_estimate - expected_result$estimate) > 1e-8 ||
    abs(cutoff_se - expected_result$se) > 1e-8
  ) {
    stop(sprintf("No-income plot/model mismatch for %s %s.", sample_i, outcome_i), call. = FALSE)
  }

  rd_terms <- c("side", "running_distance", "side:running_distance")
  if (!all(rd_terms %in% names(coef(cutoff_model)))) {
    stop("The density RD model is missing a side or distance coefficient.", call. = FALSE)
  }
  rd_coefficients <- coef(cutoff_model)[rd_terms]
  plot_data <- plot_data %>%
    mutate(
      adjusted_outcome = as.numeric(resid(cutoff_model)) +
        rd_coefficients[["side"]] * side +
        rd_coefficients[["running_distance"]] * running_distance +
        rd_coefficients[["side:running_distance"]] * side * running_distance,
      partial_outcome = as.numeric(resid(cutoff_model)) +
        rd_coefficients[["side"]] * side
    )

  cutoff_stars <- case_when(
    cutoff_p <= 0.01 ~ "***",
    cutoff_p <= 0.05 ~ "**",
    cutoff_p <= 0.10 ~ "*",
    TRUE ~ ""
  )
  breaks_m <- seq(
    -plot_bandwidth_m,
    plot_bandwidth_m,
    length.out = 2L * bins_per_side + 1L
  )
  bin_width_m <- plot_bandwidth_m / bins_per_side
  bins <- plot_data %>%
    mutate(
      bin = pmin(
        findInterval(
          running_distance,
          breaks_m,
          rightmost.closed = TRUE,
          all.inside = TRUE
        ),
        length(breaks_m) - 1L
      ),
      bin_center_m = breaks_m[bin] + bin_width_m / 2
    ) %>%
    group_by(bin, bin_center_m, side) %>%
    summarise(mean_y = mean(adjusted_outcome), .groups = "drop") %>%
    mutate(bin_center = bin_center_m * distance_display$scale)

  partial_bins <- plot_data %>%
    mutate(
      bin = pmin(
        findInterval(
          running_distance,
          breaks_m,
          rightmost.closed = TRUE,
          all.inside = TRUE
        ),
        length(breaks_m) - 1L
      ),
      bin_center_m = breaks_m[bin] + bin_width_m / 2
    ) %>%
    group_by(bin, bin_center_m, side) %>%
    summarise(mean_y = mean(partial_outcome), .groups = "drop") %>%
    mutate(bin_center = bin_center_m * distance_display$scale)

  residual_model <- feols(
    as.formula(paste0(
      "outcome_value ~ ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = plot_data,
    notes = FALSE
  )
  if (nobs(residual_model) != nrow(plot_data)) {
    stop("The descriptive and adjusted density samples do not align.", call. = FALSE)
  }
  descriptive_data <- plot_data %>%
    mutate(descriptive_outcome = as.numeric(resid(residual_model)))
  descriptive_model <- feols(
    descriptive_outcome ~ side * running_distance,
    data = descriptive_data,
    cluster = ~ward_pair,
    notes = FALSE
  )
  descriptive_table <- coeftable(descriptive_model)
  descriptive_jump <- unname(descriptive_table["side", "Estimate"])
  descriptive_se <- unname(descriptive_table["side", "Std. Error"])
  descriptive_p <- unname(descriptive_table["side", "Pr(>|t|)"])
  descriptive_results[[panel_i]] <- tibble(
    outcome = outcome_i,
    construction_sample = sample_i,
    estimate = descriptive_jump,
    se = descriptive_se,
    p_value = descriptive_p
  )

  descriptive_bins <- descriptive_data %>%
    mutate(
      bin = pmin(
        findInterval(
          running_distance,
          breaks_m,
          rightmost.closed = TRUE,
          all.inside = TRUE
        ),
        length(breaks_m) - 1L
      ),
      bin_center_m = breaks_m[bin] + bin_width_m / 2
    ) %>%
    group_by(bin, bin_center_m, side) %>%
    summarise(mean_y = mean(descriptive_outcome), .groups = "drop") %>%
    mutate(bin_center = bin_center_m * distance_display$scale)

  line_data <- tibble(
    running_distance = c(
      seq(-plot_bandwidth_m, 0, length.out = 200),
      seq(0, plot_bandwidth_m, length.out = 200)[-1]
    )
  ) %>%
    mutate(side = as.integer(running_distance > 0))
  design_matrix <- cbind(
    side = line_data$side,
    running_distance = line_data$running_distance,
    `side:running_distance` = line_data$side * line_data$running_distance
  )
  plotted_jump <- unname(c(1, 0, 0) %*% rd_coefficients)
  if (abs(plotted_jump - cutoff_estimate) > 1e-10) {
    stop("The plotted density discontinuity does not match the RD estimate.", call. = FALSE)
  }
  rd_vcov <- vcov(cutoff_model)[rd_terms, rd_terms, drop = FALSE]
  critical_value <- qt(
    0.975,
    df = max(n_distinct(plot_data$ward_pair) - 1L, 1L)
  )
  line_data <- line_data %>%
    mutate(
      fit = as.numeric(design_matrix %*% rd_coefficients),
      fit_se = sqrt(pmax(
        rowSums((design_matrix %*% rd_vcov) * design_matrix),
        0
      )),
      ci_low = fit - critical_value * fit_se,
      ci_high = fit + critical_value * fit_se,
      running_distance_display = running_distance * distance_display$scale
    )

  descriptive_terms <- names(coef(descriptive_model))
  descriptive_design <- model.matrix(~side * running_distance, data = line_data)
  descriptive_design <- descriptive_design[, descriptive_terms, drop = FALSE]
  descriptive_vcov <- vcov(descriptive_model)
  descriptive_line_data <- line_data %>%
    mutate(
      fit = as.numeric(descriptive_design %*% coef(descriptive_model)),
      fit_se = sqrt(pmax(
        rowSums((descriptive_design %*% descriptive_vcov) * descriptive_design),
        0
      )),
      ci_low = fit - critical_value * fit_se,
      ci_high = fit + critical_value * fit_se
    )

  y_min <- min(c(bins$mean_y, line_data$ci_low), na.rm = TRUE)
  y_max <- max(c(bins$mean_y, line_data$ci_high), na.rm = TRUE)
  y_padding <- max(0.15 * (y_max - y_min), 0.05)

  panels[[panel_i]] <- ggplot() +
    geom_ribbon(
      data = line_data,
      aes(
        x = running_distance_display,
        ymin = ci_low,
        ymax = ci_high,
        fill = factor(side)
      ),
      alpha = 0.16,
      color = NA
    ) +
    geom_line(
      data = line_data,
      aes(x = running_distance_display, y = fit, color = factor(side)),
      linewidth = 0.8
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray55", linewidth = 0.35) +
    geom_point(
      data = bins,
      aes(x = bin_center, y = mean_y, fill = factor(side)),
      shape = 21,
      color = "white",
      stroke = 0.35,
      size = 1.85
    ) +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_x_continuous(limits = c(-500, 500), breaks = seq(-500, 500, by = 250)) +
    coord_cartesian(ylim = c(y_min - y_padding, y_max + y_padding)) +
    labs(
      title = panel_specs$title[panel_i],
      subtitle = sprintf("Jump = %.3f%s (SE %.3f)", cutoff_estimate, cutoff_stars, cutoff_se),
      x = "Distance to ward boundary (feet; positive = more stringent side)",
      y = paste0(
        "Covariate-adjusted Log(",
        if_else(outcome_i == "density_far", "FAR", "DUPAC"),
        ")"
      )
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      panel.grid.minor = element_blank()
    )

  outcome_center <- 0
  partial_panels[[panel_i]] <- ggplot() +
    annotate(
      "segment",
      x = -500,
      xend = 0,
      y = outcome_center,
      yend = outcome_center,
      color = "#1f77b4",
      linewidth = 0.9
    ) +
    annotate(
      "segment",
      x = 0,
      xend = 500,
      y = outcome_center + cutoff_estimate,
      yend = outcome_center + cutoff_estimate,
      color = "#d62728",
      linewidth = 0.9
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.35) +
    geom_point(
      data = partial_bins,
      aes(x = bin_center, y = mean_y, fill = factor(side)),
      shape = 21,
      color = "white",
      stroke = 0.35,
      size = 2.1
    ) +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_x_continuous(limits = c(-500, 500), breaks = seq(-500, 500, by = 250)) +
    labs(
      title = panel_specs$title[panel_i],
      subtitle = sprintf("Jump = %.3f%s (SE %.3f)", cutoff_estimate, cutoff_stars, cutoff_se),
      x = "Distance to ward boundary (feet; positive = more stringent side)",
      y = paste0(
        "Adjusted Log(",
        if_else(outcome_i == "density_far", "FAR", "DUPAC"),
        "), centered"
      )
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      panel.grid.minor = element_blank()
    )

  descriptive_panels[[panel_i]] <- ggplot() +
    geom_ribbon(
      data = descriptive_line_data,
      aes(
        x = running_distance_display,
        ymin = ci_low,
        ymax = ci_high,
        fill = factor(side)
      ),
      alpha = 0.16,
      color = NA
    ) +
    geom_line(
      data = descriptive_line_data,
      aes(x = running_distance_display, y = fit, color = factor(side)),
      linewidth = 0.8
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray55", linewidth = 0.35) +
    geom_point(
      data = descriptive_bins,
      aes(x = bin_center, y = mean_y, fill = factor(side)),
      shape = 21,
      color = "white",
      stroke = 0.35,
      size = 1.85
    ) +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_x_continuous(limits = c(-500, 500), breaks = seq(-500, 500, by = 250)) +
    labs(
      title = panel_specs$title[panel_i],
      x = "Distance to ward boundary (feet; positive = more stringent side)",
      y = paste0(
        "Residualized Log(",
        if_else(outcome_i == "density_far", "FAR", "DUPAC"),
        ")"
      )
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      panel.grid.minor = element_blank()
    )

  ggsave(
    file.path("../output", panel_specs$filename[panel_i]),
    panels[[panel_i]],
    width = 6.5,
    height = 4.6,
    dpi = 220,
    bg = "white"
  )
}

combined_plot <- (panels[[1]] | panels[[2]]) / (panels[[3]] | panels[[4]]) +
  plot_annotation(
    title = "Local-Linear Spatial RD: Score Estimated Without Income or Bachelor's Share (500 ft)"
  ) &
  theme(plot.title = element_text(face = "bold", size = 13))

ggsave(
  "../output/density_drop_income_rd_4panel.png",
  combined_plot,
  width = 12,
  height = 8.6,
  dpi = 220,
  bg = "white"
)
ggsave(
  "../output/density_drop_income_rd_4panel.pdf",
  combined_plot,
  width = 12,
  height = 8.6,
  bg = "white"
)

partial_plot <- (partial_panels[[1]] | partial_panels[[2]]) /
  (partial_panels[[3]] | partial_panels[[4]]) +
  plot_annotation(
    title = "Adjusted Density at Ward Boundaries (500 ft)"
  ) &
  theme(plot.title = element_text(face = "bold", size = 13))

ggsave(
  "../output/density_drop_income_rd_partial_4panel.png",
  partial_plot,
  width = 12,
  height = 8.6,
  dpi = 220,
  bg = "white"
)
ggsave(
  "../output/density_drop_income_rd_partial_4panel.pdf",
  partial_plot,
  width = 12,
  height = 8.6,
  bg = "white"
)

descriptive_plot <- (descriptive_panels[[1]] | descriptive_panels[[2]]) /
  (descriptive_panels[[3]] | descriptive_panels[[4]])

ggsave(
  "../output/density_drop_income_rd_original_style_4panel.png",
  descriptive_plot,
  width = 12,
  height = 8.6,
  dpi = 220,
  bg = "white"
)
ggsave(
  "../output/density_drop_income_rd_original_style_4panel.pdf",
  descriptive_plot,
  width = 12,
  height = 8.6,
  bg = "white"
)

format_tex_estimate <- function(estimate, se, p_value) {
  stars <- case_when(
    p_value <= 0.01 ~ "***",
    p_value <= 0.05 ~ "**",
    p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
  estimate_text <- sprintf("%.3f", estimate)
  estimate_text <- sub("^-0", "-", estimate_text)
  estimate_text <- sub("^0", "", estimate_text)
  se_text <- sub("^0", "", sprintf("%.3f", se))
  star_text <- if (stars == "") "" else paste0("^{", stars, "}")
  paste0("$", estimate_text, star_text, "$ (", se_text, ")")
}

descriptive_results <- bind_rows(descriptive_results)
writeLines(
  c(
    paste0(
      "The plotted differences (standard errors in parentheses) are ",
      format_tex_estimate(
        descriptive_results$estimate[1],
        descriptive_results$se[1],
        descriptive_results$p_value[1]
      ),
      " for all-construction FAR, ",
      format_tex_estimate(
        descriptive_results$estimate[2],
        descriptive_results$se[2],
        descriptive_results$p_value[2]
      ),
      " for multifamily FAR, ",
      format_tex_estimate(
        descriptive_results$estimate[3],
        descriptive_results$se[3],
        descriptive_results$p_value[3]
      ),
      " for all-construction DUPAC, and ",
      format_tex_estimate(
        descriptive_results$estimate[4],
        descriptive_results$se[4],
        descriptive_results$p_value[4]
      ),
      " for multifamily DUPAC."
    ),
    "Table~\\ref{tab:density_main_table} reports the corresponding full-specification continuous and binary estimates."
  ),
  "../output/density_drop_income_rd_original_style_estimates.tex"
)

table_columns <- tribble(
  ~column, ~construction_sample, ~outcome,
  1L, "all", "density_far",
  2L, "all", "density_dupac",
  3L, "multifamily", "density_far",
  4L, "multifamily", "density_dupac"
)
table_results <- model_results %>%
  filter(
    spec_id == "controls_drop_income",
    treatment %in% c("continuous", "binary")
  ) %>%
  inner_join(
    table_columns,
    by = c("construction_sample", "outcome"),
    relationship = "many-to-one"
  ) %>%
  arrange(treatment, column)

if (nrow(table_results) != 8L) {
  stop("Expected eight continuous and binary density estimates for Table 2.", call. = FALSE)
}

format_table_estimate <- function(estimate, p_value) {
  stars <- case_when(
    p_value <= 0.01 ~ "^{***}",
    p_value <= 0.05 ~ "^{**}",
    p_value <= 0.10 ~ "^{*}",
    TRUE ~ ""
  )
  star_text <- if (stars == "") "" else paste0("$", stars, "$")
  paste0(sprintf("%.3f", estimate), star_text)
}

continuous_results <- table_results %>% filter(treatment == "continuous")
binary_results <- table_results %>% filter(treatment == "binary")
dependent_variable_means <- map2_dbl(
  table_columns$construction_sample,
  table_columns$outcome,
  function(sample_i, outcome_i) {
    sample_data <- if (sample_i == "all") {
      parcels %>% filter(unitscount > 0)
    } else {
      parcels %>% filter(unitscount > 1)
    }
    sample_data %>%
      filter(is.finite(.data[[outcome_i]]), .data[[outcome_i]] > 0) %>%
      summarise(mean = mean(.data[[outcome_i]])) %>%
      pull(mean)
  }
)

writeLines(
  c(
    "\\begingroup",
    "\\centering",
    "\\begin{tabular}{lcccc}",
    "   \\toprule",
    "                    & \\multicolumn{2}{c}{All Construction} & \\multicolumn{2}{c}{Multifamily} \\\\",
    "                    & ln(FAR)       & ln(DUPAC)      & ln(FAR)       & ln(DUPAC) \\\\",
    "                    & (1)           & (2)            & (3)           & (4) \\\\",
    "   \\midrule",
    "   \\multicolumn{5}{l}{\\textit{Panel A: Continuous Stringency Specification}} \\\\",
    paste0(
      "   Stringency Index (1 SD) & ",
      paste(
        map2_chr(
          continuous_results$estimate,
          continuous_results$p_value,
          format_table_estimate
        ),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0("                    & (", paste(sprintf("%.3f", continuous_results$se), collapse = ") & ("), ") \\\\"),
    paste0("   N                & ", paste(format(continuous_results$n, big.mark = ",", scientific = FALSE, trim = TRUE), collapse = " & "), " \\\\"),
    paste0("   Dep. Var. Mean   & ", paste(sprintf("%.2f", dependent_variable_means), collapse = " & "), " \\\\"),
    paste0("   Ward Pairs       & ", paste(continuous_results$ward_pairs, collapse = " & "), " \\\\"),
    "   \\addlinespace",
    "   \\multicolumn{5}{l}{\\textit{Panel B: Binary Boundary Specification}} \\\\",
    paste0(
      "   More-Stringent Side & ",
      paste(
        map2_chr(binary_results$estimate, binary_results$p_value, format_table_estimate),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0("                    & (", paste(sprintf("%.3f", binary_results$se), collapse = ") & ("), ") \\\\"),
    paste0("   N                & ", paste(format(binary_results$n, big.mark = ",", scientific = FALSE, trim = TRUE), collapse = " & "), " \\\\"),
    paste0("   Dep. Var. Mean   & ", paste(sprintf("%.2f", dependent_variable_means), collapse = " & "), " \\\\"),
    paste0("   Ward Pairs       & ", paste(binary_results$ward_pairs, collapse = " & "), " \\\\"),
    "   \\midrule",
    "   Zoning Group FE  & $\\checkmark$  & $\\checkmark$   & $\\checkmark$  & $\\checkmark$ \\\\",
    "   Segment FE       & $\\checkmark$  & $\\checkmark$   & $\\checkmark$  & $\\checkmark$ \\\\",
    "   Year FE          & $\\checkmark$  & $\\checkmark$   & $\\checkmark$  & $\\checkmark$ \\\\",
    "   \\bottomrule",
    "\\end{tabular}",
    "\\par\\endgroup"
  ),
  "../output/drop_income_density_main_table.tex"
)
