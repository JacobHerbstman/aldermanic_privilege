# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_no_zoning_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

bandwidth_m <- 152.4
bins_per_side <- 5L
placebo_shift_m <- 304.8
donut_distances_m <- c(7.62, 15.24)

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

raw <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(segment_id),
    segment_id != ""
  )

sample_data <- function(data, sample_name, outcome_name) {
  if (sample_name == "all") {
    data <- data %>% filter(unitscount > 0)
  } else {
    data <- data %>% filter(unitscount > 1)
  }

  data %>%
    filter(
      is.finite(.data[[outcome_name]]),
      .data[[outcome_name]] > 0
    )
}

model_row <- function(model, coefficient_name, family, specification, sample_name, outcome_name, detail) {
  coefficient_table <- coeftable(model)
  if (!coefficient_name %in% rownames(coefficient_table)) {
    stop(sprintf("Model did not estimate %s.", coefficient_name), call. = FALSE)
  }

  tibble(
    family = family,
    specification = specification,
    detail = detail,
    sample = sample_name,
    outcome = outcome_name,
    estimate = unname(coefficient_table[coefficient_name, "Estimate"]),
    standard_error = unname(coefficient_table[coefficient_name, "Std. Error"]),
    p_value = unname(coefficient_table[coefficient_name, "Pr(>|t|)"]),
    n = nobs(model)
  )
}

main_rows <- list()
main_specifications <- tribble(
  ~specification, ~fixed_effects, ~require_zoning,
  "Current zoning FE", "zone_group + segment_id + construction_year", TRUE,
  "No zoning FE, common sample", "segment_id + construction_year", TRUE,
  "No zoning FE, full sample", "segment_id + construction_year", FALSE
)

for (spec_index in seq_len(nrow(main_specifications))) {
  specification <- main_specifications$specification[spec_index]
  fixed_effects <- main_specifications$fixed_effects[spec_index]
  require_zoning <- main_specifications$require_zoning[spec_index]

  for (sample_name in c("all", "multifamily")) {
    for (outcome_name in c("density_far", "density_dupac")) {
      data <- raw %>% filter(abs(signed_distance_m) <= bandwidth_m)
      if (require_zoning) {
        data <- data %>% filter(!is.na(zone_code))
      }
      data <- sample_data(data, sample_name, outcome_name)

      for (treatment_name in c("binary", "continuous")) {
        treatment_variable <- if (treatment_name == "binary") "side" else "continuous_score_difference"
        controls <- c(
          treatment_variable,
          "pair_average_score",
          "lenient_dist",
          "strict_dist",
          demographic_controls
        )
        model <- feols(
          as.formula(sprintf(
            "log(%s) ~ %s | %s",
            outcome_name,
            paste(controls, collapse = " + "),
            fixed_effects
          )),
          data = data,
          cluster = ~ward_pair
        )

        main_rows[[length(main_rows) + 1L]] <- model_row(
          model,
          treatment_variable,
          "Main regression",
          specification,
          sample_name,
          outcome_name,
          treatment_name
        )
      }
    }
  }
}

build_rd_panel <- function(data, sample_name, outcome_name, running_distance_column, controls, fixed_effects, title, subtitle_detail) {
  data <- sample_data(data, sample_name, outcome_name) %>%
    mutate(
      outcome = log(.data[[outcome_name]]),
      running_distance = .data[[running_distance_column]],
      plot_side = as.integer(running_distance > 0)
    )

  residual_model <- feols(
    as.formula(sprintf(
      "outcome ~ %s | %s",
      paste(controls, collapse = " + "),
      fixed_effects
    )),
    data = data
  )

  removed <- residual_model$obs_selection$obsRemoved
  keep_index <- if (is.null(removed)) {
    seq_len(nrow(data))
  } else {
    setdiff(seq_len(nrow(data)), abs(as.integer(removed)))
  }

  augmented <- data[keep_index, , drop = FALSE] %>%
    mutate(residualized_outcome = as.numeric(resid(residual_model)))

  if (nrow(augmented) != nobs(residual_model)) {
    stop("Residualized sample alignment failed.", call. = FALSE)
  }

  adjusted_model <- feols(
    as.formula(sprintf(
      "outcome ~ plot_side * running_distance + %s | %s",
      paste(controls, collapse = " + "),
      fixed_effects
    )),
    data = augmented,
    cluster = ~ward_pair
  )
  display_model <- feols(
    residualized_outcome ~ plot_side * running_distance,
    data = augmented,
    cluster = ~ward_pair
  )

  adjusted_result <- model_row(
    adjusted_model,
    "plot_side",
    subtitle_detail$family,
    "No zoning FE, full sample",
    sample_name,
    outcome_name,
    paste0(subtitle_detail$detail, "; adjusted")
  )
  display_result <- model_row(
    display_model,
    "plot_side",
    subtitle_detail$family,
    "No zoning FE, full sample",
    sample_name,
    outcome_name,
    paste0(subtitle_detail$detail, "; plotted")
  )

  breaks_m <- seq(-bandwidth_m, bandwidth_m, length.out = 2L * bins_per_side + 1L)
  bin_width_m <- bandwidth_m / bins_per_side
  augmented <- augmented %>%
    mutate(
      bin_index = pmin(
        findInterval(running_distance, breaks_m, rightmost.closed = TRUE, all.inside = TRUE),
        length(breaks_m) - 1L
      ),
      bin_center_m = breaks_m[bin_index] + bin_width_m / 2
    )

  bins <- augmented %>%
    group_by(bin_index, bin_center_m, plot_side) %>%
    summarise(mean_y = mean(residualized_outcome), .groups = "drop") %>%
    mutate(bin_center_ft = bin_center_m / 0.3048)

  line_distance <- subtitle_detail$line_distance
  line_data <- tibble(running_distance = line_distance) %>%
    mutate(plot_side = as.integer(running_distance > 0))
  coefficient_names <- names(coef(display_model))
  design_matrix <- model.matrix(~plot_side * running_distance, data = line_data)
  design_matrix <- design_matrix[, coefficient_names, drop = FALSE]
  covariance_matrix <- vcov(display_model)
  critical_value <- qt(0.975, df = max(n_distinct(augmented$ward_pair) - 1, 1))

  line_data <- line_data %>%
    mutate(
      fit = as.numeric(design_matrix %*% coef(display_model)),
      fit_se = sqrt(pmax(rowSums((design_matrix %*% covariance_matrix) * design_matrix), 0)),
      ci_low = fit - critical_value * fit_se,
      ci_high = fit + critical_value * fit_se,
      running_distance_ft = running_distance / 0.3048
    )

  stars <- case_when(
    adjusted_result$p_value <= 0.01 ~ "***",
    adjusted_result$p_value <= 0.05 ~ "**",
    adjusted_result$p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
  subtitle <- sprintf(
    "%s = %.3f%s (SE %.3f) | N=%s",
    subtitle_detail$label,
    adjusted_result$estimate,
    stars,
    adjusted_result$standard_error,
    format(adjusted_result$n, big.mark = ",")
  )
  if (subtitle_detail$family == "Main RD display") {
    display_stars <- case_when(
      display_result$p_value <= 0.01 ~ "***",
      display_result$p_value <= 0.05 ~ "**",
      display_result$p_value <= 0.10 ~ "*",
      TRUE ~ ""
    )
    subtitle <- sprintf(
      "Visual estimate = %.3f%s (SE %.3f)",
      display_result$estimate,
      display_stars,
      display_result$standard_error
    )
  }

  outcome_label <- if (outcome_name == "density_far") "Log(FAR)" else "Log(DUPAC)"
  plot <- ggplot() +
    geom_ribbon(
      data = line_data,
      aes(x = running_distance_ft, ymin = ci_low, ymax = ci_high, fill = factor(plot_side)),
      alpha = 0.16,
      color = NA
    ) +
    geom_line(
      data = line_data,
      aes(x = running_distance_ft, y = fit, color = factor(plot_side)),
      linewidth = 0.8
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray55", linewidth = 0.35) +
    geom_point(
      data = bins,
      aes(x = bin_center_ft, y = mean_y, fill = factor(plot_side)),
      shape = 21,
      color = "white",
      stroke = 0.35,
      size = 1.85
    ) +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_x_continuous(limits = c(-500, 500), breaks = seq(-500, 500, by = 250)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = subtitle_detail$x_label,
      y = paste("Residualized", outcome_label)
    ) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 8.5, margin = margin(b = 4)),
      axis.title = element_text(size = 8.5),
      axis.text = element_text(size = 7.5),
      panel.grid.minor = element_blank()
    )

  list(plot = plot, results = bind_rows(adjusted_result, display_result))
}

panel_specs <- tribble(
  ~outcome, ~sample,
  "density_far", "all",
  "density_far", "multifamily",
  "density_dupac", "all",
  "density_dupac", "multifamily"
)

main_panels <- list()
plot_rows <- list()
main_data <- raw %>% filter(abs(signed_distance_m) <= bandwidth_m)
for (panel_index in seq_len(nrow(panel_specs))) {
  outcome_name <- panel_specs$outcome[panel_index]
  sample_name <- panel_specs$sample[panel_index]
  sample_label <- if (sample_name == "all") "All" else "Multifamily"
  outcome_label <- if (outcome_name == "density_far") "Log(FAR)" else "Log(DUPAC)"
  panel <- build_rd_panel(
    main_data,
    sample_name,
    outcome_name,
    "signed_distance_m",
    c("pair_average_score", demographic_controls),
    "segment_id + construction_year",
    paste(sample_label, outcome_label, sep = ": "),
    list(
      family = "Main RD display",
      detail = "500ft boundary",
      label = "Jump",
      x_label = "Distance to ward boundary (feet)",
      line_distance = c(
        seq(-bandwidth_m, 0, length.out = 200),
        seq(0, bandwidth_m, length.out = 200)[-1]
      )
    )
  )
  main_panels[[panel_index]] <- panel$plot
  plot_rows[[length(plot_rows) + 1L]] <- panel$results
}

main_combined <- (main_panels[[1]] | main_panels[[2]]) / (main_panels[[3]] | main_panels[[4]])
ggsave(
  "../output/density_no_zoning_main_4panel.pdf",
  main_combined,
  width = 11.2,
  height = 8.4
)

placebo_combined <- list()
for (placebo_direction in c(-1, 1)) {
  shift_m <- placebo_direction * placebo_shift_m
  placebo_data <- raw %>%
    mutate(placebo_distance = signed_distance_m - shift_m) %>%
    filter(abs(placebo_distance) <= bandwidth_m)
  placebo_panels <- list()

  for (panel_index in seq_len(nrow(panel_specs))) {
    outcome_name <- panel_specs$outcome[panel_index]
    sample_name <- panel_specs$sample[panel_index]
    sample_label <- if (sample_name == "all") "All" else "Multifamily"
    outcome_label <- if (outcome_name == "density_far") "Log(FAR)" else "Log(DUPAC)"
    direction_label <- if (placebo_direction < 0) "lenient" else "stringent"
    panel <- build_rd_panel(
      placebo_data,
      sample_name,
      outcome_name,
      "placebo_distance",
      c("pair_average_score", demographic_controls),
      "segment_id + construction_year",
      paste(sample_label, outcome_label, sep = ": "),
      list(
        family = "Shifted-cutoff placebo",
        detail = paste0("1000ft into ", direction_label, " ward"),
        label = "Jump",
        x_label = paste0("Distance to placebo cutoff (feet; 1000ft into ", direction_label, " ward)"),
        line_distance = c(
          seq(-bandwidth_m, 0, length.out = 200),
          seq(0, bandwidth_m, length.out = 200)[-1]
        )
      )
    )
    placebo_panels[[panel_index]] <- panel$plot
    plot_rows[[length(plot_rows) + 1L]] <- panel$results
  }

  combined <- (placebo_panels[[1]] | placebo_panels[[2]]) / (placebo_panels[[3]] | placebo_panels[[4]])
  output_name <- if (placebo_direction < 0) {
    "../output/density_no_zoning_placebo_lenient_4panel.pdf"
  } else {
    "../output/density_no_zoning_placebo_stringent_4panel.pdf"
  }
  ggsave(output_name, combined, width = 11.2, height = 8.4)
}

for (donut_m in donut_distances_m) {
  donut_data <- raw %>%
    filter(
      abs(signed_distance_m) <= bandwidth_m,
      abs(signed_distance_m) >= donut_m
    )
  donut_panels <- list()

  for (panel_index in seq_len(nrow(panel_specs))) {
    outcome_name <- panel_specs$outcome[panel_index]
    sample_name <- panel_specs$sample[panel_index]
    sample_label <- if (sample_name == "all") "All" else "Multifamily"
    outcome_label <- if (outcome_name == "density_far") "Log(FAR)" else "Log(DUPAC)"
    donut_label <- paste0(round(donut_m / 0.3048), "ft donut")
    panel <- build_rd_panel(
      donut_data,
      sample_name,
      outcome_name,
      "signed_distance_m",
      c("pair_average_score", demographic_controls),
      "segment_id + construction_year",
      paste(sample_label, outcome_label, sep = ": "),
      list(
        family = "Donut RD",
        detail = donut_label,
        label = "Jump",
        x_label = "Distance to ward boundary (feet)",
        line_distance = c(
          seq(-bandwidth_m, -donut_m, length.out = 160),
          seq(donut_m, bandwidth_m, length.out = 160)
        )
      )
    )
    donut_panels[[panel_index]] <- panel$plot
    plot_rows[[length(plot_rows) + 1L]] <- panel$results
  }

  combined <- (donut_panels[[1]] | donut_panels[[2]]) / (donut_panels[[3]] | donut_panels[[4]])
  ggsave(
    sprintf("../output/density_no_zoning_donut%dft_4panel.pdf", round(donut_m / 0.3048)),
    combined,
    width = 11.2,
    height = 8.4
  )
}

results <- bind_rows(main_rows, plot_rows) %>%
  mutate(
    sample = recode(sample, all = "All construction", multifamily = "Multifamily"),
    outcome = recode(outcome, density_far = "ln(FAR)", density_dupac = "ln(DUPAC)"),
    percent_effect = 100 * (exp(estimate) - 1)
  )

write_csv(results, "../output/density_no_zoning_estimates.csv")

stars <- function(p_value) {
  case_when(
    p_value <= 0.01 ~ "***",
    p_value <= 0.05 ~ "**",
    p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
}

main_table <- results %>%
  filter(family == "Main regression") %>%
  mutate(
    treatment = recode(detail, binary = "Binary", continuous = "Continuous"),
    estimate_text = sprintf("%.3f%s", estimate, stars(p_value)),
    se_text = sprintf("(%.3f)", standard_error),
    n_text = format(n, big.mark = ","),
    spec_order = match(specification, c(
      "Current zoning FE",
      "No zoning FE, common sample",
      "No zoning FE, full sample"
    )),
    treatment_order = match(treatment, c("Binary", "Continuous")),
    sample_order = match(sample, c("All construction", "Multifamily")),
    outcome_order = match(outcome, c("ln(FAR)", "ln(DUPAC)"))
  ) %>%
  arrange(spec_order, treatment_order, sample_order, outcome_order)

table_lines <- c(
  "\\begin{tabular}{lllrrr}",
  "\\toprule",
  "Specification & Treatment & Sample/outcome & Estimate & SE & N \\\\",
  "\\midrule"
)
for (row_index in seq_len(nrow(main_table))) {
  row <- main_table[row_index, ]
  table_lines <- c(
    table_lines,
    sprintf(
      "%s & %s & %s, %s & %s & %s & %s \\\\",
      row$specification,
      row$treatment,
      row$sample,
      row$outcome,
      row$estimate_text,
      row$se_text,
      row$n_text
    )
  )
}
table_lines <- c(table_lines, "\\bottomrule", "\\end{tabular}")
writeLines(table_lines, "../output/density_no_zoning_main_table.tex")

plot_table <- results %>%
  filter(
    specification == "No zoning FE, full sample",
    family != "Main regression",
    str_detect(detail, "; adjusted$")
  ) %>%
  mutate(
    estimate_text = sprintf("%.3f%s", estimate, stars(p_value)),
    se_text = sprintf("(%.3f)", standard_error),
    n_text = format(n, big.mark = ",")
  )

plot_table_lines <- c(
  "\\begin{tabular}{lllrrr}",
  "\\toprule",
  "Check & Sample & Outcome & Estimate & SE & N \\\\",
  "\\midrule"
)
for (row_index in seq_len(nrow(plot_table))) {
  row <- plot_table[row_index, ]
  detail <- str_remove(row$detail, "; adjusted$")
  plot_table_lines <- c(
    plot_table_lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      detail,
      row$sample,
      row$outcome,
      row$estimate_text,
      row$se_text,
      row$n_text
    )
  )
}
plot_table_lines <- c(plot_table_lines, "\\bottomrule", "\\end{tabular}")
writeLines(plot_table_lines, "../output/density_no_zoning_plot_estimates.tex")

report_lines <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.7in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{graphicx}",
  "\\usepackage{longtable}",
  "\\usepackage{pdflscape}",
  "\\setlength{\\parindent}{0pt}",
  "\\begin{document}",
  "\\section*{Density Results Without Zoning Fixed Effects}",
  paste(
    "This audit holds the construction sample, alderman scores, demographic controls,",
    "distance controls, segment and construction-year fixed effects, and ward-pair clustering fixed.",
    "The only model change is removing zoning-group fixed effects.",
    "The common-sample rows retain the observations used by the zoning model; the full-sample rows also retain records with missing zoning."
  ),
  "\\subsection*{Main regressions}",
  "\\begin{center}",
  "\\resizebox{\\textwidth}{!}{\\input{../output/density_no_zoning_main_table.tex}}",
  "\\end{center}",
  "\\subsection*{Main residualized display}",
  "\\includegraphics[width=\\textwidth]{../output/density_no_zoning_main_4panel.pdf}",
  "\\clearpage",
  "\\subsection*{Shifted cutoff: 1000ft into lenient ward}",
  "\\includegraphics[width=\\textwidth]{../output/density_no_zoning_placebo_lenient_4panel.pdf}",
  "\\subsection*{Shifted cutoff: 1000ft into stringent ward}",
  "\\includegraphics[width=\\textwidth]{../output/density_no_zoning_placebo_stringent_4panel.pdf}",
  "\\clearpage",
  "\\subsection*{25ft donut}",
  "\\includegraphics[width=\\textwidth]{../output/density_no_zoning_donut25ft_4panel.pdf}",
  "\\subsection*{50ft donut}",
  "\\includegraphics[width=\\textwidth]{../output/density_no_zoning_donut50ft_4panel.pdf}",
  "\\clearpage",
  "\\subsection*{Adjusted estimates printed on robustness plots}",
  "\\begin{center}",
  "\\resizebox{\\textwidth}{!}{\\input{../output/density_no_zoning_plot_estimates.tex}}",
  "\\end{center}",
  "\\end{document}"
)
writeLines(report_lines, "../output/density_no_zoning_report.tex")
