# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

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

lineage_pins <- read_csv(
  "../output/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = cols(member_pins = col_character(), .default = col_guess())
) %>%
  left_join(
    read_csv(
      "../output/density_parcel_address_lineage_evidence.csv",
      show_col_types = FALSE,
      col_types = cols(member_pins = col_character(), .default = col_guess())
    ) %>%
      select(project_key, address_audit_recommendation),
    by = "project_key",
    relationship = "one-to-one"
  ) %>%
  filter(
    recommended_action == "candidate_for_recovery",
    address_audit_recommendation != "exclude_address_confirmed_duplicate"
  ) %>%
  select(project_key, member_pins) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  rename(pin = member_pins)

recovered <- read_csv(
  "../input/density_historical_recovered_exact_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    .default = col_guess()
  )
) %>%
  inner_join(lineage_pins, by = "pin", relationship = "one-to-one")

address_recovered <- read_csv(
  "../output/density_parcel_address_recovered_model_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    source_class = col_character(), .default = col_guess()
  )
)

production <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    .default = col_guess()
  )
)

base_data <- bind_rows(production, recovered, address_recovered) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    dist_to_boundary_m <= bandwidth_m,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != ""
  )

model_results <- read_csv(
  "../output/density_bandwidth_fe_robustness.csv",
  show_col_types = FALSE
) %>%
  filter(
    bandwidth == "500ft",
    fixed_effect_spec == "main",
    treatment == "binary"
  )

distance_display <- distance_display_config("ft")
x_limits <- c(-bandwidth_m, bandwidth_m) * distance_display$scale
x_label <- sprintf("Distance to ward boundary (%s)", distance_display$unit)

panel_specs <- tribble(
  ~outcome,       ~construction_sample, ~title,                  ~filename,
  "density_far",   "all",               "All Construction: FAR", "density_project_lineage_rd_all_far.png",
  "density_far",   "multifamily",       "Multifamily: FAR",      "density_project_lineage_rd_multifamily_far.png",
  "density_dupac", "all",               "All Construction: DUPAC", "density_project_lineage_rd_all_dupac.png",
  "density_dupac", "multifamily",       "Multifamily: DUPAC",      "density_project_lineage_rd_multifamily_dupac.png"
)

panels <- vector("list", nrow(panel_specs))
for (i in seq_len(nrow(panel_specs))) {
  outcome <- panel_specs$outcome[i]
  construction_sample <- panel_specs$construction_sample[i]

  plot_data <- if (construction_sample == "all") {
    base_data %>% filter(unitscount > 0)
  } else {
    base_data %>% filter(unitscount > 1)
  }

  plot_data <- plot_data %>%
    filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
    mutate(
      outcome_value = log(.data[[outcome]]),
      running_distance = signed_distance_m,
      side = as.integer(running_distance > 0)
    )

  residual_model <- feols(
    as.formula(paste0(
      "outcome_value ~ ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = plot_data
  )

  removed_rows <- residual_model$obs_selection$obsRemoved
  kept_rows <- if (is.null(removed_rows)) {
    seq_len(nrow(plot_data))
  } else {
    setdiff(seq_len(nrow(plot_data)), abs(as.integer(removed_rows)))
  }

  plot_data <- plot_data[kept_rows, , drop = FALSE] %>%
    mutate(residualized_outcome = as.numeric(resid(residual_model)))

  cutoff_model <- feols(
    as.formula(paste0(
      "outcome_value ~ side * running_distance + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = plot_data,
    cluster = ~ward_pair
  )

  cutoff_table <- coeftable(cutoff_model)
  cutoff_estimate <- unname(cutoff_table["side", "Estimate"])
  cutoff_se <- unname(cutoff_table["side", "Std. Error"])
  cutoff_p <- unname(cutoff_table["side", "Pr(>|t|)"])

  expected_result <- model_results %>%
    filter(
      .data$outcome == !!outcome,
      .data$construction_sample == !!construction_sample
    )
  if (nrow(expected_result) != 1 ||
      abs(cutoff_estimate - expected_result$estimate) > 1e-8 ||
      abs(cutoff_se - expected_result$se) > 1e-8) {
    stop(
      sprintf(
        paste0(
          "Plot/table mismatch for %s %s: plot %.12f (%.12f), ",
          "table %.12f (%.12f)."
        ),
        construction_sample,
        outcome,
        cutoff_estimate,
        cutoff_se,
        expected_result$estimate,
        expected_result$se
      ),
      call. = FALSE
    )
  }

  cutoff_stars <- case_when(
    cutoff_p <= 0.01 ~ "***",
    cutoff_p <= 0.05 ~ "**",
    cutoff_p <= 0.10 ~ "*",
    TRUE ~ ""
  )

  display_model <- feols(
    residualized_outcome ~ side * running_distance,
    data = plot_data,
    cluster = ~ward_pair
  )

  breaks_m <- seq(-bandwidth_m, bandwidth_m, length.out = 2L * bins_per_side + 1L)
  bin_width_m <- bandwidth_m / bins_per_side
  plot_data <- plot_data %>%
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
    )

  bins <- plot_data %>%
    group_by(bin, bin_center_m, side) %>%
    summarise(mean_y = mean(residualized_outcome), .groups = "drop") %>%
    mutate(bin_center = bin_center_m * distance_display$scale)

  line_data <- tibble(
    running_distance = c(
      seq(-bandwidth_m, 0, length.out = 200),
      seq(0, bandwidth_m, length.out = 200)[-1]
    )
  ) %>%
    mutate(side = as.integer(running_distance > 0))

  coefficient_names <- names(coef(display_model))
  design_matrix <- model.matrix(~side * running_distance, data = line_data)
  design_matrix <- design_matrix[, coefficient_names, drop = FALSE]
  display_vcov <- vcov(display_model)
  critical_value <- qt(
    0.975,
    df = max(n_distinct(plot_data$ward_pair) - 1, 1)
  )

  line_data <- line_data %>%
    mutate(
      fit = as.numeric(design_matrix %*% coef(display_model)),
      fit_se = sqrt(pmax(
        rowSums((design_matrix %*% display_vcov) * design_matrix),
        0
      )),
      ci_low = fit - critical_value * fit_se,
      ci_high = fit + critical_value * fit_se,
      running_distance_display = running_distance * distance_display$scale
    )

  y_min <- min(c(bins$mean_y, line_data$ci_low), na.rm = TRUE)
  y_max <- max(c(bins$mean_y, line_data$ci_high), na.rm = TRUE)
  y_padding <- max(0.15 * (y_max - y_min), 0.05)

  panels[[i]] <- ggplot() +
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
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray30",
      linewidth = 0.35
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray55",
      linewidth = 0.35
    ) +
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
    scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 5)) +
    coord_cartesian(ylim = c(y_min - y_padding, y_max + y_padding)) +
    labs(
      title = panel_specs$title[i],
      subtitle = sprintf("Jump = %.3f%s (SE %.3f)", cutoff_estimate, cutoff_stars, cutoff_se),
      x = x_label,
      y = paste0("Residualized Log(", if_else(outcome == "density_far", "FAR", "DUPAC"), ")")
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      panel.grid.minor = element_blank()
    )

  ggsave(
    file.path("../output", panel_specs$filename[i]),
    plot = panels[[i]],
    width = 6.5,
    height = 4.6,
    dpi = 220,
    bg = "white"
  )
}

combined_plot <- (panels[[1]] | panels[[2]]) / (panels[[3]] | panels[[4]]) +
  plot_annotation(
    title = "Local-Linear Spatial RD: Lineage-Corrected Construction Sample (500 ft)"
  ) &
  theme(plot.title = element_text(face = "bold", size = 13))

ggsave(
  "../output/density_project_lineage_rd_4panel.png",
  plot = combined_plot,
  width = 12,
  height = 8.6,
  dpi = 220,
  bg = "white"
)
ggsave(
  "../output/density_project_lineage_rd_4panel.pdf",
  plot = combined_plot,
  width = 12,
  height = 8.6,
  bg = "white"
)
