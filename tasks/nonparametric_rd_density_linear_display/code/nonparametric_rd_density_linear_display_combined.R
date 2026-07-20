# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/nonparametric_rd_density_linear_display/code")
# bandwidth_m <- 152.4
# fe_spec <- "zonegroup_segment_year_additive"
# bins_per_side <- 5

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_m, fe_spec, bins_per_side)
}

if (length(cli_args) != 3) {
  stop(
    "FATAL: Script requires 3 args: <bandwidth_m> <fe_spec> <bins_per_side>.",
    call. = FALSE
  )
}

bandwidth_m <- as.numeric(cli_args[1])
fe_spec <- cli_args[2]
bins_per_side <- as.integer(cli_args[3])

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be a positive number.", call. = FALSE)
}
if (!fe_spec %in% c("zonegroup_segment_year_additive", "zonegroup_pair_year_additive", "segment_year")) {
  stop("fe_spec must be one of: zonegroup_segment_year_additive, zonegroup_pair_year_additive, segment_year", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side < 2) {
  stop("bins_per_side must be an integer >= 2.", call. = FALSE)
}

distance_display <- distance_display_config("ft")
bw_label <- format_distance_label(bandwidth_m, distance_display)

fe_formula <- dplyr::case_when(
  fe_spec == "zonegroup_segment_year_additive" ~ "zone_group + segment_id + construction_year",
  fe_spec == "zonegroup_pair_year_additive" ~ "zone_group + ward_pair + construction_year",
  fe_spec == "segment_year" ~ "segment_id + construction_year",
  TRUE ~ NA_character_
)

controls <- c(
  "pair_average_score",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

raw <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  ensure_meter_distance_columns()

base_dat <- raw %>%
  mutate(
    zone_group = zone_group_from_code(zone_code),
    pair_average_score = (strictness_own + strictness_neighbor) / 2
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    !is.na(ward_pair),
    !is.na(construction_year),
    is.finite(signed_distance_m),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != "",
    abs(signed_distance_m) <= bandwidth_m
  )

x_limits <- c(-bandwidth_m, bandwidth_m) * distance_display$scale
x_label <- sprintf("Distance to ward boundary (%s)", distance_display$unit)

panel_specs <- tribble(
  ~yvar, ~sample_filter,
  "density_far", "all",
  "density_far", "multifamily",
  "density_dupac", "all",
  "density_dupac", "multifamily"
)

panels <- vector("list", nrow(panel_specs))
display_rows <- vector("list", nrow(panel_specs))

for (i in seq_len(nrow(panel_specs))) {
  yvar <- panel_specs$yvar[i]
  sample_filter <- panel_specs$sample_filter[i]

  outcome_name <- dplyr::case_when(
    yvar == "density_far" ~ "FAR",
    yvar == "density_dupac" ~ "DUPAC",
    TRUE ~ yvar
  )
  pretty_outcome <- dplyr::case_when(
    yvar %in% c("density_far", "density_dupac") ~ paste0("Log(", outcome_name, ")"),
    TRUE ~ outcome_name
  )
  sample_label <- dplyr::case_when(
    sample_filter == "all" ~ "All",
    sample_filter == "multifamily" ~ "Multifamily",
    TRUE ~ sample_filter
  )

  dat <- base_dat
  if (sample_filter == "all") {
    dat <- dat %>% filter(unitscount > 0)
  } else if (sample_filter == "multifamily") {
    dat <- dat %>% filter(unitscount > 1)
  } else {
    stop("Unknown sample_filter.", call. = FALSE)
  }

  dat <- dat %>%
    filter(is.finite(.data[[yvar]]), .data[[yvar]] > 0) %>%
    mutate(
      outcome = log(.data[[yvar]]),
      running_distance = signed_distance_m,
      side = as.integer(running_distance > 0)
    )

  if (nrow(dat) == 0) {
    stop("No observations remain after baseline filters.", call. = FALSE)
  }

  fml_resid <- as.formula(sprintf(
    "outcome ~ %s | %s",
    paste(controls, collapse = " + "),
    fe_formula
  ))
  m_resid <- feols(fml_resid, data = dat)

  removed <- m_resid$obs_selection$obsRemoved
  if (is.null(removed)) {
    keep_idx <- seq_len(nrow(dat))
  } else {
    keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
  }

  aug <- dat[keep_idx, , drop = FALSE] %>%
    mutate(residualized_outcome = as.numeric(resid(m_resid)))

  if (nrow(aug) != nobs(m_resid)) {
    stop("Residualized sample alignment failed.", call. = FALSE)
  }

  fml_linear <- as.formula(sprintf(
    "outcome ~ side * running_distance + %s | %s",
    paste(controls, collapse = " + "),
    fe_formula
  ))
  m_linear <- feols(fml_linear, data = aug, cluster = ~ward_pair)
  linear_row <- coeftable(m_linear)[rownames(coeftable(m_linear)) %in% "side", , drop = FALSE]
  if (nrow(linear_row) != 1L) {
    stop("Could not recover the local-linear cutoff estimate.", call. = FALSE)
  }
  m_display <- feols(
    residualized_outcome ~ side * running_distance,
    data = aug,
    cluster = ~ward_pair
  )
  display_row <- coeftable(m_display)["side", , drop = FALSE]
  display_rows[[i]] <- tibble(
    outcome = outcome_name,
    sample = sample_label,
    estimate = unname(display_row[1, "Estimate"]),
    standard_error = unname(display_row[1, "Std. Error"]),
    p_value = unname(display_row[1, "Pr(>|t|)"])
  )
  visual_stars <- case_when(
    display_rows[[i]]$p_value <= 0.01 ~ "***",
    display_rows[[i]]$p_value <= 0.05 ~ "**",
    display_rows[[i]]$p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
  visual_estimate_label <- sprintf(
    "Visual estimate = %.3f%s (SE %.3f)",
    display_rows[[i]]$estimate,
    visual_stars,
    display_rows[[i]]$standard_error
  )

  breaks_m <- seq(-bandwidth_m, bandwidth_m, length.out = 2L * bins_per_side + 1L)
  bin_width_m <- bandwidth_m / bins_per_side

  aug <- aug %>%
    mutate(
      bin_idx = pmin(
        findInterval(running_distance, breaks_m, rightmost.closed = TRUE, all.inside = TRUE),
        length(breaks_m) - 1L
      ),
      bin_left_m = breaks_m[bin_idx],
      bin_center_m = bin_left_m + bin_width_m / 2
    )

  bins <- aug %>%
    group_by(bin_idx, bin_center_m, side) %>%
    summarise(
      mean_y = mean(residualized_outcome, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(bin_center_m)

  coef_names <- names(coef(m_display))
  line_df <- tibble(
    running_distance = c(
      seq(-bandwidth_m, 0, length.out = 200),
      seq(0, bandwidth_m, length.out = 200)[-1]
    )
  ) %>%
    mutate(
      side = as.integer(running_distance > 0)
    )

  xmat <- model.matrix(~ side * running_distance, data = line_df)
  missing_coef_names <- setdiff(coef_names, colnames(xmat))
  if (length(missing_coef_names) > 0) {
    stop("Prediction design matrix does not match fitted model.", call. = FALSE)
  }
  xmat <- xmat[, coef_names, drop = FALSE]

  line_vcov <- vcov(m_display)
  line_crit <- qt(0.975, df = max(n_distinct(aug$ward_pair) - 1, 1))

  line_df <- line_df %>%
    mutate(
      fit = as.numeric(xmat %*% coef(m_display)),
      fit_se = sqrt(pmax(rowSums((xmat %*% line_vcov) * xmat), 0)),
      ci_low = fit - line_crit * fit_se,
      ci_high = fit + line_crit * fit_se,
      running_distance_display = running_distance * distance_display$scale
    )

  bins <- bins %>%
    mutate(bin_center_display = bin_center_m * distance_display$scale)

  y_min <- min(c(bins$mean_y, line_df$ci_low), na.rm = TRUE)
  y_max <- max(c(bins$mean_y, line_df$ci_high), na.rm = TRUE)
  y_span <- y_max - y_min
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
  }
  y_pad <- max(0.15 * y_span, 0.05)
  y_limits <- c(y_min - y_pad, y_max + y_pad)

  plot <- ggplot() +
    geom_ribbon(
      data = line_df,
      aes(x = running_distance_display, ymin = ci_low, ymax = ci_high, fill = factor(side)),
      alpha = 0.16,
      color = NA
    ) +
    geom_line(
      data = line_df,
      aes(x = running_distance_display, y = fit, color = factor(side)),
      linewidth = 0.8
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray55", linewidth = 0.35) +
    geom_point(
      data = bins,
      aes(x = bin_center_display, y = mean_y, fill = factor(side)),
      shape = 21,
      color = "white",
      stroke = 0.35,
      size = 1.85,
      alpha = 0.98
    ) +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 5)) +
    coord_cartesian(ylim = y_limits) +
    labs(
      title = paste(sample_label, pretty_outcome, sep = ": "),
      subtitle = visual_estimate_label,
      x = x_label,
      y = paste("Residualized", pretty_outcome)
    ) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 8.5, margin = margin(b = 4)),
      axis.title = element_text(size = 8.5),
      axis.text = element_text(size = 7.5),
      panel.grid.minor = element_blank()
    )

  panels[[i]] <- plot
}

combined_plot <- (panels[[1]] | panels[[2]]) / (panels[[3]] | panels[[4]])

ggsave(
  sprintf(
    "../output/nonparametric_rd_density_linear_display_4panel_%s_all_multifamily_bins%d.pdf",
    bw_label,
    bins_per_side
  ),
  plot = combined_plot,
  width = 11.2,
  height = 8.4,
  dpi = 300
)

display_results <- bind_rows(display_rows)

if (nrow(display_results) != 4L) {
  stop("Expected four plotted density differences.", call. = FALSE)
}

writeLines(
  paste(
    "The estimates printed above the panels are the discontinuities in the residualized local-linear displays.",
    "They are not the binary or continuous regression estimates reported in Table~\\ref{tab:density_main_table}."
  ),
  sprintf(
    "../output/nonparametric_rd_density_linear_display_estimates_%s_all_multifamily_bins%d.tex",
    bw_label,
    bins_per_side
  )
)
