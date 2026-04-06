source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/nonparametric_rd_density_spec_lab/code")
# figure_type <- "type1_bins"
# yvar <- "density_far"
# bw_ft <- 250
# sample_filter <- "all"
# fe_spec <- "segment_zonegroup_year"
# output_pdf <- "../output/type1_bins_log_density_far_bw250_all_segment_zonegroup_year.pdf"
# output_csv <- "none"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    figure_type,
    yvar,
    bw_ft,
    sample_filter,
    fe_spec,
    output_pdf,
    output_csv
  )
}

if (length(args) == 6) {
  args <- c(args, "none")
}

if (length(args) != 7) {
  stop(
    "FATAL: Script requires args: <figure_type> <yvar> <bw_ft> <sample_filter> <fe_spec> <output_pdf> [output_csv]",
    call. = FALSE
  )
}

figure_type <- args[1]
yvar <- args[2]
bw_ft <- as.numeric(args[3])
sample_filter <- args[4]
fe_spec <- args[5]
output_pdf <- args[6]
output_csv <- args[7]

valid_figure_types <- c(
  "type1_bins",
  "type2_bins_plus_lines",
  "type3_local_linear",
  "type4_kulka",
  "legacy_binned_only",
  "legacy_linear_display"
)
valid_yvars <- c("density_far", "density_dupac")
valid_samples <- c("all", "multifamily")
valid_fe_specs <- c("segment_zonegroup_year", "segment_year", "wardpair_zonegroup_year")

if (!figure_type %in% valid_figure_types) {
  stop(sprintf("figure_type must be one of: %s", paste(valid_figure_types, collapse = ", ")), call. = FALSE)
}
if (!yvar %in% valid_yvars) {
  stop(sprintf("yvar must be one of: %s", paste(valid_yvars, collapse = ", ")), call. = FALSE)
}
if (!bw_ft %in% c(250, 500)) {
  stop("bw_ft must be one of: 250, 500", call. = FALSE)
}
if (!sample_filter %in% valid_samples) {
  stop(sprintf("sample_filter must be one of: %s", paste(valid_samples, collapse = ", ")), call. = FALSE)
}
if (!fe_spec %in% valid_fe_specs) {
  stop(sprintf("fe_spec must be one of: %s", paste(valid_fe_specs, collapse = ", ")), call. = FALSE)
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.1) return("*")
  ""
}

target_bins_from_bw <- function(bw_ft, figure_type) {
  10L
}

fe_rhs_from_spec <- function(fe_spec) {
  dplyr::case_when(
    fe_spec == "segment_zonegroup_year" ~ "zone_group + segment_id + construction_year",
    fe_spec == "segment_year" ~ "segment_id + construction_year",
    fe_spec == "wardpair_zonegroup_year" ~ "ward_pair + zone_group + construction_year",
    TRUE ~ NA_character_
  )
}

fe_label_from_spec <- function(fe_spec) {
  dplyr::case_when(
    fe_spec == "segment_zonegroup_year" ~ "segment + zone-group + year FE",
    fe_spec == "segment_year" ~ "segment + year FE",
    fe_spec == "wardpair_zonegroup_year" ~ "ward-pair + zone-group + year FE",
    TRUE ~ fe_spec
  )
}

pretty_outcome_from_yvar <- function(yvar) {
  dplyr::case_when(
    yvar == "density_far" ~ "Log(FAR)",
    yvar == "density_dupac" ~ "Log(DUPAC)",
    TRUE ~ yvar
  )
}

assign_bins <- function(df, bw_ft, bins_per_side) {
  breaks_ft <- seq(-bw_ft, bw_ft, length.out = 2L * bins_per_side + 1L)
  bin_width_ft <- bw_ft / bins_per_side

  df %>%
    mutate(
      distance_bin = pmin(
        findInterval(signed_distance, breaks_ft, rightmost.closed = TRUE, all.inside = TRUE),
        length(breaks_ft) - 1L
      ),
      bin_left_ft = breaks_ft[distance_bin],
      bin_right_ft = bin_left_ft + bin_width_ft,
      bin_center_ft = bin_left_ft + bin_width_ft / 2,
      side = as.integer(signed_distance > 0),
      side_label = if_else(side == 1L, "Strict side", "Lenient side")
    )
}

choose_bins_per_side <- function(df, bw_ft, target_bins_per_side) {
  for (candidate in seq(target_bins_per_side, 2L, by = -1L)) {
    tmp <- assign_bins(df, bw_ft, candidate)
    counts <- tmp %>%
      count(distance_bin, name = "bin_n")

    lenient_first_n <- counts$bin_n[counts$distance_bin == candidate]
    strict_first_n <- counts$bin_n[counts$distance_bin == candidate + 1L]

    lenient_first_n <- ifelse(length(lenient_first_n) == 0, 0L, lenient_first_n)
    strict_first_n <- ifelse(length(strict_first_n) == 0, 0L, strict_first_n)

    if (lenient_first_n > 0 && strict_first_n > 0) {
      return(list(
        bins_per_side = candidate,
        data = tmp,
        lenient_first_n = as.integer(lenient_first_n),
        strict_first_n = as.integer(strict_first_n)
      ))
    }
  }

  stop("Could not find a bins-per-side choice with populated near-cutoff bins.", call. = FALSE)
}

extract_obs_keep_idx <- function(model_obj, data_n) {
  removed <- model_obj$obs_selection$obsRemoved
  if (is.null(removed)) {
    return(seq_len(data_n))
  }
  setdiff(seq_len(data_n), abs(as.integer(removed)))
}

extract_single_row <- function(ct, term_name, error_label = "target coefficient") {
  row <- ct[rownames(ct) %in% term_name, , drop = FALSE]
  if (nrow(row) != 1) {
    stop(sprintf("Could not recover %s.", error_label), call. = FALSE)
  }
  row
}

build_linear_display_grid <- function(model_obj, bw_ft, n_clusters) {
  line_grid <- tibble(
    signed_distance = c(
      seq(-bw_ft, 0, length.out = 151L),
      seq(0, bw_ft, length.out = 151L)[-1]
    )
  ) %>%
    mutate(side = as.integer(signed_distance > 0))

  coef_names <- names(coef(model_obj))
  xmat <- matrix(0, nrow = nrow(line_grid), ncol = length(coef_names))
  colnames(xmat) <- coef_names

  if ("(Intercept)" %in% coef_names) {
    xmat[, "(Intercept)"] <- 1
  }
  if ("side" %in% coef_names) {
    xmat[, "side"] <- line_grid$side
  }
  if ("signed_distance" %in% coef_names) {
    xmat[, "signed_distance"] <- line_grid$signed_distance
  }
  if ("side:signed_distance" %in% coef_names) {
    xmat[, "side:signed_distance"] <- line_grid$side * line_grid$signed_distance
  }
  if ("signed_distance:side" %in% coef_names) {
    xmat[, "signed_distance:side"] <- line_grid$side * line_grid$signed_distance
  }

  vc <- vcov(model_obj)
  crit <- qt(0.975, df = max(n_clusters - 1, 1))

  line_grid %>%
    mutate(
      fit = as.numeric(xmat %*% coef(model_obj)),
      fit_se = sqrt(pmax(rowSums((xmat %*% vc) * xmat), 0)),
      ci_low = fit - crit * fit_se,
      ci_high = fit + crit * fit_se
    )
}

build_bin_coefficient_table <- function(m_bin, m_bin_robust, actual_bins_per_side, aug) {
  cluster_ct <- coeftable(m_bin)
  robust_ct <- coeftable(m_bin_robust)
  ref_bin <- actual_bins_per_side
  strict_bin <- actual_bins_per_side + 1L

  coef_df <- tibble(
    distance_bin = seq_len(2L * actual_bins_per_side)
  ) %>%
    left_join(
      aug %>%
        group_by(distance_bin) %>%
        summarise(
          bin_center_ft = first(bin_center_ft),
          bin_left_ft = first(bin_left_ft),
          bin_right_ft = first(bin_right_ft),
          bin_n = n(),
          mean_y = mean(residualized_outcome, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "distance_bin"
    ) %>%
    mutate(
      side = if_else(distance_bin > actual_bins_per_side, 1L, 0L),
      side_label = if_else(side == 1L, "Strict side", "Lenient side"),
      estimate = 0,
      se_cluster = 0,
      se_robust = 0,
      p_cluster = NA_real_
    )

  for (bin_id in coef_df$distance_bin[coef_df$distance_bin != ref_bin]) {
    term_name <- paste0("distance_bin::", bin_id)

    cluster_row <- extract_single_row(cluster_ct, term_name, paste("clustered coefficient for bin", bin_id))
    robust_row <- extract_single_row(robust_ct, term_name, paste("robust coefficient for bin", bin_id))

    coef_df$estimate[coef_df$distance_bin == bin_id] <- unname(cluster_row[1, "Estimate"])
    coef_df$se_cluster[coef_df$distance_bin == bin_id] <- unname(cluster_row[1, "Std. Error"])
    coef_df$se_robust[coef_df$distance_bin == bin_id] <- unname(robust_row[1, "Std. Error"])
    coef_df$p_cluster[coef_df$distance_bin == bin_id] <- unname(cluster_row[1, "Pr(>|t|)"])
  }

  coef_df %>%
    mutate(
      ci_low = estimate - 1.96 * se_cluster,
      ci_high = estimate + 1.96 * se_cluster,
      is_reference_bin = distance_bin == ref_bin,
      is_first_strict_bin = distance_bin == strict_bin
    )
}

build_type2_line_table <- function(coef_df) {
  bind_rows(lapply(split(coef_df, coef_df$side_label), function(side_df) {
    side_df <- side_df %>%
      filter(is.finite(bin_center_ft), is.finite(estimate), bin_n > 0)

    if (nrow(side_df) < 2) {
      return(tibble())
    }

    fit <- lm(estimate ~ bin_center_ft, data = side_df, weights = bin_n)
    grid <- tibble(bin_center_ft = seq(min(side_df$bin_center_ft), max(side_df$bin_center_ft), length.out = 100L))

    grid %>%
      mutate(
        fit = as.numeric(predict(fit, newdata = grid)),
        side_label = first(side_df$side_label)
      )
  }))
}

control_vars <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

message(sprintf("Building %s | y=%s | bw=%d | sample=%s | fe=%s", figure_type, yvar, as.integer(bw_ft), sample_filter, fe_spec))

raw <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE)

dat <- raw %>%
  mutate(zone_group = zone_group_from_code(zone_code)) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    !is.na(ward_pair),
    !is.na(construction_year),
    is.finite(signed_distance),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != "",
    abs(signed_distance) <= bw_ft
  )

if (sample_filter == "all") {
  dat <- dat %>% filter(unitscount > 0)
} else {
  dat <- dat %>% filter(unitscount > 1)
}

dat <- dat %>%
  filter(is.finite(.data[[yvar]]), .data[[yvar]] > 0) %>%
  mutate(outcome = log(.data[[yvar]]))

if (nrow(dat) == 0) {
  stop("No observations remain after sample restrictions.", call. = FALSE)
}

target_bins_per_side <- target_bins_from_bw(bw_ft, figure_type)
bin_choice <- choose_bins_per_side(dat, bw_ft, target_bins_per_side)
actual_bins_per_side <- bin_choice$bins_per_side
dat <- bin_choice$data
first_lenient_bin_n <- bin_choice$lenient_first_n
first_strict_bin_n <- bin_choice$strict_first_n

fe_rhs <- fe_rhs_from_spec(fe_spec)
control_rhs <- paste(control_vars, collapse = " + ")

fml_resid <- as.formula(sprintf("outcome ~ %s | %s", control_rhs, fe_rhs))
m_resid <- feols(fml_resid, data = dat)
keep_idx <- extract_obs_keep_idx(m_resid, nrow(dat))
aug <- dat[keep_idx, , drop = FALSE] %>%
  mutate(residualized_outcome = as.numeric(resid(m_resid)))

if (nrow(aug) != nobs(m_resid)) {
  stop("Residualized sample alignment failed.", call. = FALSE)
}

fml_gap <- as.formula(sprintf("outcome ~ side + %s | %s", control_rhs, fe_rhs))
m_gap <- feols(fml_gap, data = aug, cluster = ~ward_pair)
gap_row <- extract_single_row(coeftable(m_gap), "side", "pooled gap estimate")
gap_estimate <- unname(gap_row[1, "Estimate"])
gap_se <- unname(gap_row[1, "Std. Error"])
gap_p <- unname(gap_row[1, "Pr(>|t|)"])

fml_linear_gap <- as.formula(sprintf("outcome ~ side * signed_distance + %s | %s", control_rhs, fe_rhs))
m_linear_gap <- feols(fml_linear_gap, data = aug, cluster = ~ward_pair)
linear_row <- extract_single_row(coeftable(m_linear_gap), "side", "linear cutoff estimate")
linear_gap_estimate <- unname(linear_row[1, "Estimate"])
linear_gap_se <- unname(linear_row[1, "Std. Error"])
linear_gap_p <- unname(linear_row[1, "Pr(>|t|)"])

ref_bin <- actual_bins_per_side
strict_bin <- actual_bins_per_side + 1L

fml_bin <- as.formula(sprintf("outcome ~ i(distance_bin, ref = %d) + %s | %s", ref_bin, control_rhs, fe_rhs))
m_bin <- feols(fml_bin, data = aug, cluster = ~ward_pair)
m_bin_robust <- summary(m_bin, vcov = "hetero")
coef_df <- build_bin_coefficient_table(m_bin, m_bin_robust, actual_bins_per_side, aug)

first_strict_row <- coef_df %>% filter(distance_bin == strict_bin)
if (nrow(first_strict_row) != 1) {
  stop("Could not recover first strict-side bin coefficient.", call. = FALSE)
}

pretty_outcome <- pretty_outcome_from_yvar(yvar)
sample_label <- if_else(sample_filter == "all", "all construction", "multifamily")
subtitle_common <- c(
  paste0("bw = ", as.integer(bw_ft), " ft"),
  paste0("N = ", nobs(m_resid))
)

bin_centers_for_axis <- coef_df %>% filter(is.finite(bin_center_ft)) %>% pull(bin_center_ft)
x_limits <- c(min(bin_centers_for_axis) - bw_ft / actual_bins_per_side / 2, max(bin_centers_for_axis) + bw_ft / actual_bins_per_side / 2)

if (figure_type == "type1_bins" || figure_type == "type2_bins_plus_lines" || figure_type == "type4_kulka") {
  plot_df <- coef_df %>% filter(is.finite(bin_center_ft))

  if (figure_type == "type1_bins") {
    subtitle_bits <- c(
      sprintf(
        "First-bin gap = %.3f%s (SE %.3f)",
        first_strict_row$estimate,
        stars(first_strict_row$p_cluster),
        first_strict_row$se_cluster
      ),
      sample_label,
      subtitle_common
    )

    p <- ggplot(plot_df, aes(x = bin_center_ft, y = estimate, group = side_label)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "#b9c2cc", linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#8f8f8f", linewidth = 0.6) +
      geom_line(color = "#666666", linewidth = 0.5) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = bw_ft / actual_bins_per_side * 0.12, color = "#4f6982", linewidth = 0.55) +
      geom_point(color = "#1f3d5a", size = 2.2) +
      scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 7)) +
      labs(
        title = pretty_outcome,
        subtitle = paste(subtitle_bits, collapse = " | "),
        x = "Distance to ward boundary (ft)",
        y = "Coefficient relative to first lenient bin"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e3e7ec", linewidth = 0.35),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 10)
      )
  } else if (figure_type == "type2_bins_plus_lines") {
    line_df <- build_type2_line_table(plot_df)

    subtitle_bits <- c(
      sprintf(
        "First-bin gap = %.3f%s (SE %.3f)",
        first_strict_row$estimate,
        stars(first_strict_row$p_cluster),
        first_strict_row$se_cluster
      ),
      sample_label,
      subtitle_common
    )

    p <- ggplot(plot_df, aes(x = bin_center_ft, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "#b9c2cc", linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#8f8f8f", linewidth = 0.6) +
      geom_line(aes(group = side_label), color = "#777777", linewidth = 0.45) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = bw_ft / actual_bins_per_side * 0.12, color = "#4f6982", linewidth = 0.55) +
      geom_point(color = "#111111", size = 2.2) +
      geom_line(
        data = line_df,
        aes(x = bin_center_ft, y = fit, group = side_label),
        inherit.aes = FALSE,
        color = "#1f3d5a",
        linewidth = 0.9
      ) +
      scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 7)) +
      labs(
        title = pretty_outcome,
        subtitle = paste(subtitle_bits, collapse = " | "),
        x = "Distance to ward boundary (ft)",
        y = "Coefficient relative to first lenient bin"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e3e7ec", linewidth = 0.35),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 10)
      )
  } else {
    subtitle_bits <- c(
      sprintf(
        "First strict bin = %.3f%s (%.3f) [%.3f]",
        first_strict_row$estimate,
        stars(first_strict_row$p_cluster),
        first_strict_row$se_cluster,
        first_strict_row$se_robust
      ),
      "normalized to first lenient bin",
      sample_label,
      subtitle_common
    )

    p <- ggplot(plot_df, aes(x = bin_center_ft, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "#b9c2cc", linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#8f8f8f", linewidth = 0.6) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = bw_ft / actual_bins_per_side * 0.12, color = "#111111", linewidth = 0.55) +
      geom_point(
        aes(color = is_reference_bin),
        size = 2.3,
        show.legend = FALSE
      ) +
      scale_color_manual(values = c(`TRUE` = "#1f3d5a", `FALSE` = "#111111")) +
      scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 7)) +
      labs(
        title = pretty_outcome,
        subtitle = paste(subtitle_bits, collapse = " | "),
        x = "Distance to ward boundary (ft)",
        y = "Coefficient relative to first lenient bin"
      ) +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 10)
      )
  }
} else {
  residual_bins <- aug %>%
    group_by(distance_bin, bin_center_ft) %>%
    summarise(
      n = n(),
      mean_y = mean(residualized_outcome, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(bin_center_ft)

  y_min <- min(residual_bins$mean_y, na.rm = TRUE)
  y_max <- max(residual_bins$mean_y, na.rm = TRUE)
  y_span <- y_max - y_min
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- max(abs(c(y_min, y_max, 0)), na.rm = TRUE)
  }
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
  }
  y_pad <- max(0.25 * y_span, 0.1)
  y_limits <- c(min(y_min, 0) - y_pad, max(y_max, 0) + y_pad)

  if (figure_type == "legacy_binned_only") {
    subtitle_bits <- c(
      sprintf("Pooled gap = %.3f%s (SE %.3f)", gap_estimate, stars(gap_p), gap_se),
      sample_label,
      subtitle_common
    )

    p <- ggplot(residual_bins, aes(x = bin_center_ft, y = mean_y)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "#d8dce3", linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#6e6e6e", linewidth = 0.6) +
      geom_point(color = "#2f5d8a", size = 2.3, alpha = 0.95) +
      scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 7)) +
      labs(
        title = pretty_outcome,
        subtitle = paste(subtitle_bits, collapse = " | "),
        x = "Distance to ward boundary (ft)",
        y = paste("Residualized", pretty_outcome)
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#d8dce3", linewidth = 0.4),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 10)
      )
  } else {
    m_linear_display <- feols(
      residualized_outcome ~ side * signed_distance,
      data = aug,
      cluster = ~ward_pair
    )

    line_grid <- build_linear_display_grid(m_linear_display, bw_ft, n_distinct(aug$ward_pair))
    aug_plot <- aug %>%
      filter(
        residualized_outcome >= y_limits[1],
        residualized_outcome <= y_limits[2]
      )

    subtitle_bits <- c(
      sprintf("Estimate = %.3f%s (SE %.3f)", linear_gap_estimate, stars(linear_gap_p), linear_gap_se),
      sample_label,
      subtitle_common
    )

    if (figure_type == "type3_local_linear") {
      subtitle_bits <- c(
        sprintf("Estimate = %.3f%s (SE %.3f)", linear_gap_estimate, stars(linear_gap_p), linear_gap_se),
        sample_label,
        subtitle_common
      )
    }

    p <- ggplot(residual_bins, aes(x = bin_center_ft, y = mean_y)) +
      geom_point(
        data = aug_plot,
        aes(x = signed_distance, y = residualized_outcome),
        inherit.aes = FALSE,
        color = "#b7bfc9",
        alpha = 0.5,
        size = 0.9,
        stroke = 0
      ) +
      geom_ribbon(
        data = line_grid,
        aes(
          x = signed_distance,
          ymin = ci_low,
          ymax = ci_high,
          group = factor(side)
        ),
        inherit.aes = FALSE,
        fill = "#bdbdbd",
        alpha = 0.45,
        color = NA
      ) +
      geom_line(
        data = line_grid,
        aes(x = signed_distance, y = fit, group = factor(side)),
        inherit.aes = FALSE,
        color = "#111111",
        linewidth = 0.9
      ) +
      geom_point(color = "#111111", size = 2.2, alpha = 0.98) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "#d8dce3", linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#b3b3b3", linewidth = 0.6) +
      scale_x_continuous(limits = c(-bw_ft, bw_ft), breaks = pretty(c(-bw_ft, bw_ft), n = 7)) +
      scale_y_continuous(breaks = pretty(y_limits, n = 6)) +
      labs(
        title = pretty_outcome,
        subtitle = paste(subtitle_bits, collapse = " | "),
        x = "Distance to ward boundary (ft)",
        y = paste("Residualized", pretty_outcome)
      ) +
      coord_cartesian(ylim = y_limits) +
      theme_classic(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#eceff3", linewidth = 0.4),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 10)
      )
  }
}

ggsave(output_pdf, plot = p, width = 7.4, height = 4.9, dpi = 300, bg = "white")

if (nzchar(output_csv) && !tolower(output_csv) %in% c("none", "null", "na")) {
  write_csv(
    tibble(
      figure_type = figure_type,
      bw_ft = bw_ft,
      sample_filter = sample_filter,
      fe_spec = fe_spec,
      yvar = yvar,
      cluster_unit = "ward_pair",
      target_bins_per_side = target_bins_per_side,
      actual_bins_per_side = actual_bins_per_side,
      first_lenient_bin_n = first_lenient_bin_n,
      first_strict_bin_n = first_strict_bin_n,
      first_strict_coef = first_strict_row$estimate,
      first_strict_se_clustered = first_strict_row$se_cluster,
      first_strict_se_robust = first_strict_row$se_robust,
      linear_cutoff_coef = linear_gap_estimate,
      linear_cutoff_se_clustered = linear_gap_se,
      pooled_gap_coef = gap_estimate,
      pooled_gap_se_clustered = gap_se,
      n_obs = nobs(m_resid),
      n_ward_pairs = n_distinct(aug$ward_pair),
      n_segments = n_distinct(aug$segment_id),
      output_pdf = output_pdf
    ),
    output_csv
  )
}

message(sprintf(
  "Built %s | figure=%s | y=%s | bw=%d | sample=%s | fe=%s | bins=%d | N=%d",
  output_pdf,
  figure_type,
  yvar,
  as.integer(bw_ft),
  sample_filter,
  fe_spec,
  actual_bins_per_side,
  nobs(m_resid)
))
