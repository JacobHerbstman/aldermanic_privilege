source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/nonparametric_rd_density/code")
# yvar <- "density_far"
# use_log <- TRUE
# bw_ft <- 500
# sample_filter <- "multifamily"
# gap_split <- "all"
# output_pdf <- "../output/nonparametric_rd_log_density_far_bw500_multifamily_baseline_fe.pdf"
# axis_units <- "feet"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(yvar, use_log, bw_ft, sample_filter, gap_split, output_pdf, axis_units)
}

if (length(args) == 6) {
  args <- append(args, "all", after = 4)
}

if (length(args) != 7) {
  stop(
    "FATAL: Script requires args: <yvar> <use_log> <bw_ft> <sample_filter> <gap_split> <output_pdf> <axis_units>",
    call. = FALSE
  )
}

yvar <- args[1]
use_log <- tolower(args[2]) %in% c("true", "t", "1", "yes")
bw_ft <- as.numeric(args[3])
sample_filter <- args[4]
gap_split <- tolower(args[5])
output_pdf <- args[6]
axis_units <- tolower(args[7])

if (!yvar %in% c("density_far", "density_dupac")) {
  stop("yvar must be one of: density_far, density_dupac", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive number.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample_filter must be one of: all, multifamily", call. = FALSE)
}
if (!gap_split %in% c("all", "above_median", "below_median")) {
  stop("gap_split must be one of: all, above_median, below_median", call. = FALSE)
}
if (!axis_units %in% c("feet", "meters")) {
  stop("axis_units must be one of: feet, meters", call. = FALSE)
}

rd_input_path <- Sys.getenv("RD_INPUT_PATH", "../input/parcels_with_ward_distances.csv")
rd_summary_output_path <- Sys.getenv("RD_SUMMARY_OUTPUT_PATH", "")
rd_plot_style <- Sys.getenv("RD_PLOT_STYLE", "binned_only")
rd_display_estimate_source <- Sys.getenv(
  "RD_DISPLAY_ESTIMATE_SOURCE",
  ifelse(rd_plot_style == "linear_display", "linear_cutoff", "pooled_gap")
)

if (!rd_plot_style %in% c("binned_only", "linear_display")) {
  stop("RD_PLOT_STYLE must be one of: binned_only, linear_display", call. = FALSE)
}
if (!rd_display_estimate_source %in% c("pooled_gap", "linear_cutoff")) {
  stop("RD_DISPLAY_ESTIMATE_SOURCE must be one of: pooled_gap, linear_cutoff", call. = FALSE)
}

message(sprintf("Input: %s", rd_input_path))

raw <- read_csv(rd_input_path, show_col_types = FALSE)

if (!yvar %in% names(raw)) {
  stop(sprintf("Outcome '%s' not found in input data.", yvar), call. = FALSE)
}

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
  mutate(side = as.integer(signed_distance > 0))

if (use_log) {
  dat <- dat %>%
    filter(is.finite(.data[[yvar]]), .data[[yvar]] > 0) %>%
    mutate(outcome = log(.data[[yvar]]))
} else {
  dat <- dat %>%
    filter(is.finite(.data[[yvar]])) %>%
    mutate(outcome = .data[[yvar]])
}

if (nrow(dat) == 0) {
  stop("No observations remain after baseline filters.", call. = FALSE)
}

if (gap_split != "all") {
  pair_gaps <- dat %>%
    filter(is.finite(strictness_own), is.finite(strictness_neighbor)) %>%
    group_by(ward_pair) %>%
    summarise(
      strictness_gap = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE),
      .groups = "drop"
    )

  median_gap <- median(pair_gaps$strictness_gap, na.rm = TRUE)
  if (!is.finite(median_gap)) {
    stop("Could not compute a valid median strictness gap.", call. = FALSE)
  }

  keep_pairs <- if (gap_split == "above_median") {
    pair_gaps %>% filter(strictness_gap >= median_gap) %>% pull(ward_pair)
  } else {
    pair_gaps %>% filter(strictness_gap < median_gap) %>% pull(ward_pair)
  }

  dat <- dat %>% filter(ward_pair %in% keep_pairs)
}

if (nrow(dat) == 0) {
  stop("No observations remain after gap split.", call. = FALSE)
}

controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

fml_resid <- as.formula(paste(
  "outcome ~",
  paste(controls, collapse = " + "),
  "| zone_group + segment_id + construction_year"
))

m_resid <- feols(fml_resid, data = dat)

removed <- m_resid$obs_selection$obsRemoved
if (is.null(removed)) {
  keep_idx <- seq_len(nrow(dat))
} else {
  keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
}

aug <- dat[keep_idx, , drop = FALSE]
if (nrow(aug) != nobs(m_resid)) {
  stop(sprintf("Model/sample alignment failed: kept=%d, nobs=%d", nrow(aug), nobs(m_resid)), call. = FALSE)
}

aug <- aug %>%
  mutate(residualized_outcome = as.numeric(resid(m_resid)))

fml_gap <- as.formula(paste(
  "outcome ~ side +",
  paste(controls, collapse = " + "),
  "| zone_group + segment_id + construction_year"
))

m_gap <- feols(fml_gap, data = aug, cluster = ~ward_pair)
ct_gap <- coeftable(m_gap)
gap_row <- ct_gap[rownames(ct_gap) %in% "side", , drop = FALSE]

if (nrow(gap_row) != 1) {
  stop("Could not recover pooled side-gap estimate.", call. = FALSE)
}

gap_estimate <- unname(gap_row[1, "Estimate"])
gap_se <- unname(gap_row[1, "Std. Error"])
gap_p <- unname(gap_row[1, "Pr(>|t|)"])

fml_linear_gap <- as.formula(paste(
  "outcome ~ side * signed_distance +",
  paste(controls, collapse = " + "),
  "| zone_group + segment_id + construction_year"
))

m_linear_gap <- feols(
  fml_linear_gap,
  data = aug,
  cluster = ~ward_pair
)
linear_ct <- coeftable(m_linear_gap)
linear_gap_row <- linear_ct[rownames(linear_ct) %in% "side", , drop = FALSE]

if (nrow(linear_gap_row) != 1) {
  stop("Could not recover linear cutoff estimate.", call. = FALSE)
}

linear_gap_estimate <- unname(linear_gap_row[1, "Estimate"])
linear_gap_se <- unname(linear_gap_row[1, "Std. Error"])
linear_gap_p <- unname(linear_gap_row[1, "Pr(>|t|)"])

m_linear_display <- feols(
  residualized_outcome ~ side * signed_distance,
  data = aug,
  cluster = ~ward_pair
)

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.1) return("*")
  ""
}

bins_per_side <- 30L
bin_width_ft <- bw_ft / bins_per_side
breaks_ft <- seq(-bw_ft, bw_ft, length.out = 2L * bins_per_side + 1L)

aug <- aug %>%
  mutate(
    bin_idx = pmin(
      findInterval(signed_distance, breaks_ft, rightmost.closed = TRUE, all.inside = TRUE),
      length(breaks_ft) - 1L
    ),
    bin_left_ft = breaks_ft[bin_idx],
    bin_center_ft = bin_left_ft + bin_width_ft / 2
  )

bins <- aug %>%
  group_by(bin_idx, bin_center_ft) %>%
  summarise(
    n = n(),
    mean_y = mean(residualized_outcome, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(bin_center_ft)

if (nrow(bins) == 0) {
  stop("No populated bins available for plotting.", call. = FALSE)
}

if (axis_units == "meters") {
  aug <- aug %>% mutate(signed_distance_display = signed_distance * 0.3048)
  bins <- bins %>% mutate(bin_center_display = bin_center_ft * 0.3048)
  x_limits <- c(-bw_ft, bw_ft) * 0.3048
  x_label <- "Distance to ward boundary (meters)"
  bw_label <- sprintf("%d m", as.integer(round(bw_ft * 0.3048)))
  axis_note <- "x-axis shown in meters"
} else {
  aug <- aug %>% mutate(signed_distance_display = signed_distance)
  bins <- bins %>% mutate(bin_center_display = bin_center_ft)
  x_limits <- c(-bw_ft, bw_ft)
  x_label <- "Distance to ward boundary (ft)"
  bw_label <- sprintf("%d ft", as.integer(bw_ft))
  axis_note <- NULL
}

pretty_outcome <- dplyr::case_when(
  yvar == "density_far" ~ "Log(FAR)",
  yvar == "density_dupac" ~ "Log(DUPAC)",
  TRUE ~ yvar
)
if (!use_log) {
  pretty_outcome <- dplyr::case_when(
    yvar == "density_far" ~ "FAR",
    yvar == "density_dupac" ~ "DUPAC",
    TRUE ~ yvar
  )
}

gap_label <- dplyr::case_when(
  gap_split == "above_median" ~ "above-median gap pairs",
  gap_split == "below_median" ~ "below-median gap pairs",
  TRUE ~ "all pairs"
)

display_estimate_label <- if (rd_display_estimate_source == "linear_cutoff") {
  sprintf("Linear cutoff = %.3f%s (SE %.3f)", linear_gap_estimate, stars(linear_gap_p), linear_gap_se)
} else {
  sprintf("Gap = %.3f%s (SE %.3f)", gap_estimate, stars(gap_p), gap_se)
}

subtitle_bits <- c(
  display_estimate_label,
  gap_label,
  paste0("bw = ", bw_label),
  "30 bins/side",
  paste0("N = ", nobs(m_resid)),
  axis_note
)

if (rd_plot_style == "linear_display") {
  line_grid <- tibble(
    signed_distance = c(
      seq(-bw_ft, 0, length.out = 151L),
      seq(0, bw_ft, length.out = 151L)[-1]
    )
  ) %>%
    mutate(side = as.integer(signed_distance > 0))

  coef_names <- names(coef(m_linear_display))
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

  vcov_linear <- vcov(m_linear_display)
  line_grid <- line_grid %>%
    mutate(
      fit = as.numeric(xmat %*% coef(m_linear_display)),
      fit_se = sqrt(pmax(rowSums((xmat %*% vcov_linear) * xmat), 0)),
      ci_crit = qt(0.975, df = max(n_distinct(aug$ward_pair) - 1, 1)),
      ci_low = fit - ci_crit * fit_se,
      ci_high = fit + ci_crit * fit_se
    )

  if (axis_units == "meters") {
    line_grid <- line_grid %>% mutate(signed_distance_display = signed_distance * 0.3048)
  } else {
    line_grid <- line_grid %>% mutate(signed_distance_display = signed_distance)
  }

  bin_y_min <- min(bins$mean_y, na.rm = TRUE)
  bin_y_max <- max(bins$mean_y, na.rm = TRUE)
  bin_y_span <- bin_y_max - bin_y_min

  if (!is.finite(bin_y_span) || bin_y_span <= 0) {
    bin_y_span <- max(abs(c(bin_y_min, bin_y_max, 0)), na.rm = TRUE)
  }
  if (!is.finite(bin_y_span) || bin_y_span <= 0) {
    bin_y_span <- 1
  }

  y_pad <- max(0.25 * bin_y_span, 0.1)
  y_limits <- c(min(bin_y_min, 0) - y_pad, max(bin_y_max, 0) + y_pad)

  aug_plot <- aug %>%
    filter(
      residualized_outcome >= y_limits[1],
      residualized_outcome <= y_limits[2]
    )

  p <- ggplot(bins, aes(x = bin_center_display, y = mean_y)) +
    geom_point(
      data = aug_plot,
      aes(x = signed_distance_display, y = residualized_outcome),
      inherit.aes = FALSE,
      color = "#c9cdd3",
      alpha = 0.35,
      size = 0.7,
      stroke = 0
    ) +
    geom_ribbon(
      data = line_grid,
      aes(
        x = signed_distance_display,
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
      aes(x = signed_distance_display, y = fit, group = factor(side)),
      inherit.aes = FALSE,
      color = "#111111",
      linewidth = 0.9
    ) +
    geom_point(color = "#111111", size = 2.2, alpha = 0.98) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "#d8dce3", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#b3b3b3", linewidth = 0.6) +
    scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 7)) +
    scale_y_continuous(breaks = pretty(y_limits, n = 6)) +
    labs(
      title = pretty_outcome,
      subtitle = paste(subtitle_bits[!is.na(subtitle_bits) & nzchar(subtitle_bits)], collapse = " | "),
      x = x_label,
      y = paste("Residualized", pretty_outcome)
    ) +
    coord_cartesian(ylim = y_limits) +
    theme_classic(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#eceff3", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )
} else {
  p <- ggplot(bins, aes(x = bin_center_display, y = mean_y)) +
    geom_point(color = "#2f5d8a", size = 2.1, alpha = 0.95) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#6e6e6e", linewidth = 0.6) +
    scale_x_continuous(limits = x_limits, breaks = pretty(x_limits, n = 7)) +
    labs(
      title = pretty_outcome,
      subtitle = paste(subtitle_bits[!is.na(subtitle_bits) & nzchar(subtitle_bits)], collapse = " | "),
      x = x_label,
      y = paste("Residualized", pretty_outcome)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#d8dce3", linewidth = 0.4),
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )
}

ggsave(output_pdf, plot = p, width = 7.4, height = 4.9, dpi = 300)

if (nzchar(rd_summary_output_path)) {
  write_csv(
    tibble(
      method = "nonparametric_rd",
      yvar = yvar,
      use_log = use_log,
      bw_ft = bw_ft,
      sample_filter = sample_filter,
      gap_split = gap_split,
      axis_units = axis_units,
      estimate = gap_estimate,
      se = gap_se,
      p_value = gap_p,
      n_obs = nobs(m_resid),
      n_ward_pairs = n_distinct(aug$ward_pair),
      input_path = rd_input_path,
      output_pdf = output_pdf
    ),
    rd_summary_output_path
  )
}

message(sprintf(
  "Built %s | sample=%s | gap_split=%s | y=%s | bw=%d | axis=%s | N=%d",
  output_pdf, sample_filter, gap_split, yvar, as.integer(bw_ft), axis_units, nobs(m_resid)
))
