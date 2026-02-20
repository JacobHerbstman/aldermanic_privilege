source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/nonparametric_rd_focused/code")
# yvar <- "density_far"
# use_log <- "TRUE"
# bw_ft <- 500
# bin_mode <- "kulka_fixed"
# bin_ft <- NA_real_
# fe_spec <- "pair_x_year"
# kernel <- "triangular"
# output_pdf <- "../output/nonparametric_rd_log_density_far_bw500_kulka_fixed_pair_x_year.pdf"
# output_csv <- "../output/nonparametric_rd_log_density_far_bw500_kulka_fixed_pair_x_year_bins.csv"
# Rscript nonparametric_rd_focused.R "density_far" "TRUE" 500 "kulka_fixed" NA_real_ "pair_x_year" "triangular" "../output/nonparametric_rd_log_density_far_bw500_kulka_fixed_pair_x_year.pdf" "../output/nonparametric_rd_log_density_far_bw500_kulka_fixed_pair_x_year_bins.csv"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 9) {
  yvar <- cli_args[1]
  use_log <- cli_args[2]
  bw_ft <- as.numeric(cli_args[3])
  bin_mode <- cli_args[4]
  bin_ft <- as.numeric(cli_args[5])
  fe_spec <- cli_args[6]
  kernel <- cli_args[7]
  output_pdf <- cli_args[8]
  output_csv <- cli_args[9]
} else {
  if (!exists("yvar") || !exists("use_log") || !exists("bw_ft") || !exists("bin_mode") || !exists("bin_ft") || !exists("fe_spec") || !exists("kernel") || !exists("output_pdf") || !exists("output_csv")) {
    stop("FATAL: Script requires 9 args: <yvar> <use_log> <bw_ft> <bin_mode> <bin_ft> <fe_spec> <kernel> <output_pdf> <output_csv>", call. = FALSE)
  }
}

use_log <- tolower(use_log) %in% c("true", "t", "1", "yes")
bw_ft <- as.numeric(bw_ft)
bin_mode <- bin_mode
fe_requested <- fe_spec
kernel <- kernel

if (!bin_mode %in% c("kulka_fixed", "data_driven")) {
  stop("bin_mode must be one of: kulka_fixed, data_driven", call. = FALSE)
}
if (!fe_requested %in% c("pair_x_year", "pair_year")) {
  stop("fe_spec must be one of: pair_x_year, pair_year", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive number.", call. = FALSE)
}

message("=== Nonparametric RD Focused ===")
message(sprintf("Outcome: %s | use_log=%s | bw=%.1fft", yvar, use_log, bw_ft))
message(sprintf("bin_mode=%s | fe_requested=%s", bin_mode, fe_requested))

fe_formulas <- list(
  pair_x_year = "ward_pair^construction_year",
  pair_year = "ward_pair + construction_year"
)

pretty_outcome <- function(yvar, use_log) {
  labels <- c(
    density_far = "Floor-Area Ratio (FAR)",
    density_dupac = "Dwelling Units Per Acre (DUPAC)"
  )
  base <- ifelse(yvar %in% names(labels), labels[[yvar]], yvar)
  if (isTRUE(use_log)) {
    paste0("Log(", base, ")")
  } else {
    base
  }
}

sig_stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.10) return("*")
  ""
}

choose_bin <- function(df, outcome, bw_ft, bin_mode, bin_ft, kernel) {
  if (bin_mode == "kulka_fixed") {
    if (!is.finite(bin_ft) || bin_ft <= 0) {
      stop("bin_ft must be supplied and positive for kulka_fixed mode.", call. = FALSE)
    }
    return(list(
      bin_ft = bin_ft,
      source = "kulka_fixed",
      j_left = NA_real_,
      j_right = NA_real_
    ))
  }

  rd_obj <- tryCatch(
    rdplot(
      y = outcome,
      x = df$signed_distance,
      c = 0,
      h = bw_ft,
      p = 1,
      kernel = kernel,
      binselect = "es",
      hide = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(rd_obj) || is.null(rd_obj$J) || any(!is.finite(rd_obj$J))) {
    return(list(
      bin_ft = bw_ft / 10,
      source = "data_driven_fallback_default",
      j_left = NA_real_,
      j_right = NA_real_
    ))
  }

  j_left <- as.numeric(rd_obj$J[1])
  j_right <- as.numeric(rd_obj$J[2])
  j_eff <- max(2, round(mean(c(j_left, j_right), na.rm = TRUE)))

  list(
    bin_ft = bw_ft / j_eff,
    source = "data_driven_rdplot_es",
    j_left = j_left,
    j_right = j_right
  )
}

fit_binned <- function(df, fe_name) {
  fe_formula <- fe_formulas[[fe_name]]
  fml <- as.formula(sprintf("outcome ~ i(bindex, ref = -1) | %s", fe_formula))

  est <- tryCatch(
    feols(fml, data = df, cluster = ~ward_pair),
    error = function(e) NULL
  )

  if (is.null(est)) {
    return(list(success = FALSE))
  }

  bins <- broom::tidy(est) %>%
    filter(str_starts(term, "bindex::")) %>%
    mutate(bin = suppressWarnings(as.integer(str_remove(term, "bindex::"))))

  present_bins <- sort(unique(df$bindex))
  candidate_bins <- setdiff(present_bins, -1L)
  n_candidate <- length(candidate_bins)
  n_est <- nrow(bins)
  drop_frac <- if (n_candidate > 0) (n_candidate - n_est) / n_candidate else NA_real_
  cutoff_present <- any(bins$bin == 0L)

  sparse <- isFALSE(cutoff_present) || n_est < 8 || (!is.na(drop_frac) && drop_frac > 0.30)

  cutoff_row <- bins %>% filter(bin == 0L) %>% slice_head(n = 1)
  rd_est <- if (nrow(cutoff_row) == 1) cutoff_row$estimate else NA_real_
  rd_se <- if (nrow(cutoff_row) == 1) cutoff_row$std.error else NA_real_
  rd_p <- if (nrow(cutoff_row) == 1) cutoff_row$p.value else NA_real_

  list(
    success = TRUE,
    fe_name = fe_name,
    est = est,
    bins = bins,
    n_candidate = n_candidate,
    n_est = n_est,
    drop_frac = drop_frac,
    cutoff_present = cutoff_present,
    sparse = sparse,
    rd_est = rd_est,
    rd_se = rd_se,
    rd_p = rd_p,
    n_obs = nobs(est)
  )
}

df <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount >= 2,
    unitscount <= 100,
    construction_year >= 2006,
    abs(signed_distance) <= bw_ft
  )

if (!yvar %in% names(df)) {
  stop(sprintf("yvar '%s' not found in data.", yvar), call. = FALSE)
}

if (use_log) {
  df <- df %>%
    filter(is.finite(.data[[yvar]]), .data[[yvar]] > 0) %>%
    mutate(outcome = log(.data[[yvar]]))
} else {
  df <- df %>%
    filter(is.finite(.data[[yvar]])) %>%
    mutate(outcome = .data[[yvar]])
}

df <- df %>% filter(!is.na(ward_pair), !is.na(construction_year))

if (nrow(df) == 0) {
  stop("No observations after filtering.", call. = FALSE)
}

bin_choice <- choose_bin(
  df = df,
  outcome = df$outcome,
  bw_ft = bw_ft,
  bin_mode = bin_mode,
  bin_ft = bin_ft,
  kernel = kernel
)

bin_ft_used <- bin_choice$bin_ft

df <- df %>%
  mutate(
    bindex = floor(signed_distance / bin_ft_used),
    bin_center_ft = (bindex + 0.5) * bin_ft_used
  )

fit_primary <- fit_binned(df, fe_requested)
if (!fit_primary$success) {
  stop(sprintf("Failed to estimate requested FE spec: %s", fe_requested), call. = FALSE)
}

fit_final <- fit_primary
fe_used <- fe_requested
fallback_used <- FALSE
fallback_reason <- ""

if (fe_requested == "pair_x_year" && isTRUE(fit_primary$sparse)) {
  fit_pair_year <- fit_binned(df, "pair_year")
  if (fit_pair_year$success) {
    fit_final <- fit_pair_year
    fe_used <- "pair_year"
    fallback_used <- TRUE
    fallback_reason <- sprintf(
      "pair_x_year sparse/collapsed (cutoff=%s, n_bins=%d, drop_frac=%.3f)",
      fit_primary$cutoff_present,
      fit_primary$n_est,
      fit_primary$drop_frac
    )
  } else {
    fallback_reason <- "pair_x_year sparse/collapsed but fallback pair_year failed"
  }
}

bins_out <- fit_final$bins %>%
  mutate(
    bin_center_ft = (bin + 0.5) * bin_ft_used,
    bin_center_mi = bin_center_ft / 5280,
    lo95 = estimate - 1.96 * std.error,
    hi95 = estimate + 1.96 * std.error,
    side = if_else(bin_center_ft >= 0, "strict", "lenient"),
    is_normalization_point = FALSE
  ) %>%
  arrange(bin_center_ft)

if (nrow(bins_out) == 0) {
  stop("No bin coefficients estimated.", call. = FALSE)
}

norm_point <- tibble(
  term = "bindex::-1_normalized",
  estimate = 0,
  std.error = 0,
  statistic = NA_real_,
  p.value = NA_real_,
  bin = -1L,
  bin_center_ft = (-0.5) * bin_ft_used,
  bin_center_mi = ((-0.5) * bin_ft_used) / 5280,
  lo95 = 0,
  hi95 = 0,
  side = "lenient",
  is_normalization_point = TRUE
)

bins_plot <- bind_rows(bins_out, norm_point) %>%
  arrange(bin_center_ft)

rd_stars <- sig_stars(fit_final$rd_p)
rd_display <- if (is.finite(fit_final$rd_est) && is.finite(fit_final$rd_se)) {
  sprintf(
    "RD bin 0 (first strict bin) = %.3f%s (SE %.3f)",
    fit_final$rd_est,
    rd_stars,
    fit_final$rd_se
  )
} else {
  "RD bin 0 (first strict bin) unavailable"
}

subtitle <- sprintf(
  "%s | bw=%.0fft | bin=%.1fft (%s) | FE requested=%s, used=%s | N=%d",
  rd_display, bw_ft, bin_ft_used, bin_mode, fe_requested, fe_used, fit_final$n_obs
)
if (fallback_used) {
  subtitle <- paste0(subtitle, " | fallback=TRUE")
}

left_bins <- bins_plot %>% filter(side == "lenient")
right_bins <- bins_plot %>% filter(side == "strict")

p <- ggplot() +
  geom_ribbon(
    data = left_bins,
    aes(x = bin_center_mi, ymin = lo95, ymax = hi95),
    fill = "#1f77b4",
    alpha = 0.22
  ) +
  geom_ribbon(
    data = right_bins,
    aes(x = bin_center_mi, ymin = lo95, ymax = hi95),
    fill = "#d62728",
    alpha = 0.22
  ) +
  geom_line(
    data = left_bins,
    aes(x = bin_center_mi, y = estimate),
    color = "#1f77b4",
    linewidth = 1
  ) +
  geom_line(
    data = right_bins,
    aes(x = bin_center_mi, y = estimate),
    color = "#d62728",
    linewidth = 1
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = paste0("Nonparametric Border RD: ", pretty_outcome(yvar, use_log)),
    subtitle = subtitle,
    x = "Distance to boundary (miles; right is stricter side)",
    y = pretty_outcome(yvar, use_log),
    caption = "Reference bin = [-bin, 0). Shaded bands are 95% CI; SE clustered by ward pair."
  ) +
  theme_bw(base_size = 11)

ggsave(output_pdf, p, width = 8.5, height = 5.8, dpi = 300)

meta <- tibble(
  yvar = yvar,
  use_log = use_log,
  bw_ft = bw_ft,
  bin_mode = bin_mode,
  bin_ft_requested = bin_ft,
  bin_ft_used = bin_ft_used,
  bin_source = bin_choice$source,
  j_left = bin_choice$j_left,
  j_right = bin_choice$j_right,
  fe_requested = fe_requested,
  fe_used = fe_used,
  fallback_used = fallback_used,
  fallback_reason = fallback_reason,
  n_obs = fit_final$n_obs,
  n_bins_candidate = fit_final$n_candidate,
  n_bins_estimated = fit_final$n_est,
  drop_frac = fit_final$drop_frac,
  cutoff_bin_present = fit_final$cutoff_present,
  rd_estimate = fit_final$rd_est,
  rd_se = fit_final$rd_se,
  rd_p = fit_final$rd_p,
  rd_stars = rd_stars,
  rd_label = rd_display,
  outcome_mean = mean(df$outcome, na.rm = TRUE)
)

bins_write <- bins_plot %>%
  mutate(
    yvar = meta$yvar,
    use_log = meta$use_log,
    bw_ft = meta$bw_ft,
    bin_mode = meta$bin_mode,
    bin_ft_used = meta$bin_ft_used,
    fe_requested = meta$fe_requested,
    fe_used = meta$fe_used,
    fallback_used = meta$fallback_used,
    n_obs = meta$n_obs,
    n_bins_candidate = meta$n_bins_candidate,
    n_bins_estimated = meta$n_bins_estimated,
    drop_frac = meta$drop_frac,
    cutoff_bin_present = meta$cutoff_bin_present,
    rd_estimate = meta$rd_estimate,
    rd_se = meta$rd_se,
    rd_p = meta$rd_p,
    rd_stars = meta$rd_stars,
    rd_label = meta$rd_label
  )

write_csv(bins_write, output_csv)

meta_path <- if (str_detect(output_csv, "\\.csv$")) {
  str_replace(output_csv, "\\.csv$", "_meta.csv")
} else {
  paste0(output_csv, "_meta.csv")
}
write_csv(meta, meta_path)

message("Saved:")
message(sprintf("  - %s", output_pdf))
message(sprintf("  - %s", output_csv))
message(sprintf("  - %s", meta_path))