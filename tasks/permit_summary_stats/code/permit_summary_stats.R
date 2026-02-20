source("../../setup_environment/code/packages.R")
library(fixest)

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/permit_summary_stats/code")
# input <- "../input/permits_for_uncertainty_index.csv"
# output_dir <- "../output"
# Rscript permit_summary_stats.R "../input/permits_for_uncertainty_index.csv" "../output"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 2) {
  input <- cli_args[1]
  output_dir <- cli_args[2]
} else {
  if (!exists("input") || !exists("output_dir")) {
    stop("FATAL: Script requires 2 args: <input> <output_dir>", call. = FALSE)
  }
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

high_discretion_types <- c("new_construction", "renovation", "demolition")
small_sample_threshold <- 30L
min_alderman_month_n <- 3L
cutoff_month <- as.yearmon("2019-07")
covid_start_month <- as.yearmon("2020-03")
covid_end_month <- as.yearmon("2021-12")

type_label <- c(
  new_construction = "New construction",
  renovation = "Renovation",
  demolition = "Demolition"
)

month_to_str <- function(x) {
  format(as.Date(x), "%Y-%m")
}

safe_quantile <- function(x, prob) {
  as.numeric(quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

safe_sd <- function(x) {
  if (length(x) <= 1) {
    return(NA_real_)
  }
  as.numeric(sd(x, na.rm = TRUE))
}

safe_iqr <- function(x) {
  as.numeric(IQR(x, na.rm = TRUE))
}

trim_alderman <- function(x) {
  gsub("\\s+", " ", trimws(as.character(x)))
}

permits <- read_csv(input, show_col_types = FALSE) %>%
  mutate(
    month = as.yearmon(month),
    median_hh_income_10k = median_hh_income / 10000,
    pop_total_10k = pop_total / 10000
  )

place_covariates <- c("dist_cbd_km", "dist_lake_km", "n_rail_stations_800m")
if (!all(place_covariates %in% names(permits))) {
  place_covariates <- c("dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m")
}

covariates <- c(
  "median_hh_income_10k", "share_black", "share_hisp", "share_white",
  "homeownership_rate", "share_bach_plus", "pop_total_10k",
  place_covariates
)

high_discretion <- permits %>%
  filter(permit_type_clean %in% high_discretion_types)

new_construction <- high_discretion %>%
  filter(permit_type_clean == "new_construction")

if (nrow(high_discretion) == 0) {
  stop("No high-discretion permits found.", call. = FALSE)
}
if (nrow(new_construction) == 0) {
  stop("No new construction permits found.", call. = FALSE)
}

# ==============================================================================
# Existing count outputs (kept)
# ==============================================================================
counts_table <- high_discretion %>%
  count(permit_type_clean, name = "n_permits") %>%
  mutate(
    permit_type = type_label[permit_type_clean],
    share = n_permits / sum(n_permits)
  ) %>%
  arrange(match(permit_type_clean, high_discretion_types)) %>%
  select(permit_type, n_permits, share)

counts_with_total <- bind_rows(
  counts_table,
  tibble(permit_type = "Total", n_permits = sum(counts_table$n_permits), share = 1)
)

write_csv(
  counts_with_total %>% mutate(share_pct = 100 * share),
  file.path(output_dir, "high_discretion_permit_counts.csv")
)

counts_tex <- file.path(output_dir, "high_discretion_permit_counts.tex")
cat(
  "\\begin{tabular}{lrr}\n",
  "\\toprule\n",
  "Permit type & Count & Share (\\%) \\\\\n",
  "\\midrule\n",
  file = counts_tex
)

for (i in seq_len(nrow(counts_with_total))) {
  row <- counts_with_total[i, ]
  cat(
    sprintf(
      "%s & %s & %.1f \\\\\n",
      row$permit_type,
      format(row$n_permits, big.mark = ","),
      100 * row$share
    ),
    file = counts_tex,
    append = TRUE
  )
}

cat("\\bottomrule\n\\end{tabular}\n", file = counts_tex, append = TRUE)

# ==============================================================================
# Existing distribution plots (kept)
# ==============================================================================
residualize_processing <- function(df) {
  fml <- as.formula(
    paste0("log_processing_time ~ ", paste(covariates, collapse = " + "), " | month")
  )

  model <- feols(fml, data = df, warn = FALSE)

  removed <- model$obs_selection$obsRemoved
  keep_idx <- if (is.null(removed)) {
    seq_len(nrow(df))
  } else {
    setdiff(seq_len(nrow(df)), abs(as.integer(removed)))
  }

  aligned <- df[keep_idx, , drop = FALSE]
  if (nrow(aligned) != nobs(model)) {
    stop("Residual alignment failed.", call. = FALSE)
  }

  aligned %>%
    mutate(residual_log_processing_time = as.numeric(resid(model)))
}

plot_distribution <- function(df, x_var, x_label, title, subtitle, output_file) {
  p <- ggplot(df, aes(x = .data[[x_var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "#88b2ac", color = NA, alpha = 0.9) +
    geom_density(color = "#1f3c4a", linewidth = 1) +
    theme_bw(base_size = 12) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = "Density"
    )
  ggsave(output_file, p, width = 9, height = 5.5, dpi = 300)
}

raw_high_discretion <- high_discretion %>%
  filter(is.finite(log_processing_time), processing_time > 0)

raw_new_construction <- new_construction %>%
  filter(is.finite(log_processing_time), processing_time > 0)

resid_high_discretion <- high_discretion %>%
  filter(
    processing_time > 0,
    is.finite(log_processing_time),
    !is.na(month),
    if_all(all_of(covariates), ~ is.finite(.x))
  ) %>%
  residualize_processing()

resid_new_construction <- new_construction %>%
  filter(
    processing_time > 0,
    is.finite(log_processing_time),
    !is.na(month),
    if_all(all_of(covariates), ~ is.finite(.x))
  ) %>%
  residualize_processing()

plot_distribution(
  df = raw_high_discretion,
  x_var = "log_processing_time",
  x_label = "Log processing time",
  title = "Raw Log Processing Time Distribution",
  subtitle = sprintf("All high-discretion permits | N = %s", format(nrow(raw_high_discretion), big.mark = ",")),
  output_file = file.path(output_dir, "processing_time_raw_high_discretion.pdf")
)

plot_distribution(
  df = raw_new_construction,
  x_var = "log_processing_time",
  x_label = "Log processing time",
  title = "Raw Log Processing Time Distribution",
  subtitle = sprintf("New construction only | N = %s", format(nrow(raw_new_construction), big.mark = ",")),
  output_file = file.path(output_dir, "processing_time_raw_new_construction.pdf")
)

plot_distribution(
  df = resid_high_discretion,
  x_var = "residual_log_processing_time",
  x_label = "Residualized log processing time",
  title = "Residualized Processing Time Distribution",
  subtitle = sprintf("All high-discretion permits | N = %s", format(nrow(resid_high_discretion), big.mark = ",")),
  output_file = file.path(output_dir, "processing_time_residualized_high_discretion.pdf")
)

plot_distribution(
  df = resid_new_construction,
  x_var = "residual_log_processing_time",
  x_label = "Residualized log processing time",
  title = "Residualized Processing Time Distribution",
  subtitle = sprintf("New construction only | N = %s", format(nrow(resid_new_construction), big.mark = ",")),
  output_file = file.path(output_dir, "processing_time_residualized_new_construction.pdf")
)

# ==============================================================================
# New raw summary outputs
# ==============================================================================
raw_core <- high_discretion %>%
  filter(
    permit_type_clean %in% high_discretion_types,
    processing_time > 0,
    is.finite(log_processing_time)
  ) %>%
  mutate(
    alderman = trim_alderman(alderman),
    permit_type = type_label[permit_type_clean]
  ) %>%
  filter(!is.na(alderman), alderman != "")

if (nrow(raw_core) == 0) {
  stop("No valid raw core permits found after filtering.", call. = FALSE)
}

summarize_distribution <- function(df, group_vars, threshold = 30L) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_permits = n(),
      processing_time_mean = mean(processing_time, na.rm = TRUE),
      processing_time_sd = safe_sd(processing_time),
      processing_time_min = min(processing_time, na.rm = TRUE),
      processing_time_p10 = safe_quantile(processing_time, 0.10),
      processing_time_p25 = safe_quantile(processing_time, 0.25),
      processing_time_median = median(processing_time, na.rm = TRUE),
      processing_time_p75 = safe_quantile(processing_time, 0.75),
      processing_time_p90 = safe_quantile(processing_time, 0.90),
      processing_time_max = max(processing_time, na.rm = TRUE),
      processing_time_iqr = safe_iqr(processing_time),
      log_processing_time_mean = mean(log_processing_time, na.rm = TRUE),
      log_processing_time_sd = safe_sd(log_processing_time),
      log_processing_time_min = min(log_processing_time, na.rm = TRUE),
      log_processing_time_p10 = safe_quantile(log_processing_time, 0.10),
      log_processing_time_p25 = safe_quantile(log_processing_time, 0.25),
      log_processing_time_median = median(log_processing_time, na.rm = TRUE),
      log_processing_time_p75 = safe_quantile(log_processing_time, 0.75),
      log_processing_time_p90 = safe_quantile(log_processing_time, 0.90),
      log_processing_time_max = max(log_processing_time, na.rm = TRUE),
      log_processing_time_iqr = safe_iqr(log_processing_time),
      .groups = "drop"
    ) %>%
    mutate(
      small_sample_threshold = threshold,
      small_sample_flag_n30 = n_permits < threshold
    )
}

alderman_distribution <- summarize_distribution(raw_core, "alderman", threshold = small_sample_threshold)

alderman_composition <- raw_core %>%
  group_by(alderman) %>%
  summarise(
    n_new_construction = sum(permit_type_clean == "new_construction"),
    n_renovation = sum(permit_type_clean == "renovation"),
    n_demolition = sum(permit_type_clean == "demolition"),
    total_n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share_new_construction = n_new_construction / total_n,
    share_renovation = n_renovation / total_n,
    share_demolition = n_demolition / total_n
  ) %>%
  select(-total_n)

alderman_distribution <- alderman_distribution %>%
  left_join(alderman_composition, by = "alderman") %>%
  relocate(
    n_new_construction, n_renovation, n_demolition,
    share_new_construction, share_renovation, share_demolition,
    .after = n_permits
  ) %>%
  arrange(desc(n_permits), alderman)

write_csv(
  alderman_distribution,
  file.path(output_dir, "processing_time_distribution_by_alderman.csv")
)

permit_type_distribution <- summarize_distribution(raw_core, "permit_type_clean", threshold = small_sample_threshold) %>%
  mutate(permit_type = type_label[permit_type_clean]) %>%
  relocate(permit_type, .after = permit_type_clean) %>%
  arrange(match(permit_type_clean, high_discretion_types))

write_csv(
  permit_type_distribution,
  file.path(output_dir, "processing_time_distribution_by_permit_type.csv")
)

alderman_type_distribution <- summarize_distribution(raw_core, c("alderman", "permit_type_clean"), threshold = small_sample_threshold) %>%
  mutate(permit_type = type_label[permit_type_clean]) %>%
  relocate(permit_type, .after = permit_type_clean) %>%
  arrange(alderman, match(permit_type_clean, high_discretion_types))

write_csv(
  alderman_type_distribution,
  file.path(output_dir, "processing_time_distribution_by_alderman_type.csv")
)

variation_across_aldermen_by_type <- alderman_type_distribution %>%
  group_by(permit_type_clean, permit_type) %>%
  summarise(
    n_aldermen = n(),
    alderman_n_permits_mean = mean(n_permits, na.rm = TRUE),
    alderman_n_permits_p10 = safe_quantile(n_permits, 0.10),
    alderman_n_permits_median = median(n_permits, na.rm = TRUE),
    alderman_n_permits_p90 = safe_quantile(n_permits, 0.90),
    processing_time_mean_across_aldermen_sd = safe_sd(processing_time_mean),
    processing_time_mean_across_aldermen_iqr = safe_iqr(processing_time_mean),
    processing_time_mean_across_aldermen_p10 = safe_quantile(processing_time_mean, 0.10),
    processing_time_mean_across_aldermen_median = median(processing_time_mean, na.rm = TRUE),
    processing_time_mean_across_aldermen_p90 = safe_quantile(processing_time_mean, 0.90),
    processing_time_median_across_aldermen_sd = safe_sd(processing_time_median),
    processing_time_median_across_aldermen_iqr = safe_iqr(processing_time_median),
    processing_time_median_across_aldermen_p10 = safe_quantile(processing_time_median, 0.10),
    processing_time_median_across_aldermen_median = median(processing_time_median, na.rm = TRUE),
    processing_time_median_across_aldermen_p90 = safe_quantile(processing_time_median, 0.90),
    log_processing_time_mean_across_aldermen_sd = safe_sd(log_processing_time_mean),
    log_processing_time_mean_across_aldermen_iqr = safe_iqr(log_processing_time_mean),
    log_processing_time_mean_across_aldermen_p10 = safe_quantile(log_processing_time_mean, 0.10),
    log_processing_time_mean_across_aldermen_median = median(log_processing_time_mean, na.rm = TRUE),
    log_processing_time_mean_across_aldermen_p90 = safe_quantile(log_processing_time_mean, 0.90),
    .groups = "drop"
  ) %>%
  arrange(match(permit_type_clean, high_discretion_types))

write_csv(
  variation_across_aldermen_by_type,
  file.path(output_dir, "processing_time_variation_across_aldermen_by_type.csv")
)

# ==============================================================================
# New convergence outputs around July 1, 2019
# ==============================================================================
add_type_groups <- function(df) {
  bind_rows(
    df %>% mutate(type_group = "all_core_types"),
    df %>% mutate(type_group = permit_type_clean)
  )
}

convergence_data <- add_type_groups(raw_core)

cross_alderman_monthly <- convergence_data %>%
  group_by(type_group, month, alderman) %>%
  summarise(
    cell_n_permits = n(),
    mean_log_processing_time = mean(log_processing_time, na.rm = TRUE),
    mean_processing_time = mean(processing_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(cell_n_permits >= min_alderman_month_n) %>%
  group_by(type_group, month) %>%
  summarise(
    n_aldermen_included = n(),
    sigma_mean_log = safe_sd(mean_log_processing_time),
    iqr_mean_log = safe_iqr(mean_log_processing_time),
    sigma_mean_days = safe_sd(mean_processing_time),
    iqr_mean_days = safe_iqr(mean_processing_time),
    .groups = "drop"
  ) %>%
  mutate(
    post_july2019 = month >= cutoff_month
  ) %>%
  arrange(type_group, month)

overall_monthly <- convergence_data %>%
  group_by(type_group, month) %>%
  summarise(
    n_permits = n(),
    sd_log = safe_sd(log_processing_time),
    iqr_log = safe_iqr(log_processing_time),
    sd_days = safe_sd(processing_time),
    iqr_days = safe_iqr(processing_time),
    .groups = "drop"
  ) %>%
  mutate(
    post_july2019 = month >= cutoff_month
  ) %>%
  arrange(type_group, month)

write_csv(
  cross_alderman_monthly %>%
    mutate(month = month_to_str(month)),
  file.path(output_dir, "permit_processing_convergence_monthly_cross_alderman.csv")
)

write_csv(
  overall_monthly %>%
    mutate(month = month_to_str(month)),
  file.path(output_dir, "permit_processing_convergence_monthly_overall.csv")
)

compute_segmented_regression <- function(df, cutoff) {
  if (nrow(df) < 8) {
    return(list(
      b1_pretrend = NA_real_, b1_pretrend_pvalue = NA_real_,
      b2_level_shift = NA_real_, b2_level_shift_pvalue = NA_real_,
      b3_slope_shift = NA_real_, b3_slope_shift_pvalue = NA_real_
    ))
  }

  reg_df <- df %>%
    arrange(month) %>%
    mutate(
      t_index = seq_len(n()),
      post = as.integer(month >= cutoff),
      t_post = t_index * post
    )

  if (length(unique(reg_df$post)) < 2) {
    return(list(
      b1_pretrend = NA_real_, b1_pretrend_pvalue = NA_real_,
      b2_level_shift = NA_real_, b2_level_shift_pvalue = NA_real_,
      b3_slope_shift = NA_real_, b3_slope_shift_pvalue = NA_real_
    ))
  }

  fit <- tryCatch(
    lm(value ~ t_index + post + t_post, data = reg_df),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(list(
      b1_pretrend = NA_real_, b1_pretrend_pvalue = NA_real_,
      b2_level_shift = NA_real_, b2_level_shift_pvalue = NA_real_,
      b3_slope_shift = NA_real_, b3_slope_shift_pvalue = NA_real_
    ))
  }

  coef_tbl <- summary(fit)$coefficients
  get_coef <- function(term, col) {
    if (!term %in% rownames(coef_tbl)) {
      return(NA_real_)
    }
    as.numeric(coef_tbl[term, col])
  }

  list(
    b1_pretrend = get_coef("t_index", "Estimate"),
    b1_pretrend_pvalue = get_coef("t_index", "Pr(>|t|)"),
    b2_level_shift = get_coef("post", "Estimate"),
    b2_level_shift_pvalue = get_coef("post", "Pr(>|t|)"),
    b3_slope_shift = get_coef("t_post", "Estimate"),
    b3_slope_shift_pvalue = get_coef("t_post", "Pr(>|t|)")
  )
}

compute_prepost_row <- function(series_df, keys, cutoff, window_name) {
  series_clean <- series_df %>%
    filter(is.finite(value)) %>%
    arrange(month)

  pre_vals <- series_clean %>%
    filter(month < cutoff) %>%
    pull(value)
  post_vals <- series_clean %>%
    filter(month >= cutoff) %>%
    pull(value)

  welch_p <- if (length(pre_vals) >= 2 && length(post_vals) >= 2) {
    tryCatch(
      t.test(post_vals, pre_vals, var.equal = FALSE)$p.value,
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }

  pre_mean <- if (length(pre_vals) > 0) mean(pre_vals, na.rm = TRUE) else NA_real_
  post_mean <- if (length(post_vals) > 0) mean(post_vals, na.rm = TRUE) else NA_real_
  post_minus_pre <- post_mean - pre_mean
  post_over_pre <- if (is.na(pre_mean) || pre_mean == 0) NA_real_ else post_mean / pre_mean

  seg <- compute_segmented_regression(series_clean, cutoff)

  tibble(
    source_series = keys$source_series[[1]],
    type_group = keys$type_group[[1]],
    metric_id = keys$metric_id[[1]],
    window = window_name,
    pre_months = length(pre_vals),
    post_months = length(post_vals),
    pre_mean = pre_mean,
    post_mean = post_mean,
    post_minus_pre = post_minus_pre,
    post_over_pre = post_over_pre,
    welch_t_pvalue = welch_p,
    b1_pretrend = seg$b1_pretrend,
    b1_pretrend_pvalue = seg$b1_pretrend_pvalue,
    b2_level_shift = seg$b2_level_shift,
    b2_level_shift_pvalue = seg$b2_level_shift_pvalue,
    b3_slope_shift = seg$b3_slope_shift,
    b3_slope_shift_pvalue = seg$b3_slope_shift_pvalue
  )
}

build_prepost_tests <- function(series_long, window_name, exclude_covid = FALSE) {
  data_window <- series_long
  if (exclude_covid) {
    data_window <- data_window %>%
      filter(month < covid_start_month | month > covid_end_month)
  }

  if (nrow(data_window) == 0) {
    return(tibble())
  }

  split_groups <- split(
    data_window,
    interaction(data_window$source_series, data_window$type_group, data_window$metric_id, drop = TRUE)
  )

  bind_rows(lapply(split_groups, function(df_group) {
    keys <- df_group %>%
      slice(1) %>%
      select(source_series, type_group, metric_id)

    compute_prepost_row(
      series_df = df_group %>% select(month, value),
      keys = keys,
      cutoff = cutoff_month,
      window_name = window_name
    )
  }))
}

cross_metrics <- c("sigma_mean_log", "iqr_mean_log", "sigma_mean_days", "iqr_mean_days")
overall_metrics <- c("sd_log", "iqr_log", "sd_days", "iqr_days")

cross_long <- cross_alderman_monthly %>%
  select(type_group, month, all_of(cross_metrics)) %>%
  pivot_longer(
    cols = all_of(cross_metrics),
    names_to = "metric_id",
    values_to = "value"
  ) %>%
  mutate(source_series = "cross_alderman")

overall_long <- overall_monthly %>%
  select(type_group, month, all_of(overall_metrics)) %>%
  pivot_longer(
    cols = all_of(overall_metrics),
    names_to = "metric_id",
    values_to = "value"
  ) %>%
  mutate(source_series = "overall_permit")

all_series_long <- bind_rows(cross_long, overall_long)

prepost_tests <- bind_rows(
  build_prepost_tests(
    series_long = all_series_long,
    window_name = "full_sample",
    exclude_covid = FALSE
  ),
  build_prepost_tests(
    series_long = all_series_long,
    window_name = "exclude_covid_2020_03_to_2021_12",
    exclude_covid = TRUE
  )
) %>%
  arrange(source_series, metric_id, type_group, window)

write_csv(
  prepost_tests,
  file.path(output_dir, "permit_processing_convergence_prepost_tests.csv")
)

type_group_levels <- c("all_core_types", "new_construction", "renovation", "demolition")
type_group_labels <- c(
  all_core_types = "All core types",
  new_construction = "New construction",
  renovation = "Renovation",
  demolition = "Demolition"
)

plot_cross <- cross_alderman_monthly %>%
  select(month, type_group, value = sigma_mean_log) %>%
  mutate(panel = "Cross-Alderman Dispersion (SD of alderman-month mean log time)")

plot_overall <- overall_monthly %>%
  select(month, type_group, value = sd_log) %>%
  mutate(panel = "Overall Permit Dispersion (SD of log processing time)")

plot_df <- bind_rows(plot_cross, plot_overall) %>%
  filter(type_group %in% type_group_levels) %>%
  mutate(
    type_group = factor(type_group, levels = type_group_levels, labels = type_group_labels[type_group_levels])
  ) %>%
  filter(is.finite(value))

p_convergence <- ggplot(plot_df, aes(x = as.Date(month), y = value, color = type_group)) +
  geom_rect(
    data = tibble(
      xmin = as.Date(covid_start_month),
      xmax = as.Date(covid_end_month, frac = 1),
      ymin = -Inf,
      ymax = Inf
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "gray80",
    alpha = 0.35
  ) +
  geom_vline(xintercept = as.Date(cutoff_month), linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_line(linewidth = 0.9, alpha = 0.95) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("#1f77b4", "#d62728", "#2ca02c", "#9467bd")) +
  labs(
    title = "Permit Processing Dispersion Around July 1, 2019",
    subtitle = "Dashed line: July 2019 cutoff. Gray band: March 2020-December 2021.",
    x = "Month",
    y = "Dispersion",
    color = "Type group"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "white")
  )

ggsave(
  filename = file.path(output_dir, "permit_processing_convergence_july2019.pdf"),
  plot = p_convergence,
  width = 10,
  height = 8,
  dpi = 300
)

message("Saved outputs in: ", output_dir)
