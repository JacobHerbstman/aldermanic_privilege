source("../../setup_environment/code/packages.R")
library(fixest)

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/permit_summary_stats/code")
# Rscript permit_summary_stats.R "../input/permits_for_uncertainty_index.csv" "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH.csv" "../output"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 3) {
  permits_input <- cli_args[1]
  scores_input <- cli_args[2]
  output_dir <- cli_args[3]
} else {
  if (!exists("permits_input") || !exists("scores_input") || !exists("output_dir")) {
    stop("FATAL: Script requires 3 args: <permits_input> <scores_input> <output_dir>", call. = FALSE)
  }
}

high_discretion_types <- c("new_construction", "renovation", "demolition")
sample_levels <- c("all_high_discretion", "new_construction")
sample_label <- c(
  all_high_discretion = "All high-discretion permits",
  new_construction = "New construction only"
)
series_label <- c(
  processing_time_days = "Raw days",
  processing_time_days_w99 = "Winsorized days (p99 upper-tail cap)",
  log_processing_time = "Log days"
)
tail_probabilities <- c(p95 = 0.95, p99 = 0.99)
entity_levels <- c("alderman", "ward")
entity_level_label <- c(
  alderman = "Alderman",
  ward = "Ward"
)
small_sample_threshold <- 30L
min_alderman_month_n <- 3L
winsor_prob <- 0.99
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

safe_mean <- function(x) {
  if (!any(is.finite(x))) {
    return(NA_real_)
  }
  as.numeric(mean(x, na.rm = TRUE))
}

safe_sd <- function(x) {
  if (length(x) <= 1) {
    return(NA_real_)
  }
  as.numeric(sd(x, na.rm = TRUE))
}

safe_min <- function(x) {
  if (!any(is.finite(x))) {
    return(NA_real_)
  }
  as.numeric(min(x, na.rm = TRUE))
}

safe_max <- function(x) {
  if (!any(is.finite(x))) {
    return(NA_real_)
  }
  as.numeric(max(x, na.rm = TRUE))
}

safe_iqr <- function(x) {
  as.numeric(IQR(x, na.rm = TRUE))
}

safe_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 2) {
    return(NA_real_)
  }

  x_sd <- sd(x)
  if (!is.finite(x_sd) || x_sd == 0) {
    return(0)
  }

  as.numeric(mean((x - mean(x))^3) / (x_sd^3))
}

safe_share <- function(num, denom) {
  out <- num / denom
  out[!is.finite(denom) | denom == 0] <- NA_real_
  as.numeric(out)
}

trim_alderman <- function(x) {
  gsub("\\s+", " ", trimws(as.character(x)))
}

winsorize_upper <- function(x, prob = winsor_prob) {
  pmin(x, safe_quantile(x, prob))
}

fmt_num <- function(x, digits = 1) {
  if (!is.finite(x)) {
    return("")
  }
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

extract_term_stats <- function(model, term) {
  coef_tbl <- as.data.frame(coeftable(model))
  if (!term %in% rownames(coef_tbl)) {
    return(tibble(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_))
  }

  tibble(
    estimate = as.numeric(coef_tbl[term, "Estimate"]),
    std_error = as.numeric(coef_tbl[term, "Std. Error"]),
    p_value = as.numeric(coef_tbl[term, "Pr(>|t|)"])
  )
}

compute_hhi <- function(x) {
  total_x <- sum(x, na.rm = TRUE)
  if (!is.finite(total_x) || total_x <= 0) {
    return(NA_real_)
  }

  shares <- x / total_x
  as.numeric(sum(shares^2, na.rm = TRUE))
}

compute_top_share <- function(x, n_top) {
  total_x <- sum(x, na.rm = TRUE)
  if (!is.finite(total_x) || total_x <= 0) {
    return(NA_real_)
  }

  as.numeric(sum(sort(x, decreasing = TRUE)[seq_len(min(n_top, length(x)))], na.rm = TRUE) / total_x)
}

format_entity_display <- function(entity_level, entity_value) {
  if (entity_level == "ward") {
    return(paste("Ward", entity_value))
  }
  str_trunc(entity_value, width = 28)
}

summarize_series <- function(x, sample_id, sample_name, series_id, series_name, winsor_cap = NA_real_) {
  tibble(
    sample_id = sample_id,
    sample = sample_name,
    series_id = series_id,
    series = series_name,
    n = sum(is.finite(x)),
    mean = safe_mean(x),
    sd = safe_sd(x),
    min = safe_min(x),
    p01 = safe_quantile(x, 0.01),
    p05 = safe_quantile(x, 0.05),
    p10 = safe_quantile(x, 0.10),
    p25 = safe_quantile(x, 0.25),
    median = safe_quantile(x, 0.50),
    p75 = safe_quantile(x, 0.75),
    p90 = safe_quantile(x, 0.90),
    p95 = safe_quantile(x, 0.95),
    p99 = safe_quantile(x, 0.99),
    max = safe_max(x),
    iqr = safe_iqr(x),
    skewness = safe_skewness(x),
    winsor_cap_99 = winsor_cap
  )
}

permits <- read_csv(permits_input, show_col_types = FALSE) %>%
  mutate(
    alderman = trim_alderman(alderman),
    month = as.yearmon(month),
    median_hh_income_10k = median_hh_income / 10000,
    pop_total_10k = pop_total / 10000
  )

scores <- read_csv(scores_input, show_col_types = FALSE) %>%
  transmute(
    alderman = trim_alderman(alderman),
    uncertainty_index = as.numeric(uncertainty_index)
  ) %>%
  filter(is.finite(uncertainty_index)) %>%
  group_by(alderman) %>%
  summarise(uncertainty_index = mean(uncertainty_index, na.rm = TRUE), .groups = "drop")

if (nrow(scores) == 0) {
  stop("No non-missing uncertainty_index values found in score input.", call. = FALSE)
}

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

sample_data <- list(
  all_high_discretion = high_discretion,
  new_construction = new_construction
)

sample_frames <- setNames(
  lapply(names(sample_data), function(sample_id) {
    df <- sample_data[[sample_id]] %>%
      filter(processing_time > 0, is.finite(processing_time), is.finite(log_processing_time))

    cap_99 <- safe_quantile(df$processing_time, winsor_prob)

    df %>%
      mutate(
        sample_id = sample_id,
        sample = unname(sample_label[sample_id]),
        processing_time_days_w99 = pmin(processing_time, cap_99),
        winsor_cap_99 = cap_99
      )
  }),
  names(sample_data)
)

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
# New overall summaries and skew diagnostics
# ==============================================================================
overall_summary_stats <- bind_rows(
  lapply(names(sample_frames), function(sample_id) {
    df <- sample_frames[[sample_id]]
    bind_rows(
      summarize_series(
        x = df$processing_time,
        sample_id = sample_id,
        sample_name = unname(sample_label[sample_id]),
        series_id = "processing_time_days",
        series_name = unname(series_label["processing_time_days"]),
        winsor_cap = unique(df$winsor_cap_99)[1]
      ),
      summarize_series(
        x = df$processing_time_days_w99,
        sample_id = sample_id,
        sample_name = unname(sample_label[sample_id]),
        series_id = "processing_time_days_w99",
        series_name = unname(series_label["processing_time_days_w99"]),
        winsor_cap = unique(df$winsor_cap_99)[1]
      ),
      summarize_series(
        x = df$log_processing_time,
        sample_id = sample_id,
        sample_name = unname(sample_label[sample_id]),
        series_id = "log_processing_time",
        series_name = unname(series_label["log_processing_time"])
      )
    )
  })
) %>%
  mutate(
    sample_id = factor(sample_id, levels = sample_levels),
    series_id = factor(series_id, levels = c("processing_time_days", "processing_time_days_w99", "log_processing_time"))
  ) %>%
  arrange(sample_id, series_id) %>%
  mutate(
    sample_id = as.character(sample_id),
    series_id = as.character(series_id)
  )

write_csv(
  overall_summary_stats,
  file.path(output_dir, "permit_processing_time_summary_stats.csv")
)

summary_tex <- file.path(output_dir, "permit_processing_time_summary_stats.tex")
cat(
  "\\begin{tabular}{llrrrrrrrrrr}\n",
  "\\toprule\n",
  "Sample & Series & N & Mean & SD & Min & P25 & Median & P75 & P90 & P95 & P99 & Max \\\\\n",
  "\\midrule\n",
  file = summary_tex
)

for (i in seq_len(nrow(overall_summary_stats))) {
  row <- overall_summary_stats[i, ]
  cat(
    sprintf(
      "%s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\\n",
      row$sample,
      row$series,
      format(row$n, big.mark = ","),
      fmt_num(row$mean, 1),
      fmt_num(row$sd, 1),
      fmt_num(row$min, 1),
      fmt_num(row$p25, 1),
      fmt_num(row$median, 1),
      fmt_num(row$p75, 1),
      fmt_num(row$p90, 1),
      fmt_num(row$p95, 1),
      fmt_num(row$p99, 1),
      fmt_num(row$max, 1)
    ),
    file = summary_tex,
    append = TRUE
  )
}

cat("\\bottomrule\n\\end{tabular}\n", file = summary_tex, append = TRUE)

skew_diagnostics <- bind_rows(
  lapply(names(sample_frames), function(sample_id) {
    df <- sample_frames[[sample_id]]
    cap_99 <- unique(df$winsor_cap_99)[1]
    winsor_mean <- mean(df$processing_time_days_w99, na.rm = TRUE)

    tibble(
      sample_id = sample_id,
      sample = unname(sample_label[sample_id]),
      n = nrow(df),
      mean_days = safe_mean(df$processing_time),
      median_days = safe_quantile(df$processing_time, 0.50),
      p95_days = safe_quantile(df$processing_time, 0.95),
      p99_days = cap_99,
      max_days = safe_max(df$processing_time),
      mean_over_median = safe_mean(df$processing_time) / safe_quantile(df$processing_time, 0.50),
      p99_over_median = cap_99 / safe_quantile(df$processing_time, 0.50),
      max_over_p99 = safe_max(df$processing_time) / cap_99,
      skewness_days = safe_skewness(df$processing_time),
      share_over_180_days = mean(df$processing_time > 180, na.rm = TRUE),
      share_over_365_days = mean(df$processing_time > 365, na.rm = TRUE),
      share_over_p99 = mean(df$processing_time > cap_99, na.rm = TRUE),
      winsor_cap_99 = cap_99,
      winsorized_mean_days = winsor_mean,
      raw_minus_winsorized_mean_days = safe_mean(df$processing_time) - winsor_mean
    )
  })
) %>%
  mutate(sample_id = factor(sample_id, levels = sample_levels)) %>%
  arrange(sample_id) %>%
  mutate(sample_id = as.character(sample_id))

write_csv(
  skew_diagnostics,
  file.path(output_dir, "permit_processing_time_skew_diagnostics.csv")
)

build_tail_entity_summary <- function(df, sample_id, entity_level, tail_id, tail_prob) {
  threshold_days <- safe_quantile(df$processing_time, tail_prob)
  entity_col <- rlang::sym(entity_level)

  entity_df <- df %>%
    filter(!is.na(!!entity_col), !!entity_col != "") %>%
    mutate(
      entity_value = as.character(!!entity_col),
      entity_display = format_entity_display(entity_level, entity_value),
      is_tail = processing_time >= threshold_days
    ) %>%
    group_by(entity_value, entity_display) %>%
    summarise(
      total_permits = n(),
      tail_permits = sum(is_tail, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      sample_id = sample_id,
      sample = unname(sample_label[sample_id]),
      entity_level = entity_level,
      entity_level_label = unname(entity_level_label[entity_level]),
      tail_id = tail_id,
      tail_prob = tail_prob,
      threshold_days = threshold_days,
      total_permits_all = sum(total_permits, na.rm = TRUE),
      total_tail_permits = sum(tail_permits, na.rm = TRUE),
      overall_tail_rate = safe_share(sum(tail_permits, na.rm = TRUE), sum(total_permits, na.rm = TRUE)),
      permit_share = safe_share(total_permits, sum(total_permits, na.rm = TRUE)),
      tail_share = safe_share(tail_permits, sum(tail_permits, na.rm = TRUE)),
      tail_rate = safe_share(tail_permits, total_permits),
      expected_tail_permits = total_permits * overall_tail_rate,
      excess_tail_permits = tail_permits - expected_tail_permits,
      location_quotient = safe_share(tail_rate, overall_tail_rate)
    ) %>%
    arrange(desc(tail_permits), desc(excess_tail_permits), desc(total_permits), entity_display) %>%
    mutate(
      rank_tail_permits = min_rank(desc(tail_permits)),
      rank_excess_tail = min_rank(desc(excess_tail_permits)),
      rank_tail_rate = min_rank(desc(tail_rate))
    )

  summary_row <- entity_df %>%
    {
      tibble(
        sample_id = first(.$sample_id),
        sample = first(.$sample),
        entity_level = first(.$entity_level),
        entity_level_label = first(.$entity_level_label),
        tail_id = first(.$tail_id),
        tail_prob = first(.$tail_prob),
        threshold_days = first(.$threshold_days),
        total_permits = first(.$total_permits_all),
        total_tail_permits = first(.$total_tail_permits),
        realized_tail_share = first(.$overall_tail_rate),
        n_entities = nrow(.),
        n_entities_with_tail = sum(.$tail_permits > 0, na.rm = TRUE),
        tail_hhi = compute_hhi(.$tail_permits),
        total_hhi = compute_hhi(.$total_permits),
        tail_top1_share = compute_top_share(.$tail_permits, 1),
        tail_top3_share = compute_top_share(.$tail_permits, 3),
        tail_top5_share = compute_top_share(.$tail_permits, 5),
        total_top1_share = compute_top_share(.$total_permits, 1),
        total_top3_share = compute_top_share(.$total_permits, 3),
        total_top5_share = compute_top_share(.$total_permits, 5),
        effective_n_tail = safe_share(1, compute_hhi(.$tail_permits)),
        effective_n_total = safe_share(1, compute_hhi(.$total_permits)),
        top_entity = entity_df$entity_display[1],
        top_entity_tail_permits = entity_df$tail_permits[1],
        top_entity_tail_share = entity_df$tail_share[1],
        top_entity_tail_rate = entity_df$tail_rate[1],
        top_entity_excess_tail_permits = entity_df$excess_tail_permits[1],
        top_entity_location_quotient = entity_df$location_quotient[1]
      )
    }

  list(entity = entity_df, summary = summary_row)
}

tail_outputs <- lapply(names(sample_frames), function(sample_id) {
  sample_df <- sample_frames[[sample_id]]

  lapply(entity_levels, function(entity_level) {
    lapply(names(tail_probabilities), function(tail_id) {
      build_tail_entity_summary(
        df = sample_df,
        sample_id = sample_id,
        entity_level = entity_level,
        tail_id = tail_id,
        tail_prob = tail_probabilities[[tail_id]]
      )
    })
  })
})

tail_entity_all <- bind_rows(
  lapply(tail_outputs, function(sample_list) {
    bind_rows(
      lapply(sample_list, function(entity_list) {
        bind_rows(lapply(entity_list, `[[`, "entity"))
      })
    )
  })
)

tail_concentration_summary <- bind_rows(
  lapply(tail_outputs, function(sample_list) {
    bind_rows(
      lapply(sample_list, function(entity_list) {
        bind_rows(lapply(entity_list, `[[`, "summary"))
      })
    )
  })
) %>%
  mutate(
    sample_id = factor(sample_id, levels = sample_levels),
    entity_level = factor(entity_level, levels = entity_levels),
    tail_id = factor(tail_id, levels = names(tail_probabilities))
  ) %>%
  arrange(sample_id, entity_level, tail_id) %>%
  mutate(
    sample_id = as.character(sample_id),
    entity_level = as.character(entity_level),
    tail_id = as.character(tail_id)
  )

write_csv(
  tail_concentration_summary,
  file.path(output_dir, "permit_right_tail_concentration_summary.csv")
)

write_csv(
  tail_entity_all %>%
    filter(entity_level == "alderman") %>%
    arrange(sample_id, tail_id, desc(tail_permits), desc(excess_tail_permits)),
  file.path(output_dir, "permit_right_tail_by_alderman.csv")
)

write_csv(
  tail_entity_all %>%
    filter(entity_level == "ward") %>%
    arrange(sample_id, tail_id, desc(tail_permits), desc(excess_tail_permits)),
  file.path(output_dir, "permit_right_tail_by_ward.csv")
)

tail_scatter_df <- tail_entity_all %>%
  filter(entity_level == "alderman", tail_id == "p95") %>%
  left_join(
    scores %>% rename(entity_value = alderman, stringency_score = uncertainty_index),
    by = "entity_value"
  ) %>%
  filter(is.finite(stringency_score))

build_tail_scatter_plot <- function(metric_name, y_label, title_text, output_file) {
  plot_df <- tail_scatter_df %>%
    mutate(metric_value = .data[[metric_name]])

  cor_df <- plot_df %>%
    group_by(sample) %>%
    summarise(
      r = cor(stringency_score, metric_value, use = "complete.obs"),
      x_min = min(stringency_score, na.rm = TRUE),
      x_max = max(stringency_score, na.rm = TRUE),
      y_min = min(metric_value, na.rm = TRUE),
      y_max = max(metric_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      x = x_min + 0.05 * pmax(x_max - x_min, 1),
      y = y_max - 0.05 * pmax(y_max - y_min, 1),
      label = sprintf("r = %.2f", r)
    )

  p <- ggplot(plot_df, aes(x = stringency_score, y = metric_value)) +
    geom_point(color = "#1f3c4a", alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "#b74d2c", linewidth = 0.9) +
    geom_text(
      data = cor_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = 3.6
    ) +
    facet_wrap(~sample, scales = "free_y") +
    theme_bw(base_size = 12) +
    labs(
      title = title_text,
      subtitle = "Alderman-level p95 tail metric versus current numeric stringency score",
      x = "Stringency score",
      y = y_label
    )

  ggsave(output_file, p, width = 10, height = 5.5, dpi = 300)
}

build_tail_scatter_plot(
  metric_name = "tail_rate",
  y_label = "Share of permits at or above sample-specific p95",
  title_text = "Right-Tail Rate and Stringency",
  output_file = file.path(output_dir, "permit_right_tail_rate_vs_stringency_p95.pdf")
)

build_tail_scatter_plot(
  metric_name = "excess_tail_permits",
  y_label = "Excess permits at or above sample-specific p95",
  title_text = "Excess Right-Tail Permits and Stringency",
  output_file = file.path(output_dir, "permit_right_tail_excess_vs_stringency_p95.pdf")
)

p95_top_entities <- tail_entity_all %>%
  filter(tail_id == "p95") %>%
  group_by(sample, entity_level_label) %>%
  arrange(desc(excess_tail_permits), desc(tail_permits), .by_group = TRUE) %>%
  slice_head(n = 12) %>%
  ungroup() %>%
  mutate(
    facet_entity = paste(entity_display, sample, entity_level_label, sep = "___")
  )

p_tail <- ggplot(
  p95_top_entities,
  aes(x = reorder(facet_entity, excess_tail_permits), y = excess_tail_permits, fill = location_quotient)
) +
  geom_col() +
  coord_flip() +
  facet_grid(entity_level_label ~ sample, scales = "free_y", space = "free_y") +
  scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
  scale_fill_gradient2(
    low = "#92c5de",
    mid = "#f7f7f7",
    high = "#d6604d",
    midpoint = 1,
    na.value = "gray85"
  ) +
  theme_bw(base_size = 11) +
  labs(
    title = "Who Accounts for the Longest Permit Delays?",
    subtitle = "Top entities ranked by excess permits at or above the sample-specific 95th percentile",
    x = NULL,
    y = "Excess right-tail permits relative to permit volume",
    fill = "Tail rate /\noverall tail rate"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "white")
  )

ggsave(
  filename = file.path(output_dir, "permit_right_tail_top_entities_p95.pdf"),
  plot = p_tail,
  width = 11,
  height = 8.5,
  dpi = 300
)

days_plot_df <- bind_rows(
  lapply(names(sample_frames), function(sample_id) {
    df <- sample_frames[[sample_id]]
    tibble(
      sample = unname(sample_label[sample_id]),
      processing_time_display = pmin(df$processing_time, df$winsor_cap_99),
      winsor_cap_99 = df$winsor_cap_99
    )
  })
)

days_plot_refs <- bind_rows(
  lapply(names(sample_frames), function(sample_id) {
    df <- sample_frames[[sample_id]]
    tibble(
      sample = unname(sample_label[sample_id]),
      stat = c("Median", "Mean", "P99"),
      value = c(
        safe_quantile(df$processing_time, 0.50),
        safe_mean(df$processing_time),
        unique(df$winsor_cap_99)[1]
      )
    )
  })
)

p_days <- ggplot(days_plot_df, aes(x = processing_time_display)) +
  geom_histogram(bins = 60, fill = "#9fc1b7", color = NA, alpha = 0.95) +
  geom_vline(
    data = days_plot_refs,
    aes(xintercept = value, color = stat, linetype = stat),
    linewidth = 0.7,
    alpha = 0.95
  ) +
  facet_wrap(~sample, scales = "free_y") +
  scale_color_manual(values = c(Mean = "#8c2d04", Median = "#1f3c4a", P99 = "#7b3294")) +
  scale_linetype_manual(values = c(Mean = "dashed", Median = "solid", P99 = "dotdash")) +
  scale_x_continuous(labels = scales::comma) +
  theme_bw(base_size = 12) +
  labs(
    title = "Permit Processing Time in Days",
    subtitle = "Each panel caps the upper 1% at the sample-specific 99th percentile for display only",
    x = "Processing time (days)",
    y = "Number of permits",
    color = NULL,
    linetype = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(output_dir, "permit_processing_time_days_distribution.pdf"),
  plot = p_days,
  width = 10,
  height = 6,
  dpi = 300
)

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

plot_distribution <- function(df, x_var, x_label, title, output_file) {
  p <- ggplot(df, aes(x = .data[[x_var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "#88b2ac", color = NA, alpha = 0.9) +
    geom_density(color = "#1f3c4a", linewidth = 1) +
    theme_bw(base_size = 12) +
    labs(
      title = title,
      subtitle = NULL,
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
  output_file = file.path(output_dir, "processing_time_raw_high_discretion.pdf")
)

plot_distribution(
  df = raw_new_construction,
  x_var = "log_processing_time",
  x_label = "Log processing time",
  title = "Raw Log Processing Time Distribution",
  output_file = file.path(output_dir, "processing_time_raw_new_construction.pdf")
)

plot_distribution(
  df = resid_high_discretion,
  x_var = "residual_log_processing_time",
  x_label = "Residualized log processing time",
  title = "Residualized Processing Time Distribution",
  output_file = file.path(output_dir, "processing_time_residualized_high_discretion.pdf")
)

plot_distribution(
  df = resid_new_construction,
  x_var = "residual_log_processing_time",
  x_label = "Residualized log processing time",
  title = "Residualized Processing Time Distribution",
  output_file = file.path(output_dir, "processing_time_residualized_new_construction.pdf")
)

# ==============================================================================
# New stringency-to-days translation outputs
# ==============================================================================
build_stringency_translation <- function(df, sample_id) {
  include_type_control <- identical(sample_id, "all_high_discretion")

  reg_df <- df %>%
    left_join(scores, by = "alderman") %>%
    filter(
      !is.na(month),
      is.finite(uncertainty_index),
      if_all(all_of(covariates), ~ is.finite(.x))
    ) %>%
    mutate(
      processing_time_days_w99 = winsorize_upper(processing_time, winsor_prob)
    )

  rhs_terms <- c("uncertainty_index", covariates)
  if (include_type_control) {
    rhs_terms <- c(rhs_terms, "permit_type_clean")
  }

  rhs_string <- paste(rhs_terms, collapse = " + ")
  model_log <- feols(
    as.formula(paste0("log_processing_time ~ ", rhs_string, " | month")),
    data = reg_df,
    vcov = ~alderman,
    warn = FALSE
  )
  model_days_w99 <- feols(
    as.formula(paste0("processing_time_days_w99 ~ ", rhs_string, " | month")),
    data = reg_df,
    vcov = ~alderman,
    warn = FALSE
  )

  log_term <- extract_term_stats(model_log, "uncertainty_index")
  days_term <- extract_term_stats(model_days_w99, "uncertainty_index")
  pct_change <- exp(log_term$estimate) - 1

  tibble(
    sample_id = sample_id,
    sample = unname(sample_label[sample_id]),
    n_permits = nrow(reg_df),
    n_aldermen = n_distinct(reg_df$alderman),
    lookup_score_mean = safe_mean(scores$uncertainty_index),
    lookup_score_sd = safe_sd(scores$uncertainty_index),
    permit_weighted_score_mean = safe_mean(reg_df$uncertainty_index),
    permit_weighted_score_sd = safe_sd(reg_df$uncertainty_index),
    includes_permit_type_control = include_type_control,
    beta_log_days = log_term$estimate,
    se_log_days = log_term$std_error,
    p_value_log_days = log_term$p_value,
    pct_change_days = 100 * pct_change,
    baseline_mean_days = safe_mean(reg_df$processing_time),
    baseline_median_days = safe_quantile(reg_df$processing_time, 0.50),
    baseline_winsor99_mean_days = safe_mean(reg_df$processing_time_days_w99),
    implied_days_at_mean = safe_mean(reg_df$processing_time) * pct_change,
    implied_days_at_median = safe_quantile(reg_df$processing_time, 0.50) * pct_change,
    implied_days_at_winsor99_mean = safe_mean(reg_df$processing_time_days_w99) * pct_change,
    beta_days_winsor99 = days_term$estimate,
    se_days_winsor99 = days_term$std_error,
    p_value_days_winsor99 = days_term$p_value,
    winsor_cap_99 = safe_quantile(reg_df$processing_time, winsor_prob)
  )
}

stringency_translation <- bind_rows(
  lapply(names(sample_frames), function(sample_id) {
    build_stringency_translation(sample_frames[[sample_id]], sample_id)
  })
) %>%
  mutate(sample_id = factor(sample_id, levels = sample_levels)) %>%
  arrange(sample_id) %>%
  mutate(sample_id = as.character(sample_id))

write_csv(
  stringency_translation,
  file.path(output_dir, "permit_stringency_score_translation.csv")
)

translation_tex <- file.path(output_dir, "permit_stringency_score_translation.tex")
cat(
  "\\begin{tabular}{lrrrrrr}\n",
  "\\toprule\n",
  "Sample & N & Aldermen & \\%$\\Delta$ days & $\\Delta$ days @ median & $\\Delta$ days @ winsorized mean & Direct $\\Delta$ days (winsorized) \\\\\n",
  "\\midrule\n",
  file = translation_tex
)

for (i in seq_len(nrow(stringency_translation))) {
  row <- stringency_translation[i, ]
  cat(
    sprintf(
      "%s & %s & %s & %s & %s & %s & %s \\\\\n",
      row$sample,
      format(row$n_permits, big.mark = ","),
      format(row$n_aldermen, big.mark = ","),
      fmt_num(row$pct_change_days, 1),
      fmt_num(row$implied_days_at_median, 1),
      fmt_num(row$implied_days_at_winsor99_mean, 1),
      fmt_num(row$beta_days_winsor99, 1)
    ),
    file = translation_tex,
    append = TRUE
  )
}

cat("\\bottomrule\n\\end{tabular}\n", file = translation_tex, append = TRUE)

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
    subtitle = NULL,
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
