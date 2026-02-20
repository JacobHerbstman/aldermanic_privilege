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

covariates <- c(
  "median_hh_income_10k", "share_black", "share_hisp", "share_white",
  "homeownership_rate", "share_bach_plus", "pop_total_10k",
  "dist_cbd_km", "lakefront_share_1km", "n_rail_stations_800m"
)

permits <- read_csv(input, show_col_types = FALSE) %>%
  mutate(
    month = as.yearmon(month),
    median_hh_income_10k = median_hh_income / 10000,
    pop_total_10k = pop_total / 10000
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

type_label <- c(
  new_construction = "New construction",
  renovation = "Renovation",
  demolition = "Demolition"
)

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

message("Saved outputs in: ", output_dir)