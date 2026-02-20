source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_pair_fe/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# sample_filter <- "all"
# use_controls <- TRUE
# n_bins <- 40
# min_strictness_diff_pctile <- 0
# output_pdf <- NA
# output_meta_csv <- NA
# output_bins_csv <- NA
# Rscript rental_binscatter.R "../input/rent_with_ward_distances.parquet" 1000 "pre_2021" "all" TRUE 40 0 NA NA NA
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 10) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  use_controls <- tolower(cli_args[5]) %in% c("true", "t", "1", "yes")
  n_bins <- suppressWarnings(as.integer(cli_args[6]))
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[7]))
  output_pdf <- cli_args[8]
  output_meta_csv <- cli_args[9]
  output_bins_csv <- cli_args[10]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") || !exists("use_controls") || !exists("n_bins") || !exists("min_strictness_diff_pctile") || !exists("output_pdf") || !exists("output_meta_csv") || !exists("output_bins_csv")) {
    stop("FATAL: Script requires 10 args: <input> <bw_ft> <window> <sample_filter> <use_controls> <n_bins> <min_strictness_diff_pctile> <output_pdf> <output_meta_csv> <output_bins_csv>", call. = FALSE)
  }
}

apply_window <- function(df, window_name) {
  if (window_name == "full") return(df)
  if (window_name == "pre_covid") return(df %>% filter(year <= 2019))
  if (window_name == "pre_2021") return(df %>% filter(year <= 2020))
  if (window_name == "pre_2023") return(df %>% filter(year <= 2022))
  if (window_name == "drop_mid") return(df %>% filter(year <= 2020 | year >= 2024))
  df
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

message("=== Strictness Binscatter ===")
message(sprintf("bw=%d | window=%s | sample=%s | controls=%s",
                bw_ft, window, sample_filter, use_controls))

# Load and filter data
dat <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id)
  ) %>%
  filter(
    !is.na(file_date),
    !is.na(ward_pair),
    !is.na(rent_price), rent_price > 0,
    !is.na(signed_dist),
    !is.na(strictness_own),
    abs(signed_dist) <= bw_ft
  ) %>%
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

if (min_strictness_diff_pctile > 0) {
  pair_diffs <- dat %>%
    group_by(ward_pair) %>%
    summarise(diff = first(abs(strictness_own - strictness_neighbor)), .groups = "drop")
  cutoff <- quantile(pair_diffs$diff, min_strictness_diff_pctile / 100)
  keep_pairs <- pair_diffs %>% filter(diff >= cutoff) %>% pull(ward_pair)
  dat <- dat %>% filter(ward_pair %in% keep_pairs)
  message(sprintf("After p%d filter (cutoff=%.3f): %d obs, %d ward pairs",
                  min_strictness_diff_pctile, cutoff, nrow(dat), n_distinct(dat$ward_pair)))
}

dat <- dat %>%
  mutate(
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  )

if (use_controls) {
  dat <- dat %>%
    filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths))
}

stopifnot(nrow(dat) > 0, length(unique(dat$ward_pair)) >= 2)

# Main model — use strictness_own directly
n_type_levels <- n_distinct(dat$building_type_factor)
rhs <- "strictness_own"
if (use_controls) {
  ctrl <- "log_sqft + log_beds + log_baths"
  if (n_type_levels >= 2) ctrl <- paste0(ctrl, " + building_type_factor")
  rhs <- paste0(rhs, " + ", ctrl)
}

fml_main <- as.formula(paste0("log(rent_price) ~ ", rhs, " | ward_pair^year_month"))
m_main <- feols(fml_main, data = dat, cluster = ~ward_pair)
ct <- coeftable(m_main)

b_strict <- ct["strictness_own", "Estimate"]
se_strict <- ct["strictness_own", "Std. Error"]
p_strict <- ct["strictness_own", "Pr(>|t|)"]

message(sprintf("Main coef: %.6f (SE %.6f, p=%.4f)", b_strict, se_strict, p_strict))

# Handle obs removed by FE (singletons)
removed_main <- m_main$obs_selection$obsRemoved
if (is.null(removed_main)) {
  aug <- dat
} else {
  keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed_main)))
  aug <- dat[keep_idx, , drop = FALSE]
}

# Adjusted y: residual + fitted strictness component
y_resid <- as.numeric(resid(m_main))
aug$y_adj <- y_resid + b_strict * aug$strictness_own

# Quantile bins of raw strictness
aug$bin <- ntile(aug$strictness_own, n_bins)

bins <- aug %>%
  group_by(bin) %>%
  summarise(
    n = n(),
    mean_x = mean(strictness_own, na.rm = TRUE),
    mean_y = mean(y_adj, na.rm = TRUE),
    se_y = sd(y_adj, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

# Fitted line through bins
fit <- lm(mean_y ~ mean_x, data = bins)
x_range <- range(bins$mean_x)
line_df <- tibble(
  x = seq(x_range[1], x_range[2], length.out = 200),
  y = predict(fit, newdata = data.frame(mean_x = seq(x_range[1], x_range[2], length.out = 200)))
)

coef_label <- sprintf(
  "Coef = %.4f%s (SE %.4f, p = %.3f)\nN = %s | Ward pairs = %d",
  b_strict, stars(p_strict), se_strict, p_strict,
  format(nobs(m_main), big.mark = ","), n_distinct(aug$ward_pair)
)

p <- ggplot() +
  geom_point(
    data = bins,
    aes(x = mean_x, y = mean_y),
    color = "#2c3e50", size = 2.5, alpha = 0.9
  ) +
  geom_line(
    data = line_df,
    aes(x = x, y = y),
    color = "#d62728", linewidth = 1.1
  ) +
  annotate("text",
    x = -Inf, y = Inf,
    label = coef_label,
    hjust = -0.05, vjust = 1.5, size = 3.3, fontface = "bold"
  ) +
  labs(
    title = "Alderman Strictness and Rental Prices",
    subtitle = sprintf("bw=%d ft | window=%s | controls=%s%s",
                        bw_ft, window, use_controls,
                        if (min_strictness_diff_pctile > 0) sprintf(" | top %d%% pairs", 100 - min_strictness_diff_pctile) else ""),
    x = "Alderman Strictness Index",
    y = "Adjusted Log(Rent)",
    caption = "Quantile bins of alderman strictness. Y residualized on ward-pair x year-month FE and controls."
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(output_pdf, p, width = 8.6, height = 6, dpi = 300, bg = "white")

# write_csv(bins, output_bins_csv)
# write_csv(
#   tibble(
#     bw_ft = bw_ft, window = window, sample_filter = sample_filter,
#     use_controls = use_controls, min_strictness_diff = opt$min_strictness_diff,
#     coef_estimate = b_strict, coef_se = se_strict, coef_p = p_strict,
#     n_obs = nobs(m_main), ward_pairs = n_distinct(aug$ward_pair)
#   ),
#   output_meta_csv
# )

message(sprintf("Saved: %s", output_pdf))