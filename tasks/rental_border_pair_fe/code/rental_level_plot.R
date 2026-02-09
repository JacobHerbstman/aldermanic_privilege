source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--window", type = "character", default = "pre_2021"),
  make_option("--sample_filter", type = "character", default = "all"),
  make_option("--use_controls", type = "logical", default = TRUE),
  make_option("--bins_per_side", type = "integer", default = 5),
  make_option("--min_strictness_diff_pctile", type = "integer", default = 0),
  make_option("--output_pdf", type = "character"),
  make_option("--output_meta_csv", type = "character"),
  make_option("--output_bins_csv", type = "character")
)
opt <- parse_args(OptionParser(option_list = option_list))

apply_window <- function(df, window_name) {
  if (window_name == "full") return(df)
  if (window_name == "pre_covid") return(df %>% filter(year <= 2019))
  if (window_name == "pre_2021") return(df %>% filter(year <= 2020))
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

message("=== Side-Level Comparison Plot ===")
message(sprintf("bw=%d | window=%s | sample=%s | controls=%s",
                opt$bw_ft, opt$window, opt$sample_filter, opt$use_controls))

# Load and filter data
dat <- read_parquet(opt$input) %>%
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
    abs(signed_dist) <= opt$bw_ft
  ) %>%
  apply_window(opt$window)

if (opt$sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

if (opt$min_strictness_diff_pctile > 0) {
  pair_diffs <- dat %>%
    group_by(ward_pair) %>%
    summarise(diff = first(abs(strictness_own - strictness_neighbor)), .groups = "drop")
  cutoff <- quantile(pair_diffs$diff, opt$min_strictness_diff_pctile / 100)
  keep_pairs <- pair_diffs %>% filter(diff >= cutoff) %>% pull(ward_pair)
  dat <- dat %>% filter(ward_pair %in% keep_pairs)
  message(sprintf("After p%d filter (cutoff=%.3f): %d obs, %d ward pairs",
                  opt$min_strictness_diff_pctile, cutoff, nrow(dat), n_distinct(dat$ward_pair)))
}

dat <- dat %>%
  mutate(
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    right = as.integer(signed_dist >= 0)
  )

if (opt$use_controls) {
  dat <- dat %>%
    filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths))
}

stopifnot(nrow(dat) > 0, length(unique(dat$ward_pair)) >= 2)

# Model: just a side indicator (no distance terms)
n_type_levels <- n_distinct(dat$building_type_factor)
rhs <- "right"
if (opt$use_controls) {
  ctrl <- "log_sqft + log_beds + log_baths"
  if (n_type_levels >= 2) ctrl <- paste0(ctrl, " + building_type_factor")
  rhs <- paste0(rhs, " + ", ctrl)
}

fml <- as.formula(paste0("log(rent_price) ~ ", rhs, " | ward_pair^year_month"))
m <- feols(fml, data = dat, cluster = ~ward_pair)
ct <- coeftable(m)

b_right <- ct["right", "Estimate"]
se_right <- ct["right", "Std. Error"]
p_right <- ct["right", "Pr(>|t|)"]

# Frisch-Waugh adjusted outcome: resid + b_right * right
removed <- m$obs_selection$obsRemoved
if (is.null(removed)) {
  keep_idx <- seq_len(nrow(dat))
} else {
  keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
}
aug <- dat[keep_idx, , drop = FALSE]
stopifnot(nrow(aug) == nobs(m))
aug$.resid <- as.numeric(resid(m))
aug <- aug %>% mutate(y_adj = .resid + b_right * right)

# Binning by distance
bin_w <- opt$bw_ft / opt$bins_per_side
bins <- aug %>%
  mutate(
    bin_id = floor(signed_dist / bin_w),
    bin_center = (bin_id + 0.5) * bin_w
  ) %>%
  group_by(bin_center) %>%
  summarise(
    n = n(),
    mean_y = mean(y_adj, na.rm = TRUE),
    se_y = sd(y_adj, na.rm = TRUE) / sqrt(n),
    side = if_else(first(bin_center) >= 0, "Stricter side", "Less strict side"),
    .groups = "drop"
  )

# Flat mean lines for each side
mean_left <- mean(aug$y_adj[aug$right == 0], na.rm = TRUE)
mean_right <- mean(aug$y_adj[aug$right == 1], na.rm = TRUE)

line_df <- bind_rows(
  tibble(x = c(-opt$bw_ft, 0), y = mean_left, side = "Less strict side"),
  tibble(x = c(0, opt$bw_ft), y = mean_right, side = "Stricter side")
)

gap_label <- sprintf(
  "Gap = %.4f%s (SE %.4f, p = %.3f)\nN = %s | Ward pairs = %d",
  b_right, stars(p_right), se_right, p_right,
  format(nobs(m), big.mark = ","), n_distinct(aug$ward_pair)
)

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(
    data = bins,
    aes(x = bin_center, y = mean_y, color = side),
    size = 2.5, alpha = 0.9
  ) +
  geom_line(
    data = line_df,
    aes(x = x, y = y, color = side),
    linewidth = 1.1
  ) +
  scale_color_manual(
    values = c("Less strict side" = "#1f77b4", "Stricter side" = "#d62728"),
    name = ""
  ) +
  annotate("text",
    x = -Inf, y = Inf,
    label = gap_label,
    hjust = -0.05, vjust = 1.5, size = 3.3, fontface = "bold"
  ) +
  labs(
    title = "Rental Prices by Side of Ward Boundary (FE-Adjusted)",
    subtitle = sprintf("bw=%d ft | window=%s | controls=%s%s",
                        opt$bw_ft, opt$window, opt$use_controls,
                        if (opt$min_strictness_diff_pctile > 0) sprintf(" | top %d%% pairs", 100 - opt$min_strictness_diff_pctile) else ""),
    x = "Distance to Ward Boundary (feet; positive = stricter side)",
    y = "FE-Adjusted Log(Rent)",
    caption = "Points: binned means. Lines: side-level means."
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(opt$output_pdf, p, width = 8.6, height = 6, dpi = 300, bg = "white")

# write_csv(bins, opt$output_bins_csv)
# write_csv(
#   tibble(
#     bw_ft = opt$bw_ft, window = opt$window, sample_filter = opt$sample_filter,
#     use_controls = opt$use_controls, min_strictness_diff = opt$min_strictness_diff,
#     gap_estimate = b_right, gap_se = se_right, gap_p = p_right,
#     mean_left = mean_left, mean_right = mean_right,
#     n_obs = nobs(m), ward_pairs = n_distinct(aug$ward_pair)
#   ),
#   opt$output_meta_csv
# )

message(sprintf("Saved: %s", opt$output_pdf))
