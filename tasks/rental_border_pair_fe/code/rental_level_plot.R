source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_pair_fe/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# sample_filter <- "all"
# use_controls <- TRUE
# bins_per_side <- 5
# min_strictness_diff_pctile <- 0
# output_pdf <- NA
# output_meta_csv <- NA
# output_bins_csv <- NA

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_ft, window, sample_filter, use_controls, bins_per_side, min_strictness_diff_pctile, output_pdf, output_meta_csv, output_bins_csv)
}

if (length(cli_args) >= 12) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  use_controls <- tolower(cli_args[5]) %in% c("true", "t", "1", "yes")
  bins_per_side <- suppressWarnings(as.integer(cli_args[6]))
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[7]))
  output_pdf <- cli_args[8]
  output_meta_csv <- cli_args[9]
  output_bins_csv <- cli_args[10]
  fe_geo <- tolower(cli_args[11])
  cluster_level <- tolower(cli_args[12])
} else if (length(cli_args) >= 10) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  use_controls <- tolower(cli_args[5]) %in% c("true", "t", "1", "yes")
  bins_per_side <- suppressWarnings(as.integer(cli_args[6]))
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[7]))
  output_pdf <- cli_args[8]
  output_meta_csv <- cli_args[9]
  output_bins_csv <- cli_args[10]
  fe_geo <- tolower(Sys.getenv("FE_GEO", "segment"))
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
} else {
  stop("FATAL: Script requires args: <input> <bw_ft> <window> <sample_filter> <use_controls> <bins_per_side> <min_strictness_diff_pctile> <output_pdf> <output_meta_csv> <output_bins_csv> [<fe_geo> <cluster_level>]", call. = FALSE)
}
if (!fe_geo %in% c("segment", "ward_pair")) stop("--fe_geo must be one of: segment, ward_pair", call. = FALSE)
if (!cluster_level %in% c("segment", "ward_pair")) stop("--cluster_level must be one of: segment, ward_pair", call. = FALSE)

message("=== Side-Level Comparison Plot ===")
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
  )

if (window == "pre_covid") {
  dat <- dat %>% filter(year <= 2019)
} else if (window == "pre_2021") {
  dat <- dat %>% filter(year <= 2020)
} else if (window == "pre_2023") {
  dat <- dat %>% filter(year <= 2022)
} else if (window == "drop_mid") {
  dat <- dat %>% filter(year <= 2020 | year >= 2024)
}

if (sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}
need_segment <- fe_geo == "segment" || cluster_level == "segment"
if (need_segment) {
  dat <- dat %>% filter(!is.na(segment_id), segment_id != "")
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
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    right = as.integer(signed_dist >= 0)
  )

if (use_controls) {
  dat <- dat %>%
    filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths))
}

stopifnot(nrow(dat) > 0, length(unique(dat$ward_pair)) >= 2)

# Model: just a side indicator (no distance terms)
n_type_levels <- n_distinct(dat$building_type_factor)
rhs <- "right"
if (use_controls) {
  ctrl <- "log_sqft + log_beds + log_baths"
  if (n_type_levels >= 2) ctrl <- paste0(ctrl, " + building_type_factor")
  rhs <- paste0(rhs, " + ", ctrl)
}

fml <- as.formula(paste0(
  "log(rent_price) ~ ", rhs, " | ",
  ifelse(fe_geo == "segment", "segment_id^year_month", "ward_pair^year_month")
))
m <- feols(
  fml,
  data = dat,
  cluster = if (cluster_level == "segment") ~segment_id else ~ward_pair
)
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
bin_w <- bw_ft / bins_per_side
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
    side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
    .groups = "drop"
  )

# Flat mean lines for each side
mean_left <- mean(aug$y_adj[aug$right == 0], na.rm = TRUE)
mean_right <- mean(aug$y_adj[aug$right == 1], na.rm = TRUE)

line_df <- bind_rows(
  tibble(x = c(-bw_ft, 0), y = mean_left, side = "Less Stringent"),
  tibble(x = c(0, bw_ft), y = mean_right, side = "More Stringent")
)

jump_label <- sprintf(
  "Jump = %.4f%s (SE %.4f) | bw=%d ft | N=%s",
  b_right,
  if (!is.finite(p_right)) {
    ""
  } else if (p_right < 0.01) {
    "***"
  } else if (p_right < 0.05) {
    "**"
  } else if (p_right < 0.1) {
    "*"
  } else {
    ""
  },
  se_right,
  bw_ft,
  format(nobs(m), big.mark = ",")
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
    values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"),
    name = ""
  ) +
  labs(
    title = "Rental Prices by Side of Ward Boundary",
    subtitle = jump_label,
    x = "Distance to Ward Boundary (feet; positive = more stringent side)",
    y = "FE-Adjusted Log(Rent)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(output_pdf, p, width = 8.6, height = 6, dpi = 300, bg = "white")

# write_csv(bins, output_bins_csv)
# write_csv(
#   tibble(
#     bw_ft = bw_ft, window = window, sample_filter = sample_filter,
#     use_controls = use_controls, min_strictness_diff = opt$min_strictness_diff,
#     gap_estimate = b_right, gap_se = se_right, gap_p = p_right,
#     mean_left = mean_left, mean_right = mean_right,
#     n_obs = nobs(m), ward_pairs = n_distinct(aug$ward_pair)
#   ),
#   output_meta_csv
# )

message(sprintf("Saved: %s", output_pdf))
