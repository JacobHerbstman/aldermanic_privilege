source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--window", type = "character", default = "pre_2021"),
  make_option("--sample_filter", type = "character", default = "all"),
  make_option("--use_controls", type = "logical", default = TRUE),
  make_option("--bins_per_side", type = "integer", default = 10),
  make_option("--min_strictness_diff_pctile", type = "integer", default = 0),
  make_option("--output_pdf", type = "character", default = "../output/bfm_rd_plot_bw1000_pre_2021_all.pdf"),
  make_option("--output_meta_csv", type = "character", default = "../output/bfm_rd_plot_bw1000_pre_2021_all_meta.csv"),
  make_option("--output_bins_csv", type = "character", default = "../output/bfm_rd_plot_bw1000_pre_2021_all_bins.csv")
)
opt <- parse_args(OptionParser(option_list = option_list))

if (!opt$window %in% c("full", "pre_covid", "pre_2021", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, drop_mid", call. = FALSE)
}
if (!opt$sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}

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

message("=== Rental BFM-Style RD Plot ===")
message(sprintf("bw=%d | window=%s | sample=%s | controls=%s",
                opt$bw_ft, opt$window, opt$sample_filter, opt$use_controls))

# Load and filter data (matching rental_border_pair_fe.R exactly)
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

if (nrow(dat) == 0 || length(unique(dat$ward_pair)) < 2) {
  stop("Insufficient data after filtering.", call. = FALSE)
}

# Frisch-Waugh: include RD components + controls in one model
n_type_levels <- n_distinct(dat$building_type_factor)
rd_rhs <- "right + signed_dist + right:signed_dist"
if (opt$use_controls) {
  ctrl <- "log_sqft + log_beds + log_baths"
  if (n_type_levels >= 2) ctrl <- paste0(ctrl, " + building_type_factor")
  rd_rhs <- paste0(rd_rhs, " + ", ctrl)
}

m <- feols(
  as.formula(paste0("log(rent_price) ~ ", rd_rhs, " | ward_pair^year_month")),
  data = dat,
  cluster = ~ward_pair
)
ct <- coeftable(m)

get_coef <- function(nms) {
  idx <- which(rownames(ct) %in% nms)
  if (length(idx) == 0) return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
  c(estimate = ct[idx[1], "Estimate"], se = ct[idx[1], "Std. Error"], p = ct[idx[1], "Pr(>|t|)"])
}

b_side <- get_coef("right")
b_x <- get_coef("signed_dist")
b_int <- get_coef(c("right:signed_dist", "signed_dist:right"))

# Align model sample and compute Frisch-Waugh adjusted outcome
removed <- m$obs_selection$obsRemoved
if (is.null(removed)) {
  keep_idx <- seq_len(nrow(dat))
} else {
  keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
}
aug <- dat[keep_idx, , drop = FALSE]
stopifnot(nrow(aug) == nobs(m))
aug$.resid <- as.numeric(resid(m))
aug <- aug %>%
  mutate(
    xb = b_side["estimate"] * right +
      b_x["estimate"] * signed_dist +
      b_int["estimate"] * (right * signed_dist),
    y_adj = .resid + xb
  )

# Binning
bin_w <- opt$bw_ft / opt$bins_per_side
bins <- aug %>%
  mutate(bin_id = floor(signed_dist / bin_w),
         bin_center = (bin_id + 0.5) * bin_w) %>%
  group_by(bin_center) %>%
  summarise(
    n = n(),
    mean_y = mean(y_adj, na.rm = TRUE),
    se_y = sd(y_adj, na.rm = TRUE) / sqrt(n),
    side = if_else(first(bin_center) >= 0, 1L, 0L),
    .groups = "drop"
  )

# Fitted lines from coefficients
x_left <- seq(-opt$bw_ft, 0, length.out = 200)
x_right <- seq(0, opt$bw_ft, length.out = 200)
line_df <- bind_rows(
  tibble(signed_dist = x_left, side = 0L,
         fit = b_x["estimate"] * x_left),
  tibble(signed_dist = x_right, side = 1L,
         fit = b_side["estimate"] + (b_x["estimate"] + b_int["estimate"]) * x_right)
)

jump_label <- sprintf(
  "Jump at cutoff = %.3f%s (SE %.3f)",
  b_side["estimate"], stars(b_side["p"]), b_side["se"]
)

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(
    data = bins,
    aes(x = bin_center, y = mean_y, color = factor(side)),
    size = 2.5, alpha = 0.9
  ) +
  geom_line(
    data = line_df,
    aes(x = signed_dist, y = fit, color = factor(side)),
    linewidth = 1.1
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#d62728"),
    labels = c("0" = "Lenient side", "1" = "Strict side"),
    name = ""
  ) +
  annotate("text",
    x = -Inf, y = Inf,
    label = jump_label,
    hjust = -0.05, vjust = 1.5, size = 3.3, fontface = "bold"
  ) +
  labs(
    title = "Rental Prices at Ward Boundary (FE-Adjusted)",
    subtitle = sprintf("%s | bw=%d ft | N=%s | Ward pairs=%d%s",
                        jump_label, opt$bw_ft,
                        format(nobs(m), big.mark = ","),
                        dplyr::n_distinct(aug$ward_pair),
                        if (opt$min_strictness_diff_pctile > 0) sprintf(" | top %d%% pairs", 100 - opt$min_strictness_diff_pctile) else ""),
    x = "Distance to Stricter Ward Boundary (feet)",
    y = "FE+Controls-Adjusted Log(Rent)",
    caption = "Points: binned means of FE+controls-adjusted outcome. Lines: fitted side-jump model."
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(opt$output_pdf, p, width = 8.6, height = 6, dpi = 300, bg = "white")

# write_csv(bins, opt$output_bins_csv)
# write_csv(
#   tibble(
#     bw_ft = opt$bw_ft, window = opt$window, sample_filter = opt$sample_filter,
#     use_controls = opt$use_controls, min_strictness_diff = opt$min_strictness_diff,
#     jump_estimate = b_side["estimate"], jump_se = b_side["se"], jump_p = b_side["p"],
#     slope_left = b_x["estimate"], slope_diff = b_int["estimate"],
#     n_obs = nobs(m), ward_pairs = dplyr::n_distinct(aug$ward_pair)
#   ),
#   opt$output_meta_csv
# )

message(sprintf("Saved: %s", opt$output_pdf))
