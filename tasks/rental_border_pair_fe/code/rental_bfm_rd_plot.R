source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_pair_fe/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# sample_filter <- "all"
# use_controls <- TRUE
# bins_per_side <- 10
# min_strictness_diff_pctile <- 0
# output_pdf <- "../output/bfm_rd_plot_bw1000_pre_2021_all.pdf"
# output_meta_csv <- "../output/bfm_rd_plot_bw1000_pre_2021_all_meta.csv"
# output_bins_csv <- "../output/bfm_rd_plot_bw1000_pre_2021_all_bins.csv"
# Rscript rental_bfm_rd_plot.R "../input/rent_with_ward_distances.parquet" 1000 "pre_2021" "all" TRUE 10 0 "../output/bfm_rd_plot_bw1000_pre_2021_all.pdf" "../output/bfm_rd_plot_bw1000_pre_2021_all_meta.csv" "../output/bfm_rd_plot_bw1000_pre_2021_all_bins.csv"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
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
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") ||
      !exists("use_controls") || !exists("bins_per_side") || !exists("min_strictness_diff_pctile") ||
      !exists("output_pdf") || !exists("output_meta_csv") || !exists("output_bins_csv") ||
      !exists("fe_geo") || !exists("cluster_level")) {
    stop("FATAL: Script requires args: <input> <bw_ft> <window> <sample_filter> <use_controls> <bins_per_side> <min_strictness_diff_pctile> <output_pdf> <output_meta_csv> <output_bins_csv> [<fe_geo> <cluster_level>]", call. = FALSE)
  }
}
if (!fe_geo %in% c("segment", "ward_pair")) stop("--fe_geo must be one of: segment, ward_pair", call. = FALSE)
if (!cluster_level %in% c("segment", "ward_pair")) stop("--cluster_level must be one of: segment, ward_pair", call. = FALSE)

if (!window %in% c("full", "pre_covid", "pre_2021", "pre_2023", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, pre_2023, drop_mid", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
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

message("=== Rental BFM-Style RD Plot ===")
message(sprintf("bw=%d | window=%s | sample=%s | controls=%s",
                bw_ft, window, sample_filter, use_controls))

# Load and filter data (matching rental_border_pair_fe.R exactly)
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

if (nrow(dat) == 0 || length(unique(dat$ward_pair)) < 2) {
  stop("Insufficient data after filtering.", call. = FALSE)
}

# Frisch-Waugh: include RD components + controls in one model
n_type_levels <- n_distinct(dat$building_type_factor)
rd_rhs <- "right + signed_dist + right:signed_dist"
if (use_controls) {
  ctrl <- "log_sqft + log_beds + log_baths"
  if (n_type_levels >= 2) ctrl <- paste0(ctrl, " + building_type_factor")
  rd_rhs <- paste0(rd_rhs, " + ", ctrl)
}

m <- feols(
  as.formula(paste0(
    "log(rent_price) ~ ", rd_rhs, " | ",
    ifelse(fe_geo == "segment", "segment_id + year_month", "ward_pair^year_month")
  )),
  data = dat,
  cluster = if (cluster_level == "segment") ~segment_id else ~ward_pair
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
bin_w <- bw_ft / bins_per_side
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
x_left <- seq(-bw_ft, 0, length.out = 200)
x_right <- seq(0, bw_ft, length.out = 200)
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
    subtitle = sprintf("%s | bw=%d ft | fe=%s | clust=%s | N=%s | Ward pairs=%d%s",
                        jump_label, bw_ft,
                        fe_geo, cluster_level,
                        format(nobs(m), big.mark = ","),
                        dplyr::n_distinct(aug$ward_pair),
                        if (min_strictness_diff_pctile > 0) sprintf(" | top %d%% pairs", 100 - min_strictness_diff_pctile) else ""),
    x = "Distance to Stricter Ward Boundary (feet)",
    y = "FE+Controls-Adjusted Log(Rent)",
    caption = "Points: binned means of FE+controls-adjusted outcome. Lines: fitted side-jump model."
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(output_pdf, p, width = 8.6, height = 6, dpi = 300, bg = "white")

# write_csv(bins, output_bins_csv)
# write_csv(
#   tibble(
#     bw_ft = bw_ft, window = window, sample_filter = sample_filter,
#     use_controls = use_controls, min_strictness_diff = opt$min_strictness_diff,
#     jump_estimate = b_side["estimate"], jump_se = b_side["se"], jump_p = b_side["p"],
#     slope_left = b_x["estimate"], slope_diff = b_int["estimate"],
#     n_obs = nobs(m), ward_pairs = dplyr::n_distinct(aug$ward_pair)
#   ),
#   output_meta_csv
# )

message(sprintf("Saved: %s", output_pdf))
