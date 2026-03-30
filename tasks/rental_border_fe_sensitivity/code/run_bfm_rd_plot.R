source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_fe_sensitivity/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# sample_filter <- "all"
# use_controls <- TRUE
# bins_per_side <- 10
# min_strictness_diff <- 0
# output_csv <- "../output/bfm_rd_plot_pre_2021_all_bw1000.csv"
# output_bins_csv <- "../output/bfm_rd_plot_bins_pre_2021_all_bw1000.csv"
# output_pdf <- "../output/bfm_rd_plot_pre_2021_all_bw1000.pdf"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_ft, window, sample_filter, use_controls, bins_per_side, min_strictness_diff, output_csv, output_bins_csv, output_pdf)
}

if (length(cli_args) >= 10) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  use_controls <- tolower(cli_args[5]) %in% c("true", "t", "1", "yes")
  bins_per_side <- suppressWarnings(as.integer(cli_args[6]))
  min_strictness_diff <- as.numeric(cli_args[7])
  output_csv <- cli_args[8]
  output_bins_csv <- cli_args[9]
  output_pdf <- cli_args[10]
} else {
  stop("FATAL: Script requires 10 args: <input> <bw_ft> <window> <sample_filter> <use_controls> <bins_per_side> <min_strictness_diff> <output_csv> <output_bins_csv> <output_pdf>", call. = FALSE)
}

if (!window %in% c("full", "pre_covid", "pre_2021", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, drop_mid", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("--bw_ft must be positive", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side < 2) {
  stop("--bins_per_side must be >= 2", call. = FALSE)
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

message("=== BFM-Style RD Plot ===")
message(sprintf("Bandwidth: %d ft | Window: %s | Sample: %s | Controls: %s",
                bw_ft, window, sample_filter, use_controls))

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

if (min_strictness_diff > 0) {
  dat <- dat %>%
    mutate(strictness_diff = abs(strictness_own - strictness_neighbor)) %>%
    filter(strictness_diff >= min_strictness_diff)
  message(sprintf("After strictness_diff >= %.3f filter: %d obs, %d ward pairs",
                  min_strictness_diff, nrow(dat), n_distinct(dat$ward_pair)))
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
    filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths), !is.na(building_type_factor))
}

if (nrow(dat) == 0 || length(unique(dat$ward_pair)) < 2) {
  stop("Insufficient data after filtering.", call. = FALSE)
}

# Frisch-Waugh approach: include RD components + controls in one model
n_type_levels <- n_distinct(dat$building_type_factor)
rd_rhs <- "right + signed_dist + right:signed_dist"
if (use_controls) {
  ctrl_terms <- "log_sqft + log_beds + log_baths"
  if (n_type_levels >= 2) ctrl_terms <- paste0(ctrl_terms, " + building_type_factor")
  rd_rhs <- paste0(rd_rhs, " + ", ctrl_terms)
}

fml <- as.formula(paste0("log(rent_price) ~ ", rd_rhs, " | ward_pair^year_month"))
m <- feols(fml, data = dat, cluster = ~ward_pair)
ct <- coeftable(m)

get_coef <- function(names_vec) {
  idx <- which(rownames(ct) %in% names_vec)
  if (length(idx) == 0) return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
  c(estimate = ct[idx[1], "Estimate"], se = ct[idx[1], "Std. Error"], p = ct[idx[1], "Pr(>|t|)"])
}

b_side <- get_coef("right")
b_x <- get_coef("signed_dist")
b_int <- get_coef(c("right:signed_dist", "signed_dist:right"))

# Frisch-Waugh adjusted outcome: residual + RD-component contribution
removed <- m$obs_selection$obsRemoved
if (is.null(removed)) {
  keep_idx <- seq_len(nrow(dat))
} else {
  keep_idx <- setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
}

aug <- dat[keep_idx, , drop = FALSE]
if (nrow(aug) != nobs(m)) {
  stop(sprintf("Model/sample alignment failed: kept=%d, nobs=%d", nrow(aug), nobs(m)), call. = FALSE)
}
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
  mutate(
    bin_id = floor(signed_dist / bin_w),
    bin_center = (bin_id + 0.5) * bin_w
  ) %>%
  group_by(bin_center) %>%
  summarise(
    n = n(),
    mean_y = mean(y_adj, na.rm = TRUE),
    se_y = sd(y_adj, na.rm = TRUE) / sqrt(n),
    side = if_else(first(bin_center) >= 0, "Strict side", "Lenient side"),
    .groups = "drop"
  )

# Fitted lines from model coefficients
x_left <- seq(-bw_ft, 0, length.out = 200)
x_right <- seq(0, bw_ft, length.out = 200)
line_df <- bind_rows(
  tibble(
    signed_dist = x_left,
    side = "Lenient side",
    fit = b_x["estimate"] * x_left
  ),
  tibble(
    signed_dist = x_right,
    side = "Strict side",
    fit = b_side["estimate"] + (b_x["estimate"] + b_int["estimate"]) * x_right
  )
)

jump_label <- sprintf(
  "Jump = %.4f%s (SE %.4f, p = %.3f)\nN = %s | Ward pairs = %d",
  b_side["estimate"], stars(b_side["p"]), b_side["se"], b_side["p"],
  format(nobs(m), big.mark = ","), dplyr::n_distinct(aug$ward_pair)
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
    aes(x = signed_dist, y = fit, color = side),
    linewidth = 1.1
  ) +
  scale_color_manual(values = c("Lenient side" = "#1f77b4", "Strict side" = "#d62728")) +
  annotate("text",
    x = -bw_ft * 0.95, y = max(bins$mean_y, na.rm = TRUE),
    label = jump_label,
    hjust = 0, vjust = 1, size = 3.3, fontface = "bold", color = "#1f2d3d"
  ) +
  labs(
    title = "Rent Discontinuity at Ward Boundary (FE-Adjusted)",
    subtitle = sprintf("bw = %d ft | window = %s | sample = %s | controls = %s%s",
                        bw_ft, window, sample_filter, use_controls,
                        if (min_strictness_diff > 0) sprintf(" | min diff = %.1f", min_strictness_diff) else ""),
    x = "Signed distance to boundary (feet; right is stricter side)",
    y = "FE+Controls-Adjusted Log(Rent)",
    color = "",
    caption = "Points: binned means of FE+controls-adjusted outcome. Lines: fitted side-jump model."
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(output_pdf, p, width = 9, height = 6, dpi = 300, bg = "white")

# Save outputs
summary_df <- tibble(
  bw_ft = bw_ft,
  window = window,
  sample_filter = sample_filter,
  use_controls = use_controls,
  min_strictness_diff = min_strictness_diff,
  jump_estimate = b_side["estimate"],
  jump_se = b_side["se"],
  jump_p = b_side["p"],
  slope_left = b_x["estimate"],
  slope_diff = b_int["estimate"],
  n_obs = nobs(m),
  ward_pairs = dplyr::n_distinct(aug$ward_pair)
)
# write_csv(summary_df, output_csv)
# write_csv(bins, output_bins_csv)

message(sprintf("Saved: %s", output_pdf))
