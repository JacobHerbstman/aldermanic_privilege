source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_pair_fe/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 500
# bin_width_ft <- 50
# window <- "pre_2023"
# sample_filter <- "all"
# use_controls <- TRUE
# output_pdf <- "../output/continuous_treatment_by_signed_distance_bw500_pre_2023_ctrl.pdf"
# output_csv <- "../output/continuous_treatment_by_signed_distance_bw500_pre_2023_ctrl.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_ft, bin_width_ft, window, sample_filter, use_controls, output_pdf, output_csv)
}

if (length(cli_args) >= 8) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  bin_width_ft <- suppressWarnings(as.integer(cli_args[3]))
  window <- cli_args[4]
  sample_filter <- cli_args[5]
  use_controls <- tolower(cli_args[6]) %in% c("true", "t", "1", "yes")
  output_pdf <- cli_args[7]
  output_csv <- cli_args[8]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("bin_width_ft") || !exists("window") || !exists("sample_filter") || !exists("use_controls") || !exists("output_pdf") || !exists("output_csv")) {
    stop("FATAL: Script requires 8 args: <input> <bw_ft> <bin_width_ft> <window> <sample_filter> <use_controls> <output_pdf> <output_csv>", call. = FALSE)
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

message("=== Continuous Treatment by Distance Plot ===")
message(sprintf("bw=%d | bin_width=%d | window=%s | sample=%s | controls=%s",
                bw_ft, bin_width_ft, window, sample_filter, use_controls))

if (!is.finite(bw_ft) || bw_ft <= 0 || !is.finite(bin_width_ft) || bin_width_ft <= 0) {
  stop("--bw_ft and --bin_width_ft must be positive integers", call. = FALSE)
}
if (bw_ft %% bin_width_ft != 0) {
  stop("--bw_ft must be divisible by --bin_width_ft", call. = FALSE)
}

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
    !is.na(strictness_neighbor),
    abs(signed_dist) <= bw_ft
  ) %>%
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

dat <- dat %>%
  mutate(
    treat_diff = strictness_own - strictness_neighbor,
    bin_id = floor(signed_dist / bin_width_ft),
    bin_id = if_else(bin_id == (bw_ft / bin_width_ft), bin_id - 1L, bin_id),
    bin_id = pmax(bin_id, -(bw_ft / bin_width_ft)),
    bin_id = pmin(bin_id, (bw_ft / bin_width_ft) - 1L),
    bin_center = (bin_id + 0.5) * bin_width_ft,
    side = if_else(bin_center >= 0, "More Uncertain", "Less Uncertain"),
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  )

if (use_controls) {
  dat <- dat %>% filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths))
}

n_type_levels <- n_distinct(dat$building_type_factor)
dat <- dat %>%
  mutate(bin_factor = factor(bin_center, levels = sort(unique(bin_center))))

rhs <- "0 + treat_diff:bin_factor"
if (use_controls) {
  ctrl <- "log_sqft + log_beds + log_baths"
  if (n_type_levels >= 2) ctrl <- paste0(ctrl, " + building_type_factor")
  rhs <- paste0(rhs, " + ", ctrl)
}
fml <- as.formula(paste0("log(rent_price) ~ ", rhs, " | ward_pair^year_month"))

m <- feols(fml, data = dat, cluster = ~ward_pair)
ct <- coeftable(m)

bin_stats <- dat %>%
  group_by(bin_id, bin_center, side) %>%
  summarise(
    n_obs = n(),
    ward_pairs = n_distinct(ward_pair),
    .groups = "drop"
  ) %>%
  arrange(bin_center)

get_bin_coef <- function(center) {
  term <- paste0("treat_diff:bin_factor", center)
  idx <- which(rownames(ct) == term)
  if (length(idx) == 0) {
    return(c(NA_real_, NA_real_, NA_real_))
  }
  c(
    ct[idx[1], "Estimate"],
    ct[idx[1], "Std. Error"],
    ct[idx[1], "Pr(>|t|)"]
  )
}

coef_list <- lapply(bin_stats$bin_center, get_bin_coef)
est <- bin_stats %>%
  mutate(
    estimate = vapply(coef_list, function(x) x[1], numeric(1)),
    std_error = vapply(coef_list, function(x) x[2], numeric(1)),
    p_value = vapply(coef_list, function(x) x[3], numeric(1))
  ) %>%
  mutate(
    ci_low = estimate - 1.96 * std_error,
    ci_high = estimate + 1.96 * std_error
  )

write_csv(est, output_csv)

valid <- est %>% filter(is.finite(estimate), is.finite(std_error))
if (nrow(valid) == 0) {
  stop("No valid bin-level estimates were produced.", call. = FALSE)
}

overall_label <- valid %>%
  summarise(
    mean_beta = mean(estimate, na.rm = TRUE),
    mean_se = mean(std_error, na.rm = TRUE),
    total_n = nobs(m),
    pairs = max(ward_pairs, na.rm = TRUE)
  ) %>%
  transmute(lbl = sprintf(
    "Mean bin coef = %.4f (avg SE %.4f)\nBin width = %d ft | Estimation N = %s | Max pairs = %d",
    mean_beta, mean_se, bin_width_ft, format(total_n, big.mark = ","), pairs
  )) %>%
  pull(lbl)

p <- ggplot(valid, aes(x = bin_center, y = estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.8) +
  geom_linerange(aes(ymin = ci_low, ymax = ci_high, color = side), alpha = 0.7) +
  geom_point(aes(size = n_obs, color = side), alpha = 0.9) +
  geom_line(aes(color = side), linewidth = 0.9, alpha = 0.85) +
  scale_color_manual(values = c("Less Uncertain" = "#1f77b4", "More Uncertain" = "#d62728"), name = "") +
  scale_x_continuous(
    breaks = seq(-bw_ft + (bin_width_ft / 2), bw_ft - (bin_width_ft / 2), by = 100)
  ) +
  scale_size_continuous(name = "Bin N", range = c(2.2, 5.2)) +
  annotate("text", x = -Inf, y = Inf, label = overall_label,
           hjust = -0.05, vjust = 1.4, size = 3.2, fontface = "bold") +
  labs(
    title = "Continuous Treatment Effect by Signed Distance to Boundary",
    subtitle = sprintf("Coef on (strictness_own - strictness_neighbor) by signed %dft bin | bw=%d | window=%s | controls=%s",
                       bin_width_ft, bw_ft, window, use_controls),
    x = "Distance to Ward Boundary (feet; positive = more uncertain side)",
    y = "Estimated Effect on Log(Rent)",
    caption = "Each point is a separate FE regression within a distance bin: log(rent) ~ strictness gap + controls | ward-pair x year-month FE."
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right", panel.grid.minor = element_blank())

ggsave(output_pdf, p, width = 9.2, height = 6.2, dpi = 300, bg = "white")
message(sprintf("Saved: %s", output_pdf))
message(sprintf("Saved: %s", output_csv))
