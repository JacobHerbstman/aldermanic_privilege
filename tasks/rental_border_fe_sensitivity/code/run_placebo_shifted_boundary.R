source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_fe_sensitivity/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 250
# window <- "pre_2021"
# sample_filter <- "all"
# shifts <- "-750,750"
# use_controls <- TRUE
# output_csv <- "../output/placebo_shifted_pre_2021_all_bw250.csv"
# output_pdf <- "../output/placebo_shifted_pre_2021_all_bw250.pdf"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_ft, window, sample_filter, shifts, use_controls, output_csv, output_pdf)
}

if (length(cli_args) >= 8) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  shifts <- cli_args[5]
  use_controls <- tolower(cli_args[6]) %in% c("true", "t", "1", "yes")
  output_csv <- cli_args[7]
  output_pdf <- cli_args[8]
} else {
  stop("FATAL: Script requires 8 args: <input> <bw_ft> <window> <sample_filter> <shifts> <use_controls> <output_csv> <output_pdf>", call. = FALSE)
}

if (!window %in% c("full", "pre_covid", "pre_2021", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, drop_mid", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}

apply_window <- function(df, window_name) {
  if (window_name == "full") return(df)
  if (window_name == "pre_covid") return(df %>% filter(year <= 2019))
  if (window_name == "pre_2021") return(df %>% filter(year <= 2020))
  if (window_name == "drop_mid") return(df %>% filter(year <= 2020 | year >= 2024))
  df
}

shift_values <- as.numeric(trimws(strsplit(shifts, ",")[[1]]))
shift_values <- shift_values[is.finite(shift_values)]
if (length(shift_values) == 0) {
  stop("No valid placebo shifts provided.", call. = FALSE)
}
all_shifts <- c(0, shift_values)
max_shift <- max(abs(all_shifts))

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
    !is.na(strictness_own), !is.na(strictness_neighbor),
    abs(signed_dist) <= (max_shift + bw_ft)
  ) %>%
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

sd_strict <- sd(dat$strictness_own, na.rm = TRUE)
if (!is.finite(sd_strict) || sd_strict <= 0) {
  stop("strictness_own has invalid SD in filtered sample.", call. = FALSE)
}

dat <- dat %>%
  mutate(
    strict_high = pmax(strictness_own, strictness_neighbor),
    strict_low = pmin(strictness_own, strictness_neighbor),
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  )

results <- list()

for (s in all_shifts) {
  df_s <- dat %>%
    mutate(
      shifted_dist = signed_dist - s,
      weight = pmax(0, 1 - abs(shifted_dist) / bw_ft)
    ) %>%
    filter(abs(shifted_dist) <= bw_ft)

  if (use_controls) {
    df_s <- df_s %>%
      filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths), !is.na(building_type_factor))
  }

  if (nrow(df_s) == 0 || length(unique(df_s$ward_pair)) < 2) next

  df_s <- df_s %>%
    mutate(
      placebo_score = if_else(shifted_dist >= 0, strict_high, strict_low) / sd_strict
    )

  if (use_controls) {
    m_y <- feols(
      log(rent_price) ~ log_sqft + log_beds + log_baths + building_type_factor | ward_pair^year_month,
      data = df_s,
      weights = ~weight
    )
    m_x <- feols(
      placebo_score ~ log_sqft + log_beds + log_baths + building_type_factor | ward_pair^year_month,
      data = df_s,
      weights = ~weight
    )
    m_direct <- feols(
      log(rent_price) ~ placebo_score + log_sqft + log_beds + log_baths + building_type_factor | ward_pair^year_month,
      data = df_s,
      weights = ~weight,
      cluster = ~ward_pair
    )
  } else {
    m_y <- feols(log(rent_price) ~ 1 | ward_pair^year_month, data = df_s, weights = ~weight)
    m_x <- feols(placebo_score ~ 1 | ward_pair^year_month, data = df_s, weights = ~weight)
    m_direct <- feols(
      log(rent_price) ~ placebo_score | ward_pair^year_month,
      data = df_s,
      weights = ~weight,
      cluster = ~ward_pair
    )
  }

  df_s <- df_s %>%
    mutate(
      y_resid = resid(m_y),
      x_resid = resid(m_x)
    )

  m_resid <- feols(
    y_resid ~ x_resid,
    data = df_s,
    weights = ~weight,
    cluster = ~ward_pair
  )

  b_res <- coef(m_resid)[["x_resid"]]
  se_res <- se(m_resid)[["x_resid"]]
  p_res <- pvalue(m_resid)[["x_resid"]]
  b_dir <- coef(m_direct)[["placebo_score"]]

  results[[length(results) + 1]] <- tibble(
    shift_ft = s,
    estimate_residualized = b_res,
    std_error = se_res,
    p_value = p_res,
    estimate_direct = b_dir,
    abs_diff_resid_vs_direct = abs(b_res - b_dir),
    n_obs = nrow(df_s),
    ward_pairs = length(unique(df_s$ward_pair)),
    controls = use_controls
  )
}

out <- bind_rows(results) %>% arrange(shift_ft)
if (nrow(out) == 0) stop("No placebo models estimated.", call. = FALSE)

# write_csv(out, output_csv)

plot_df <- out %>%
  mutate(
    ci_lo = estimate_residualized - 1.96 * std_error,
    ci_hi = estimate_residualized + 1.96 * std_error,
    type = if_else(shift_ft == 0, "True cutoff", "Shifted placebo")
  )

p <- ggplot(plot_df, aes(x = shift_ft, y = estimate_residualized, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 40, linewidth = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(values = c("True cutoff" = "#2C3E50", "Shifted placebo" = "#C44E52")) +
  labs(
    title = "Shifted-Cutoff Placebo Test (Residualized Border FE)",
    subtitle = sprintf("bw = %d ft | window = %s | sample = %s", bw_ft, window, sample_filter),
    x = "Cutoff shift (feet)",
    y = "Coefficient on placebo strictness score"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(output_pdf, p, width = 8, height = 5, bg = "white")

message(sprintf("Saved: %s", output_csv))
message(sprintf("Saved: %s", output_pdf))
