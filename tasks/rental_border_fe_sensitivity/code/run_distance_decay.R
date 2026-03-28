source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_fe_sensitivity/code")
# input <- "../input/rent_with_ward_distances.parquet"
# window <- "pre_2021"
# sample_filter <- "all"
# rings <- "0-250,250-500,500-1000,1000-1500"
# use_controls <- TRUE
# output_csv <- "../output/distance_decay_pre_2021_all.csv"
# output_pdf <- "../output/distance_decay_pre_2021_all.pdf"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, window, sample_filter, rings, use_controls, output_csv, output_pdf)
}

if (length(cli_args) >= 7) {
  input <- cli_args[1]
  window <- cli_args[2]
  sample_filter <- cli_args[3]
  rings <- cli_args[4]
  use_controls <- tolower(cli_args[5]) %in% c("true", "t", "1", "yes")
  output_csv <- cli_args[6]
  output_pdf <- cli_args[7]
} else {
  if (!exists("input") || !exists("window") || !exists("sample_filter") || !exists("rings") || !exists("use_controls") || !exists("output_csv") || !exists("output_pdf")) {
    stop("FATAL: Script requires 7 args: <input> <window> <sample_filter> <rings> <use_controls> <output_csv> <output_pdf>", call. = FALSE)
  }
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

parse_ring <- function(x) {
  vals <- as.numeric(strsplit(trimws(x), "-", fixed = TRUE)[[1]])
  if (length(vals) != 2 || any(!is.finite(vals)) || vals[1] >= vals[2]) {
    stop(sprintf("Invalid ring: %s", x), call. = FALSE)
  }
  tibble(lower = vals[1], upper = vals[2], label = sprintf("%d-%d", vals[1], vals[2]))
}

ring_list <- strsplit(rings, ",", fixed = TRUE)[[1]]
rings <- bind_rows(lapply(ring_list, parse_ring))
max_outer <- max(rings$upper)

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
    abs(signed_dist) <= max_outer
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
    strictness_std = strictness_own / sd_strict,
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    abs_dist = abs(signed_dist)
  )

results <- list()

for (i in seq_len(nrow(rings))) {
  lo <- rings$lower[i]
  hi <- rings$upper[i]

  df_i <- dat %>% filter(abs_dist > lo, abs_dist <= hi)
  if (use_controls) {
    df_i <- df_i %>% filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths), !is.na(building_type_factor))
  }

  if (nrow(df_i) == 0 || length(unique(df_i$ward_pair)) < 2) next

  if (use_controls) {
    m <- feols(
      log(rent_price) ~ strictness_std + log_sqft + log_beds + log_baths + building_type_factor | ward_pair^year_month,
      data = df_i,
      cluster = ~ward_pair
    )
  } else {
    m <- feols(
      log(rent_price) ~ strictness_std | ward_pair^year_month,
      data = df_i,
      cluster = ~ward_pair
    )
  }

  results[[length(results) + 1]] <- tibble(
    ring = rings$label[i],
    lower_ft = lo,
    upper_ft = hi,
    midpoint_ft = (lo + hi) / 2,
    estimate = coef(m)[["strictness_std"]],
    std_error = se(m)[["strictness_std"]],
    p_value = pvalue(m)[["strictness_std"]],
    n_obs = m$nobs,
    ward_pairs = length(unique(df_i$ward_pair)),
    controls = use_controls
  )
}

out <- bind_rows(results) %>% arrange(midpoint_ft)
if (nrow(out) == 0) stop("No distance-decay models estimated.", call. = FALSE)

# write_csv(out, output_csv)

p <- out %>%
  mutate(
    ci_lo = estimate - 1.96 * std_error,
    ci_hi = estimate + 1.96 * std_error
  ) %>%
  ggplot(aes(x = midpoint_ft, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 60, color = "#2C3E50") +
  geom_line(color = "#2C3E50", linewidth = 0.8) +
  geom_point(size = 2.5, color = "#2C3E50") +
  scale_x_continuous(breaks = out$midpoint_ft, labels = out$ring) +
  labs(
    title = "Distance-Decay Check: Rent Border FE Coefficient by Ring",
    subtitle = sprintf("window = %s | sample = %s | controls = %s", window, sample_filter, use_controls),
    x = "Distance ring (feet)",
    y = "Coefficient on strictness score"
  ) +
  theme_minimal(base_size = 12)

ggsave(output_pdf, p, width = 8, height = 5, bg = "white")

message(sprintf("Saved: %s", output_csv))
message(sprintf("Saved: %s", output_pdf))
