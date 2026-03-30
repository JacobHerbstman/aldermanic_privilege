source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_fe_sensitivity/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# sample_filter <- "all"
# shifts <- "-750,-500,-250,0,250,500,750"
# use_controls <- TRUE
# output_csv <- "../output/placebo_main_spec_pre_2021_all_bw1000.csv"
# output_pdf <- "../output/placebo_main_spec_pre_2021_all_bw1000.pdf"

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

if (!window %in% c("full", "pre_covid", "pre_2021", "pre_2023", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, pre_2023, drop_mid", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("--bw_ft must be positive", call. = FALSE)
}

apply_window <- function(df, window_name) {
  if (window_name == "full") return(df)
  if (window_name == "pre_covid") return(df %>% filter(year <= 2019))
  if (window_name == "pre_2021") return(df %>% filter(year <= 2020))
  if (window_name == "pre_2023") return(df %>% filter(year <= 2022))
  if (window_name == "drop_mid") return(df %>% filter(year <= 2020 | year >= 2024))
  df
}

shift_values <- as.numeric(trimws(strsplit(shifts, ",")[[1]]))
shift_values <- shift_values[is.finite(shift_values)]
shift_values <- sort(unique(shift_values))
if (length(shift_values) == 0) {
  stop("No valid shifts provided.", call. = FALSE)
}
max_shift <- max(abs(shift_values))

message("=== Placebo Shifted Main Spec ===")
message(sprintf("Bandwidth: %d ft | Window: %s | Shifts: %s",
                bw_ft, window, paste(shift_values, collapse = ", ")))

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
    abs(signed_dist) <= (bw_ft + max_shift)
  ) %>%
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

dat <- dat %>%
  mutate(
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  )

results <- list()

for (s in shift_values) {
  df_s <- dat %>%
    mutate(shifted_dist = signed_dist - s) %>%
    filter(abs(shifted_dist) <= bw_ft)

  if (use_controls) {
    df_s <- df_s %>%
      filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths), !is.na(building_type_factor))
  }

  if (nrow(df_s) == 0 || length(unique(df_s$ward_pair)) < 2) next

  # Standardize strictness within this shifted sample
  sd_strict <- sd(df_s$strictness_own, na.rm = TRUE)
  if (!is.finite(sd_strict) || sd_strict <= 0) next

  df_s <- df_s %>% mutate(strictness_std = strictness_own / sd_strict)

  # Run the SAME regression as the main table
  n_type_levels <- n_distinct(df_s$building_type_factor)
  if (use_controls) {
    rhs <- "strictness_std + log_sqft + log_beds + log_baths"
    if (n_type_levels >= 2) rhs <- paste0(rhs, " + building_type_factor")
  } else {
    rhs <- "strictness_std"
  }

  m <- tryCatch(
    feols(
      as.formula(paste0("log(rent_price) ~ ", rhs, " | ward_pair^year_month")),
      data = df_s,
      cluster = ~ward_pair
    ),
    error = function(e) NULL
  )

  if (is.null(m) || !"strictness_std" %in% names(coef(m))) next

  results[[length(results) + 1]] <- tibble(
    shift_ft = s,
    estimate = coef(m)[["strictness_std"]],
    std_error = se(m)[["strictness_std"]],
    p_value = pvalue(m)[["strictness_std"]],
    n_obs = m$nobs,
    ward_pairs = length(unique(df_s$ward_pair)),
    sd_strictness = sd_strict,
    controls = use_controls
  )

  message(sprintf("  shift=%+5d: coef=%.4f (SE=%.4f, p=%.3f) N=%d pairs=%d sd_strict=%.3f",
                  s, coef(m)[["strictness_std"]], se(m)[["strictness_std"]],
                  pvalue(m)[["strictness_std"]], m$nobs,
                  length(unique(df_s$ward_pair)), sd_strict))
}

out <- bind_rows(results) %>% arrange(shift_ft)
if (nrow(out) == 0) stop("No models estimated.", call. = FALSE)

write_csv(out, output_csv)

plot_df <- out %>%
  mutate(
    ci_lo = estimate - 1.96 * std_error,
    ci_hi = estimate + 1.96 * std_error,
    type = if_else(shift_ft == 0, "True cutoff", "Shifted placebo")
  )

p <- ggplot(plot_df, aes(x = shift_ft, y = estimate, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 30, linewidth = 0.7) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("True cutoff" = "#2C3E50", "Shifted placebo" = "#C44E52")) +
  labs(
    title = "Placebo Test: Main Spec at Shifted Boundaries",
    subtitle = sprintf("bw = %d ft | window = %s | sample = %s | controls = %s",
                        bw_ft, window, sample_filter, use_controls),
    x = "Boundary shift (feet; 0 = true boundary)",
    y = "Coefficient on standardized strictness score",
    color = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(output_pdf, p, width = 9, height = 5.5, dpi = 300, bg = "white")

message(sprintf("Saved: %s", output_csv))
message(sprintf("Saved: %s", output_pdf))
