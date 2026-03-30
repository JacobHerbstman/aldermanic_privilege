source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_fe_sensitivity/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 250
# window <- "pre_2021"
# sample_filter <- "all"
# shifts <- "-750,0,750"
# use_controls <- TRUE
# bins_per_side <- 6
# output_csv <- "../output/shifted_cutoff_visuals_pre_2021_all_bw250_shift750.csv"
# output_bins_csv <- "../output/shifted_cutoff_visuals_bins_pre_2021_all_bw250_shift750.csv"
# output_pdf <- "../output/shifted_cutoff_visuals_pre_2021_all_bw250_shift750.pdf"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_ft, window, sample_filter, shifts, use_controls, bins_per_side, output_csv, output_bins_csv, output_pdf)
}

if (length(cli_args) >= 10) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  shifts <- cli_args[5]
  use_controls <- tolower(cli_args[6]) %in% c("true", "t", "1", "yes")
  bins_per_side <- suppressWarnings(as.integer(cli_args[7]))
  output_csv <- cli_args[8]
  output_bins_csv <- cli_args[9]
  output_pdf <- cli_args[10]
} else {
  stop("FATAL: Script requires 10 args: <input> <bw_ft> <window> <sample_filter> <shifts> <use_controls> <bins_per_side> <output_csv> <output_bins_csv> <output_pdf>", call. = FALSE)
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

star_code <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

shift_values <- as.numeric(trimws(strsplit(shifts, ",")[[1]]))
shift_values <- shift_values[is.finite(shift_values)]
shift_values <- sort(unique(shift_values))
if (length(shift_values) == 0) {
  stop("No valid shifts provided.", call. = FALSE)
}
max_shift <- max(abs(shift_values))

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

summary_rows <- list()
bin_rows <- list()
fit_rows <- list()

bin_ft <- bw_ft / bins_per_side
bin_breaks <- seq(-bw_ft, bw_ft, by = bin_ft)
if (tail(bin_breaks, 1) < bw_ft) {
  bin_breaks <- c(bin_breaks, bw_ft)
}

for (s in shift_values) {
  df_s <- dat %>%
    mutate(
      shift_ft = s,
      shifted_dist = signed_dist - s,
      weight = pmax(0, 1 - abs(shifted_dist) / bw_ft)
    ) %>%
    filter(abs(shifted_dist) <= bw_ft)

  if (use_controls) {
    df_s <- df_s %>%
      filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths), !is.na(building_type_factor))
  }

  if (nrow(df_s) == 0 || length(unique(df_s$ward_pair)) < 2) {
    next
  }

  if (use_controls) {
    m_y <- feols(
      log(rent_price) ~ log_sqft + log_beds + log_baths + building_type_factor | ward_pair^year_month,
      data = df_s,
      weights = ~weight
    )
  } else {
    m_y <- feols(
      log(rent_price) ~ 1 | ward_pair^year_month,
      data = df_s,
      weights = ~weight
    )
  }

  df_s <- df_s %>% mutate(y_resid = resid(m_y), right = as.integer(shifted_dist >= 0))

  m_jump <- feols(
    y_resid ~ right + shifted_dist + right:shifted_dist,
    data = df_s,
    weights = ~weight,
    cluster = ~ward_pair
  )

  if (!"right" %in% names(coef(m_jump))) {
    next
  }

  b <- coef(m_jump)
  jump <- b[["right"]]
  jump_se <- se(m_jump)[["right"]]
  jump_p <- pvalue(m_jump)[["right"]]

  b0 <- ifelse("(Intercept)" %in% names(b), b[["(Intercept)"]], 0)
  b_dist <- ifelse("shifted_dist" %in% names(b), b[["shifted_dist"]], 0)
  int_name <- ifelse("right:shifted_dist" %in% names(b), "right:shifted_dist", "shifted_dist:right")
  b_int <- ifelse(int_name %in% names(b), b[[int_name]], 0)

  fit_left <- tibble(
    shift_ft = s,
    shifted_dist = seq(-bw_ft, 0, length.out = 120),
    fit = b0 + b_dist * shifted_dist,
    side = "Lenient side"
  )
  fit_right <- tibble(
    shift_ft = s,
    shifted_dist = seq(0, bw_ft, length.out = 120),
    fit = b0 + jump + (b_dist + b_int) * shifted_dist,
    side = "Strict side"
  )

  fit_rows[[length(fit_rows) + 1]] <- bind_rows(fit_left, fit_right)

  binned <- df_s %>%
    mutate(bin = cut(shifted_dist, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)) %>%
    filter(!is.na(bin)) %>%
    group_by(bin) %>%
    summarise(
      shift_ft = first(shift_ft),
      bin_center = weighted.mean(shifted_dist, w = weight, na.rm = TRUE),
      y_bin = weighted.mean(y_resid, w = weight, na.rm = TRUE),
      n_bin = n(),
      .groups = "drop"
    ) %>%
    mutate(side = if_else(bin_center >= 0, "Strict side", "Lenient side"))

  bin_rows[[length(bin_rows) + 1]] <- binned

  summary_rows[[length(summary_rows) + 1]] <- tibble(
    shift_ft = s,
    jump_estimate = jump,
    std_error = jump_se,
    p_value = jump_p,
    sig = star_code(jump_p),
    n_obs = nrow(df_s),
    ward_pairs = length(unique(df_s$ward_pair))
  )
}

summary_df <- bind_rows(summary_rows) %>% arrange(shift_ft)
bins_df <- bind_rows(bin_rows)
fits_df <- bind_rows(fit_rows)

if (nrow(summary_df) == 0 || nrow(bins_df) == 0 || nrow(fits_df) == 0) {
  stop("No models estimated for visual diagnostics.", call. = FALSE)
}

label_df <- summary_df %>%
  mutate(
    shift_label = case_when(
      shift_ft == 0 ~ "True cutoff (0 ft)",
      shift_ft > 0 ~ paste0("Placebo shift (+", shift_ft, " ft)"),
      TRUE ~ paste0("Placebo shift (", shift_ft, " ft)")
    ),
    annotation = sprintf("Jump = %.3f%s (SE %.3f)\\nN = %s, Pairs = %s", jump_estimate, sig, std_error, n_obs, ward_pairs)
  )

bins_df <- bins_df %>%
  left_join(label_df %>% select(shift_ft, shift_label), by = "shift_ft")
fits_df <- fits_df %>%
  left_join(label_df %>% select(shift_ft, shift_label), by = "shift_ft")

ann_pos <- bins_df %>%
  group_by(shift_ft, shift_label) %>%
  summarise(
    x = min(bin_center, na.rm = TRUE) + 0.05 * (max(bin_center, na.rm = TRUE) - min(bin_center, na.rm = TRUE)),
    y = max(y_bin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(label_df %>% select(shift_ft, annotation), by = "shift_ft")

# write_csv(summary_df, output_csv)
# write_csv(bins_df, output_bins_csv)

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray55") +
  geom_point(
    data = bins_df,
    aes(x = bin_center, y = y_bin, color = side),
    size = 2,
    alpha = 0.95
  ) +
  geom_line(
    data = fits_df,
    aes(x = shifted_dist, y = fit, color = side),
    linewidth = 1
  ) +
  geom_text(
    data = ann_pos,
    aes(x = x, y = y, label = annotation),
    hjust = 0,
    vjust = 1,
    size = 3.3,
    color = "#1f2d3d"
  ) +
  facet_wrap(~shift_label, ncol = 1) +
  scale_color_manual(values = c("Lenient side" = "#2C7FB8", "Strict side" = "#D95F02")) +
  labs(
    title = "Residualized Rent Border RD with Shifted-Cutoff Placebos",
    subtitle = sprintf("bw = %d ft | window = %s | sample = %s | controls = %s", bw_ft, window, sample_filter, use_controls),
    x = "Signed distance to tested cutoff (feet; right is stricter side)",
    y = "Residualized log(rent)",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(output_pdf, p, width = 9.5, height = 11, bg = "white")

message(sprintf("Saved: %s", output_csv))
message(sprintf("Saved: %s", output_bins_csv))
message(sprintf("Saved: %s", output_pdf))
