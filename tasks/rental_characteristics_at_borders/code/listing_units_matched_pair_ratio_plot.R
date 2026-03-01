source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 500
# window <- "pre_2023"
# sample_filter <- "all"
# unit_def <- "unit_proxy"
# min_strictness_diff_pctile <- 0
# output_pdf <- NA
# output_csv <- NA_character_
# Rscript listing_units_matched_pair_ratio_plot.R "../input/rent_with_ward_distances.parquet" 500 "pre_2023" "all" "unit_proxy" 0 NA NA_character_
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 8) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  unit_def <- cli_args[5]
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[6]))
  output_pdf <- cli_args[7]
  output_csv <- cli_args[8]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") || !exists("unit_def") || !exists("min_strictness_diff_pctile") || !exists("output_pdf") || !exists("output_csv")) {
    stop("FATAL: Script requires 8 args: <input> <bw_ft> <window> <sample_filter> <unit_def> <min_strictness_diff_pctile> <output_pdf> <output_csv>", call. = FALSE)
  }
}

if (!unit_def %in% c("id", "loc_key", "unit_proxy")) {
  stop("--unit_def must be one of: id, loc_key, unit_proxy", call. = FALSE)
}

prune_sample_raw <- tolower(Sys.getenv("PRUNE_SAMPLE", "all"))
if (prune_sample_raw %in% c("all", "false", "f", "0", "no", "off")) {
  prune_sample <- "all"
} else if (prune_sample_raw %in% c("pruned", "true", "t", "1", "yes", "on")) {
  prune_sample <- "pruned"
} else {
  stop("PRUNE_SAMPLE must map to one of: all/false/0 or pruned/true/1", call. = FALSE)
}
confound_flags_path <- Sys.getenv("CONFOUND_FLAGS_PATH", "../input/confounded_pair_era_flags.csv")

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  out <- rep(NA_character_, length(x))
  if (!any(ok)) return(out)
  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) return(NA_character_)
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
  out
}

era_from_year <- function(y) {
  y <- as.integer(y)
  ifelse(
    y < 2003L, "1998_2002",
    ifelse(y < 2015L, "2003_2014", ifelse(y < 2023L, "2015_2023", "post_2023"))
  )
}

apply_window <- function(df, w) {
  if (w == "full") return(df)
  if (w == "pre_2021") return(df %>% filter(year <= 2020))
  if (w == "pre_2023") return(df %>% filter(year <= 2022))
  if (w == "pre_covid") return(df %>% filter(year <= 2019))
  df
}

message(sprintf("=== Matched-Pair Ratio Plot | bw=%d | window=%s | sample=%s | pctile=%d | unit_def=%s ===",
                bw_ft, window, sample_filter, min_strictness_diff_pctile, unit_def))
message(sprintf("Pruning spec: %s", prune_sample))

dat <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    right = as.integer(signed_dist >= 0),
    listing_id = as.character(id),
    loc_key = paste(round(latitude, 5), round(longitude, 5), sep = "_"),
    unit_proxy_key = paste(
      round(latitude, 5),
      round(longitude, 5),
      coalesce(as.character(beds), "NA"),
      coalesce(as.character(baths), "NA"),
      coalesce(as.character(sqft), "NA"),
      sep = "_"
    )
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(segment_id), segment_id != "",
    !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
    !is.na(latitude), !is.na(longitude),
    abs(signed_dist) <= bw_ft
  ) %>%
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

if (prune_sample == "pruned") {
  if (!file.exists(confound_flags_path)) {
    stop(sprintf("Missing confound flags file for pruned run: %s", confound_flags_path), call. = FALSE)
  }

  conf_flags <- read_csv(
    confound_flags_path,
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "drop_confound")
  ) %>%
    transmute(
      pair_dash = normalize_pair_dash(ward_pair_id_dash),
      era = as.character(era),
      keep_pair_era = !as.logical(drop_confound)
    ) %>%
    distinct()

  if (anyNA(conf_flags$pair_dash) || anyNA(conf_flags$era)) {
    stop("Confound flags have invalid pair/era keys.", call. = FALSE)
  }
  if (anyDuplicated(conf_flags[, c("pair_dash", "era")]) > 0) {
    stop("Confound flags contain duplicate pair-era keys.", call. = FALSE)
  }

  dat <- dat %>%
    mutate(
      pair_dash = normalize_pair_dash(ward_pair),
      era = era_from_year(year)
    ) %>%
    left_join(conf_flags, by = c("pair_dash", "era"))

  n_missing <- sum(is.na(dat$keep_pair_era))
  if (n_missing > 0) {
    message(sprintf(
      "Pruned run: %d observations have no pair-era pruning flag and will be dropped.",
      n_missing
    ))
    dat <- dat %>% mutate(keep_pair_era = if_else(is.na(keep_pair_era), FALSE, keep_pair_era))
  }

  n_before_prune <- nrow(dat)
  dat <- dat %>% filter(keep_pair_era)
  message(sprintf("Observations after pair-era pruning: %d -> %d", n_before_prune, nrow(dat)))
}

if (min_strictness_diff_pctile > 0) {
  segment_diffs <- dat %>%
    group_by(segment_id) %>%
    summarise(diff = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE), .groups = "drop")
  cutoff <- quantile(segment_diffs$diff, min_strictness_diff_pctile / 100, na.rm = TRUE)
  keep_segments <- segment_diffs %>% filter(diff >= cutoff) %>% pull(segment_id)
  dat <- dat %>% filter(segment_id %in% keep_segments)
  message(sprintf("  After p%d filter (cutoff=%.3f): %d obs, %d segments",
                  min_strictness_diff_pctile, cutoff, nrow(dat), n_distinct(dat$segment_id)))
}

dat <- dat %>%
  mutate(unit_key = case_when(
    unit_def == "id" ~ listing_id,
    unit_def == "loc_key" ~ loc_key,
    unit_def == "unit_proxy" ~ unit_proxy_key,
    TRUE ~ listing_id
  )) %>%
  filter(!is.na(unit_key), unit_key != "")

segment_month <- dat %>% distinct(segment_id, year_month)
side_template <- segment_month %>% tidyr::crossing(right = c(0L, 1L))

side_counts <- dat %>%
  distinct(segment_id, year_month, right, unit_key) %>%
  count(segment_id, year_month, right, name = "n_units")

side_strict <- dat %>%
  group_by(segment_id, year_month, right) %>%
  summarise(strict_side = median(strictness_own, na.rm = TRUE), .groups = "drop")

side_panel <- side_template %>%
  left_join(side_strict, by = c("segment_id", "year_month", "right")) %>%
  left_join(side_counts, by = c("segment_id", "year_month", "right")) %>%
  mutate(n_units = as.integer(coalesce(n_units, 0L)))

wide <- side_panel %>%
  select(segment_id, year_month, right, strict_side, n_units) %>%
  pivot_wider(
    names_from = right,
    values_from = c(strict_side, n_units),
    names_glue = "{.value}_r{right}"
  )

plot_df <- wide %>%
  filter(!is.na(strict_side_r0), !is.na(strict_side_r1)) %>%
  mutate(
    strict_diff = abs(strict_side_r1 - strict_side_r0),
    n_high = if_else(strict_side_r1 >= strict_side_r0, n_units_r1, n_units_r0),
    n_low = if_else(strict_side_r1 >= strict_side_r0, n_units_r0, n_units_r1)
  ) %>%
  filter(n_high > 0, n_low > 0) %>%
  mutate(
    log_ratio = log(n_high) - log(n_low),
    weight_fit = n_high + n_low
  )

if (nrow(plot_df) == 0) {
  stop("No pair-month cells with positive listings on both sides after filters.", call. = FALSE)
}

fit_visual <- lm(log_ratio ~ strict_diff, data = plot_df, weights = weight_fit)
fit_reg <- feols(log_ratio ~ strict_diff, data = plot_df, weights = ~weight_fit, cluster = ~segment_id)
ct <- coeftable(fit_reg)
p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)
if (length(p_col) == 0) {
  stop("Could not find p-value column in coeftable output.", call. = FALSE)
}

b <- ct["strict_diff", "Estimate"]
se <- ct["strict_diff", "Std. Error"]
pv <- ct["strict_diff", p_col[1]]

x_seq <- seq(min(plot_df$strict_diff), max(plot_df$strict_diff), length.out = 200)
pred <- predict(fit_visual, newdata = tibble(strict_diff = x_seq), se.fit = TRUE)
pred_df <- tibble(
  strict_diff = x_seq,
  fit = as.numeric(pred$fit),
  lwr = as.numeric(pred$fit - 1.96 * pred$se.fit),
  upr = as.numeric(pred$fit + 1.96 * pred$se.fit)
)

label <- sprintf(
  "Weighted slope = %.3f%s (SE %.3f)\nN = %s segment-month cells, %d segments",
  b, stars(pv), se, format(nrow(plot_df), big.mark = ","), n_distinct(plot_df$segment_id)
)

p <- ggplot(plot_df, aes(x = strict_diff, y = log_ratio)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_point(aes(size = weight_fit), alpha = 0.25, color = "#1f77b4") +
  geom_ribbon(
    data = pred_df,
    aes(x = strict_diff, ymin = lwr, ymax = upr),
    inherit.aes = FALSE,
    fill = "#d62728",
    alpha = 0.2
  ) +
  geom_line(
    data = pred_df,
    aes(x = strict_diff, y = fit),
    inherit.aes = FALSE,
    color = "#d62728",
    linewidth = 1
  ) +
  annotate("text", x = -Inf, y = Inf, label = label, hjust = -0.05, vjust = 1.3, size = 3.2, fontface = "bold") +
  scale_size_continuous(range = c(0.8, 3.5), guide = "none") +
  labs(
    x = "Strictness Gap (More Uncertain - Less Uncertain)",
    y = "log(N_high) - log(N_low)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )

ggsave(output_pdf, p, width = 7, height = 5, dpi = 300, bg = "white")
message(sprintf("Saved: %s", output_pdf))

if (!is.na(output_csv) && nzchar(output_csv)) {
  write_csv(plot_df, output_csv)
  message(sprintf("Saved: %s", output_csv))
}
