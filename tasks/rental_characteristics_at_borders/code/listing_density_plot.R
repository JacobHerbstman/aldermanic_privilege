source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# sample_filter <- "all"
# min_strictness_diff_pctile <- 0
# bins_per_side <- 15
# output_pdf <- NA
# Rscript listing_density_plot.R "../input/rent_with_ward_distances.parquet" 1000 "pre_2021" "all" 0 15 NA
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 7) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[5]))
  bins_per_side <- suppressWarnings(as.integer(cli_args[6]))
  output_pdf <- cli_args[7]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") || !exists("min_strictness_diff_pctile") || !exists("bins_per_side") || !exists("output_pdf")) {
    stop("FATAL: Script requires 7 args: <input> <bw_ft> <window> <sample_filter> <min_strictness_diff_pctile> <bins_per_side> <output_pdf>", call. = FALSE)
  }
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

message(sprintf("=== Listing Density Plot | bw=%d | window=%s | sample=%s | pctile=%d ===",
                bw_ft, window, sample_filter, min_strictness_diff_pctile))
message(sprintf("Pruning spec: %s", prune_sample))

# ── Load and filter ──
dat <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    loc_key = paste(round(latitude, 5), round(longitude, 5))
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(segment_id), segment_id != "",
    !is.na(signed_dist), !is.na(strictness_own),
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
  # Within-segment strictness differences can vary across months/years; use the
  # segment-level median so the percentile filter is stable and order-invariant.
  segment_diffs <- dat %>%
    group_by(segment_id) %>%
    summarise(diff = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE), .groups = "drop")
  cutoff <- quantile(segment_diffs$diff, min_strictness_diff_pctile / 100, na.rm = TRUE)
  keep_segments <- segment_diffs %>% filter(diff >= cutoff) %>% pull(segment_id)
  dat <- dat %>% filter(segment_id %in% keep_segments)
  message(sprintf("  After p%d filter (cutoff=%.3f): %d obs, %d segments",
                  min_strictness_diff_pctile, cutoff, nrow(dat), n_distinct(dat$segment_id)))
}

dat <- dat %>% mutate(right = as.integer(signed_dist >= 0))

# ── Side-level regression: log(count) ~ right | segment_id^year_month ──
side_cells <- dat %>%
  distinct(segment_id, right, year_month, loc_key) %>%
  group_by(segment_id, right, year_month) %>%
  summarise(n_listings = n(), .groups = "drop") %>%
  mutate(log_n = log(n_listings))

m <- feols(log_n ~ right | segment_id^year_month, data = side_cells, cluster = ~segment_id)
ct <- coeftable(m)
b_right <- ct["right", "Estimate"]
se_right <- ct["right", "Std. Error"]
p_right <- ct["right", "Pr(>|t|)"]

message(sprintf("  Side-level: b=%.4f (SE %.4f, p=%.3f), N cells=%s, %d segments",
                b_right, se_right, p_right,
                format(nobs(m), big.mark = ","), n_distinct(side_cells$segment_id)))

# ── Bin-level counts for plot visual ──
bin_w <- bw_ft / bins_per_side
bin_cells <- dat %>%
  mutate(bin_center = (floor(signed_dist / bin_w) + 0.5) * bin_w) %>%
  distinct(segment_id, bin_center, year_month, loc_key) %>%
  group_by(segment_id, bin_center, year_month) %>%
  summarise(n_listings = n(), .groups = "drop") %>%
  mutate(right = as.integer(bin_center >= 0), log_n = log(n_listings))

# Frisch-Waugh on bin-level data using the side-level model
m_bin <- feols(log_n ~ right | segment_id^year_month, data = bin_cells, cluster = ~segment_id)
removed <- m_bin$obs_selection$obsRemoved
keep_idx <- if (is.null(removed)) seq_len(nrow(bin_cells)) else setdiff(seq_len(nrow(bin_cells)), abs(as.integer(removed)))
aug <- bin_cells[keep_idx, , drop = FALSE]
stopifnot(nrow(aug) == nobs(m_bin))
aug$y_adj <- as.numeric(resid(m_bin)) + b_right * aug$right

# Bin-level means
bins <- aug %>%
  group_by(bin_center) %>%
  summarise(mean_y = mean(y_adj), side = if_else(first(bin_center) >= 0, "More Uncertain", "Less Uncertain"),
            .groups = "drop")

mean_left <- mean(aug$y_adj[aug$right == 0])
mean_right <- mean(aug$y_adj[aug$right == 1])

line_df <- bind_rows(
  tibble(x = c(-bw_ft, 0), y = mean_left, side = "Less Uncertain"),
  tibble(x = c(0, bw_ft), y = mean_right, side = "More Uncertain")
)

gap_label <- sprintf("Gap = %.4f%s (SE %.4f)\nN = %s segment-side-months | %d segments",
                     b_right, stars(p_right), se_right,
                     format(nobs(m), big.mark = ","), n_distinct(side_cells$segment_id))

sample_label <- sample_filter
if (prune_sample == "pruned") {
  sample_label <- paste0(sample_label, "_pruned")
}

ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.5, alpha = 0.9) +
  geom_line(data = line_df, aes(x = x, y = y, color = side), linewidth = 1.1) +
  scale_color_manual(values = c("Less Uncertain" = "#1f77b4", "More Uncertain" = "#d62728"), name = "") +
  annotate("text", x = -Inf, y = Inf, label = gap_label,
           hjust = -0.05, vjust = 1.5, size = 3.3, fontface = "bold") +
  labs(title = "Rental Listing Density by Side of Ward Boundary",
       subtitle = sprintf("bw=%d ft | sample=%s%s",
                        bw_ft, sample_label,
                        if (min_strictness_diff_pctile > 0) sprintf(" | top %d%% segments", 100 - min_strictness_diff_pctile) else ""),
       x = "Distance to Ward Boundary (feet; positive = more uncertain side)",
       y = "FE-Adjusted Log(Distinct Listings per Bin)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(output_pdf, width = 8.6, height = 6, dpi = 300, bg = "white")
message(sprintf("Saved: %s", output_pdf))
