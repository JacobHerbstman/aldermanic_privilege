source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--window", type = "character", default = "pre_2021"),
  make_option("--sample_filter", type = "character", default = "all"),
  make_option("--unit_def", type = "character", default = "unit_proxy"),
  make_option("--min_strictness_diff_pctile", type = "integer", default = 0),
  make_option("--output_pdf", type = "character")
)
opt <- parse_args(OptionParser(option_list = option_list))

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

apply_window <- function(df, w) {
  if (w == "full") return(df)
  if (w == "pre_2021") return(df %>% filter(year <= 2020))
  if (w == "pre_2023") return(df %>% filter(year <= 2022))
  if (w == "pre_covid") return(df %>% filter(year <= 2019))
  df
}

message(sprintf("=== Side Comparison Plot | bw=%d | window=%s | sample=%s | pctile=%d | unit_def=%s ===",
                opt$bw_ft, opt$window, opt$sample_filter, opt$min_strictness_diff_pctile, opt$unit_def))

stopifnot(opt$unit_def %in% c("id", "loc_key", "unit_proxy"))

dat <- read_parquet(opt$input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    right = as.integer(signed_dist >= 0),
    listing_id = as.character(id),
    loc_key = paste(round(latitude, 5), round(longitude, 5), sep = "_"),
    unit_proxy_key = paste(
      round(latitude, 5), round(longitude, 5),
      coalesce(as.character(beds), "NA"),
      coalesce(as.character(baths), "NA"),
      coalesce(as.character(sqft), "NA"),
      sep = "_"
    ),
    strict_more = pmax(strictness_own, strictness_neighbor),
    strict_less = pmin(strictness_own, strictness_neighbor)
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
    !is.na(latitude), !is.na(longitude),
    abs(signed_dist) <= opt$bw_ft
  ) %>%
  apply_window(opt$window)

dat <- dat %>%
  mutate(unit_key = case_when(
    opt$unit_def == "id" ~ if_else(!is.na(listing_id) & listing_id != "", listing_id, unit_proxy_key),
    opt$unit_def == "loc_key" ~ loc_key,
    opt$unit_def == "unit_proxy" ~ unit_proxy_key
  )) %>%
  filter(!is.na(unit_key), unit_key != "")

if (opt$sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

if (opt$min_strictness_diff_pctile > 0) {
  pair_diffs <- dat %>%
    group_by(ward_pair) %>%
    summarise(diff = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE), .groups = "drop")
  cutoff <- quantile(pair_diffs$diff, opt$min_strictness_diff_pctile / 100, na.rm = TRUE)
  keep_pairs <- pair_diffs %>% filter(diff >= cutoff) %>% pull(ward_pair)
  dat <- dat %>% filter(ward_pair %in% keep_pairs)
}

# --- Build balanced panel (same as PPML spec) ---
pair_month_map <- dat %>%
  group_by(ward_pair, year_month) %>%
  summarise(
    strict_more = max(strict_more, na.rm = TRUE),
    strict_less = min(strict_less, na.rm = TRUE),
    .groups = "drop"
  )

side_template <- bind_rows(
  pair_month_map %>% transmute(ward_pair, year_month, right = 0L, strictness_own = strict_less),
  pair_month_map %>% transmute(ward_pair, year_month, right = 1L, strictness_own = strict_more)
)

side_counts <- dat %>%
  distinct(ward_pair, right, year_month, unit_key) %>%
  count(ward_pair, right, year_month, name = "n_units")

panel <- side_template %>%
  left_join(side_counts, by = c("ward_pair", "right", "year_month")) %>%
  mutate(n_units = as.integer(coalesce(n_units, 0L)))

strictness_sd <- sd(panel$strictness_own, na.rm = TRUE)
panel <- panel %>% mutate(strictness_std = strictness_own / strictness_sd)

# --- PPML regression (matches main table) ---
m <- fepois(n_units ~ strictness_std | ward_pair^year_month, data = panel, cluster = ~ward_pair)
ct <- coeftable(m)
p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)
b <- ct["strictness_std", "Estimate"]
se <- ct["strictness_std", "Std. Error"]
pv <- ct["strictness_std", p_col[1]]
implied_pct <- 100 * (exp(b) - 1)

message(sprintf("  PPML: b=%.4f (SE %.4f, p=%.3f), implied=%.2f%%", b, se, pv, implied_pct))

# --- Residualize for plot ---
# Use OLS on log(n_units+1) for Frisch-Waugh visual (PPML has no clean residuals)
panel_pos <- panel %>% filter(n_units > 0) %>% mutate(log_n = log(n_units))
m_ols <- feols(log_n ~ right | ward_pair^year_month, data = panel_pos, cluster = ~ward_pair)
ct_ols <- coeftable(m_ols)
b_ols <- ct_ols["right", "Estimate"]

removed <- m_ols$obs_selection$obsRemoved
keep_idx <- if (is.null(removed)) seq_len(nrow(panel_pos)) else setdiff(seq_len(nrow(panel_pos)), abs(as.integer(removed)))
aug <- panel_pos[keep_idx, , drop = FALSE]
stopifnot(nrow(aug) == nobs(m_ols))
aug$y_adj <- as.numeric(resid(m_ols)) + b_ols * aug$right

# --- Side means with CI ---
side_stats <- aug %>%
  group_by(right) %>%
  summarise(
    mean_y = mean(y_adj),
    se_y = sd(y_adj) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    side = if_else(right == 1, "More Uncertain", "Less Uncertain"),
    side = factor(side, levels = c("Less Uncertain", "More Uncertain"))
  )

ppml_label <- sprintf(
  "PPML: %.3f%s (SE %.3f)\nImplied: %.1f%% per 1 SD uncertainty",
  b, stars(pv), se, implied_pct
)

p <- ggplot(side_stats, aes(x = side, y = mean_y, color = side)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_errorbar(aes(ymin = mean_y - 1.96 * se_y, ymax = mean_y + 1.96 * se_y),
                width = 0.15, linewidth = 0.8) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Less Uncertain" = "#1f77b4", "More Uncertain" = "#d62728")) +
  annotate("text", x = 1.5, y = max(side_stats$mean_y + 1.96 * side_stats$se_y) + 0.01,
           label = ppml_label, size = 3.3, fontface = "bold") +
  labs(
    x = NULL,
    y = "Log(Distinct Listed Units), Residualized"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_blank()
  )

ggsave(opt$output_pdf, p, width = 5, height = 5, dpi = 300, bg = "white")
message(sprintf("Saved: %s", opt$output_pdf))
