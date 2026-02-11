source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--window", type = "character", default = "pre_2021"),
  make_option("--sample_filter", type = "character", default = "all"),
  make_option("--unit_def", type = "character", default = "id"),
  make_option("--min_strictness_diff_pctile", type = "integer", default = 0),
  make_option("--bins_per_side", type = "integer", default = 15),
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

message(sprintf("=== Listing Units Plot | bw=%d | window=%s | sample=%s | pctile=%d ===",
                opt$bw_ft, opt$window, opt$sample_filter, opt$min_strictness_diff_pctile))

if (!opt$unit_def %in% c("id", "loc_key", "unit_proxy")) {
  stop("--unit_def must be one of: id, loc_key, unit_proxy", call. = FALSE)
}

dat <- read_parquet(opt$input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    listing_id = as.character(id),
    loc_key = paste(round(latitude, 5), round(longitude, 5), sep = "_"),
    unit_proxy = paste(
      round(latitude, 5),
      round(longitude, 5),
      coalesce(as.character(beds), "NA"),
      coalesce(as.character(baths), "NA"),
      coalesce(as.character(sqft), "NA"),
      sep = "_"
    )
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(signed_dist), !is.na(strictness_own),
    !is.na(latitude), !is.na(longitude),
    abs(signed_dist) <= opt$bw_ft
  ) %>%
  apply_window(opt$window)

if (opt$unit_def == "id") {
  dat <- dat %>% mutate(listing_key = listing_id)
} else if (opt$unit_def == "loc_key") {
  dat <- dat %>% mutate(listing_key = loc_key)
} else if (opt$unit_def == "unit_proxy") {
  dat <- dat %>% mutate(listing_key = unit_proxy)
}

dat <- dat %>% filter(!is.na(listing_key), listing_key != "")

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
  message(sprintf("  After p%d filter (cutoff=%.3f): %d obs, %d pairs",
                  opt$min_strictness_diff_pctile, cutoff, nrow(dat), n_distinct(dat$ward_pair)))
}

dat <- dat %>% mutate(right = as.integer(signed_dist >= 0))

side_cells <- dat %>%
  distinct(ward_pair, right, year_month, listing_key) %>%
  group_by(ward_pair, right, year_month) %>%
  summarise(n_units = n(), .groups = "drop") %>%
  mutate(log_n = log(n_units))

stopifnot(nrow(side_cells) > 0, n_distinct(side_cells$ward_pair) >= 2)

m <- feols(log_n ~ right | ward_pair^year_month, data = side_cells, cluster = ~ward_pair)
ct <- coeftable(m)
b_right <- ct["right", "Estimate"]
se_right <- ct["right", "Std. Error"]
p_right <- ct["right", "Pr(>|t|)"]

message(sprintf("  Side-level: b=%.4f (SE %.4f, p=%.3f), N cells=%s, %d pairs",
                b_right, se_right, p_right,
                format(nobs(m), big.mark = ","), n_distinct(side_cells$ward_pair)))

bin_w <- opt$bw_ft / opt$bins_per_side
stopifnot(is.finite(bin_w), bin_w > 0)

bin_cells <- dat %>%
  mutate(bin_center = (floor(signed_dist / bin_w) + 0.5) * bin_w) %>%
  distinct(ward_pair, bin_center, year_month, listing_key) %>%
  group_by(ward_pair, bin_center, year_month) %>%
  summarise(n_units = n(), .groups = "drop") %>%
  mutate(right = as.integer(bin_center >= 0), log_n = log(n_units))

m_bin <- feols(log_n ~ right | ward_pair^year_month, data = bin_cells, cluster = ~ward_pair)
removed <- m_bin$obs_selection$obsRemoved
keep_idx <- if (is.null(removed)) seq_len(nrow(bin_cells)) else setdiff(seq_len(nrow(bin_cells)), abs(as.integer(removed)))
aug <- bin_cells[keep_idx, , drop = FALSE]
stopifnot(nrow(aug) == nobs(m_bin))
aug$y_adj <- as.numeric(resid(m_bin)) + b_right * aug$right

bins <- aug %>%
  group_by(bin_center) %>%
  summarise(
    mean_y = mean(y_adj),
    side = if_else(first(bin_center) >= 0, "More Uncertain", "Less Uncertain"),
    .groups = "drop"
  )

mean_left <- mean(aug$y_adj[aug$right == 0])
mean_right <- mean(aug$y_adj[aug$right == 1])

line_df <- bind_rows(
  tibble(x = c(-opt$bw_ft, 0), y = mean_left, side = "Less Uncertain"),
  tibble(x = c(0, opt$bw_ft), y = mean_right, side = "More Uncertain")
)

gap_label <- sprintf("Gap = %.4f%s (SE %.4f)\nN = %s pair-side-months | %d pairs",
                     b_right, stars(p_right), se_right,
                     format(nobs(m), big.mark = ","), n_distinct(side_cells$ward_pair))

unit_label <- c(
  id = "Listing ID",
  loc_key = "Location Key (rounded lat/lon)",
  unit_proxy = "Unit Proxy Key (loc+beds+baths+sqft)"
)

ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.5, alpha = 0.9) +
  geom_line(data = line_df, aes(x = x, y = y, color = side), linewidth = 1.1) +
  scale_color_manual(values = c("Less Uncertain" = "#1f77b4", "More Uncertain" = "#d62728"), name = "") +
  annotate("text", x = -Inf, y = Inf, label = gap_label,
           hjust = -0.05, vjust = 1.5, size = 3.3, fontface = "bold") +
  labs(
    title = "Distinct Listed Units by Side of Ward Boundary",
    subtitle = sprintf(
      "bw=%d ft | sample=%s | key=%s%s",
      opt$bw_ft, opt$sample_filter,
      unit_label[[opt$unit_def]],
      if (opt$min_strictness_diff_pctile > 0) sprintf(" | top %d%% pairs", 100 - opt$min_strictness_diff_pctile) else ""
    ),
    x = "Distance to Ward Boundary (feet; positive = more uncertain side)",
    y = "FE-Adjusted Log(Distinct Listed Units per Bin)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(opt$output_pdf, width = 8.6, height = 6, dpi = 300, bg = "white")
message(sprintf("Saved: %s", opt$output_pdf))
