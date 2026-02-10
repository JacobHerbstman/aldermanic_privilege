source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/sales_with_hedonics.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--use_controls", type = "logical", default = TRUE),
  make_option("--bins_per_side", type = "integer", default = 15),
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

message(sprintf("=== Sales Level Plot | bw=%d | controls=%s ===", opt$bw_ft, opt$use_controls))

# ── Load and filter ──
dat <- read_parquet(opt$input) %>%
  as_tibble() %>%
  mutate(ward_pair = as.character(ward_pair_id)) %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    !is.na(ward_pair), !is.na(signed_dist), !is.na(strictness_own),
    abs(signed_dist) <= opt$bw_ft
  )

# Percentile-based pair filter
if (opt$min_strictness_diff_pctile > 0) {
  pair_diffs <- dat %>%
    group_by(ward_pair) %>%
    summarise(diff = first(abs(strictness_own - strictness_neighbor)), .groups = "drop")
  cutoff <- quantile(pair_diffs$diff, opt$min_strictness_diff_pctile / 100)
  dat <- dat %>% filter(ward_pair %in% pair_diffs$ward_pair[pair_diffs$diff >= cutoff])
  message(sprintf("After p%d filter: %d obs, %d pairs",
                  opt$min_strictness_diff_pctile, nrow(dat), n_distinct(dat$ward_pair)))
}

dat <- dat %>% mutate(right = as.integer(signed_dist >= 0))

if (opt$use_controls) {
  dat <- dat %>%
    filter(!is.na(log_sqft), !is.na(log_land_sqft), !is.na(log_building_age),
           !is.na(log_bedrooms), !is.na(log_baths), !is.na(has_garage))
}

stopifnot(nrow(dat) > 0, n_distinct(dat$ward_pair) >= 2)

# ── Model: side indicator with ward-pair × year-quarter FE ──
rhs <- if (opt$use_controls) {
  "right + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage"
} else {
  "right"
}

m <- feols(as.formula(paste0("log(sale_price) ~ ", rhs, " | ward_pair^year_quarter")),
           data = dat, cluster = ~ward_pair)
ct <- coeftable(m)
b_right <- ct["right", "Estimate"]
se_right <- ct["right", "Std. Error"]
p_right <- ct["right", "Pr(>|t|)"]

# ── Frisch-Waugh adjusted outcome ──
removed <- m$obs_selection$obsRemoved
keep_idx <- if (is.null(removed)) seq_len(nrow(dat)) else setdiff(seq_len(nrow(dat)), abs(as.integer(removed)))
aug <- dat[keep_idx, , drop = FALSE]
stopifnot(nrow(aug) == nobs(m))
aug$y_adj <- as.numeric(resid(m)) + b_right * aug$right

# ── Bin and plot ──
bin_w <- opt$bw_ft / opt$bins_per_side
bins <- aug %>%
  mutate(bin_center = (floor(signed_dist / bin_w) + 0.5) * bin_w) %>%
  group_by(bin_center) %>%
  summarise(mean_y = mean(y_adj), side = if_else(first(bin_center) >= 0, "More Uncertain", "Less Uncertain"),
            .groups = "drop")

mean_left <- mean(aug$y_adj[aug$right == 0])
mean_right <- mean(aug$y_adj[aug$right == 1])

line_df <- bind_rows(
  tibble(x = c(-opt$bw_ft, 0), y = mean_left, side = "Less Uncertain"),
  tibble(x = c(0, opt$bw_ft), y = mean_right, side = "More Uncertain")
)

gap_label <- sprintf("Gap = %.4f%s (SE %.4f, p = %.3f)\nN = %s | %d pairs",
                     b_right, stars(p_right), se_right, p_right,
                     format(nobs(m), big.mark = ","), n_distinct(aug$ward_pair))

pctile_label <- if (opt$min_strictness_diff_pctile > 0) {
  sprintf(" | top %d%% pairs", 100 - opt$min_strictness_diff_pctile)
} else ""

ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.5, alpha = 0.9) +
  geom_line(data = line_df, aes(x = x, y = y, color = side), linewidth = 1.1) +
  scale_color_manual(values = c("Less Uncertain" = "#1f77b4", "More Uncertain" = "#d62728"), name = "") +
  annotate("text", x = -Inf, y = Inf, label = gap_label,
           hjust = -0.05, vjust = 1.5, size = 3.3, fontface = "bold") +
  labs(title = "Sale Prices by Side of Ward Boundary (FE-Adjusted)",
       subtitle = sprintf("bw=%d ft | controls=%s%s", opt$bw_ft, opt$use_controls, pctile_label),
       x = "Distance to Ward Boundary (feet; positive = more uncertain side)",
       y = "FE-Adjusted Log(Sale Price)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(opt$output_pdf, width = 8.6, height = 6, dpi = 300, bg = "white")
message(sprintf("Saved: %s", opt$output_pdf))
