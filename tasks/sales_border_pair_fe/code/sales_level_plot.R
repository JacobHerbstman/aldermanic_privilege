source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")
# input <- "../input/sales_with_hedonics.parquet"
# bw_ft <- 1000
# use_controls <- TRUE
# bins_per_side <- 15
# min_strictness_diff_pctile <- 0
# output_pdf <- NA

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_ft, use_controls, bins_per_side, min_strictness_diff_pctile, output_pdf)
}

if (length(cli_args) >= 8) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  use_controls <- tolower(cli_args[3]) %in% c("true", "t", "1", "yes")
  bins_per_side <- suppressWarnings(as.integer(cli_args[4]))
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[5]))
  output_pdf <- cli_args[6]
  fe_geo <- tolower(cli_args[7])
  cluster_level <- tolower(cli_args[8])
} else if (length(cli_args) >= 6) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  use_controls <- tolower(cli_args[3]) %in% c("true", "t", "1", "yes")
  bins_per_side <- suppressWarnings(as.integer(cli_args[4]))
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[5]))
  output_pdf <- cli_args[6]
  fe_geo <- tolower(Sys.getenv("FE_GEO", "segment"))
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
} else {
  stop("FATAL: Script requires args: <input> <bw_ft> <use_controls> <bins_per_side> <min_strictness_diff_pctile> <output_pdf> [<fe_geo> <cluster_level>]", call. = FALSE)
}
if (!fe_geo %in% c("segment", "ward_pair")) stop("--fe_geo must be one of: segment, ward_pair", call. = FALSE)
if (!cluster_level %in% c("segment", "ward_pair")) stop("--cluster_level must be one of: segment, ward_pair", call. = FALSE)

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

message(sprintf("=== Sales Level Plot | bw=%d | controls=%s ===", bw_ft, use_controls))

# ── Load and filter ──
dat <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(ward_pair = as.character(ward_pair_id)) %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    !is.na(ward_pair), !is.na(signed_dist), !is.na(strictness_own),
    abs(signed_dist) <= bw_ft
  )
need_segment <- fe_geo == "segment" || cluster_level == "segment"
if (need_segment) {
  dat <- dat %>% filter(!is.na(segment_id), segment_id != "")
}

# Percentile-based pair filter
if (min_strictness_diff_pctile > 0) {
  pair_diffs <- dat %>%
    group_by(ward_pair) %>%
    summarise(diff = first(abs(strictness_own - strictness_neighbor)), .groups = "drop")
  cutoff <- quantile(pair_diffs$diff, min_strictness_diff_pctile / 100)
  dat <- dat %>% filter(ward_pair %in% pair_diffs$ward_pair[pair_diffs$diff >= cutoff])
  message(sprintf("After p%d filter: %d obs, %d pairs",
                  min_strictness_diff_pctile, nrow(dat), n_distinct(dat$ward_pair)))
}

dat <- dat %>% mutate(right = as.integer(signed_dist >= 0))

if (use_controls) {
  dat <- dat %>%
    filter(!is.na(log_sqft), !is.na(log_land_sqft), !is.na(log_building_age),
           !is.na(log_bedrooms), !is.na(log_baths), !is.na(has_garage))
}

stopifnot(nrow(dat) > 0, n_distinct(dat$ward_pair) >= 2)

# ── Model: side indicator with ward-pair × year-quarter FE ──
rhs <- if (use_controls) {
  "right + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage"
} else {
  "right"
}

m <- feols(
  as.formula(paste0(
    "log(sale_price) ~ ", rhs, " | ",
    ifelse(fe_geo == "segment", "segment_id^year_quarter", "ward_pair^year_quarter")
  )),
  data = dat,
  cluster = if (cluster_level == "segment") ~segment_id else ~ward_pair
)
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
bin_w <- bw_ft / bins_per_side
bins <- aug %>%
  mutate(bin_center = (floor(signed_dist / bin_w) + 0.5) * bin_w) %>%
  group_by(bin_center) %>%
  summarise(mean_y = mean(y_adj), side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
            .groups = "drop")

mean_left <- mean(aug$y_adj[aug$right == 0])
mean_right <- mean(aug$y_adj[aug$right == 1])

line_df <- bind_rows(
  tibble(x = c(-bw_ft, 0), y = mean_left, side = "Less Stringent"),
  tibble(x = c(0, bw_ft), y = mean_right, side = "More Stringent")
)

jump_label <- sprintf("Jump = %.4f%s (SE %.4f) | bw=%d ft | N=%s",
                      b_right, stars(p_right), se_right, bw_ft, format(nobs(m), big.mark = ","))

ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.5, alpha = 0.9) +
  geom_line(data = line_df, aes(x = x, y = y, color = side), linewidth = 1.1) +
  scale_color_manual(values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"), name = "") +
  labs(title = "Sale Prices by Side of Ward Boundary (FE-Adjusted)",
       subtitle = jump_label,
       x = "Distance to Ward Boundary (feet; positive = more stringent side)",
       y = "FE-Adjusted Log(Sale Price)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(output_pdf, width = 8.6, height = 6, dpi = 300, bg = "white")
message(sprintf("Saved: %s", output_pdf))
