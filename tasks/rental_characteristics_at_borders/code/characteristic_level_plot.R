source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--outcome", type = "character"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--window", type = "character", default = "pre_2021"),
  make_option("--bins_per_side", type = "integer", default = 15),
  make_option("--output_pdf", type = "character")
)
opt <- parse_args(OptionParser(option_list = option_list))

# Outcome configuration
outcome_config <- list(
  sqft       = list(var = "sqft",       label = "Square Footage",      log = FALSE),
  log_sqft   = list(var = "sqft",       label = "Log(Square Footage)", log = TRUE),
  beds       = list(var = "beds",       label = "Bedrooms",            log = FALSE),
  baths      = list(var = "baths",      label = "Bathrooms",           log = FALSE),
  multifamily = list(var = "is_multifamily", label = "Share Multi-Family", log = FALSE),
  laundry    = list(var = "laundry",    label = "Has Laundry",         log = FALSE),
  gym        = list(var = "gym",        label = "Has Gym",             log = FALSE)
)

cfg <- outcome_config[[opt$outcome]]
if (is.null(cfg)) stop(sprintf("Unknown outcome: %s. Options: %s",
                                opt$outcome, paste(names(outcome_config), collapse = ", ")))

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
  if (w == "pre_covid") return(df %>% filter(year <= 2019))
  df
}

message(sprintf("=== Characteristic Level Plot | %s | bw=%d ===", opt$outcome, opt$bw_ft))

# ── Load and filter ──
dat <- read_parquet(opt$input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    is_multifamily = as.integer(building_type_clean == "multi_family")
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(signed_dist), !is.na(strictness_own),
    abs(signed_dist) <= opt$bw_ft
  ) %>%
  apply_window(opt$window)

# Build outcome variable
if (cfg$log) {
  dat <- dat %>%
    mutate(Y = if_else(!is.na(.data[[cfg$var]]) & .data[[cfg$var]] > 0,
                       log(.data[[cfg$var]]), NA_real_))
} else {
  dat <- dat %>% mutate(Y = as.numeric(.data[[cfg$var]]))
}

dat <- dat %>% filter(!is.na(Y)) %>% mutate(right = as.integer(signed_dist >= 0))
stopifnot(nrow(dat) > 0, n_distinct(dat$ward_pair) >= 2)

# ── Model: Y ~ right | ward_pair^year_month ──
m <- feols(Y ~ right | ward_pair^year_month, data = dat, cluster = ~ward_pair)
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
  summarise(mean_y = mean(y_adj), side = if_else(first(bin_center) >= 0, "Stricter", "Less strict"),
            .groups = "drop")

mean_left <- mean(aug$y_adj[aug$right == 0])
mean_right <- mean(aug$y_adj[aug$right == 1])

line_df <- bind_rows(
  tibble(x = c(-opt$bw_ft, 0), y = mean_left, side = "Less strict"),
  tibble(x = c(0, opt$bw_ft), y = mean_right, side = "Stricter")
)

gap_label <- sprintf("Gap = %.4f%s (SE %.4f, p = %.3f)\nN = %s | %d pairs",
                     b_right, stars(p_right), se_right, p_right,
                     format(nobs(m), big.mark = ","), n_distinct(aug$ward_pair))

ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.5, alpha = 0.9) +
  geom_line(data = line_df, aes(x = x, y = y, color = side), linewidth = 1.1) +
  scale_color_manual(values = c("Less strict" = "#1f77b4", "Stricter" = "#d62728"), name = "") +
  annotate("text", x = -Inf, y = Inf, label = gap_label,
           hjust = -0.05, vjust = 1.5, size = 3.3, fontface = "bold") +
  labs(title = sprintf("%s by Side of Ward Boundary (FE-Adjusted)", cfg$label),
       subtitle = sprintf("bw=%d ft | window=%s", opt$bw_ft, opt$window),
       x = "Distance to Ward Boundary (feet; positive = stricter side)",
       y = sprintf("FE-Adjusted %s", cfg$label)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(opt$output_pdf, width = 8.6, height = 6, dpi = 300, bg = "white")
message(sprintf("Saved: %s", opt$output_pdf))
