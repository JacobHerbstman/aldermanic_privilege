source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../input/rent_with_ward_distances.parquet"
# outcome <- NA
# bw_ft <- 1000
# window <- "pre_2021"
# bins_per_side <- 15
# output_pdf <- NA
# Rscript characteristic_level_plot.R "../input/rent_with_ward_distances.parquet" NA 1000 "pre_2021" 15 NA
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 6) {
  input <- cli_args[1]
  outcome <- cli_args[2]
  bw_ft <- suppressWarnings(as.integer(cli_args[3]))
  window <- cli_args[4]
  bins_per_side <- suppressWarnings(as.integer(cli_args[5]))
  output_pdf <- cli_args[6]
} else {
  if (!exists("input") || !exists("outcome") || !exists("bw_ft") || !exists("window") || !exists("bins_per_side") || !exists("output_pdf")) {
    stop("FATAL: Script requires 6 args: <input> <outcome> <bw_ft> <window> <bins_per_side> <output_pdf>", call. = FALSE)
  }
}

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

cfg <- outcome_config[[outcome]]
if (is.null(cfg)) stop(sprintf("Unknown outcome: %s. Options: %s",
                                outcome, paste(names(outcome_config), collapse = ", ")))

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

message(sprintf("=== Characteristic Level Plot | %s | bw=%d ===", outcome, bw_ft))

# ── Load and filter ──
dat <- read_parquet(input) %>%
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
    abs(signed_dist) <= bw_ft
  ) %>%
  apply_window(window)

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
bin_w <- bw_ft / bins_per_side
bins <- aug %>%
  mutate(bin_center = (floor(signed_dist / bin_w) + 0.5) * bin_w) %>%
  group_by(bin_center) %>%
  summarise(mean_y = mean(y_adj), side = if_else(first(bin_center) >= 0, "More Uncertain", "Less Uncertain"),
            .groups = "drop")

mean_left <- mean(aug$y_adj[aug$right == 0])
mean_right <- mean(aug$y_adj[aug$right == 1])

line_df <- bind_rows(
  tibble(x = c(-bw_ft, 0), y = mean_left, side = "Less Uncertain"),
  tibble(x = c(0, bw_ft), y = mean_right, side = "More Uncertain")
)

gap_label <- sprintf("Gap = %.4f%s (SE %.4f)\nN = %s | %d pairs",
                     b_right, stars(p_right), se_right,
                     format(nobs(m), big.mark = ","), n_distinct(aug$ward_pair))

ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.5, alpha = 0.9) +
  geom_line(data = line_df, aes(x = x, y = y, color = side), linewidth = 1.1) +
  scale_color_manual(values = c("Less Uncertain" = "#1f77b4", "More Uncertain" = "#d62728"), name = "") +
  annotate("text", x = -Inf, y = Inf, label = gap_label,
           hjust = -0.05, vjust = 1.5, size = 3.3, fontface = "bold") +
  labs(title = sprintf("%s by Side of Ward Boundary (FE-Adjusted)", cfg$label),
       subtitle = sprintf("bw=%d ft | window=%s", bw_ft, window),
       x = "Distance to Ward Boundary (feet; positive = more uncertain side)",
       y = sprintf("FE-Adjusted %s", cfg$label)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(output_pdf, width = 8.6, height = 6, dpi = 300, bg = "white")
message(sprintf("Saved: %s", output_pdf))