source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--window", type = "character", default = "pre_2021"),
  make_option("--sample_filter", type = "character", default = "all"),
  make_option("--use_controls", type = "logical", default = TRUE),
  make_option("--n_perms", type = "integer", default = 500),
  make_option("--seed", type = "integer", default = 42),
  make_option("--min_date", type = "character", default = "2015-05-18"),
  make_option("--output_csv", type = "character",
              default = "../output/permutation_test_pre_2021_all_bw1000.csv"),
  make_option("--output_pdf", type = "character",
              default = "../output/permutation_test_pre_2021_all_bw1000.pdf")
)
opt <- parse_args(OptionParser(option_list = option_list))

if (!opt$window %in% c("full", "pre_covid", "pre_2021", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, drop_mid", call. = FALSE)
}
if (!opt$sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}

apply_window <- function(df, window_name) {
  if (window_name == "full") return(df)
  if (window_name == "pre_covid") return(df %>% filter(year <= 2019))
  if (window_name == "pre_2021") return(df %>% filter(year <= 2020))
  if (window_name == "drop_mid") return(df %>% filter(year <= 2020 | year >= 2024))
  df
}

set.seed(opt$seed)

message("=== Permutation Test: Shuffle Strictness Across Aldermen ===")
message(sprintf("bw=%d | window=%s | sample=%s | n_perms=%d | seed=%d | min_date=%s",
                opt$bw_ft, opt$window, opt$sample_filter, opt$n_perms, opt$seed, opt$min_date))

# ── Load and filter data ──
dat_raw <- read_parquet(opt$input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    abs_dist = abs(signed_dist)
  ) %>%
  filter(
    !is.na(file_date),
    file_date >= as.Date(opt$min_date),
    !is.na(ward_pair),
    !is.na(rent_price), rent_price > 0,
    !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
    !is.na(alderman_own), !is.na(alderman_neighbor),
    abs_dist <= opt$bw_ft
  ) %>%
  apply_window(opt$window)

if (opt$sample_filter == "multifamily_only") {
  dat_raw <- dat_raw %>% filter(building_type_clean == "multi_family")
}

dat_raw <- dat_raw %>%
  mutate(
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  )

if (opt$use_controls) {
  dat_raw <- dat_raw %>%
    filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths))
}

if (nrow(dat_raw) == 0) stop("No data after filtering.", call. = FALSE)

# ── Build the alderman-to-score mapping ──
# Shuffle at the alderman level: permute which score each alderman gets,
# keeping the (ward, date) -> alderman mapping fixed
ald_own <- dat_raw %>%
  distinct(alderman_own, strictness_own) %>%
  rename(alderman = alderman_own, score = strictness_own)

ald_neighbor <- dat_raw %>%
  distinct(alderman_neighbor, strictness_neighbor) %>%
  rename(alderman = alderman_neighbor, score = strictness_neighbor)

ald_map <- bind_rows(ald_own, ald_neighbor) %>%
  distinct(alderman, score)

n_aldermen <- nrow(ald_map)
message(sprintf("Found %d unique aldermen with strictness scores.", n_aldermen))

# ── Regression function ──
run_regression <- function(df) {
  df <- df %>%
    mutate(
      sign_new = case_when(
        strictness_own_perm > strictness_neighbor_perm ~ 1,
        strictness_own_perm < strictness_neighbor_perm ~ -1,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(sign_new)) %>%
    mutate(signed_dist_perm = abs_dist * sign_new)

  if (nrow(df) == 0 || length(unique(df$ward_pair)) < 2) return(NA_real_)

  sd_strict <- sd(df$strictness_own_perm, na.rm = TRUE)
  if (!is.finite(sd_strict) || sd_strict <= 0) return(NA_real_)

  df <- df %>% mutate(strictness_std = strictness_own_perm / sd_strict)

  n_types <- n_distinct(df$building_type_factor)
  if (opt$use_controls) {
    rhs <- "strictness_std + log_sqft + log_beds + log_baths"
    if (n_types >= 2) rhs <- paste0(rhs, " + building_type_factor")
  } else {
    rhs <- "strictness_std"
  }

  m <- tryCatch(
    feols(
      as.formula(paste0("log(rent_price) ~ ", rhs, " | ward_pair^year_month")),
      data = df,
      cluster = ~ward_pair
    ),
    error = function(e) NULL
  )

  if (is.null(m) || !"strictness_std" %in% names(coef(m))) return(NA_real_)
  coef(m)[["strictness_std"]]
}

# ── Real estimate ──
dat_real <- dat_raw %>%
  mutate(
    strictness_own_perm = strictness_own,
    strictness_neighbor_perm = strictness_neighbor
  )
real_coef <- run_regression(dat_real)
message(sprintf("Real coefficient: %.6f", real_coef))

# ── Run permutations: shuffle alderman -> score mapping ──
perm_coefs <- numeric(opt$n_perms)
aldermen_vec <- ald_map$alderman
scores_vec <- ald_map$score

message(sprintf("Running %d permutations...", opt$n_perms))

for (i in seq_len(opt$n_perms)) {
  if (i %% 50 == 0) message(sprintf("  permutation %d / %d", i, opt$n_perms))

  shuffled_scores <- sample(scores_vec)
  perm_map <- tibble(alderman = aldermen_vec, perm_score = shuffled_scores)

  dat_perm <- dat_raw %>%
    left_join(perm_map, by = c("alderman_own" = "alderman")) %>%
    rename(strictness_own_perm = perm_score) %>%
    left_join(perm_map, by = c("alderman_neighbor" = "alderman")) %>%
    rename(strictness_neighbor_perm = perm_score)

  perm_coefs[i] <- run_regression(dat_perm)
}

valid_perms <- perm_coefs[is.finite(perm_coefs)]
n_valid <- length(valid_perms)
message(sprintf("Valid permutations: %d / %d", n_valid, opt$n_perms))

if (n_valid == 0) stop("No valid permutation estimates.", call. = FALSE)

# ── Compute p-value ──
p_two_sided <- mean(abs(valid_perms) >= abs(real_coef))
p_one_sided <- mean(valid_perms >= real_coef)

message(sprintf("Permutation p-value (two-sided): %.4f", p_two_sided))
message(sprintf("Permutation p-value (one-sided): %.4f", p_one_sided))

# ── Save results ──
out <- tibble(
  bw_ft = opt$bw_ft,
  window = opt$window,
  sample_filter = opt$sample_filter,
  use_controls = opt$use_controls,
  n_perms = opt$n_perms,
  n_valid = n_valid,
  n_aldermen = n_aldermen,
  real_coefficient = real_coef,
  perm_mean = mean(valid_perms),
  perm_sd = sd(valid_perms),
  perm_median = median(valid_perms),
  p_two_sided = p_two_sided,
  p_one_sided = p_one_sided,
  perm_q025 = quantile(valid_perms, 0.025),
  perm_q975 = quantile(valid_perms, 0.975),
  seed = opt$seed,
  min_date = opt$min_date
)
write_csv(out, opt$output_csv)

# ── Plot ──
plot_df <- tibble(coef = valid_perms)

p <- ggplot(plot_df, aes(x = coef)) +
  geom_histogram(bins = 40, fill = "gray70", color = "gray50", alpha = 0.8) +
  geom_vline(xintercept = real_coef, color = "#d62728", linewidth = 1.2, linetype = "solid") +
  geom_vline(xintercept = 0, color = "gray30", linewidth = 0.5, linetype = "dashed") +
  annotate("text",
    x = real_coef, y = Inf,
    label = sprintf("Real = %.4f\np(two-sided) = %.3f\np(one-sided) = %.3f",
                    real_coef, p_two_sided, p_one_sided),
    hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "bold", color = "#d62728"
  ) +
  labs(
    title = "Permutation Test: Shuffled Strictness Scores Across Aldermen",
    subtitle = sprintf("bw = %d ft | window = %s | %d aldermen | %d permutations",
                        opt$bw_ft, opt$window, n_aldermen, n_valid),
    x = "Coefficient on standardized strictness score",
    y = "Count",
    caption = "Gray histogram: distribution under random alderman-score assignment.\nRed line: actual estimate."
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(opt$output_pdf, p, width = 9, height = 6, dpi = 300, bg = "white")

message(sprintf("Saved: %s", opt$output_csv))
message(sprintf("Saved: %s", opt$output_pdf))
