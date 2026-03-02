source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_fe_sensitivity/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# sample_filter <- "all"
# use_controls <- TRUE
# n_perms <- 500
# seed <- 42
# min_date <- "2015-05-18"
# output_csv <- "../output/permutation_test_pre_2021_all_bw1000.csv"
# output_pdf <- "../output/permutation_test_pre_2021_all_bw1000.pdf"
# Rscript run_permutation_test.R "../input/rent_with_ward_distances.parquet" 1000 "pre_2021" "all" TRUE 500 42 "2015-05-18" "../output/permutation_test_pre_2021_all_bw1000.csv" "../output/permutation_test_pre_2021_all_bw1000.pdf"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 10) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  use_controls <- tolower(cli_args[5]) %in% c("true", "t", "1", "yes")
  n_perms <- suppressWarnings(as.integer(cli_args[6]))
  seed <- suppressWarnings(as.integer(cli_args[7]))
  min_date <- cli_args[8]
  output_csv <- cli_args[9]
  output_pdf <- cli_args[10]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") || !exists("use_controls") || !exists("n_perms") || !exists("seed") || !exists("min_date") || !exists("output_csv") || !exists("output_pdf")) {
    stop("FATAL: Script requires 10 args: <input> <bw_ft> <window> <sample_filter> <use_controls> <n_perms> <seed> <min_date> <output_csv> <output_pdf>", call. = FALSE)
  }
}

if (!window %in% c("full", "pre_covid", "pre_2021", "pre_2023", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, pre_2023, drop_mid", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}

apply_window <- function(df, window_name) {
  if (window_name == "full") return(df)
  if (window_name == "pre_covid") return(df %>% filter(year <= 2019))
  if (window_name == "pre_2021") return(df %>% filter(year <= 2020))
  if (window_name == "pre_2023") return(df %>% filter(year <= 2022))
  if (window_name == "drop_mid") return(df %>% filter(year <= 2020 | year >= 2024))
  df
}

set.seed(seed)

message("=== Permutation Test: Shuffle Strictness Across Aldermen ===")
message(sprintf("bw=%d | window=%s | sample=%s | n_perms=%d | seed=%d | min_date=%s",
                bw_ft, window, sample_filter, n_perms, seed, min_date))

# ── Load and filter data ──
dat_raw <- read_parquet(input) %>%
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
    file_date >= as.Date(min_date),
    !is.na(ward_pair),
    !is.na(rent_price), rent_price > 0,
    !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
    !is.na(alderman_own), !is.na(alderman_neighbor),
    abs_dist <= bw_ft
  ) %>%
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat_raw <- dat_raw %>% filter(building_type_clean == "multi_family")
}

dat_raw <- dat_raw %>%
  mutate(
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  )

if (use_controls) {
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
    mutate(
      signed_dist_perm = abs_dist * sign_new,
      uniform_w = 1
    )

  if (nrow(df) == 0 || length(unique(df$ward_pair)) < 2) return(NA_real_)

  sd_strict <- sd(df$strictness_own_perm, na.rm = TRUE)
  if (!is.finite(sd_strict) || sd_strict <= 0) return(NA_real_)

  df <- df %>% mutate(strictness_std = strictness_own_perm / sd_strict)

  n_types <- n_distinct(df$building_type_factor)
  if (use_controls) {
    rhs <- "strictness_std + log_sqft + log_beds + log_baths"
    if (n_types >= 2) rhs <- paste0(rhs, " + building_type_factor")
  } else {
    rhs <- "strictness_std"
  }

  m <- tryCatch(
    feols(
      as.formula(paste0("log(rent_price) ~ ", rhs, " | ward_pair^year_month")),
      data = df,
      weights = ~uniform_w,
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
perm_coefs <- numeric(n_perms)
aldermen_vec <- ald_map$alderman
scores_vec <- ald_map$score

message(sprintf("Running %d permutations...", n_perms))

for (i in seq_len(n_perms)) {
  if (i %% 50 == 0) message(sprintf("  permutation %d / %d", i, n_perms))

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
message(sprintf("Valid permutations: %d / %d", n_valid, n_perms))

if (n_valid == 0) stop("No valid permutation estimates.", call. = FALSE)

# ── Compute p-value ──
p_two_sided <- mean(abs(valid_perms) >= abs(real_coef))
p_one_sided <- mean(valid_perms >= real_coef)

message(sprintf("Permutation p-value (two-sided): %.4f", p_two_sided))
message(sprintf("Permutation p-value (one-sided): %.4f", p_one_sided))

# ── Save results ──
out <- tibble(
  bw_ft = bw_ft,
  window = window,
  sample_filter = sample_filter,
  use_controls = use_controls,
  n_perms = n_perms,
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
  seed = seed,
  min_date = min_date
)
write_csv(out, output_csv)

# ── Plot ──
plot_df <- tibble(coef = valid_perms)

p <- ggplot(plot_df, aes(x = coef)) +
  geom_histogram(bins = 40, fill = "gray70", color = "gray50", alpha = 0.8) +
  geom_vline(xintercept = real_coef, color = "#d62728", linewidth = 1.2, linetype = "solid") +
  geom_vline(xintercept = 0, color = "gray30", linewidth = 0.5, linetype = "dashed") +
  labs(
    title = "Permutation Test: Shuffled Stringency Scores Across Aldermen",
    subtitle = NULL,
    x = "Coefficient on standardized stringency score",
    y = "Count",
    caption = "Gray histogram: distribution under random alderman-score assignment.\nRed line: actual estimate."
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(output_pdf, p, width = 9, height = 6, dpi = 300, bg = "white")

message(sprintf("Saved: %s", output_csv))
message(sprintf("Saved: %s", output_pdf))
