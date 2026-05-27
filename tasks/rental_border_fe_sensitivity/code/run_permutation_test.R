# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_fe_sensitivity/code")
# bw_ft <- 500
# window <- "pre_2023"
# sample_filter <- "all"
# use_controls <- TRUE
# n_perms <- 500
# seed <- 42
# min_date <- "2015-05-18"

source("../../setup_environment/code/packages.R", local = new.env(parent = globalenv()))

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bw_ft, window, sample_filter, use_controls, n_perms, seed, min_date)
}

if (length(cli_args) == 7) {
  bw_ft <- suppressWarnings(as.integer(cli_args[1]))
  window <- cli_args[2]
  sample_filter <- cli_args[3]
  use_controls <- tolower(cli_args[4]) %in% c("true", "t", "1", "yes")
  n_perms <- suppressWarnings(as.integer(cli_args[5]))
  seed <- suppressWarnings(as.integer(cli_args[6]))
  min_date <- cli_args[7]
} else {
  stop("FATAL: Script requires 7 args: <bw_ft> <window> <sample_filter> <use_controls> <n_perms> <seed> <min_date>", call. = FALSE)
}

if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive integer.", call. = FALSE)
}
if (!window %in% c("full", "pre_covid", "pre_2021", "pre_2023", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, pre_2023, drop_mid", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}
if (!is.finite(n_perms) || n_perms <= 0) {
  stop("n_perms must be a positive integer.", call. = FALSE)
}
if (!is.finite(seed)) {
  stop("seed must be finite.", call. = FALSE)
}

window_label <- c(
  full = "full",
  pre_covid = "2014_2019",
  pre_2021 = "2014_2020",
  pre_2023 = "2014_2022",
  drop_mid = "drop_mid"
)[[window]]
control_label <- if (use_controls) "ctrl" else "raw"
output_stem <- sprintf(
  "permutation_test_%s_%s_bw%d_%s_clust_ward_pair",
  window_label,
  sample_filter,
  bw_ft,
  control_label
)
output_pdf <- sprintf("../output/%s.pdf", output_stem)

set.seed(seed)

dat_raw <- read_parquet("../input/rent_with_ward_distances.parquet") %>%
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
  )

if (window == "pre_covid") {
  dat_raw <- dat_raw %>% filter(year <= 2019)
} else if (window == "pre_2021") {
  dat_raw <- dat_raw %>% filter(year <= 2020)
} else if (window == "pre_2023") {
  dat_raw <- dat_raw %>% filter(year <= 2022)
} else if (window == "drop_mid") {
  dat_raw <- dat_raw %>% filter(year <= 2020 | year >= 2024)
}

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

ald_own <- dat_raw %>%
  distinct(alderman_own, strictness_own) %>%
  rename(alderman = alderman_own, score = strictness_own)

ald_neighbor <- dat_raw %>%
  distinct(alderman_neighbor, strictness_neighbor) %>%
  rename(alderman = alderman_neighbor, score = strictness_neighbor)

ald_map <- bind_rows(ald_own, ald_neighbor) %>%
  distinct(alderman, score)

if (anyDuplicated(ald_map$alderman) > 0) {
  stop("Alderman-score map must be unique by alderman before permutation.", call. = FALSE)
}

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

dat_real <- dat_raw %>%
  mutate(
    strictness_own_perm = strictness_own,
    strictness_neighbor_perm = strictness_neighbor
  )
real_coef <- run_regression(dat_real)

perm_coefs <- numeric(n_perms)
aldermen_vec <- ald_map$alderman
scores_vec <- ald_map$score

for (i in seq_len(n_perms)) {
  shuffled_scores <- sample(scores_vec)
  perm_map <- tibble(alderman = aldermen_vec, perm_score = shuffled_scores)

  dat_perm <- dat_raw %>%
    left_join(perm_map, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_own_perm = perm_score) %>%
    left_join(perm_map, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_neighbor_perm = perm_score)

  perm_coefs[i] <- run_regression(dat_perm)
}

valid_perms <- perm_coefs[is.finite(perm_coefs)]
n_valid <- length(valid_perms)

if (n_valid == 0) stop("No valid permutation estimates.", call. = FALSE)

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
