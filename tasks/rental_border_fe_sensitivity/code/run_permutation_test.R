# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_fe_sensitivity/code")

source("../../setup_environment/code/packages.R", local = new.env(parent = globalenv()))

bw_ft <- 500L
n_perms <- 500L
output_pdf <- "../output/permutation_test_2014_2022_all_bw500_ctrl_clust_ward_pair.pdf"

set.seed(42L)

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
    file_date >= as.Date("2015-05-18"),
    year <= 2022,
    !is.na(ward_pair),
    !is.na(rent_price), rent_price > 0,
    !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
    !is.na(alderman_own), !is.na(alderman_neighbor),
    abs_dist <= bw_ft
  )

dat_raw <- dat_raw %>%
  mutate(
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  ) %>%
  filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths))

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
  rhs <- "strictness_std + log_sqft + log_beds + log_baths"
  if (n_types >= 2) rhs <- paste0(rhs, " + building_type_factor")

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
