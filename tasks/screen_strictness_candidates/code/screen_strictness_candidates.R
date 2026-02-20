source("../../setup_environment/code/packages.R")

normalize_name <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out <- gsub("[^a-z0-9 ]", "", out)
  out <- gsub("\\b(jr|sr)\\b", "", out)
  out <- gsub(" +", " ", out)
  trimws(out)
}

candidate_cols <- c(
  "raw_down_share_t0",
  "eb_down_share_t0",
  "raw_far_down_share_t0",
  "eb_far_down_share_t0",
  "eb_net_restrictive_t0",
  "comp_adj_down_t0",
  "raw_down_share_t01",
  "eb_down_share_t01",
  "raw_far_down_share_t01",
  "eb_far_down_share_t01",
  "eb_net_restrictive_t01",
  "comp_adj_down_t01"
)

candidate_path <- "../input/alderman_strictness_candidates.csv"
parcel_path <- "../input/parcels_pre_scores.csv"

results_csv_path <- "../output/candidate_screen_results.csv"
rd_csv_path <- "../output/candidate_screen_rd_results.csv"
unmatched_csv_path <- "../output/candidate_unmatched_names.csv"
summary_txt_path <- "../output/candidate_screen_summary.txt"
winner_local_path <- "../output/alderman_rezoning_strictness_scores.csv"
winner_upstream_path <- "../../create_rezoning_strictness_scores/output/alderman_rezoning_strictness_scores.csv"

candidates <- read_csv(candidate_path, show_col_types = FALSE)
parcels <- read_csv(parcel_path, show_col_types = FALSE)

missing_candidate_cols <- setdiff(candidate_cols, names(candidates))
if (length(missing_candidate_cols) > 0) {
  stop("Missing candidate columns: ", paste(missing_candidate_cols, collapse = ", "))
}

scores_norm <- candidates %>%
  mutate(norm_name = normalize_name(alderman))

dup_norm <- scores_norm %>% count(norm_name) %>% filter(n > 1)
if (nrow(dup_norm) > 0) {
  stop("Duplicate normalized alderman names in candidate scores.")
}

parcels_norm <- parcels %>%
  mutate(
    own_norm = normalize_name(alderman_own),
    neighbor_norm = normalize_name(alderman_neighbor)
  )

controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own",
  "avg_rent_own"
)

bws <- c(250, 500, 1000)

all_results <- list()
all_rd_results <- list()
all_missing <- list()

for (cand in candidate_cols) {
  lookup <- scores_norm %>%
    transmute(norm_name, score = .data[[cand]])

  merged <- parcels_norm %>%
    left_join(lookup, by = c("own_norm" = "norm_name")) %>%
    rename(strictness_own = score) %>%
    left_join(lookup, by = c("neighbor_norm" = "norm_name")) %>%
    rename(strictness_neighbor = score) %>%
    mutate(
      sign = case_when(
        strictness_own > strictness_neighbor ~ 1,
        strictness_own < strictness_neighbor ~ -1,
        strictness_own == strictness_neighbor ~ 0,
        TRUE ~ NA_real_
      ),
      signed_distance = dist_to_boundary * sign
    )

  n_total <- nrow(merged)
  n_own <- sum(!is.na(merged$strictness_own))
  n_neighbor <- sum(!is.na(merged$strictness_neighbor))
  n_signed <- sum(!is.na(merged$signed_distance))

  own_coverage <- n_own / n_total
  neighbor_coverage <- n_neighbor / n_total
  signed_coverage <- n_signed / n_total

  own_missing <- merged %>%
    filter(is.na(strictness_own)) %>%
    count(alderman_own, sort = TRUE) %>%
    transmute(candidate = cand, side = "own", alderman = alderman_own, parcels = n)

  neighbor_missing <- merged %>%
    filter(is.na(strictness_neighbor)) %>%
    count(alderman_neighbor, sort = TRUE) %>%
    transmute(candidate = cand, side = "neighbor", alderman = alderman_neighbor, parcels = n)

  all_missing[[cand]] <- bind_rows(own_missing, neighbor_missing)

  fe_data <- merged %>%
    filter(
      arealotsf > 1,
      areabuilding > 1,
      unitscount > 1,
      unitscount <= 100,
      construction_year >= 1999,
      dist_to_boundary <= 500,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      is.finite(density_dupac),
      density_dupac > 0,
      !is.na(ward_pair),
      !is.na(construction_year)
    ) %>%
    filter(if_all(all_of(controls), ~ is.finite(.x)))

  fe_coef <- NA_real_
  fe_se <- NA_real_
  fe_p <- NA_real_
  fe_t <- NA_real_
  fe_signal <- NA_real_
  n_fe <- nrow(fe_data)

  if (n_fe > 0) {
    sd_strict <- sd(fe_data$strictness_own, na.rm = TRUE)
    if (is.finite(sd_strict) && sd_strict > 0) {
      fe_data <- fe_data %>% mutate(strictness_std = strictness_own / sd_strict)
      fml <- as.formula(
        paste0(
          "log(density_dupac) ~ strictness_std + ",
          paste(controls, collapse = " + "),
          " | ward_pair^construction_year"
        )
      )

      fe_model <- tryCatch(
        feols(fml, data = fe_data, cluster = ~ward_pair),
        error = function(e) NULL
      )

      if (!is.null(fe_model)) {
        ct <- coeftable(fe_model)
        if ("strictness_std" %in% rownames(ct)) {
          fe_coef <- ct["strictness_std", "Estimate"]
          fe_se <- ct["strictness_std", "Std. Error"]
          fe_p <- ct["strictness_std", "Pr(>|t|)"]
          fe_t <- fe_coef / fe_se
          fe_signal <- -fe_t
          n_fe <- nobs(fe_model)
        }
      }
    }
  }

  rd_rows <- list()

  rd_base <- merged %>%
    filter(
      arealotsf > 1,
      areabuilding > 1,
      unitscount > 1,
      unitscount <= 100,
      construction_year >= 1999,
      !is.na(signed_distance),
      signed_distance != 0,
      is.finite(density_dupac),
      density_dupac > 0,
      !is.na(ward_pair),
      !is.na(boundary_year),
      !is.na(zone_code)
    ) %>%
    mutate(
      outcome = log(density_dupac),
      cluster_id = paste(boundary_year, ward_pair, zone_code, sep = "_")
    )

  for (bw in bws) {
    rd_data <- rd_base %>% filter(abs(signed_distance) <= bw)

    rd_coef <- NA_real_
    rd_se <- NA_real_
    rd_p <- NA_real_
    rd_t <- NA_real_
    rd_signal <- NA_real_
    n_rd <- nrow(rd_data)

    if (n_rd > 30 && n_distinct(sign(rd_data$signed_distance)) > 1) {
      rd_fit <- tryCatch(
        rdrobust(
          y = rd_data$outcome,
          x = rd_data$signed_distance,
          c = 0,
          kernel = "triangular",
          p = 1,
          h = bw,
          cluster = rd_data$cluster_id
        ),
        error = function(e) NULL
      )

      if (!is.null(rd_fit)) {
        idx <- if (length(rd_fit$coef) >= 3) 3 else 1
        rd_coef <- as.numeric(rd_fit$coef[idx])
        rd_se <- as.numeric(rd_fit$se[idx])
        rd_p <- as.numeric(rd_fit$pv[idx])
        rd_t <- rd_coef / rd_se
        rd_signal <- -rd_t
      }
    }

    rd_rows[[as.character(bw)]] <- tibble(
      candidate = cand,
      bw = bw,
      n_rd = n_rd,
      rd_coef = rd_coef,
      rd_se = rd_se,
      rd_p = rd_p,
      rd_t = rd_t,
      rd_signal = rd_signal
    )
  }

  rd_tbl <- bind_rows(rd_rows)
  all_rd_results[[cand]] <- rd_tbl

  rd_main <- rd_tbl %>% filter(bw == 500)

  rd_coef_main <- rd_main$rd_coef[1]
  rd_se_main <- rd_main$rd_se[1]
  rd_p_main <- rd_main$rd_p[1]
  rd_t_main <- rd_main$rd_t[1]
  rd_signal_main <- rd_main$rd_signal[1]
  n_rd_main <- rd_main$n_rd[1]

  sign_ok_fe <- is.finite(fe_coef) && fe_coef < 0
  sign_ok_rd <- is.finite(rd_coef_main) && rd_coef_main < 0

  combined_p <- fe_p + rd_p_main
  if (!is.finite(combined_p)) {
    combined_p <- Inf
  }

  rank_score_base <- 0.6 * fe_signal + 0.4 * rd_signal_main
  if (!is.finite(rank_score_base)) {
    rank_score_base <- -Inf
  }

  penalty <- ifelse(sign_ok_fe && sign_ok_rd, 0, 100)
  rank_score <- rank_score_base - penalty

  all_results[[cand]] <- tibble(
    candidate = cand,
    own_coverage = own_coverage,
    neighbor_coverage = neighbor_coverage,
    signed_coverage = signed_coverage,
    n_fe = n_fe,
    fe_coef = fe_coef,
    fe_se = fe_se,
    fe_p = fe_p,
    fe_t = fe_t,
    fe_signal = fe_signal,
    n_rd_main = n_rd_main,
    rd_coef_main = rd_coef_main,
    rd_se_main = rd_se_main,
    rd_p_main = rd_p_main,
    rd_t_main = rd_t_main,
    rd_signal_main = rd_signal_main,
    sign_ok_fe = sign_ok_fe,
    sign_ok_rd = sign_ok_rd,
    combined_p = combined_p,
    rank_score = rank_score
  )
}

results <- bind_rows(all_results) %>% arrange(desc(rank_score), candidate)
rd_results <- bind_rows(all_rd_results) %>% arrange(candidate, bw)
missing_tbl <- bind_rows(all_missing)

if (nrow(missing_tbl) == 0) {
  missing_tbl <- tibble(candidate = character(), side = character(), alderman = character(), parcels = integer())
}

write_csv(results, results_csv_path)
write_csv(rd_results, rd_csv_path)
write_csv(missing_tbl, unmatched_csv_path)

coverage_fail <- results %>%
  filter(own_coverage < 0.99 | neighbor_coverage < 0.99)

if (nrow(coverage_fail) > 0) {
  writeLines(
    c(
      "candidate screen summary",
      "status: failed",
      "reason: own or neighbor coverage below 0.99",
      paste0("failing_candidates: ", paste(coverage_fail$candidate, collapse = ", "))
    ),
    summary_txt_path
  )
  stop("Coverage below 99% for at least one candidate.")
}

both_sign_ok <- results %>% filter(sign_ok_fe, sign_ok_rd)

if (nrow(both_sign_ok) > 0) {
  winner_row <- both_sign_ok %>%
    arrange(desc(rank_score), combined_p, candidate) %>%
    slice(1)
  winner_rule <- "both_signs_ok_max_rank_score"
} else {
  fe_sign_ok <- results %>% filter(sign_ok_fe)
  if (nrow(fe_sign_ok) > 0) {
    winner_row <- fe_sign_ok %>%
      arrange(combined_p, desc(rank_score), candidate) %>%
      slice(1)
    winner_rule <- "no_both_signs_use_min_combined_p_with_fe_sign"
  } else {
    winner_row <- results %>%
      arrange(combined_p, desc(rank_score), candidate) %>%
      slice(1)
    winner_rule <- "no_fe_sign_ok_use_min_combined_p_all"
  }
}

winner_candidate <- winner_row$candidate[[1]]

winner_scores <- candidates %>%
  transmute(
    alderman,
    strictness_index = .data[[winner_candidate]],
    winner_candidate = winner_candidate,
    n_rezonings = coalesce(n_rezonings, 0L),
    imputed_zero_rezonings = coalesce(imputed_zero_rezonings, 0L)
  )

write_csv(winner_scores, winner_local_path)
write_csv(winner_scores, winner_upstream_path)

summary_lines <- c(
  "candidate screen summary",
  "status: success",
  paste0("winner_candidate: ", winner_candidate),
  paste0("winner_rule: ", winner_rule),
  paste0("winner_fe_coef: ", round(winner_row$fe_coef[[1]], 6)),
  paste0("winner_fe_p: ", round(winner_row$fe_p[[1]], 6)),
  paste0("winner_rd_coef_main: ", round(winner_row$rd_coef_main[[1]], 6)),
  paste0("winner_rd_p_main: ", round(winner_row$rd_p_main[[1]], 6)),
  paste0("winner_rank_score: ", round(winner_row$rank_score[[1]], 6)),
  "top_candidates:",
  paste0(
    "  ",
    results %>% slice_head(n = 5) %>% pull(candidate),
    " | rank_score=",
    round(results %>% slice_head(n = 5) %>% pull(rank_score), 4),
    " | fe_p=",
    round(results %>% slice_head(n = 5) %>% pull(fe_p), 4),
    " | rd_p=",
    round(results %>% slice_head(n = 5) %>% pull(rd_p_main), 4)
  )
)

writeLines(summary_lines, summary_txt_path)

message("Wrote ", results_csv_path)
message("Wrote ", rd_csv_path)
message("Wrote ", unmatched_csv_path)
message("Wrote ", winner_local_path)
message("Wrote ", winner_upstream_path)
message("Wrote ", summary_txt_path)
