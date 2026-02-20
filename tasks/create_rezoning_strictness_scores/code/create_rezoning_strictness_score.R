source("../../setup_environment/code/packages.R")

parse_mixed_date <- function(x) {
  chars <- as.character(x)
  chars <- trimws(chars)
  chars[chars %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      chars,
      orders = c("mdy", "ymd", "dmy", "m/d/y", "Y-m-d"),
      quiet = TRUE
    )
  )
  as.Date(parsed)
}

normalize_name <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out <- gsub("[^a-z0-9 ]", "", out)
  out <- gsub("\\b(jr|sr)\\b", "", out)
  out <- gsub(" +", " ", out)
  trimws(out)
}

clamp_01 <- function(x) {
  pmin(pmax(x, 0), 1)
}

signal_variance <- function(raw, se2) {
  raw_var <- var(raw, na.rm = TRUE)
  noise <- mean(se2, na.rm = TRUE)
  out <- raw_var - noise
  if (!is.finite(out) || out < 1e-6) {
    out <- 1e-6
  }
  out
}

eb_shrink <- function(raw, se2, prior_mean) {
  sv <- signal_variance(raw, se2)
  b <- sv / (sv + se2)
  b <- clamp_01(b)
  est <- b * raw + (1 - b) * prior_mean
  list(est = est, b = b, signal_var = sv)
}

affine_01 <- function(x) {
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  if (!is.finite(x_min) || !is.finite(x_max) || abs(x_max - x_min) < 1e-12) {
    return(rep(0.5, length(x)))
  }
  (x - x_min) / (x_max - x_min)
}

compute_candidates_for_threshold <- function(data, threshold, suffix, top_bins) {
  if (threshold == 0) {
    down <- data$far_change < 0
    up <- data$far_change > 0
    eligible_weight <- data$far_change != 0
  } else {
    down <- data$far_change <= -threshold
    up <- data$far_change >= threshold
    eligible_weight <- abs(data$far_change) >= threshold
  }

  work <- data %>%
    mutate(
      down = down,
      up = up,
      eligible_weight = eligible_weight,
      weight_abs = abs(far_change)
    )

  agg <- work %>%
    group_by(alderman) %>%
    summarise(
      n_total = n(),
      n_down = sum(down, na.rm = TRUE),
      n_up = sum(up, na.rm = TRUE),
      raw_down_share = n_down / n_total,
      raw_up_share = n_up / n_total,
      weight_down = sum(weight_abs * down, na.rm = TRUE),
      weight_eligible = sum(weight_abs * eligible_weight, na.rm = TRUE),
      weight_sq_eligible = sum((weight_abs * eligible_weight)^2, na.rm = TRUE),
      raw_far_down_share = if_else(weight_eligible > 0, weight_down / weight_eligible, NA_real_),
      n_eff_weight = if_else(weight_sq_eligible > 0, (weight_eligible^2) / weight_sq_eligible, NA_real_),
      .groups = "drop"
    )

  city_down <- sum(agg$n_down, na.rm = TRUE) / sum(agg$n_total, na.rm = TRUE)
  city_up <- sum(agg$n_up, na.rm = TRUE) / sum(agg$n_total, na.rm = TRUE)

  sum_weight_eligible <- sum(agg$weight_eligible, na.rm = TRUE)
  city_far_down <- if (is.finite(sum_weight_eligible) && sum_weight_eligible > 0) {
    sum(agg$weight_down, na.rm = TRUE) / sum_weight_eligible
  } else {
    0
  }

  p_tilde_down <- (agg$n_down + 0.5) / (agg$n_total + 1)
  se2_down <- p_tilde_down * (1 - p_tilde_down) / (agg$n_total + 1)
  down_shr <- eb_shrink(agg$raw_down_share, se2_down, city_down)

  p_tilde_up <- (agg$n_up + 0.5) / (agg$n_total + 1)
  se2_up <- p_tilde_up * (1 - p_tilde_up) / (agg$n_total + 1)
  up_shr <- eb_shrink(agg$raw_up_share, se2_up, city_up)

  raw_far_safe <- agg$raw_far_down_share
  raw_far_safe[!is.finite(raw_far_safe)] <- city_far_down

  n_eff_safe <- agg$n_eff_weight
  n_eff_safe[!is.finite(n_eff_safe) | n_eff_safe <= 0] <- 1

  p_tilde_far <- (agg$weight_down + 0.5) / (agg$weight_eligible + 1)
  p_tilde_far[!is.finite(p_tilde_far)] <- city_far_down
  se2_far <- p_tilde_far * (1 - p_tilde_far) / n_eff_safe
  se2_far[!is.finite(se2_far)] <- 1

  far_shr <- eb_shrink(raw_far_safe, se2_far, city_far_down)

  eb_net <- down_shr$est - up_shr$est
  eb_net_rescaled <- (eb_net + 1) / 2

  work_bins <- work %>%
    mutate(from_bin = if_else(from_code %in% top_bins, from_code, "OTHER"))

  by_bin <- work_bins %>%
    group_by(alderman, from_bin) %>%
    summarise(
      n_bin = n(),
      n_down_bin = sum(down, na.rm = TRUE),
      raw_down_bin = n_down_bin / n_bin,
      .groups = "drop"
    )

  city_bin <- by_bin %>%
    group_by(from_bin) %>%
    summarise(
      city_down_bin = sum(n_down_bin, na.rm = TRUE) / sum(n_bin, na.rm = TRUE),
      .groups = "drop"
    )

  by_bin <- by_bin %>%
    left_join(city_bin, by = "from_bin") %>%
    group_by(from_bin) %>%
    mutate(
      p_tilde_bin = (n_down_bin + 0.5) / (n_bin + 1),
      se2_bin = p_tilde_bin * (1 - p_tilde_bin) / (n_bin + 1),
      signal_bin = signal_variance(raw_down_bin, se2_bin),
      b_bin = clamp_01(signal_bin / (signal_bin + se2_bin)),
      eb_down_bin = b_bin * raw_down_bin + (1 - b_bin) * city_down_bin
    ) %>%
    ungroup()

  comp_adj <- by_bin %>%
    group_by(alderman) %>%
    mutate(bin_share = n_bin / sum(n_bin, na.rm = TRUE)) %>%
    summarise(
      comp_adj_raw = sum(bin_share * (eb_down_bin - city_down_bin), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(comp_adj_scaled = affine_01(comp_adj_raw))

  out <- agg %>%
    left_join(comp_adj, by = "alderman") %>%
    transmute(
      alderman,
      !!paste0("n_total_", suffix) := n_total,
      !!paste0("n_down_", suffix) := n_down,
      !!paste0("n_up_", suffix) := n_up,
      !!paste0("raw_down_share_", suffix) := raw_down_share,
      !!paste0("eb_down_share_", suffix) := down_shr$est,
      !!paste0("raw_far_down_share_", suffix) := raw_far_safe,
      !!paste0("eb_far_down_share_", suffix) := far_shr$est,
      !!paste0("eb_net_restrictive_", suffix) := eb_net_rescaled,
      !!paste0("comp_adj_down_", suffix) := comp_adj_scaled
    )

  out
}

rezoning_path <- "../input/rezoning_dataset_usable_19990101_20260212.csv"
panel_path <- "../input/chicago_alderman_panel.csv"
trusted_summary_path <- "../input/alderman_rezoning_summary_19990101_20260212.csv"

candidate_output_path <- "../output/alderman_strictness_candidates.csv"
winner_output_path <- "../output/alderman_rezoning_strictness_scores.csv"
plot_output_path <- "../output/strictness_candidates_rank_plot.pdf"
qc_csv_path <- "../output/strictness_candidates_qc.csv"
qc_txt_path <- "../output/strictness_candidates_qc.txt"

rezoning <- read_csv(rezoning_path, show_col_types = FALSE)
panel <- read_csv(panel_path, show_col_types = FALSE)
trusted_summary <- read_csv(trusted_summary_path, show_col_types = FALSE)

panel_monthly <- panel %>%
  mutate(
    ward = as.integer(ward),
    alderman = str_squish(as.character(alderman)),
    panel_month = parse_mixed_date(paste0("1 ", month)),
    panel_month = as.Date(lubridate::floor_date(panel_month, "month"))
  ) %>%
  filter(!is.na(ward), !is.na(alderman), alderman != "", !is.na(panel_month)) %>%
  distinct(ward, panel_month, alderman)

panel_dup <- panel_monthly %>% count(ward, panel_month) %>% filter(n > 1)
if (nrow(panel_dup) > 0) {
  write_csv(panel_dup, qc_csv_path)
  stop("Panel has duplicate ward-month rows.")
}

panel_terms <- panel_monthly %>%
  arrange(ward, panel_month) %>%
  group_by(ward) %>%
  mutate(run_id = cumsum(alderman != lag(alderman, default = first(alderman)))) %>%
  group_by(ward, run_id, alderman) %>%
  summarise(term_start = min(panel_month), .groups = "drop") %>%
  arrange(ward, term_start) %>%
  group_by(ward) %>%
  mutate(
    next_start = lead(term_start),
    term_end = if_else(!is.na(next_start), next_start - 1, as.Date("2100-12-31"))
  ) %>%
  ungroup() %>%
  select(ward, alderman, term_start, term_end)

all_aldermen <- panel_monthly %>%
  distinct(alderman) %>%
  arrange(alderman) %>%
  pull(alderman)

rezoning_base <- rezoning %>%
  mutate(
    row_id = row_number(),
    ward = as.integer(ward),
    intro_date = parse_mixed_date(matter_intro_date),
    far_change = suppressWarnings(as.numeric(far_change)),
    from_code = str_squish(as.character(from_code)),
    from_code = if_else(is.na(from_code) | from_code == "", "UNKNOWN", from_code),
    assigned_alderman = str_squish(as.character(assigned_alderman))
  ) %>%
  filter(
    !is.na(far_change),
    !is.na(intro_date),
    !is.na(ward),
    ward >= 1,
    ward <= 50,
    !is.na(assigned_alderman),
    assigned_alderman != ""
  )

map_candidates <- rezoning_base %>%
  select(row_id, ward, intro_date) %>%
  left_join(panel_terms, by = "ward", relationship = "many-to-many") %>%
  filter(intro_date >= term_start, intro_date <= term_end) %>%
  select(row_id, alderman)

ambiguous_map <- map_candidates %>% count(row_id) %>% filter(n > 1)
if (nrow(ambiguous_map) > 0) {
  detailed <- rezoning_base %>%
    semi_join(ambiguous_map, by = "row_id") %>%
    select(row_id, matter_id, ward, intro_date, assigned_alderman)
  write_csv(detailed, qc_csv_path)
  writeLines(
    c(
      "strictness candidate qc",
      paste0("ambiguous_mapping_rows: ", nrow(ambiguous_map)),
      "stopped due to ambiguous ward-term mapping"
    ),
    qc_txt_path
  )
  stop("Ambiguous ward-term mapping detected.")
}

rezoning_mapped <- rezoning_base %>%
  left_join(map_candidates, by = "row_id")

unmapped_rows <- rezoning_mapped %>% filter(is.na(alderman))
rezoning_scoring <- rezoning_mapped %>% filter(!is.na(alderman))

our_summary <- rezoning_scoring %>%
  mutate(norm_name = normalize_name(alderman)) %>%
  group_by(norm_name) %>%
  summarise(
    total_rezonings_our = n(),
    downzone_count_our = sum(far_change < 0, na.rm = TRUE),
    .groups = "drop"
  )

trusted_norm <- trusted_summary %>%
  transmute(
    norm_name = normalize_name(assigned_alderman),
    total_rezonings_trusted = as.integer(total_rezonings),
    downzone_count_trusted = as.integer(downzone_count)
  ) %>%
  group_by(norm_name) %>%
  summarise(
    total_rezonings_trusted = sum(total_rezonings_trusted, na.rm = TRUE),
    downzone_count_trusted = sum(downzone_count_trusted, na.rm = TRUE),
    .groups = "drop"
  )

integrity_compare <- full_join(trusted_norm, our_summary, by = "norm_name") %>%
  mutate(
    total_rezonings_trusted = coalesce(total_rezonings_trusted, 0L),
    downzone_count_trusted = coalesce(downzone_count_trusted, 0L),
    total_rezonings_our = coalesce(total_rezonings_our, 0L),
    downzone_count_our = coalesce(downzone_count_our, 0L),
    total_diff = total_rezonings_our - total_rezonings_trusted,
    down_diff = downzone_count_our - downzone_count_trusted
  )

integrity_mismatch <- integrity_compare %>%
  filter(total_diff != 0 | down_diff != 0)

if (nrow(integrity_mismatch) > 0) {
  qc_fail <- integrity_mismatch %>% arrange(desc(abs(total_diff)), desc(abs(down_diff)))
  write_csv(qc_fail, qc_csv_path)
  writeLines(
    c(
      "strictness candidate qc",
      paste0("integrity_mismatch_rows: ", nrow(integrity_mismatch)),
      "stopped due to trusted summary mismatch"
    ),
    qc_txt_path
  )
  stop("Trusted summary mismatch after canonical mapping.")
}

top_bins <- rezoning_scoring %>%
  count(from_code, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(from_code)

scores_t0 <- compute_candidates_for_threshold(rezoning_scoring, threshold = 0, suffix = "t0", top_bins = top_bins)
scores_t01 <- compute_candidates_for_threshold(rezoning_scoring, threshold = 0.1, suffix = "t01", top_bins = top_bins)

counts_all <- rezoning_scoring %>%
  group_by(alderman) %>%
  summarise(
    n_rezonings = n(),
    n_downzones_lt0 = sum(far_change < 0, na.rm = TRUE),
    n_downzones_le0_1 = sum(far_change <= -0.1, na.rm = TRUE),
    n_upzones_gt0 = sum(far_change > 0, na.rm = TRUE),
    n_upzones_ge0_1 = sum(far_change >= 0.1, na.rm = TRUE),
    mean_far_change = mean(far_change, na.rm = TRUE),
    .groups = "drop"
  )

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

candidate_df <- tibble(alderman = all_aldermen) %>%
  left_join(counts_all, by = "alderman") %>%
  left_join(scores_t0, by = "alderman") %>%
  left_join(scores_t01, by = "alderman") %>%
  mutate(
    n_rezonings = coalesce(n_rezonings, 0L),
    imputed_zero_rezonings = as.integer(n_rezonings == 0)
  )

for (col in candidate_cols) {
  city_mean <- mean(candidate_df[[col]][candidate_df$n_rezonings > 0], na.rm = TRUE)
  if (!is.finite(city_mean)) {
    city_mean <- 0
  }
  candidate_df[[col]] <- ifelse(candidate_df$n_rezonings == 0, city_mean, candidate_df[[col]])
}

if (any(!is.finite(as.matrix(candidate_df[, candidate_cols])))) {
  stop("Non-finite values found in candidate score columns after imputation.")
}

write_csv(candidate_df, candidate_output_path)

baseline_winner <- candidate_df %>%
  transmute(
    alderman,
    strictness_index = eb_down_share_t0,
    winner_candidate = "eb_down_share_t0",
    n_rezonings,
    imputed_zero_rezonings
  )
write_csv(baseline_winner, winner_output_path)

rank_plot_df <- candidate_df %>%
  select(alderman, all_of(candidate_cols)) %>%
  pivot_longer(
    cols = all_of(candidate_cols),
    names_to = "candidate",
    values_to = "score"
  ) %>%
  group_by(candidate) %>%
  arrange(desc(score), .by_group = TRUE) %>%
  mutate(rank = row_number(), n_in_group = n()) %>%
  filter(rank <= 12 | rank > (n_in_group - 12)) %>%
  ungroup() %>%
  arrange(candidate, score) %>%
  mutate(label_id = paste(candidate, alderman, sep = "__"))

rank_plot_df$label_id <- factor(rank_plot_df$label_id, levels = unique(rank_plot_df$label_id))

p <- ggplot(rank_plot_df, aes(x = score, y = label_id, fill = score)) +
  geom_col() +
  facet_wrap(~candidate, scales = "free_y", ncol = 2) +
  scale_y_discrete(labels = function(x) sub(".*__", "", x)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Strictness Candidate Ranks (Top/Bottom 12 per Candidate)",
    x = "Score (higher = stricter)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 5),
    strip.text = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(plot_output_path, p, width = 11, height = 14, device = "pdf", bg = "white")

rank_snaps <- candidate_df %>%
  select(alderman, n_rezonings, all_of(candidate_cols)) %>%
  pivot_longer(
    cols = all_of(candidate_cols),
    names_to = "candidate",
    values_to = "score"
  ) %>%
  group_by(candidate) %>%
  arrange(desc(score), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3 | rank > (n() - 3)) %>%
  ungroup()

scalar_qc <- tibble(
  section = "scalar",
  check = c(
    "rezoning_rows_raw",
    "rezoning_rows_base",
    "rezoning_rows_scoring",
    "rezoning_rows_unmapped",
    "canonical_aldermen_in_panel",
    "candidate_rows",
    "candidate_cols_count",
    "zero_rezoning_imputed",
    "trusted_mismatch_rows"
  ),
  value_numeric = c(
    nrow(rezoning),
    nrow(rezoning_base),
    nrow(rezoning_scoring),
    nrow(unmapped_rows),
    length(all_aldermen),
    nrow(candidate_df),
    length(candidate_cols),
    sum(candidate_df$imputed_zero_rezonings),
    nrow(integrity_mismatch)
  ),
  value_text = c(
    "raw input rows",
    "rows passing base filters",
    "rows mapped to canonical alderman",
    "rows with missing canonical alderman after mapping",
    "unique aldermen in panel",
    "rows in candidate output",
    "number of candidate score columns",
    "aldermen with n_rezonings == 0",
    "trusted summary mismatch rows"
  ),
  alderman = NA_character_,
  candidate = NA_character_,
  score = NA_real_,
  rank = NA_real_
)

integrity_qc <- integrity_compare %>%
  transmute(
    section = "integrity_by_name",
    check = norm_name,
    value_numeric = as.numeric(total_diff),
    value_text = paste0("down_diff=", down_diff),
    alderman = NA_character_,
    candidate = NA_character_,
    score = NA_real_,
    rank = NA_real_
  )

rank_qc <- rank_snaps %>%
  transmute(
    section = "candidate_rank_snapshot",
    check = candidate,
    value_numeric = as.numeric(n_rezonings),
    value_text = paste0("rank=", rank),
    alderman,
    candidate,
    score,
    rank = as.numeric(rank)
  )

qc_table <- bind_rows(scalar_qc, integrity_qc, rank_qc)
write_csv(qc_table, qc_csv_path)

qc_lines <- c(
  "strictness candidates qc",
  paste0("rezoning_rows_raw: ", nrow(rezoning)),
  paste0("rezoning_rows_base: ", nrow(rezoning_base)),
  paste0("rezoning_rows_scoring: ", nrow(rezoning_scoring)),
  paste0("rezoning_rows_unmapped: ", nrow(unmapped_rows)),
  paste0("canonical_aldermen_in_panel: ", length(all_aldermen)),
  paste0("candidate_rows: ", nrow(candidate_df)),
  paste0("zero_rezoning_imputed: ", sum(candidate_df$imputed_zero_rezonings)),
  paste0("trusted_mismatch_rows: ", nrow(integrity_mismatch)),
  "top_3_eb_down_share_t0:",
  paste0(
    "  ",
    candidate_df %>%
      arrange(desc(eb_down_share_t0)) %>%
      slice_head(n = 3) %>%
      pull(alderman),
    " | score=",
    round(candidate_df %>% arrange(desc(eb_down_share_t0)) %>% slice_head(n = 3) %>% pull(eb_down_share_t0), 4),
    " | n=",
    candidate_df %>% arrange(desc(eb_down_share_t0)) %>% slice_head(n = 3) %>% pull(n_rezonings)
  )
)

writeLines(qc_lines, qc_txt_path)

message("Wrote ", candidate_output_path)
message("Wrote ", winner_output_path)
message("Wrote ", plot_output_path)
message("Wrote ", qc_csv_path)
message("Wrote ", qc_txt_path)
