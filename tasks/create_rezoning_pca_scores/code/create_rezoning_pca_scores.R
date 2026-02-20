source("../../setup_environment/code/packages.R")

parse_mixed_date <- function(x) {
  chars <- as.character(x)
  chars <- trimws(chars)
  chars[chars %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      chars,
      orders = c("ymd", "mdy", "dmy"),
      quiet = TRUE
    )
  )
  as.Date(parsed)
}

run_weighted_pca <- function(x, weights) {
  w <- as.numeric(weights)
  if (any(!is.finite(w)) || any(w <= 0)) {
    stop("PCA weights must be finite and strictly positive.")
  }

  w <- w / sum(w)
  center <- colSums(sweep(x, 1, w, "*"))
  x_centered <- sweep(x, 2, center, "-")
  scale <- sqrt(colSums(sweep(x_centered^2, 1, w, "*")))
  scale[!is.finite(scale) | scale <= 0] <- 1
  z <- sweep(x_centered, 2, scale, "/")

  z_weighted <- sweep(z, 1, sqrt(w), "*")
  eig <- eigen(crossprod(z_weighted), symmetric = TRUE)

  list(
    scores = z %*% eig$vectors,
    rotation = eig$vectors,
    prop_variance = eig$values / sum(eig$values)
  )
}

name_aliases <- c(
  "Felix Cardona Jr" = "Felix Cardona Jr.",
  "Michael Scott Jr" = "Michael Scott Jr.",
  "Walter Burnett, Jr" = "Walter Burnett, Jr."
)

rezoning <- read_csv(
  "../input/rezoning_dataset_usable_19990101_20260212.csv",
  show_col_types = FALSE
)

required_cols <- c(
  "assigned_alderman",
  "ward",
  "matter_intro_date",
  "matter_passed_date",
  "far_change",
  "days_intro_to_passed"
)
missing_cols <- setdiff(required_cols, names(rezoning))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  transmute(
    ward = suppressWarnings(as.integer(ward)),
    intro_month = str_squish(as.character(month)),
    alderman_panel = str_squish(as.character(alderman))
  ) %>%
  filter(!is.na(ward), !is.na(intro_month), intro_month != "", !is.na(alderman_panel), alderman_panel != "") %>%
  distinct()

all_aldermen <- alderman_panel %>%
  distinct(alderman_panel) %>%
  arrange(alderman_panel) %>%
  pull(alderman_panel)

rezoning <- rezoning %>%
  mutate(
    ward = suppressWarnings(as.integer(ward)),
    intro_date = parse_mixed_date(matter_intro_date),
    passed_date = parse_mixed_date(matter_passed_date),
    intro_month = if_else(!is.na(intro_date), format(intro_date, "%b %Y"), NA_character_),
    assigned_alderman = str_squish(as.character(assigned_alderman)),
    assigned_alderman = recode(assigned_alderman, !!!name_aliases),
    far_change = suppressWarnings(as.numeric(far_change)),
    days_intro_to_passed = suppressWarnings(as.numeric(days_intro_to_passed))
  ) %>%
  left_join(alderman_panel, by = c("ward", "intro_month")) %>%
  mutate(
    alderman = coalesce(
      alderman_panel,
      if_else(assigned_alderman %in% all_aldermen, assigned_alderman, NA_character_)
    )
  )

unmatched_assigned <- rezoning %>%
  filter(!is.na(assigned_alderman), assigned_alderman != "", is.na(alderman)) %>%
  distinct(assigned_alderman) %>%
  arrange(assigned_alderman)

if (nrow(unmatched_assigned) > 0) {
  stop(
    "Assigned alderman names missing from panel after mapping: ",
    paste(unmatched_assigned$assigned_alderman, collapse = ", ")
  )
}

base_sample <- rezoning %>%
  filter(!is.na(alderman), alderman != "", !is.na(far_change))

if (nrow(base_sample) == 0) {
  stop("No rezoning rows available after alderman/date/FAR filtering.")
}

global_wait_median <- median(base_sample$days_intro_to_passed, na.rm = TRUE)
if (!is.finite(global_wait_median)) {
  global_wait_median <- 0
}

alderman_features <- base_sample %>%
  group_by(alderman) %>%
  summarise(
    n_rezonings = n(),
    n_upzones = sum(far_change > 0, na.rm = TRUE),
    n_downzones = sum(far_change < 0, na.rm = TRUE),
    downzone_share = n_downzones / n_rezonings,
    upzone_share = n_upzones / n_rezonings,
    downzone_share_conditional = if_else(
      (n_upzones + n_downzones) > 0,
      n_downzones / (n_upzones + n_downzones),
      NA_real_
    ),
    upzone_share_conditional = if_else(
      (n_upzones + n_downzones) > 0,
      n_upzones / (n_upzones + n_downzones),
      NA_real_
    ),
    median_upzone_far = if_else(
      n_upzones > 0,
      median(far_change[far_change > 0], na.rm = TRUE),
      NA_real_
    ),
    median_downzone_far = if_else(
      n_downzones > 0,
      median(far_change[far_change < 0], na.rm = TRUE),
      NA_real_
    ),
    median_downzone_far_abs = if_else(
      n_downzones > 0,
      median(abs(far_change[far_change < 0]), na.rm = TRUE),
      NA_real_
    ),
    avg_wait_days = if_else(
      sum(!is.na(days_intro_to_passed)) > 0,
      mean(days_intro_to_passed, na.rm = TRUE),
      NA_real_
    ),
    n_wait_obs = sum(!is.na(days_intro_to_passed)),
    .groups = "drop"
  ) %>%
  mutate(
    median_upzone_far = replace_na(median_upzone_far, 0),
    median_downzone_far = replace_na(median_downzone_far, 0),
    median_downzone_far_abs = replace_na(median_downzone_far_abs, 0),
    avg_wait_days = replace_na(avg_wait_days, global_wait_median)
  )

pca_features <- alderman_features %>%
  transmute(
    alderman,
    n_rezonings,
    downzone_share,
    median_downzone_far_abs,
    upzone_size_strict = -1 * median_upzone_far,
    avg_wait_days
  )

pca_complete <- pca_features %>%
  filter(
    !is.na(downzone_share),
    !is.na(median_downzone_far_abs),
    !is.na(upzone_size_strict),
    !is.na(avg_wait_days),
    is.finite(downzone_share),
    is.finite(median_downzone_far_abs),
    is.finite(upzone_size_strict),
    is.finite(avg_wait_days)
  )

if (nrow(pca_complete) < 5) {
  stop("Too few complete alderman observations for PCA.")
}

pca_matrix <- pca_complete %>%
  select(downzone_share, median_downzone_far_abs, upzone_size_strict, avg_wait_days) %>%
  as.matrix()

pca_fit <- prcomp(pca_matrix, center = TRUE, scale. = TRUE)
weighted_pca <- run_weighted_pca(pca_matrix, sqrt(pca_complete$n_rezonings))

score_tbl <- pca_complete %>%
  select(alderman, n_rezonings, downzone_share) %>%
  mutate(
    pc1_raw = as.numeric(pca_fit$x[, "PC1"]),
    pc2 = as.numeric(pca_fit$x[, "PC2"]),
    pc1_weighted_raw = as.numeric(weighted_pca$scores[, 1]),
    pc2_weighted = as.numeric(weighted_pca$scores[, 2])
  )

pc1_sign <- if_else(cor(score_tbl$pc1_raw, score_tbl$downzone_share) < 0, -1, 1)
pc1_weighted_sign <- if_else(cor(score_tbl$pc1_weighted_raw, score_tbl$downzone_share) < 0, -1, 1)

score_tbl <- score_tbl %>%
  mutate(
    strictness_index_pca = pc1_sign * pc1_raw,
    strictness_index_pca_weighted = pc1_weighted_sign * pc1_weighted_raw
  ) %>%
  select(
    alderman,
    strictness_index_pca,
    strictness_index_pca_weighted,
    pc1_raw,
    pc2,
    pc1_weighted_raw,
    pc2_weighted
  )

variance_unweighted <- summary(pca_fit)$importance
variance_tbl <- bind_rows(
  tibble(
    method = "unweighted",
    component = colnames(pca_fit$x),
    prop_variance = as.numeric(variance_unweighted["Proportion of Variance", ]),
    cum_variance = as.numeric(variance_unweighted["Cumulative Proportion", ])
  ),
  tibble(
    method = "weighted_sqrt_n",
    component = paste0("PC", seq_along(weighted_pca$prop_variance)),
    prop_variance = as.numeric(weighted_pca$prop_variance),
    cum_variance = cumsum(as.numeric(weighted_pca$prop_variance))
  )
)

loadings_tbl <- bind_rows(
  as_tibble(pca_fit$rotation[, 1:2, drop = FALSE], rownames = "feature") %>%
    rename(loading_pc1 = PC1, loading_pc2 = PC2) %>%
    mutate(
      method = "unweighted",
      loading_pc1_strict_oriented = pc1_sign * loading_pc1
    ),
  tibble(
    feature = colnames(pca_matrix),
    loading_pc1 = as.numeric(weighted_pca$rotation[, 1]),
    loading_pc2 = as.numeric(weighted_pca$rotation[, 2]),
    method = "weighted_sqrt_n",
    loading_pc1_strict_oriented = pc1_weighted_sign * loading_pc1
  )
) %>%
  select(method, feature, loading_pc1, loading_pc2, loading_pc1_strict_oriented)

output <- tibble(alderman = all_aldermen) %>%
  left_join(alderman_features, by = "alderman") %>%
  left_join(score_tbl, by = "alderman") %>%
  mutate(
    n_rezonings = replace_na(n_rezonings, 0L),
    strictness_index_pca = if_else(n_rezonings == 0 & is.na(strictness_index_pca), 0, strictness_index_pca),
    strictness_index_pca_weighted = if_else(
      n_rezonings == 0 & is.na(strictness_index_pca_weighted),
      0,
      strictness_index_pca_weighted
    ),
    strictness_index = strictness_index_pca_weighted
  ) %>%
  arrange(desc(strictness_index_pca_weighted), alderman)

write_csv(output, "../output/alderman_rezoning_pca_scores.csv")
write_csv(loadings_tbl, "../output/rezoning_pca_loadings.csv")
write_csv(variance_tbl, "../output/rezoning_pca_variance.csv")

plot_df <- output %>%
  filter(!is.na(strictness_index_pca_weighted)) %>%
  arrange(strictness_index_pca_weighted) %>%
  mutate(alderman = factor(alderman, levels = alderman))

p <- ggplot(plot_df, aes(x = strictness_index_pca_weighted, y = alderman, fill = strictness_index_pca_weighted)) +
  geom_col() +
  scale_fill_distiller(palette = "RdYlBu", direction = -1, name = "PCA strictness") +
  labs(
    title = "Rezoning PCA Strictness Index (Weighted by sqrt(N rezonings))",
    subtitle = "Inputs: downzone share, median downzone size, median upzone size (inverted), average wait time",
    x = "Strictness index (weighted, oriented PC1)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave("../output/rezoning_pca_strictness_index.pdf", plot = p, width = 9, height = 13, device = "pdf", bg = "white")

permit_month <- read_csv("../input/alderman_restrictiveness_scores_month_FEs.csv", show_col_types = FALSE) %>%
  transmute(alderman = str_squish(as.character(alderman)), permit_score = as.numeric(strictness_index)) %>%
  filter(!is.na(alderman), alderman != "", !is.na(permit_score))

permit_ward_month <- read_csv("../input/alderman_restrictiveness_scores_ward_month_FEs.csv", show_col_types = FALSE) %>%
  transmute(alderman = str_squish(as.character(alderman)), permit_score = as.numeric(strictness_index)) %>%
  filter(!is.na(alderman), alderman != "", !is.na(permit_score))

get_cor <- function(score_col, score_name, target_df, target_name) {
  d <- output %>%
    filter(!is.na(.data[[score_col]])) %>%
    inner_join(target_df, by = "alderman")

  tibble(
    score_variant = score_name,
    target = target_name,
    n_match = nrow(d),
    pearson = if_else(nrow(d) >= 3, cor(d[[score_col]], d$permit_score), NA_real_),
    spearman = if_else(nrow(d) >= 3, cor(d[[score_col]], d$permit_score, method = "spearman"), NA_real_)
  )
}

correlation_tbl <- bind_rows(
  get_cor("strictness_index_pca", "unweighted", permit_month, "permit_month"),
  get_cor("strictness_index_pca", "unweighted", permit_ward_month, "permit_ward_month"),
  get_cor("strictness_index_pca_weighted", "weighted_sqrt_n", permit_month, "permit_month"),
  get_cor("strictness_index_pca_weighted", "weighted_sqrt_n", permit_ward_month, "permit_ward_month")
)

write_csv(correlation_tbl, "../output/rezoning_pca_vs_permit_correlation.csv")

top_ten <- output %>%
  filter(!is.na(strictness_index_pca_weighted)) %>%
  arrange(desc(strictness_index_pca_weighted)) %>%
  slice_head(n = 10)

bottom_ten <- output %>%
  filter(!is.na(strictness_index_pca_weighted)) %>%
  arrange(strictness_index_pca_weighted) %>%
  slice_head(n = 10)

qc_lines <- c(
  "rezoning pca qc summary",
  paste0("rows_with_alderman_and_far_change: ", nrow(base_sample)),
  paste0("aldermen_in_panel: ", length(all_aldermen)),
  paste0("aldermen_with_feature_rows: ", nrow(alderman_features)),
  paste0("aldermen_used_in_pca_complete_case: ", nrow(pca_complete)),
  paste0(
    "pc1_prop_variance_unweighted: ",
    round(variance_tbl$prop_variance[variance_tbl$method == "unweighted" & variance_tbl$component == "PC1"], 4)
  ),
  paste0(
    "pc1_prop_variance_weighted_sqrt_n: ",
    round(variance_tbl$prop_variance[variance_tbl$method == "weighted_sqrt_n" & variance_tbl$component == "PC1"], 4)
  ),
  paste0(
    "corr_unweighted_permit_month_spearman: ",
    round(correlation_tbl$spearman[correlation_tbl$score_variant == "unweighted" & correlation_tbl$target == "permit_month"], 4)
  ),
  paste0(
    "corr_weighted_permit_month_spearman: ",
    round(correlation_tbl$spearman[correlation_tbl$score_variant == "weighted_sqrt_n" & correlation_tbl$target == "permit_month"], 4)
  ),
  paste0(
    "corr_unweighted_permit_ward_month_spearman: ",
    round(correlation_tbl$spearman[correlation_tbl$score_variant == "unweighted" & correlation_tbl$target == "permit_ward_month"], 4)
  ),
  paste0(
    "corr_weighted_permit_ward_month_spearman: ",
    round(correlation_tbl$spearman[correlation_tbl$score_variant == "weighted_sqrt_n" & correlation_tbl$target == "permit_ward_month"], 4)
  ),
  "top_10_strictness_pca_weighted:",
  paste0(
    "  ", seq_len(nrow(top_ten)), ". ", top_ten$alderman,
    " | n=", top_ten$n_rezonings,
    " | score=", round(top_ten$strictness_index_pca_weighted, 4)
  ),
  "bottom_10_strictness_pca_weighted:",
  paste0(
    "  ", seq_len(nrow(bottom_ten)), ". ", bottom_ten$alderman,
    " | n=", bottom_ten$n_rezonings,
    " | score=", round(bottom_ten$strictness_index_pca_weighted, 4)
  )
)

writeLines(qc_lines, "../output/rezoning_pca_qc.txt")

message("Wrote ../output/alderman_rezoning_pca_scores.csv")
message("Wrote ../output/rezoning_pca_loadings.csv")
message("Wrote ../output/rezoning_pca_variance.csv")
message("Wrote ../output/rezoning_pca_vs_permit_correlation.csv")
message("Wrote ../output/rezoning_pca_strictness_index.pdf")
message("Wrote ../output/rezoning_pca_qc.txt")
