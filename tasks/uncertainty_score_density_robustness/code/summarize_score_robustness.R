## Summarize score and density robustness results
## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/uncertainty_score_density_robustness/code")
# Rscript summarize_score_robustness.R "../output"

source("../../setup_environment/code/packages.R")
source("../../create_alderman_uncertainty_index/code/uncertainty_index_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  output_dir <- args[1]
} else {
  stop("FATAL: Script requires 1 arg: <output_dir>", call. = FALSE)
}

variant_ids <- c("baseline", "raw_rank_days", "days_unlogged", "reduced_ses")

fmt_num <- function(x, digits = 3) {
  ifelse(
    is.finite(x),
    formatC(x, format = "f", digits = digits, big.mark = ","),
    ""
  )
}

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}

write_tabular_tex <- function(df, file) {
  lines <- c(
    paste0("\\begin{tabular}{", paste(rep("l", ncol(df)), collapse = ""), "}"),
    "\\toprule",
    paste(paste(latex_escape(names(df)), collapse = " & "), "\\\\"),
    "\\midrule"
  )

  for (i in seq_len(nrow(df))) {
    row_text <- vapply(df[i, ], function(x) latex_escape(as.character(x)), character(1))
    lines <- c(lines, paste(paste(row_text, collapse = " & "), "\\\\"))
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, file)
}

read_variant_csv <- function(prefix, variant_id) {
  read_csv(file.path(output_dir, paste0(prefix, "_", variant_id, ".csv")), show_col_types = FALSE)
}

scores_long <- bind_rows(lapply(variant_ids, function(variant_id) {
  read_variant_csv("alderman_uncertainty_index", variant_id) %>%
    transmute(
      variant_id = variant_id,
      alderman = alderman,
      uncertainty_index = as.numeric(uncertainty_index),
      n_permits = if ("n_permits" %in% names(.)) as.numeric(n_permits) else NA_real_
    )
}))

metadata <- bind_rows(lapply(variant_ids, function(variant_id) {
  read_variant_csv("score_variant_metadata", variant_id)
}))

baseline_scores <- scores_long %>%
  filter(variant_id == "baseline") %>%
  transmute(
    alderman,
    baseline_score = uncertainty_index,
    baseline_rank = rank(-uncertainty_index, ties.method = "average")
  )

rank_changes <- bind_rows(lapply(variant_ids, function(current_variant) {
  scores_long %>%
    filter(variant_id == current_variant) %>%
    transmute(
      alderman,
      variant_id = current_variant,
      variant_score = uncertainty_index,
      variant_rank = rank(-uncertainty_index, ties.method = "average"),
      n_permits_variant = n_permits
    ) %>%
    left_join(baseline_scores, by = "alderman") %>%
    mutate(
      rank_change = variant_rank - baseline_rank,
      abs_rank_change = abs(rank_change)
    )
}))

write_csv(rank_changes, file.path(output_dir, "score_variant_rank_changes.csv"))

rank_summary <- rank_changes %>%
  group_by(variant_id) %>%
  summarise(
    mean_abs_rank_change = mean(abs_rank_change, na.rm = TRUE),
    max_abs_rank_change = max(abs_rank_change, na.rm = TRUE),
    .groups = "drop"
  )

correlation_summary <- bind_rows(lapply(variant_ids, function(current_variant) {
  joined <- scores_long %>%
    filter(variant_id == current_variant) %>%
    transmute(alderman, variant_score = uncertainty_index) %>%
    left_join(baseline_scores, by = "alderman")

  tibble(
    variant_id = current_variant,
    corr_with_baseline = cor(joined$variant_score, joined$baseline_score, use = "complete.obs"),
    spearman_with_baseline = cor(
      joined$variant_score,
      joined$baseline_score,
      use = "complete.obs",
      method = "spearman"
    )
  )
}))

baseline_parcels <- read_variant_csv("parcels_with_ward_distances", "baseline") %>%
  transmute(pin = as.character(pin), baseline_sign = as.numeric(sign))

parcel_summary <- bind_rows(lapply(variant_ids, function(current_variant) {
  variant_parcels <- read_variant_csv("parcels_with_ward_distances", current_variant) %>%
    transmute(pin = as.character(pin), variant_sign = as.numeric(sign))

  joined <- baseline_parcels %>%
    inner_join(variant_parcels, by = "pin")

  agreement <- mean(joined$baseline_sign == joined$variant_sign, na.rm = TRUE)
  tibble(
    variant_id = current_variant,
    parcel_sign_agreement_with_baseline = agreement,
    parcel_reclassification_rate_vs_baseline = 1 - agreement
  )
}))

diagnostics <- metadata %>%
  left_join(correlation_summary, by = "variant_id") %>%
  left_join(rank_summary, by = "variant_id") %>%
  left_join(parcel_summary, by = "variant_id") %>%
  select(
    variant_id,
    construction_rule,
    n_aldermen,
    n_permits_used,
    corr_with_baseline,
    spearman_with_baseline,
    mean_abs_rank_change,
    max_abs_rank_change,
    parcel_sign_agreement_with_baseline,
    parcel_reclassification_rate_vs_baseline,
    stage1_nobs,
    stage1_r2
  )

write_csv(diagnostics, file.path(output_dir, "score_variant_diagnostics.csv"))

diagnostics_tex <- diagnostics %>%
  mutate(
    Variant = variant_display_label(variant_id),
    `Corr. w/ baseline` = fmt_num(corr_with_baseline),
    Spearman = fmt_num(spearman_with_baseline),
    `Mean |rank change|` = fmt_num(mean_abs_rank_change),
    `Max |rank change|` = fmt_num(max_abs_rank_change),
    `Sign agreement` = fmt_num(parcel_sign_agreement_with_baseline),
    `Reclass. rate` = fmt_num(parcel_reclassification_rate_vs_baseline),
    `Permits used` = fmt_num(n_permits_used, 0),
    `Stage 1 adj. R2` = fmt_num(stage1_r2)
  ) %>%
  select(
    Variant,
    `Corr. w/ baseline`,
    Spearman,
    `Mean |rank change|`,
    `Max |rank change|`,
    `Sign agreement`,
    `Reclass. rate`,
    `Permits used`,
    `Stage 1 adj. R2`
  )

write_tabular_tex(diagnostics_tex, file.path(output_dir, "score_variant_diagnostics.tex"))

stage1_terms <- bind_rows(lapply(variant_ids, function(variant_id) {
  read_variant_csv("score_variant_stage1_terms", variant_id) %>%
    mutate(
      variant_id = as.character(variant_id),
      term = as.character(term),
      estimate = as.numeric(estimate),
      std_error = as.numeric(std_error),
      p_value = as.numeric(p_value)
    )
}))

write_csv(stage1_terms, file.path(output_dir, "score_variant_stage1_terms.csv"))

fe_summary_long <- bind_rows(lapply(variant_ids, function(current_variant) {
  read_variant_csv(
    "fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_segment",
    current_variant
  ) %>%
    mutate(variant_id = current_variant)
}))

density_score_robustness <- fe_summary_long %>%
  mutate(
    outcome_key = case_when(
      yvar == "log(density_far)" ~ "far",
      yvar == "log(density_dupac)" ~ "dupac",
      yvar == "log(unitscount)" ~ "units",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(outcome_key)) %>%
  select(variant_id, outcome_key, estimate, se, p_value) %>%
  pivot_wider(
    names_from = outcome_key,
    values_from = c(estimate, se, p_value),
    names_glue = "{outcome_key}_{.value}"
  ) %>%
  left_join(
    fe_summary_long %>%
      group_by(variant_id) %>%
      summarise(
        n_obs = max(n_obs, na.rm = TRUE),
        n_ward_pairs = max(n_ward_pairs, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "variant_id"
  ) %>%
  select(
    variant_id,
    far_estimate = far_estimate,
    far_se = far_se,
    far_p_value = far_p_value,
    dupac_estimate = dupac_estimate,
    dupac_se = dupac_se,
    dupac_p_value = dupac_p_value,
    units_estimate = units_estimate,
    units_se = units_se,
    units_p_value = units_p_value,
    n_obs,
    n_ward_pairs
  )

write_csv(
  density_score_robustness,
  file.path(output_dir, "density_score_robustness_summary.csv")
)

density_tex <- density_score_robustness %>%
  mutate(
    Variant = variant_display_label(variant_id),
    FAR = paste0(fmt_num(far_estimate), " (", fmt_num(far_se), ")"),
    DUPAC = paste0(fmt_num(dupac_estimate), " (", fmt_num(dupac_se), ")"),
    Units = paste0(fmt_num(units_estimate), " (", fmt_num(units_se), ")"),
    N = fmt_num(n_obs, 0),
    `Ward pairs` = fmt_num(n_ward_pairs, 0)
  ) %>%
  select(Variant, FAR, DUPAC, Units, N, `Ward pairs`)

write_tabular_tex(
  density_tex,
  file.path(output_dir, "density_score_robustness_summary.tex")
)
