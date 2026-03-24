## Summarize internal density robustness checks for a permit-subset score
## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_density_robustness/code")
# Rscript summarize_new_construction_density_robustness.R "../output"

source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  output_dir <- args[1]
} else {
  stop("FATAL: Script requires 1 arg: <output_dir>", call. = FALSE)
}

variant_suffix <- if (length(args) >= 2) args[2] else "new_construction"
variant_label <- if (length(args) >= 3) args[3] else "New construction only"
comparison_output <- if (length(args) >= 4) args[4] else file.path(output_dir, "density_score_robustness_comparison.csv")
comparison_tex_output <- if (length(args) >= 5) args[5] else file.path(output_dir, "density_score_robustness_comparison.tex")
parcel_sign_output <- if (length(args) >= 6) args[6] else file.path(output_dir, paste0("parcel_sign_agreement_baseline_vs_", variant_suffix, ".csv"))
variant_estimate_col <- paste0(variant_suffix, "_estimate")
variant_se_col <- paste0(variant_suffix, "_se")
variant_p_col <- paste0(variant_suffix, "_p_value")

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
    "\\begin{tabular}{llllll}",
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

read_rd_row <- function(stem, comparison_label) {
  meta <- read_csv(file.path(output_dir, paste0(stem, "_meta.csv")), show_col_types = FALSE)
  tibble(
    comparison = comparison_label,
    estimate = meta$rd_jump_estimate[[1]],
    se = meta$rd_jump_se[[1]],
    p_value = meta$rd_jump_p[[1]]
  )
}

read_fe_row <- function(path, yvar_name, comparison_label) {
  summary_df <- read_csv(path, show_col_types = FALSE)
  row <- summary_df %>% filter(yvar == yvar_name)
  if (nrow(row) != 1) {
    stop(sprintf("Expected one FE summary row for %s in %s.", yvar_name, path), call. = FALSE)
  }
  tibble(
    comparison = comparison_label,
    estimate = row$estimate[[1]],
    se = row$se[[1]],
    p_value = row$p_value[[1]]
  )
}

baseline_rows <- bind_rows(
  read_rd_row(
    "rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline",
    "RD log(FAR)"
  ),
  read_rd_row(
    "rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline",
    "RD log(DUPAC)"
  ),
  read_fe_row(
    file.path(output_dir, "fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv"),
    "log(density_far)",
    "FE log(FAR)"
  ),
  read_fe_row(
    file.path(output_dir, "fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv"),
    "log(density_dupac)",
    "FE log(DUPAC)"
  ),
  read_fe_row(
    file.path(output_dir, "fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv"),
    "log(unitscount)",
    "FE log(Units)"
  )
) %>%
  rename(
    baseline_estimate = estimate,
    baseline_se = se,
    baseline_p_value = p_value
  )

new_rows <- bind_rows(
  read_rd_row(
    paste0(
      "rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_",
      variant_suffix
    ),
    "RD log(FAR)"
  ),
  read_rd_row(
    paste0(
      "rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_",
      variant_suffix
    ),
    "RD log(DUPAC)"
  ),
  read_fe_row(
    file.path(
      output_dir,
      paste0("fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_", variant_suffix, ".csv")
    ),
    "log(density_far)",
    "FE log(FAR)"
  ),
  read_fe_row(
    file.path(
      output_dir,
      paste0("fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_", variant_suffix, ".csv")
    ),
    "log(density_dupac)",
    "FE log(DUPAC)"
  ),
  read_fe_row(
    file.path(
      output_dir,
      paste0("fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_", variant_suffix, ".csv")
    ),
    "log(unitscount)",
    "FE log(Units)"
  )
)

names(new_rows)[names(new_rows) == "estimate"] <- variant_estimate_col
names(new_rows)[names(new_rows) == "se"] <- variant_se_col
names(new_rows)[names(new_rows) == "p_value"] <- variant_p_col

comparison_df <- baseline_rows %>%
  inner_join(new_rows, by = "comparison") %>%
  mutate(delta_estimate = .data[[variant_estimate_col]] - baseline_estimate)

comparison_tex <- comparison_df %>%
  mutate(
    Comparison = comparison,
    `Baseline est (SE)` = paste0(fmt_num(baseline_estimate), " (", fmt_num(baseline_se), ")"),
    `Baseline p` = fmt_num(baseline_p_value),
    `Variant est (SE)` = paste0(fmt_num(.data[[variant_estimate_col]]), " (", fmt_num(.data[[variant_se_col]]), ")"),
    `Variant p` = fmt_num(.data[[variant_p_col]]),
    Delta = fmt_num(delta_estimate)
  ) %>%
  select(Comparison, `Baseline est (SE)`, `Baseline p`, `Variant est (SE)`, `Variant p`, Delta)

baseline_parcels <- read_csv(
  file.path(output_dir, "parcels_with_ward_distances_baseline.csv"),
  show_col_types = FALSE
) %>%
  transmute(pin = as.character(pin), baseline_sign = as.numeric(sign))

new_parcels <- read_csv(
  file.path(output_dir, paste0("parcels_with_ward_distances_", variant_suffix, ".csv")),
  show_col_types = FALSE
) %>%
  transmute(pin = as.character(pin), new_construction_sign = as.numeric(sign))

parcel_sign_agreement <- baseline_parcels %>%
  inner_join(new_parcels, by = "pin") %>%
  summarise(
    n_matched_parcels = n(),
    sign_agreement_rate = mean(baseline_sign == new_construction_sign, na.rm = TRUE),
    reclassification_rate = 1 - sign_agreement_rate
  )

write_csv(comparison_df, comparison_output)
write_tabular_tex(comparison_tex, comparison_tex_output)
write_csv(parcel_sign_agreement, parcel_sign_output)

message("Saved density robustness summaries:")
message("  Variant: ", variant_suffix, " (", variant_label, ")")
message("  Comparison CSV: ", comparison_output)
message("  Comparison TEX: ", comparison_tex_output)
message("  Parcel sign agreement CSV: ", parcel_sign_output)
