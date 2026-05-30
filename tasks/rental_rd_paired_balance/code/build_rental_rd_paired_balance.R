# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_paired_balance/code")
# bandwidth_ft <- 500

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <bandwidth_ft>.", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))
min_cluster_ward_pairs <- 20L

rent <- read_parquet(sprintf("../input/rental_rd_characteristics_panel_bw%s.parquet", bandwidth_label)) %>%
  as_tibble()

if (!all(c("segment_id", "ward_pair", "signed_dist_ft") %in% names(rent))) {
  stop("Rental characteristics panel must include segment_id, ward_pair, and signed_dist_ft.", call. = FALSE)
}

rent <- rent %>%
  mutate(
    segment_id = as.character(segment_id),
    ward_pair = as.character(ward_pair),
    score_side = case_when(
      signed_dist_ft < 0 ~ "lenient",
      signed_dist_ft > 0 ~ "strict",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != "",
    !is.na(score_side)
  )

covariates <- tibble::tribble(
  ~variable, ~label, ~digits,
  "nearest_school_dist_ft", "Dist. to school", 0,
  "nearest_park_dist_ft", "Dist. to park", 0,
  "nearest_major_road_dist_ft", "Dist. to major road", 0,
  "nearest_cta_stop_dist_ft", "Dist. to CTA stop", 0,
  "lake_michigan_dist_ft", "Dist. to Lake Michigan", 0
) %>%
  filter(variable %in% names(rent))

if (nrow(covariates) == 0) {
  stop("No external amenity covariates are available for the paired balance table.", call. = FALSE)
}

paired_balance_rows <- list()
for (j in seq_len(nrow(covariates))) {
  variable <- covariates$variable[[j]]
  side_means <- rent %>%
    mutate(balance_value = as.numeric(.data[[variable]])) %>%
    filter(is.finite(balance_value)) %>%
    group_by(ward_pair, segment_id, score_side) %>%
    summarise(
      side_mean = mean(balance_value),
      side_n = n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = score_side,
      values_from = c(side_mean, side_n)
    )

  if (!all(c("side_mean_lenient", "side_mean_strict") %in% names(side_means))) {
    next
  }

  paired <- side_means %>%
    filter(is.finite(side_mean_lenient), is.finite(side_mean_strict)) %>%
    mutate(difference = side_mean_strict - side_mean_lenient)

  if (nrow(paired) == 0) {
    next
  }

  pooled_sd <- sqrt((var(paired$side_mean_lenient) + var(paired$side_mean_strict)) / 2)
  paired_test_result <- paired_balance_test(paired, min_cluster_ward_pairs)

  paired_balance_rows[[length(paired_balance_rows) + 1]] <- tibble(
    variable = variable,
    label = covariates$label[[j]],
    lenient_mean = mean(paired$side_mean_lenient),
    strict_mean = mean(paired$side_mean_strict),
    difference = mean(paired$difference),
    se = paired_test_result$se[[1]],
    p_value = paired_test_result$p_value[[1]],
    normalized_difference = ifelse(is.finite(pooled_sd) && pooled_sd > 0, mean(paired$difference) / pooled_sd, NA_real_),
    n_segments = nrow(paired),
    n_ward_pairs = n_distinct(paired$ward_pair),
    digits = covariates$digits[[j]]
  )
}

external_balance <- bind_rows(paired_balance_rows) %>%
  mutate(
    label_tex = gsub("\\\\", "\\\\textbackslash{}", label),
    label_tex = gsub("([_%$#&{}])", "\\\\\\1", label_tex, perl = TRUE),
    lenient_text = mapply(
      function(x, d) if (is.na(x)) "" else formatC(x, format = "f", digits = d, big.mark = ","),
      lenient_mean,
      digits
    ),
    strict_text = mapply(
      function(x, d) if (is.na(x)) "" else formatC(x, format = "f", digits = d, big.mark = ","),
      strict_mean,
      digits
    ),
    difference_text = mapply(
      function(x, d) if (is.na(x)) "" else formatC(x, format = "f", digits = d, big.mark = ","),
      difference,
      digits
    ),
    se_text = mapply(
      function(x, d) if (is.na(x)) "" else formatC(x, format = "f", digits = d, big.mark = ","),
      se,
      digits
    ),
    p_value_text = ifelse(
      is.na(p_value),
      "",
      formatC(p_value, format = "f", digits = 3, big.mark = ",")
    ),
    normalized_difference_text = ifelse(
      is.na(normalized_difference),
      "",
      formatC(normalized_difference, format = "f", digits = 3, big.mark = ",")
    ),
    n_segments_text = ifelse(
      is.na(n_segments),
      "",
      formatC(round(n_segments), format = "d", big.mark = ",")
    ),
    n_ward_pairs_text = ifelse(
      is.na(n_ward_pairs),
      "",
      formatC(round(n_ward_pairs), format = "d", big.mark = ",")
    )
  )

if (nrow(external_balance) == 0) {
  stop("No external amenity rows available for the paired balance table.", call. = FALSE)
}

table_rows <- sprintf(
  "%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
  external_balance$label_tex,
  external_balance$lenient_text,
  external_balance$strict_text,
  external_balance$difference_text,
  external_balance$se_text,
  external_balance$p_value_text,
  external_balance$normalized_difference_text,
  external_balance$n_segments_text,
  external_balance$n_ward_pairs_text
)

writeLines(
  c(
    "\\begingroup",
    "\\centering",
    "\\small",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{lrrrrrrrr}",
    "\\toprule",
    "Covariate & Less stringent mean & More stringent mean & Diff. & SE & $p$-value & Norm. diff. & Segments & Ward pairs \\\\",
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "}%",
    sprintf(
      "\\par\\vspace{0.5em}\\parbox{0.94\\linewidth}{\\footnotesize Notes: Listed-rent observations within %sft of ward boundaries. Rows first collapse observations to segment-by-side means, then compare more-stringent-side and less-stringent-side means within the same boundary segment. Difference is more stringent minus less stringent. Standard errors are clustered by ward pair across segment-level paired differences when at least %d ward pairs are available. Normalized differences divide the paired mean difference by the pooled standard deviation of the segment-side means.}",
      bandwidth_label,
      min_cluster_ward_pairs
    ),
    "\\par\\endgroup"
  ),
  sprintf("../output/rental_rd_paired_external_amenity_balance_bw%s.tex", bandwidth_label)
)
