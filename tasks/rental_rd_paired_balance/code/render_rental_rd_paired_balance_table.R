# Render the listed-rent RD paired external amenity balance table.

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_paired_balance/code")
# bandwidth_ft <- 500

source("../../setup_environment/code/packages.R", local = new.env(parent = globalenv()))

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <bandwidth_ft>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))
min_cluster_ward_pairs <- 20L

external_balance <- read_csv(
  sprintf("../temp/rental_rd_paired_covariate_balance_bw%s.csv", bandwidth_label),
  show_col_types = FALSE
) %>%
  filter(
    sample == "all",
    group == "External amenities"
  ) %>%
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

message("Saved listed-rent RD paired external amenity balance table.")
