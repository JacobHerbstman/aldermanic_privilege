## Export ordered score rankings to a LaTeX longtable

source("../../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/new_construction_score_comparison/code")
# variant_id <- "restricted_renovation"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(variant_id)
}

if (length(args) != 1) {
  stop("FATAL: Script requires 1 arg: <variant_id>", call. = FALSE)
}

variant_id <- args[1]
variant_labels <- c(restricted_renovation = "restricted-renovation")
if (!variant_id %in% names(variant_labels)) {
  stop("variant_id must be restricted_renovation", call. = FALSE)
}

score_col <- paste0(variant_id, "_score")
rank_col <- paste0(variant_id, "_rank")
variant_label <- variant_labels[[variant_id]]

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x <- gsub("#", "\\\\#", x, fixed = TRUE)
  x
}

rankings <- read_csv(
  sprintf("../output/score_comparison_%s_vs_baseline.csv", variant_id),
  show_col_types = FALSE
) %>%
  transmute(
    rank = as.integer(round(.data[[rank_col]])),
    alderman = as.character(alderman),
    variant_score = as.numeric(.data[[score_col]]),
    baseline_rank = as.integer(round(baseline_rank)),
    rank_change = as.integer(round(rank_change))
  ) %>%
  arrange(rank, alderman)

lines <- c(
  "\\begin{longtable}{r l r r r}",
  sprintf("\\caption{Alderman rankings using the %s stringency score}\\\\", latex_escape(variant_label)),
  "\\toprule",
  "Rank & Alderman & Score & Baseline Rank & Rank Change\\\\",
  "\\midrule",
  "\\endfirsthead",
  "\\toprule",
  "Rank & Alderman & Score & Baseline Rank & Rank Change\\\\",
  "\\midrule",
  "\\endhead",
  "\\midrule",
  "\\multicolumn{5}{r}{\\emph{Continued on next page}}\\\\",
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot"
)

for (i in seq_len(nrow(rankings))) {
  row <- rankings[i, ]
  lines <- c(
    lines,
    sprintf(
      "%d & %s & %.3f & %d & %d\\\\",
      row$rank,
      latex_escape(row$alderman),
      row$variant_score,
      row$baseline_rank,
      row$rank_change
    )
  )
}

lines <- c(lines, "\\end{longtable}")
rankings_output <- sprintf("../output/score_rankings_%s.tex", variant_id)
writeLines(lines, rankings_output)

message("Saved LaTeX rankings table: ", rankings_output)
