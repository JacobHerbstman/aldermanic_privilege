## Export ordered score rankings to a LaTeX longtable

source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_comparison/code")
# comparison_input <- "../output/score_comparison_no_renovation_vs_baseline.csv"
# rankings_output <- "../output/score_rankings_no_renovation.tex"
# variant_id <- "no_renovation"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(comparison_input, rankings_output, variant_id)
}

if (length(args) >= 2) {
  comparison_input <- args[1]
  rankings_output <- args[2]
} else {
  stop(
    "FATAL: Script requires at least 2 args: <comparison_input> <rankings_output> [<variant_id>]",
    call. = FALSE
  )
}

variant_id <- if (length(args) >= 3) args[3] else "no_renovation"
score_col <- paste0(variant_id, "_score")
rank_col <- paste0(variant_id, "_rank")
variant_label <- str_replace_all(variant_id, "_", "-")

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x <- gsub("#", "\\\\#", x, fixed = TRUE)
  x
}

rankings <- read_csv(comparison_input, show_col_types = FALSE) %>%
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
writeLines(lines, rankings_output)

message("Saved LaTeX rankings table: ", rankings_output)
