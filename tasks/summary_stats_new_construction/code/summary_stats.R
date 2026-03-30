# summary stats of new construction

source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/summary_stats_new_construction/code")
# input_file <- "../input/parcels_with_ward_distances.csv"
# table_output_file <- "../output/summary_stats.tex"
# tabular_output_file <- "../output/summary_stats_tabular.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(input_file, table_output_file, tabular_output_file)
}

if (length(args) != 3) {
  stop("FATAL: Script requires 3 args: <input_file> <table_output_file> <tabular_output_file>", call. = FALSE)
}
input_file <- args[1]
table_output_file <- args[2]
tabular_output_file <- args[3]

cat("Loading data from:", input_file, "\n")
df <- read_csv(input_file, show_col_types = FALSE)

df_clean <- df %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 0,
    construction_year >= 2006
  )

df_mf <- df_clean %>%
  filter(unitscount >= 2)

stats_all <- tibble(
  avg_density_far = mean(df_clean$density_far, na.rm = TRUE),
  avg_units = mean(df_clean$unitscount, na.rm = TRUE),
  avg_stories = mean(df_clean$storiescount, na.rm = TRUE),
  avg_dupac = mean(df_clean$density_dupac, na.rm = TRUE),
  median_dist = median(df_clean$dist_to_boundary, na.rm = TRUE),
  avg_strictness = mean(df_clean$strictness_own, na.rm = TRUE),
  n = nrow(df_clean)
)

stats_mf <- tibble(
  avg_density_far = mean(df_mf$density_far, na.rm = TRUE),
  avg_units = mean(df_mf$unitscount, na.rm = TRUE),
  avg_stories = mean(df_mf$storiescount, na.rm = TRUE),
  avg_dupac = mean(df_mf$density_dupac, na.rm = TRUE),
  median_dist = median(df_mf$dist_to_boundary, na.rm = TRUE),
  avg_strictness = mean(df_mf$strictness_own, na.rm = TRUE),
  n = nrow(df_mf)
)

tabular_content <- paste0(
  "\\begin{tabular}{lcc}\n",
  "\\toprule\n",
  " & (1) & (2) \\\\\n",
  " & All New Construction & Multifamily Only \\\\\n",
  "\\midrule\n",
  "Average FAR & ", formatC(stats_all$avg_density_far, format = "f", digits = 2, big.mark = ","), " & ", formatC(stats_mf$avg_density_far, format = "f", digits = 2, big.mark = ","), " \\\\\n",
  "Average Units & ", formatC(stats_all$avg_units, format = "f", digits = 2, big.mark = ","), " & ", formatC(stats_mf$avg_units, format = "f", digits = 2, big.mark = ","), " \\\\\n",
  "Average Stories & ", formatC(stats_all$avg_stories, format = "f", digits = 2, big.mark = ","), " & ", formatC(stats_mf$avg_stories, format = "f", digits = 2, big.mark = ","), " \\\\\n",
  "Average DUPAC & ", formatC(stats_all$avg_dupac, format = "f", digits = 2, big.mark = ","), " & ", formatC(stats_mf$avg_dupac, format = "f", digits = 2, big.mark = ","), " \\\\\n",
  "Median Dist. to Boundary (ft) & ", formatC(stats_all$median_dist, format = "f", digits = 2, big.mark = ","), " & ", formatC(stats_mf$median_dist, format = "f", digits = 2, big.mark = ","), " \\\\\n",
  "Average Strictness Score & ", formatC(stats_all$avg_strictness, format = "f", digits = 3, big.mark = ","), " & ", formatC(stats_mf$avg_strictness, format = "f", digits = 3, big.mark = ","), " \\\\\n",
  "\\midrule\n",
  "Observations & ", formatC(stats_all$n, format = "d", big.mark = ","), " & ", formatC(stats_mf$n, format = "d", big.mark = ","), " \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)

table_content <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Summary Statistics of New Residential Construction}\n",
  "\\label{tab:summary_stats}\n",
  tabular_content,
  "\\end{table}\n"
)

cat("Writing table to:", table_output_file, "\n")
writeLines(table_content, table_output_file)

cat("Writing tabular to:", tabular_output_file, "\n")
writeLines(tabular_content, tabular_output_file)

cat("Done!\n")
