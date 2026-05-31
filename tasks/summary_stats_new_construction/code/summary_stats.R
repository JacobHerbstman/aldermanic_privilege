# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/summary_stats_new_construction/code")
# start_construction_year <- 2006
# end_construction_year <- 2022

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_construction_year, end_construction_year)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <start_construction_year> <end_construction_year>.", call. = FALSE)
}

start_construction_year <- suppressWarnings(as.integer(cli_args[1]))
end_construction_year <- suppressWarnings(as.integer(cli_args[2]))
if (
  !is.finite(start_construction_year) ||
    !is.finite(end_construction_year) ||
    start_construction_year > end_construction_year
) {
  stop("start_construction_year and end_construction_year must be valid integer years with start <= end.", call. = FALSE)
}

df <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE)
df <- ensure_meter_distance_columns(df)

df_clean <- df %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 0,
    construction_year >= start_construction_year,
    construction_year <= end_construction_year
  )

df_mf <- df_clean %>%
  filter(unitscount >= 2)

stats_all <- tibble(
  avg_density_far = mean(df_clean$density_far, na.rm = TRUE),
  avg_units = mean(df_clean$unitscount, na.rm = TRUE),
  avg_stories = mean(df_clean$storiescount, na.rm = TRUE),
  avg_dupac = mean(df_clean$density_dupac, na.rm = TRUE),
  median_dist_ft = median(df_clean$dist_to_boundary_m * M_TO_FT, na.rm = TRUE),
  avg_strictness = mean(df_clean$strictness_own, na.rm = TRUE),
  n = nrow(df_clean)
)

stats_mf <- tibble(
  avg_density_far = mean(df_mf$density_far, na.rm = TRUE),
  avg_units = mean(df_mf$unitscount, na.rm = TRUE),
  avg_stories = mean(df_mf$storiescount, na.rm = TRUE),
  avg_dupac = mean(df_mf$density_dupac, na.rm = TRUE),
  median_dist_ft = median(df_mf$dist_to_boundary_m * M_TO_FT, na.rm = TRUE),
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
  "Median Dist. to Boundary (ft) & ", formatC(stats_all$median_dist_ft, format = "f", digits = 2, big.mark = ","), " & ", formatC(stats_mf$median_dist_ft, format = "f", digits = 2, big.mark = ","), " \\\\\n",
  "Average Strictness Score & ", formatC(stats_all$avg_strictness, format = "f", digits = 3, big.mark = ","), " & ", formatC(stats_mf$avg_strictness, format = "f", digits = 3, big.mark = ","), " \\\\\n",
  "\\midrule\n",
  "N & ", formatC(stats_all$n, format = "d", big.mark = ","), " & ", formatC(stats_mf$n, format = "d", big.mark = ","), " \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)

table_content <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Summary Statistics of New Residential Construction}\n",
  "\\label{tab:summary_stats}\n",
  tabular_content,
  sprintf(
    "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: Sample includes new residential construction from %d--%d with positive lot area, positive building area, and at least one dwelling unit. Multifamily is defined as two or more units.}\n",
    start_construction_year,
    end_construction_year
  ),
  "\\end{table}\n"
)

writeLines(table_content, "../output/summary_stats.tex")
