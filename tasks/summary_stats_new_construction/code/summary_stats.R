# summary stats of new construction 


## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")


# Load packages
source("../../setup_environment/code/packages.R")

# 1. CLI ARGS
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
    input_file <- args[1]
    output_file <- args[2]
} else {
    # Default for interactive testing
    input_file <- "../input/parcels_with_ward_distances.csv"
    output_file <- "../output/summary_stats.tex"
}

# 2. LOAD DATA
cat("Loading data from:", input_file, "\n")
df <- read_csv(input_file, show_col_types = FALSE)

# 3. FILTER DATA
# Apply basic quality filters consistent with other tasks
df_clean <- df %>%
    filter(
        arealotsf > 1,
        areabuilding > 1,
        unitscount > 0
    )

# Define Multifamily (Units >= 2)
df_mf <- df_clean %>%
    filter(unitscount >= 2)

# 4. CALCULATE STATISTICS

calc_stats <- function(data) {
    list(
        avg_density_far = mean(data$density_far, na.rm = TRUE),
        avg_units = mean(data$unitscount, na.rm = TRUE),
        avg_stories = mean(data$storiescount, na.rm = TRUE),
        avg_dupac = mean(data$density_dupac, na.rm = TRUE),
        median_dist = median(data$dist_to_boundary, na.rm = TRUE),
        avg_strictness = mean(data$strictness_own, na.rm = TRUE),
        n = nrow(data)
    )
}

stats_all <- calc_stats(df_clean)
stats_mf <- calc_stats(df_mf)

# 5. FORMAT TABLE

# Helper to format numbers
fmt <- function(x, digits = 2) {
    formatC(x, format = "f", digits = digits, big.mark = ",")
}

fmt_int <- function(x) {
    formatC(x, format = "d", big.mark = ",")
}

# Create LaTeX content
tex_content <- paste0(
    "\\begin{table}[htbp]\n",
    "\\centering\n",
    "\\caption{Summary Statistics of New Residential Construction}\n",
    "\\label{tab:summary_stats}\n",
    "\\begin{tabular}{lcc}\n",
    "\\toprule\n",
    " & (1) & (2) \\\\\n",
    " & All New Construction & Multifamily Only \\\\\n",
    "\\midrule\n",
    "Average FAR & ", fmt(stats_all$avg_density_far), " & ", fmt(stats_mf$avg_density_far), " \\\\\n",
    "Average Units & ", fmt(stats_all$avg_units), " & ", fmt(stats_mf$avg_units), " \\\\\n",
    "Average Stories & ", fmt(stats_all$avg_stories), " & ", fmt(stats_mf$avg_stories), " \\\\\n",
    "Average DUPAC & ", fmt(stats_all$avg_dupac), " & ", fmt(stats_mf$avg_dupac), " \\\\\n",
    "Median Dist. to Boundary (ft) & ", fmt(stats_all$median_dist), " & ", fmt(stats_mf$median_dist), " \\\\\n",
    "Average Strictness Score & ", fmt(stats_all$avg_strictness, 3), " & ", fmt(stats_mf$avg_strictness, 3), " \\\\\n",
    "\\midrule\n",
    "Observations & ", fmt_int(stats_all$n), " & ", fmt_int(stats_mf$n), " \\\\\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{table}\n"
)

# 6. SAVE OUTPUT
cat("Writing table to:", output_file, "\n")
writeLines(tex_content, output_file)

cat("Done!\n")
