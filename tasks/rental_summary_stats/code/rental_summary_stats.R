# rental_summary_stats.R
# Creates summary statistics table for rental listing data by year.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_summary_stats/code")
source("../../setup_environment/code/packages.R")

# 1. Load Data
message("Loading rent panel...")
rent_panel <- read_parquet("../input/chicago_rent_panel.parquet")

# 2. Data Prep
# Ensure year is available and filter out any bad dates if necessary
df <- rent_panel %>%
  mutate(year = year(file_date)) %>%
  filter(!is.na(year)) %>% 
  # Optional: Filter strict outliers if "rent_price" has crazy values (e.g. $1 or $1M)
  filter(rent_price > 100 & rent_price < 50000)

# 3. Calculate Summary Stats
summary_stats <- df %>%
  group_by(year) %>%
  summarise(
    N = n(),
    Mean_Rent = mean(rent_price, na.rm = TRUE),
    Median_Rent = median(rent_price, na.rm = TRUE),
    Mean_Sqft = mean(sqft, na.rm = TRUE),
    # Check data quality: how many listings are missing square footage?
    Missing_Sqft_Pct = mean(is.na(sqft)), 
    # Check composition: are we seeing more "luxury" over time?
    Share_Doorman = mean(doorman, na.rm = TRUE),
    Share_Gym = mean(gym, na.rm = TRUE)
  ) %>%
  arrange(year)

# 4. Save CSV (for inspection)
write_csv(summary_stats, "../output/rental_summary_stats.csv")
message("Saved CSV summary.")

# 5. Save TeX Table (for the paper)
# Helper to format numbers nicely
fmt_num <- function(x, d=0) formatC(x, format="f", digits=d, big.mark=",")
fmt_pct <- function(x) paste0(formatC(x * 100, format="f", digits=1), "\\%")

tex_body <- summary_stats %>%
  mutate(
    Year = as.character(year),
    N = formatC(N, format="d", big.mark=","),
    Mean_Rent = fmt_num(Mean_Rent, 0),
    Median_Rent = fmt_num(Median_Rent, 0),
    Mean_Sqft = fmt_num(Mean_Sqft, 0),
    Missing_Sqft = fmt_pct(Missing_Sqft_Pct),
    Share_Doorman = fmt_pct(Share_Doorman),
    Share_Gym = fmt_pct(Share_Gym)
  ) %>%
  select(Year, N, Mean_Rent, Median_Rent, Mean_Sqft, Missing_Sqft, Share_Doorman, Share_Gym)

# Create LaTeX string
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Summary Statistics of Rental Listings by Year}",
  "\\label{tab:rental_summary_stats}",
  "\\resizebox{\\textwidth}{!}{",
  "\\begin{tabular}{lccccccc}",
  "\\toprule",
  "Year & N & Mean Rent & Median Rent & Mean Sqft & Missing Sqft (\\%) & Doorman (\\%) & Gym (\\%) \\\\",
  "\\midrule"
)

# Add rows
row_strings <- apply(tex_body, 1, function(x) paste(x, collapse = " & "))
tex_lines <- c(tex_lines, paste(row_strings, "\\\\"))

# Close table
tex_lines <- c(
  tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  "\\end{table}"
)

writeLines(tex_lines, "../output/rental_summary_stats.tex")
message("Saved TeX summary.")