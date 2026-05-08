source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/event_study_exact_balance/code")
# permit_panel_input <- "../input/permit_block_year_panel_2015.parquet"
# sales_panel_input <- "../input/sales_transaction_panel_2015.parquet"
# block_baseline_input <- "../output/block_parcel_baselines_2014.csv"
# output_csv <- "../output/redistricting_sample_support_summary_250m.csv"
# output_tex <- "../output/redistricting_sample_support_summary_250m.tex"
# bandwidth_m <- 250

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(permit_panel_input, sales_panel_input, block_baseline_input, output_csv, output_tex, bandwidth_m)
}

if (length(args) != 6) {
  stop(
    paste(
      "FATAL: Script requires args:",
      "<permit_panel_input> <sales_panel_input> <block_baseline_input>",
      "<output_csv> <output_tex> <bandwidth_m>"
    ),
    call. = FALSE
  )
}

permit_panel_input <- args[1]
sales_panel_input <- args[2]
block_baseline_input <- args[3]
output_csv <- args[4]
output_tex <- args[5]
bandwidth_m <- as.numeric(args[6])

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be positive.", call. = FALSE)
}

fmt_integer <- function(x) {
  if (!is.finite(x)) {
    return("")
  }
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_decimal <- function(x, digits = 2) {
  if (!is.finite(x)) {
    return("")
  }
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_year_range <- function(years) {
  years <- sort(unique(years[is.finite(years)]))
  if (length(years) == 0) {
    return("")
  }
  if (min(years) == max(years)) {
    return(as.character(min(years)))
  }
  paste0(min(years), "--", max(years))
}

summarize_block_support <- function(data, block_var, pair_var, year_var, observations, design_label, block_baselines) {
  block_support <- data %>%
    distinct(
      block_id = .data[[block_var]],
      ward_pair_id = .data[[pair_var]],
      treat,
      strictness_change
    ) %>%
    filter(!is.na(block_id), block_id != "")

  baseline_sample <- block_support %>%
    filter(treat == 0 | (treat == 1 & strictness_change != 0)) %>%
    distinct(block_id) %>%
    mutate(block_id = as.character(block_id)) %>%
    left_join(block_baselines, by = "block_id")

  tibble(
    design = design_label,
    treated_blocks = n_distinct(block_support$block_id[block_support$treat == 1 & block_support$strictness_change != 0]),
    control_blocks = n_distinct(block_support$block_id[block_support$treat == 0]),
    moved_stricter_blocks = n_distinct(block_support$block_id[block_support$treat == 1 & block_support$strictness_change > 0]),
    moved_lenient_blocks = n_distinct(block_support$block_id[block_support$treat == 1 & block_support$strictness_change < 0]),
    ward_pairs = n_distinct(data[[pair_var]]),
    observations = observations,
    mean_zoned_far = mean(baseline_sample$mean_zoned_far, na.rm = TRUE),
    mean_parcels_per_block = mean(baseline_sample$n_parcels, na.rm = TRUE),
    pre_period_years = fmt_year_range(data[[year_var]][data$relative_year < 0]),
    post_period_years = fmt_year_range(data[[year_var]][data$relative_year >= 0])
  )
}

message("Loading block-level parcel baselines...")
block_baselines <- read_csv(block_baseline_input, show_col_types = FALSE) %>%
  mutate(block_id = as.character(block_id))

message("Loading permit event-study panel...")
permit_data <- read_parquet(permit_panel_input) %>%
  filter(
    !is.na(ward_pair_id), ward_pair_id != "",
    !is.na(block_id), block_id != "",
    !is.na(strictness_change),
    !is.na(n_high_discretion_issue),
    dist_m <= bandwidth_m,
    relative_year >= -5,
    relative_year <= 5
  ) %>%
  mutate(
    weight = 1,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

permit_model <- fepois(
  n_high_discretion_issue ~ post_treat | block_id + ward_pair_id^year,
  data = permit_data,
  weights = ~weight,
  cluster = ~block_id
)

permit_summary <- summarize_block_support(
  data = permit_data,
  block_var = "block_id",
  pair_var = "ward_pair_id",
  year_var = "year",
  observations = nobs(permit_model),
  design_label = "Permit event study",
  block_baselines = block_baselines
)

message("Loading home-sales event-study panel...")
hedonic_vars <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")
amenity_vars <- c("nearest_school_dist_m", "nearest_park_dist_m", "nearest_major_road_dist_m", "lake_michigan_dist_m")

sales_data <- read_parquet(sales_panel_input) %>%
  mutate(ward_pair = sub("_[0-9]+$", "", ward_pair_side)) %>%
  filter(
    dist_m <= bandwidth_m,
    relative_year >= -5,
    relative_year <= 5,
    !is.na(ward_pair), ward_pair != "",
    !is.na(ward_pair_side), ward_pair_side != "",
    !is.na(block_id), block_id != "",
    sale_price > 0
  ) %>%
  mutate(
    weight = 1,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  ) %>%
  filter(if_all(all_of(c(hedonic_vars, amenity_vars)), ~ is.finite(.x)))

sales_model <- feols(
  as.formula(sprintf(
    "log(sale_price) ~ post_treat + %s | ward_pair_side + ward_pair^sale_year",
    paste(c(hedonic_vars, amenity_vars), collapse = " + ")
  )),
  data = sales_data,
  weights = ~weight,
  cluster = ~block_id
)

sales_summary <- summarize_block_support(
  data = sales_data,
  block_var = "block_id",
  pair_var = "ward_pair",
  year_var = "sale_year",
  observations = nobs(sales_model),
  design_label = "Home-sales event study",
  block_baselines = block_baselines
)

support_summary <- bind_rows(permit_summary, sales_summary) %>%
  select(
    design,
    treated_blocks,
    control_blocks,
    moved_stricter_blocks,
    moved_lenient_blocks,
    ward_pairs,
    observations,
    mean_zoned_far,
    mean_parcels_per_block,
    pre_period_years,
    post_period_years
  )

write_csv(support_summary, output_csv)

permit_row <- support_summary %>%
  filter(design == "Permit event study")

sales_row <- support_summary %>%
  filter(design == "Home-sales event study")

table_rows <- tibble(
  label = c(
    "Treated blocks",
    "\\hspace{1em}To stricter ward",
    "\\hspace{1em}To lenient ward",
    "Control blocks",
    "Ward pairs",
    "$N$",
    "Mean allowed FAR",
    "Mean parcels per block"
  ),
  permit = c(
    fmt_integer(permit_row$treated_blocks[[1]]),
    fmt_integer(permit_row$moved_stricter_blocks[[1]]),
    fmt_integer(permit_row$moved_lenient_blocks[[1]]),
    fmt_integer(permit_row$control_blocks[[1]]),
    fmt_integer(permit_row$ward_pairs[[1]]),
    fmt_integer(permit_row$observations[[1]]),
    fmt_decimal(permit_row$mean_zoned_far[[1]], 2),
    fmt_decimal(permit_row$mean_parcels_per_block[[1]], 2)
  ),
  sales = c(
    fmt_integer(sales_row$treated_blocks[[1]]),
    fmt_integer(sales_row$moved_stricter_blocks[[1]]),
    fmt_integer(sales_row$moved_lenient_blocks[[1]]),
    fmt_integer(sales_row$control_blocks[[1]]),
    fmt_integer(sales_row$ward_pairs[[1]]),
    fmt_integer(sales_row$observations[[1]]),
    fmt_decimal(sales_row$mean_zoned_far[[1]], 2),
    fmt_decimal(sales_row$mean_parcels_per_block[[1]], 2)
  )
)

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\small",
  "\\caption{Ward Redistricting Treatment and Control Group Summary Stats}",
  "\\label{tab:redistricting_sample_support}",
  "\\begin{tabular}{lrr}",
  "\\toprule",
  " & Permit Event Study & Home-Sales Event Study \\\\",
  "\\midrule"
)

for (row_i in seq_len(nrow(table_rows))) {
  lines <- c(
    lines,
    sprintf(
      "%s & %s & %s \\\\",
      table_rows$label[[row_i]],
      table_rows$permit[[row_i]],
      table_rows$sales[[row_i]]
    )
  )
}

lines <- c(
  lines,
  "\\bottomrule",
  "\\end{tabular}",
  sprintf("\\par\\vspace{0.5em}\\parbox{0.86\\linewidth}{\\footnotesize \\textit{Notes:} Table summarizes the 2015 permit and home-sales redistricting designs. Both designs use 2010--2014 as the pre-period and 2015--2020 as the post-period. Block counts are unique census blocks in the %dm analysis window. Treated blocks are redistricted blocks with a nonzero change in aldermanic stringency; indented rows split treated blocks by whether they moved to a stricter or more lenient ward. Control blocks remain in the origin ward. $N$ reports the main regression-table sample size after estimator-specific dropping. Mean allowed FAR and mean parcels per block are 2014 block-level parcel averages over treated and control blocks in each design.}", as.integer(round(bandwidth_m))),
  "\\end{table}"
)

writeLines(lines, output_tex)
