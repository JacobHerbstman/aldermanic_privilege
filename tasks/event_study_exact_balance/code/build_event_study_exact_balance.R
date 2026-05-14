source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")
library(fixest)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/event_study_exact_balance/code")
# permit_panel_input <- "../input/permit_block_year_panel_2015.parquet"
# sales_panel_input <- "../input/sales_transaction_panel_2015.parquet"
# block_baseline_input <- "../output/block_parcel_baselines_2014.csv"
# permit_balance_output <- "../output/permit_exact_balance_summary_300m.csv"
# permit_balance_tex_output <- "../output/permit_exact_balance_summary_300m.tex"
# sales_balance_output <- "../output/sales_exact_balance_summary_300m.csv"
# sales_balance_tex_output <- "../output/sales_exact_balance_summary_300m.tex"
# bandwidth_m <- 300

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    permit_panel_input,
    sales_panel_input,
    block_baseline_input,
    permit_balance_output,
    permit_balance_tex_output,
    sales_balance_output,
    sales_balance_tex_output,
    bandwidth_m
  )
}

if (length(args) != 8) {
  stop(
    paste(
      "FATAL: Script requires args:",
      "<permit_panel_input> <sales_panel_input> <block_baseline_input>",
      "<permit_balance_output> <permit_balance_tex_output>",
      "<sales_balance_output> <sales_balance_tex_output> <bandwidth_m>"
    ),
    call. = FALSE
  )
}

permit_panel_input <- args[1]
sales_panel_input <- args[2]
block_baseline_input <- args[3]
permit_balance_output <- args[4]
permit_balance_tex_output <- args[5]
sales_balance_output <- args[6]
sales_balance_tex_output <- args[7]
bandwidth_m <- as.numeric(args[8])

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be positive.", call. = FALSE)
}

distance_display <- distance_display_config()
bandwidth_label <- Sys.getenv("BANDWIDTH_LABEL", format_distance_label(bandwidth_m, distance_display))

fmt_decimal <- function(x, digits = 2) {
  if (!is.finite(x)) {
    return("")
  }
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_integer <- function(x) {
  if (!is.finite(x)) {
    return("")
  }
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_covariate_value <- function(covariate_name, x) {
  if (!is.finite(x)) {
    return("")
  }
  if (covariate_name == "mean_zoned_far") {
    return(fmt_decimal(x, 2))
  }
  if (grepl("_dist_m$", covariate_name)) {
    x <- x * distance_display$scale
  }
  fmt_integer(x)
}

covariate_catalog <- tibble(
  covariate = c(
    "mean_zoned_far",
    "mean_dist_cbd_m",
    "mean_nearest_school_dist_m",
    "mean_nearest_park_dist_m",
    "mean_nearest_major_road_dist_m",
    "mean_lake_michigan_dist_m"
  ),
  covariate_label = c(
    "Average Zoned FAR",
    sprintf("Distance to CBD (%s)", distance_display$unit),
    sprintf("Distance to School (%s)", distance_display$unit),
    sprintf("Distance to Park (%s)", distance_display$unit),
    sprintf("Distance to Major Road (%s)", distance_display$unit),
    sprintf("Distance to Lake Michigan (%s)", distance_display$unit)
  )
)

build_balance_table <- function(sample_df, pair_var, output_csv, output_tex, caption_text, label_text, notes_text) {
  balance_rows <- lapply(covariate_catalog$covariate, function(covariate_i) {
    row_df <- sample_df %>%
      filter(!is.na(.data[[covariate_i]])) %>%
      mutate(outcome = .data[[covariate_i]])

    if (nrow(row_df) == 0) {
      return(NULL)
    }

    model_i <- feols(
      as.formula(sprintf("outcome ~ treated_any | %s", pair_var)),
      data = row_df,
      cluster = as.formula(paste0("~", pair_var))
    )
    coef_i <- coeftable(model_i)["treated_any", ]

    tibble(
      covariate = covariate_i,
      control_mean = mean(row_df$outcome[row_df$treated_any == 0], na.rm = TRUE),
      treated_mean = mean(row_df$outcome[row_df$treated_any == 1], na.rm = TRUE),
      within_pair_diff = unname(coef_i["Estimate"]),
      within_pair_diff_se = unname(coef_i["Std. Error"]),
      within_pair_diff_p_value = unname(coef_i["Pr(>|t|)"]),
      n_blocks = nrow(row_df),
      n_pairs = n_distinct(row_df[[pair_var]])
    )
  })

  balance_summary <- bind_rows(balance_rows) %>%
    left_join(covariate_catalog, by = "covariate") %>%
    select(
      covariate,
      covariate_label,
      control_mean,
      treated_mean,
      within_pair_diff,
      within_pair_diff_se,
      within_pair_diff_p_value,
      n_blocks,
      n_pairs
    )

  write_csv(balance_summary, output_csv)

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    sprintf("\\caption{%s}", caption_text),
    sprintf("\\label{%s}", label_text),
    "\\begin{tabular}{lrrrrr}",
    "\\toprule",
    "Variable & Control & Treated & Resid. Diff & SE & $p$-value \\\\",
    "\\midrule"
  )

  for (covariate_i in covariate_catalog$covariate) {
    row_i <- balance_summary %>%
      filter(covariate == covariate_i)

    if (nrow(row_i) == 0) {
      next
    }

    lines <- c(
      lines,
      sprintf(
        "%s & %s & %s & %s & %s & %s \\\\",
        row_i$covariate_label[[1]],
        fmt_covariate_value(covariate_i, row_i$control_mean[[1]]),
        fmt_covariate_value(covariate_i, row_i$treated_mean[[1]]),
        fmt_covariate_value(covariate_i, row_i$within_pair_diff[[1]]),
        fmt_covariate_value(covariate_i, row_i$within_pair_diff_se[[1]]),
        fmt_decimal(row_i$within_pair_diff_p_value[[1]], 3)
      )
    )
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    paste0("\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize ", notes_text, "}"),
    "\\end{table}"
  )

  writeLines(lines, output_tex)
}

message("Loading block-level parcel baselines...")
block_baselines <- read_csv(block_baseline_input, show_col_types = FALSE) %>%
  mutate(block_id = as.character(block_id))

message("Loading permit event-study panel...")
permit_panel <- read_parquet(permit_panel_input) %>%
  as_tibble() %>%
  filter(dist_m <= bandwidth_m, relative_year == -1, !is.na(strictness_change)) %>%
  mutate(
    group = case_when(
      treat == 0 ~ "control",
      strictness_change > 0 ~ "to_stricter",
      strictness_change < 0 ~ "to_lenient",
      TRUE ~ "treated_zero_change"
    ),
    treated_any = as.integer(group != "control")
  ) %>%
  filter(group != "treated_zero_change") %>%
  distinct(block_id, ward_pair_id, group, treated_any) %>%
  mutate(block_id = as.character(block_id)) %>%
  left_join(block_baselines, by = "block_id")

message("Loading sales event-study panel...")
sales_panel <- read_parquet(sales_panel_input) %>%
  as_tibble() %>%
  filter(dist_m <= bandwidth_m, relative_year == -1, !is.na(strictness_change)) %>%
  mutate(
    ward_pair_id = sub("_[0-9]+$", "", ward_pair_side),
    group = case_when(
      treat == 0 ~ "control",
      strictness_change > 0 ~ "to_stricter",
      strictness_change < 0 ~ "to_lenient",
      TRUE ~ "treated_zero_change"
    ),
    treated_any = as.integer(group != "control")
  ) %>%
  filter(group != "treated_zero_change") %>%
  distinct(block_id, ward_pair_id, group, treated_any) %>%
  mutate(block_id = as.character(block_id)) %>%
  left_join(block_baselines, by = "block_id")

build_balance_table(
  sample_df = permit_panel,
  pair_var = "ward_pair_id",
  output_csv = permit_balance_output,
  output_tex = permit_balance_tex_output,
  caption_text = "Permit Event-Study Balance: Exact Parcel Amenities and Zoned FAR",
  label_text = "tab:permit_exact_balance",
  notes_text = paste(
    sprintf("2015 cohort only. Sample uses census blocks within %s of a ward boundary in the permit event-study design.", bandwidth_label),
    "Block covariates are computed by averaging exact parcel-level amenities and zoned FAR across parcels in the 2010 census block,",
    "using parcels observed by 2014. Treated blocks are all blocks that switch wards at redistricting; control blocks remain in the origin ward.",
    "Residualized difference equals treated minus control after demeaning each covariate within ward pair.",
    "Standard errors and $p$-values come from block-level regressions with ward-pair fixed effects and standard errors clustered by ward pair."
  )
)

build_balance_table(
  sample_df = sales_panel,
  pair_var = "ward_pair_id",
  output_csv = sales_balance_output,
  output_tex = sales_balance_tex_output,
  caption_text = "Home-Sales Event-Study Balance: Exact Parcel Amenities and Zoned FAR",
  label_text = "tab:sales_exact_balance",
  notes_text = paste(
    sprintf("2015 cohort only. Sample uses census blocks within %s of a ward boundary with at least one observed sale in relative year $-1$.", bandwidth_label),
    "Block covariates are computed by averaging exact parcel-level amenities and zoned FAR across parcels in the 2010 census block,",
    "using parcels observed by 2014. Treated blocks are all blocks that switch wards at redistricting; control blocks remain in the origin ward.",
    "Residualized difference equals treated minus control after demeaning each covariate within ward pair.",
    "Standard errors and $p$-values come from block-level regressions with ward-pair fixed effects and standard errors clustered by ward pair."
  )
)
