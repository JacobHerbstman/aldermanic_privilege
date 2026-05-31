# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_density_robustness/code")
# bandwidth_ft <- 500
# start_construction_year <- 2006
# end_construction_year <- 2022

source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft, start_construction_year, end_construction_year)
}
if (length(cli_args) != 3) {
  stop("FATAL: Script requires 3 args: <bandwidth_ft> <start_construction_year> <end_construction_year>.", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
start_construction_year <- suppressWarnings(as.integer(cli_args[2]))
end_construction_year <- suppressWarnings(as.integer(cli_args[3]))
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
if (
  !is.finite(start_construction_year) ||
    !is.finite(end_construction_year) ||
    start_construction_year > end_construction_year
) {
  stop("start_construction_year and end_construction_year must be valid integer years with start <= end.", call. = FALSE)
}
bandwidth_m <- bandwidth_ft * 0.3048
bandwidth_label <- paste0(formatC(bandwidth_ft, format = "f", digits = 0), "ft")

parcels <- read_csv(
  "../temp/parcels_with_ward_distances_low_discretion_residualized.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= start_construction_year,
    construction_year <= end_construction_year,
    unitscount > 0,
    dist_to_boundary_m <= bandwidth_m,
    !is.na(segment_id),
    segment_id != ""
  )

outcome_labels <- c(
  density_far = "ln(FAR)",
  density_dupac = "ln(DUPAC)"
)
controls <- c(
  "strictness_own",
  "lenient_dist",
  "strict_dist",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

models <- list()
for (base_var in names(outcome_labels)) {
  if (!base_var %in% names(parcels)) {
    stop(sprintf("Outcome variable '%s' not found.", base_var), call. = FALSE)
  }

  model_data <- parcels %>%
    filter(is.finite(.data[[base_var]]), .data[[base_var]] > 0)

  if (nrow(model_data) == 0) {
    stop(sprintf("No rows remain for '%s' after filtering.", base_var), call. = FALSE)
  }
  if (length(unique(model_data[[base_var]])) <= 1) {
    stop(sprintf("Outcome '%s' is constant after filtering.", base_var), call. = FALSE)
  }

  model <- feols(
    as.formula(paste0(
      "log(", base_var, ") ~ ",
      paste(controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = model_data,
    cluster = ~ward_pair
  )
  if (!"strictness_own" %in% names(coef(model))) {
    stop(sprintf("Model failed to estimate strictness_own for '%s'.", base_var), call. = FALSE)
  }
  model$custom_data <- model_data
  models[[outcome_labels[[base_var]]]] <- model
}

if (length(models) != length(outcome_labels)) {
  stop("Expected one model per density outcome.", call. = FALSE)
}

fe_rows <- list(
  "_Zoning Group FE" = rep("$\\checkmark$", length(models)),
  "_Segment FE" = rep("$\\checkmark$", length(models)),
  "_Year FE" = rep("$\\checkmark$", length(models))
)
fe_rows[["_N"]] <- c(
  format(nobs(models[["ln(FAR)"]]), big.mark = ","),
  format(nobs(models[["ln(DUPAC)"]]), big.mark = ",")
)
fe_rows[["_Dep. Var. Mean"]] <- c(
  sprintf("%.2f", mean(models[["ln(FAR)"]]$custom_data$density_far, na.rm = TRUE)),
  sprintf("%.2f", mean(models[["ln(DUPAC)"]]$custom_data$density_dupac, na.rm = TRUE))
)
fe_rows[["_Ward Pairs"]] <- c(
  format(n_distinct(models[["ln(FAR)"]]$custom_data$ward_pair), big.mark = ","),
  format(n_distinct(models[["ln(DUPAC)"]]$custom_data$ward_pair), big.mark = ",")
)

table_tmp <- tempfile(fileext = ".tex")
etable(
  models,
  keep = "Stringency Index",
  fitstat = NULL,
  style.tex = style.tex(
    "aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  ),
  depvar = FALSE,
  digits = 2,
  headers = names(models),
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  dict = c("strictness_own" = "Stringency Index"),
  drop.section = "fixef",
  extralines = fe_rows,
  float = FALSE,
  tex = TRUE,
  file = table_tmp,
  replace = TRUE
)

table_tex <- readLines(table_tmp, warn = FALSE)
unlink(table_tmp)
table_tex <- table_tex[!grepl("^\\s*(Observations|R\\$\\^2\\$|Within R\\$\\^2\\$)\\s*&", table_tex)]

writeLines(
  table_tex,
  sprintf(
    "../output/fe_table_%s_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex",
    bandwidth_label
  )
)
