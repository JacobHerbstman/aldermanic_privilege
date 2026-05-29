# --- Interactive Test Block ---
# setwd("tasks/audits/run_event_study_sales_disaggregate/code")
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"
# panel_mode <- "cohort_2015"

source("../../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth, bandwidth_label, panel_mode)
}

if (length(args) != 3) {
  stop("FATAL: Script requires args: <bandwidth> <bandwidth_label> <panel_mode>", call. = FALSE)
}

bandwidth <- as.numeric(args[1])
bandwidth_label <- args[2]
panel_mode <- args[3]

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!panel_mode %in% c("cohort_2015", "cohort_2023", "stacked_implementation")) {
  stop("panel_mode must be one of: cohort_2015, cohort_2023, stacked_implementation", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}

weighting <- "uniform"
panel_label <- switch(
  panel_mode,
  "cohort_2015" = "2015",
  "cohort_2023" = "2023",
  "stacked_implementation" = "stacked_implementation"
)
output_tex <- sprintf("../output/did_table_sales_%s_%s_%s_geo_wardpair_clust_block.tex", panel_label, weighting, bandwidth_label)

hedonic_vars <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")
amenity_vars <- c("nearest_school_dist_m", "nearest_park_dist_m", "nearest_major_road_dist_m", "lake_michigan_dist_m")

panel_input <- switch(
  panel_mode,
  "cohort_2015" = "../input/sales_transaction_panel_2015.parquet",
  "cohort_2023" = "../input/sales_transaction_panel_2023.parquet",
  "stacked_implementation" = "../input/sales_transaction_panel.parquet"
)

stacked_mode <- panel_mode == "stacked_implementation"
side_var <- if (stacked_mode) "cohort_ward_pair_side" else "ward_pair_side"
pair_var <- if (stacked_mode) "cohort_ward_pair" else "ward_pair"
block_var <- if (stacked_mode) "cohort_block_id" else "block_id"

data_base <- read_parquet(panel_input) %>%
  mutate(ward_pair = sub("_[0-9]+$", "", ward_pair_side)) %>%
  mutate(
    cohort_ward_pair_side = if ("cohort_ward_pair_side" %in% names(.)) cohort_ward_pair_side else NA_character_,
    cohort_ward_pair = if (stacked_mode) {
      paste(cohort, sub("_[0-9]+$", "", sub("^[0-9]+_", "", cohort_ward_pair_side)), sep = "_")
    } else {
      NA_character_
    }
  ) %>%
  filter(
    dist_m <= bandwidth,
    relative_year >= -5, relative_year <= 5,
    !is.na(.data[[pair_var]]), .data[[pair_var]] != "",
    !is.na(.data[[side_var]]), .data[[side_var]] != "",
    !is.na(.data[[block_var]]), .data[[block_var]] != "",
    sale_price > 0
  )

score_gate_missing_change_n <- sum(is.na(data_base$strictness_change))
if (score_gate_missing_change_n > 0L) {
  stop("Requested sales DID regression sample has missing score values.", call. = FALSE)
}

data_base <- data_base %>%
  mutate(
    weight = 1,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

data <- data_base %>%
  filter(if_all(all_of(hedonic_vars), ~ is.finite(.x)))

data_amenity <- data %>%
  filter(if_all(all_of(amenity_vars), ~ is.finite(.x)))

fe_formula <- sprintf("%s + %s^sale_year", side_var, pair_var)
cluster_formula <- as.formula(paste0("~", block_var))
hedonic_formula <- paste(hedonic_vars, collapse = " + ")
amenity_formula <- paste(c(hedonic_vars, amenity_vars), collapse = " + ")

m_no_ctrl <- feols(
  as.formula(sprintf("log(sale_price) ~ post_treat | %s", fe_formula)),
  data = data,
  weights = ~weight,
  cluster = cluster_formula
)

m_ctrl <- feols(
  as.formula(sprintf("log(sale_price) ~ post_treat + %s | %s", hedonic_formula, fe_formula)),
  data = data,
  weights = ~weight,
  cluster = cluster_formula
)

dep_var_mean_no_ctrl <- mean(data$sale_price, na.rm = TRUE)
dep_var_mean_ctrl <- dep_var_mean_no_ctrl
ward_pairs_no_ctrl <- n_distinct(data[[pair_var]])
ward_pairs_ctrl <- ward_pairs_no_ctrl

m_ctrl_amenity <- feols(
  as.formula(sprintf("log(sale_price) ~ post_treat + %s | %s", amenity_formula, fe_formula)),
  data = data_amenity,
  weights = ~weight,
  cluster = cluster_formula
)
dep_var_mean_ctrl_amenity <- mean(data_amenity$sale_price, na.rm = TRUE)
ward_pairs_ctrl_amenity <- n_distinct(data_amenity[[pair_var]])

setFixest_dict(c(
  post_treat = "Post $\\times$ Stringency $\\Delta$",
  log_sqft = "Log Building Sqft",
  log_land_sqft = "Log Land Sqft",
  log_building_age = "Log Building Age",
  log_bedrooms = "Log Bedrooms",
  log_baths = "Log Bathrooms",
  has_garage = "Has Garage",
  nearest_school_dist_m = "Dist. to School (m)",
  nearest_park_dist_m = "Dist. to Park (m)",
  nearest_major_road_dist_m = "Dist. to Major Road (m)",
  lake_michigan_dist_m = "Dist. to Lake Michigan (m)"
))

etable(
  list(m_no_ctrl, m_ctrl, m_ctrl_amenity),
  fitstat = NULL,
  style.tex = style.tex("aer"),
  depvar = FALSE,
  digits = 3,
  digits.stats = 2,
  drop = "Intercept",
  keep = "%post_treat",
  drop.section = "fixef",
  extralines = list(
    "_N" = c(format(nobs(m_no_ctrl), big.mark = ","), format(nobs(m_ctrl), big.mark = ","), format(nobs(m_ctrl_amenity), big.mark = ",")),
    "_Dep. Var. Mean" = c(
      paste0("\\$", format(round(dep_var_mean_no_ctrl, 0), big.mark = ",")),
      paste0("\\$", format(round(dep_var_mean_ctrl, 0), big.mark = ",")),
      paste0("\\$", format(round(dep_var_mean_ctrl_amenity, 0), big.mark = ","))
    ),
    "_Ward Pairs" = c(format(ward_pairs_no_ctrl, big.mark = ","), format(ward_pairs_ctrl, big.mark = ","), format(ward_pairs_ctrl_amenity, big.mark = ",")),
    "_Hedonic Controls" = c("", "$\\checkmark$", "$\\checkmark$"),
    "_Amenity Controls" = c("", "", "$\\checkmark$"),
    "_Border-Pair Side FE" = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
    "_Border-Pair $\\times$ Year FE" = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  float = FALSE,
  file = output_tex,
  replace = TRUE
)

table_tex <- readLines(output_tex)
drop_patterns <- c(
  "^\\s*Observations\\s*&",
  "^\\s*R\\$\\^2\\$\\s*&",
  "^\\s*Within R\\$\\^2\\$\\s*&"
)
table_tex <- table_tex[!vapply(
  table_tex,
  function(line) any(vapply(drop_patterns, grepl, logical(1), x = line)),
  logical(1)
)]
centering_idx <- grep("\\\\centering", table_tex)[1]
if (length(centering_idx) == 1 && !is.na(centering_idx)) {
  table_tex <- append(
    table_tex,
    c(
      "   \\small",
      "   \\setlength{\\tabcolsep}{4pt}",
      "   \\renewcommand{\\arraystretch}{0.97}"
    ),
    after = centering_idx
  )
}
notes_idx <- match("   \\par \\raggedright ", table_tex)
if (!is.na(notes_idx)) {
  table_tex[notes_idx] <- "   \\par \\raggedright \\footnotesize "
}
writeLines(table_tex, output_tex)
message(sprintf("Saved %s", output_tex))
