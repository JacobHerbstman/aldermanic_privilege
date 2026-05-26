# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_disaggregate/code")
# bandwidth <- 304.8
# weighting <- "uniform"
# bandwidth_label <- "1000ft"
# table_mode <- "amenity"
# panel_mode <- "cohort_2015"

source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth, weighting, bandwidth_label, table_mode, panel_mode)
}

if (length(args) != 5) {
  stop("FATAL: Script requires args: <bandwidth> <weighting> <bandwidth_label> <table_mode> <panel_mode>", call. = FALSE)
}

bandwidth <- as.numeric(args[1])
weighting <- args[2]
bandwidth_label <- args[3]
table_mode <- args[4]
panel_mode <- args[5]

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!weighting %in% c("uniform", "triangular")) {
  stop("weighting must be one of: uniform, triangular", call. = FALSE)
}
if (!table_mode %in% c("baseline", "amenity")) {
  stop("table_mode must be one of: baseline, amenity", call. = FALSE)
}
if (!panel_mode %in% c("cohort_2015", "cohort_2023", "stacked_implementation")) {
  stop("panel_mode must be one of: cohort_2015, cohort_2023, stacked_implementation", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}

panel_label <- switch(
  panel_mode,
  "cohort_2015" = "2015",
  "cohort_2023" = "2023",
  "stacked_implementation" = "stacked_implementation"
)
output_tex <- sprintf("../output/did_table_sales_%s_%s_%s_geo_wardpair_clust_block.tex", panel_label, weighting, bandwidth_label)

min_segment_length_raw <- Sys.getenv("MIN_SEGMENT_LENGTH_FT", "")
min_segment_length_ft <- if (nzchar(min_segment_length_raw)) suppressWarnings(as.numeric(min_segment_length_raw)) else NA_real_
if (!is.na(min_segment_length_ft) && (!is.finite(min_segment_length_ft) || min_segment_length_ft < 0)) {
  stop("MIN_SEGMENT_LENGTH_FT must be a nonnegative number when supplied.", call. = FALSE)
}

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
    weight = if (weighting == "triangular") pmax(0, 1 - dist_m / bandwidth) else 1,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

segment_length_input_n <- NA_integer_
segment_length_drop_n <- NA_integer_
segment_length_missing_n <- NA_integer_
segment_length_short_n <- NA_integer_
if (is.finite(min_segment_length_ft)) {
  missing_segment_cols <- setdiff(c("segment_id_cohort", "segment_length_ft_cohort"), names(data_base))
  if (length(missing_segment_cols) > 0) {
    stop(sprintf(
      "MIN_SEGMENT_LENGTH_FT requires missing panel columns: %s",
      paste(missing_segment_cols, collapse = ", ")
    ), call. = FALSE)
  }
  segment_length_input_n <- nrow(data_base)
  segment_length_missing_n <- sum(is.na(data_base$segment_length_ft_cohort))
  segment_length_short_n <- sum(!is.na(data_base$segment_length_ft_cohort) & data_base$segment_length_ft_cohort < min_segment_length_ft)
  data_base <- data_base %>%
    filter(!is.na(segment_id_cohort), segment_id_cohort != "") %>%
    filter(!is.na(segment_length_ft_cohort), segment_length_ft_cohort >= min_segment_length_ft)
  segment_length_drop_n <- segment_length_input_n - nrow(data_base)
}

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

effect_no_ctrl <- 100 * (exp(coef(m_no_ctrl)[["post_treat"]]) - 1)
effect_ctrl <- 100 * (exp(coef(m_ctrl)[["post_treat"]]) - 1)
dep_var_mean_no_ctrl <- mean(data$sale_price, na.rm = TRUE)
dep_var_mean_ctrl <- dep_var_mean_no_ctrl
ward_pairs_no_ctrl <- n_distinct(data[[pair_var]])
ward_pairs_ctrl <- ward_pairs_no_ctrl

if (table_mode == "amenity") {
  m_ctrl_amenity <- feols(
    as.formula(sprintf("log(sale_price) ~ post_treat + %s | %s", amenity_formula, fe_formula)),
    data = data_amenity,
    weights = ~weight,
    cluster = cluster_formula
  )
  effect_ctrl_amenity <- 100 * (exp(coef(m_ctrl_amenity)[["post_treat"]]) - 1)
  dep_var_mean_ctrl_amenity <- mean(data_amenity$sale_price, na.rm = TRUE)
  ward_pairs_ctrl_amenity <- n_distinct(data_amenity[[pair_var]])
} else {
  m_ctrl_amenity <- NULL
  effect_ctrl_amenity <- NA_real_
  dep_var_mean_ctrl_amenity <- NA_real_
  ward_pairs_ctrl_amenity <- NA_integer_
}

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
  if (table_mode == "amenity") list(m_no_ctrl, m_ctrl, m_ctrl_amenity) else list(m_no_ctrl, m_ctrl),
  fitstat = NULL,
  style.tex = style.tex("aer"),
  depvar = FALSE,
  digits = 3,
  digits.stats = 2,
  drop = "Intercept",
  keep = "%post_treat",
  drop.section = "fixef",
  extralines = if (table_mode == "amenity") {
    list(
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
    )
  } else {
    list(
      "_N" = c(format(nobs(m_no_ctrl), big.mark = ","), format(nobs(m_ctrl), big.mark = ",")),
      "_Dep. Var. Mean" = c(
        paste0("\\$", format(round(dep_var_mean_no_ctrl, 0), big.mark = ",")),
        paste0("\\$", format(round(dep_var_mean_ctrl, 0), big.mark = ","))
      ),
      "_Ward Pairs" = c(format(ward_pairs_no_ctrl, big.mark = ","), format(ward_pairs_ctrl, big.mark = ",")),
      "_Hedonic Controls" = c("", "$\\checkmark$"),
      "_Border-Pair Side FE" = c("$\\checkmark$", "$\\checkmark$"),
      "_Border-Pair $\\times$ Year FE" = c("$\\checkmark$", "$\\checkmark$")
    )
  },
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

if (table_mode == "amenity") {
  message(sprintf(
    "Sales DID amenity | panel=%s | min_segment_ft=%s | no controls = %.4f (%.2f%%) | with controls = %.4f (%.2f%%) | with amenities = %.4f (%.2f%%) | N hedonic = %s | N amenity = %s | segment_rows_dropped = %s",
    panel_mode,
    if (is.finite(min_segment_length_ft)) sprintf("%.1f", min_segment_length_ft) else "none",
    coef(m_no_ctrl)[["post_treat"]],
    effect_no_ctrl,
    coef(m_ctrl)[["post_treat"]],
    effect_ctrl,
    coef(m_ctrl_amenity)[["post_treat"]],
    effect_ctrl_amenity,
    format(nobs(m_ctrl), big.mark = ","),
    format(nobs(m_ctrl_amenity), big.mark = ","),
    if (is.finite(min_segment_length_ft)) format(segment_length_drop_n, big.mark = ",") else "0"
  ))
} else {
  message(sprintf(
    "Sales DID baseline | panel=%s | min_segment_ft=%s | no controls = %.4f (%.2f%%) | with controls = %.4f (%.2f%%) | N = %s | segment_rows_dropped = %s",
    panel_mode,
    if (is.finite(min_segment_length_ft)) sprintf("%.1f", min_segment_length_ft) else "none",
    coef(m_no_ctrl)[["post_treat"]],
    effect_no_ctrl,
    coef(m_ctrl)[["post_treat"]],
    effect_ctrl,
    format(nobs(m_ctrl), big.mark = ","),
    if (is.finite(min_segment_length_ft)) format(segment_length_drop_n, big.mark = ",") else "0"
  ))
}
