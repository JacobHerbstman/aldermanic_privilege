source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# Rscript rent_amenity_attenuation.R ../output/rent_with_ward_distances_amenities.parquet 500 pre_2023 ../output/rent_amenity_attenuation_bw500_pre_2023.tex ../output/rent_amenity_attenuation_bw500_pre_2023.csv
# =======================================================================================

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 6) {
  input <- cli_args[1]
  bw_arg <- cli_args[2]
  window <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  cluster_level <- tolower(cli_args[6])
} else if (length(cli_args) >= 5) {
  input <- cli_args[1]
  bw_arg <- cli_args[2]
  window <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
} else {
  if (!exists("input") || !exists("bw_arg") || !exists("window") || !exists("output_tex") || !exists("output_csv") || !exists("cluster_level")) {
    stop("FATAL: Script requires 5 args: <input> <bw_ft> <window> <output_tex> <output_csv> [<cluster_level>]", call. = FALSE)
  }
}

parse_bw_ft <- function(x) {
  if (length(x) != 1) {
    stop("bw_ft must be a single value.", call. = FALSE)
  }
  if (is.character(x) && str_to_lower(x) %in% c("all", "full", "none", "inf", "infinity")) {
    return(Inf)
  }
  out <- suppressWarnings(as.numeric(x))
  if (!is.finite(out) || out <= 0) {
    stop("bw_ft must be a positive number or one of: all, full, none, inf.", call. = FALSE)
  }
  out
}

window_rule <- function(df, window_name) {
  if (window_name == "full") return(df)
  if (window_name == "pre_covid") return(df %>% filter(year <= 2019))
  if (window_name == "pre_2021") return(df %>% filter(year <= 2020))
  if (window_name == "pre_2023") return(df %>% filter(year <= 2022))
  if (window_name == "drop_mid") return(df %>% filter(year <= 2020 | year >= 2024))
  df
}

bw_ft <- parse_bw_ft(bw_arg)
bw_label <- if (is.finite(bw_ft)) as.character(as.integer(round(bw_ft))) else "all"

if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("cluster_level must be one of: segment, ward_pair", call. = FALSE)
}

message(sprintf("=== Rent Amenity Attenuation | bw=%s | window=%s | cluster=%s ===", bw_label, window, cluster_level))

rent_raw <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id)
  ) %>%
  filter(
    !is.na(file_date),
    !is.na(rent_price),
    rent_price > 0,
    !is.na(ward_pair),
    !is.na(signed_dist),
    !is.na(strictness_own),
    !is.na(segment_id),
    segment_id != ""
  )

if (is.finite(bw_ft)) {
  rent_raw <- rent_raw %>% filter(abs(signed_dist) <= bw_ft)
}

rent <- window_rule(rent_raw, window)

strictness_sd <- sd(rent$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero/invalid SD in this sample.", call. = FALSE)
}

rent <- rent %>%
  mutate(
    strictness_std = strictness_own / strictness_sd,
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  )

rent_hedonics <- rent %>%
  filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths), !is.na(building_type_factor))

rent_amenities <- rent_hedonics %>%
  filter(
    !is.na(nearest_school_dist_ft),
    !is.na(nearest_park_dist_ft),
    !is.na(nearest_major_road_dist_ft),
    !is.na(lake_michigan_dist_ft)
  )

cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair
cluster_label <- if (cluster_level == "segment") "Segment" else "Ward Pair"

n_type_levels <- n_distinct(rent_hedonics$building_type_factor)
hedonic_rhs <- "strictness_std + log_sqft + log_beds + log_baths"
if (n_type_levels >= 2) {
  hedonic_rhs <- paste0(hedonic_rhs, " + building_type_factor")
}

m_no_hed <- feols(
  log(rent_price) ~ strictness_std | segment_id^year_month,
  data = rent,
  cluster = cluster_formula
)
m_no_hed$custom_data <- rent

m_hed <- feols(
  as.formula(paste0("log(rent_price) ~ ", hedonic_rhs, " | segment_id^year_month")),
  data = rent_hedonics,
  cluster = cluster_formula
)
m_hed$custom_data <- rent_hedonics

m_amenity <- feols(
  as.formula(paste0(
    "log(rent_price) ~ ", hedonic_rhs,
    " + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + lake_michigan_dist_ft | segment_id^year_month"
  )),
  data = rent_amenities,
  cluster = cluster_formula
)
m_amenity$custom_data <- rent_amenities

setFixest_dict(c(strictness_std = "Stringency Index"))

etable(
  list(m_no_hed, m_hed, m_amenity),
  keep = "Stringency Index",
  fitstat = ~ n,
  style.tex = style.tex("aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  ),
  depvar = FALSE,
  drop.section = "fixef",
  digits = 3,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  extralines = list(
    "_Hedonic Controls" = c("", "$\\checkmark$", "$\\checkmark$"),
    "_Amenity Controls" = c("", "", "$\\checkmark$"),
    "_FE Structure" = c("Segment $\\times$ Year-Month FE", "Segment $\\times$ Year-Month FE", "Segment $\\times$ Year-Month FE"),
    "_Cluster Level" = c(cluster_label, cluster_label, cluster_label)
  ),
  file = output_tex,
  replace = TRUE
)

extract_model_row <- function(model, specification, sample_name) {
  tibble(
    specification = specification,
    sample_name = sample_name,
    estimate = coef(model)[["strictness_std"]],
    std_error = se(model)[["strictness_std"]],
    p_value = pvalue(model)[["strictness_std"]],
    n_obs = model$nobs,
    dep_var_mean = mean(model$custom_data$rent_price, na.rm = TRUE),
    ward_pairs = length(unique(model$custom_data$ward_pair)),
    bandwidth_ft = bw_ft,
    bandwidth_label = bw_label,
    window = window,
    sample_filter = "all",
    cluster_level = cluster_level,
    fe_geo = "segment",
    use_amenity_controls = specification == "amenity_hedonic"
  )
}

coef_tbl <- bind_rows(
  extract_model_row(m_no_hed, "no_hedonics", "all_rows"),
  extract_model_row(m_hed, "baseline_hedonic", "hedonic_complete_case"),
  extract_model_row(m_amenity, "amenity_hedonic", "hedonic_plus_amenity_complete_case")
)

write_csv(coef_tbl, output_csv)

message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))
