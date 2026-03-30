source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../output/rent_with_ward_distances_amenities.parquet"
# bw_arg <- 500
# window <- "pre_2023"
# output_tex <- "../output/rent_amenity_attenuation_bw500_pre_2023.tex"
# output_csv <- "../output/rent_amenity_attenuation_bw500_pre_2023.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_arg, window, output_tex, output_csv)
}

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
  stop("FATAL: Script requires 5 args: <input> <bw_ft> <window> <output_tex> <output_csv> [<cluster_level>]", call. = FALSE)
}

if (length(bw_arg) != 1) {
  stop("bw_ft must be a single value.", call. = FALSE)
}
if (is.character(bw_arg) && str_to_lower(bw_arg) %in% c("all", "full", "none", "inf", "infinity")) {
  bw_ft <- Inf
} else {
  bw_ft <- suppressWarnings(as.numeric(bw_arg))
  if (!is.finite(bw_ft) || bw_ft <= 0) {
    stop("bw_ft must be a positive number or one of: all, full, none, inf.", call. = FALSE)
  }
}
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

rent <- rent_raw
if (window == "pre_covid") {
  rent <- rent %>% filter(year <= 2019)
} else if (window == "pre_2021") {
  rent <- rent %>% filter(year <= 2020)
} else if (window == "pre_2023") {
  rent <- rent %>% filter(year <= 2022)
} else if (window == "drop_mid") {
  rent <- rent %>% filter(year <= 2020 | year >= 2024)
}

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
    "_N" = c(format(nobs(m_no_hed), big.mark = ","), format(nobs(m_hed), big.mark = ","), format(nobs(m_amenity), big.mark = ",")),
    "_Dep. Var. Mean" = c(
      paste0("\\$", format(round(mean(m_no_hed$custom_data$rent_price, na.rm = TRUE), 0), big.mark = ",")),
      paste0("\\$", format(round(mean(m_hed$custom_data$rent_price, na.rm = TRUE), 0), big.mark = ",")),
      paste0("\\$", format(round(mean(m_amenity$custom_data$rent_price, na.rm = TRUE), 0), big.mark = ","))
    ),
    "_Ward Pairs" = c(
      format(length(unique(m_no_hed$custom_data$ward_pair)), big.mark = ","),
      format(length(unique(m_hed$custom_data$ward_pair)), big.mark = ","),
      format(length(unique(m_amenity$custom_data$ward_pair)), big.mark = ",")
    ),
    "_Hedonic Controls" = c("", "$\\checkmark$", "$\\checkmark$"),
    "_Amenity Controls" = c("", "", "$\\checkmark$"),
    "_FE Structure" = c("Segment $\\times$ Year-Month FE", "Segment $\\times$ Year-Month FE", "Segment $\\times$ Year-Month FE"),
    "_Cluster Level" = c(cluster_label, cluster_label, cluster_label)
  ),
  file = output_tex,
  replace = TRUE
)

coef_tbl <- bind_rows(
  tibble(
    specification = "no_hedonics",
    sample_name = "all_rows",
    estimate = coef(m_no_hed)[["strictness_std"]],
    std_error = se(m_no_hed)[["strictness_std"]],
    p_value = pvalue(m_no_hed)[["strictness_std"]],
    n_obs = m_no_hed$nobs,
    dep_var_mean = mean(m_no_hed$custom_data$rent_price, na.rm = TRUE),
    ward_pairs = length(unique(m_no_hed$custom_data$ward_pair)),
    bandwidth_ft = bw_ft,
    bandwidth_label = bw_label,
    window = window,
    sample_filter = "all",
    cluster_level = cluster_level,
    fe_geo = "segment",
    use_amenity_controls = FALSE
  ),
  tibble(
    specification = "baseline_hedonic",
    sample_name = "hedonic_complete_case",
    estimate = coef(m_hed)[["strictness_std"]],
    std_error = se(m_hed)[["strictness_std"]],
    p_value = pvalue(m_hed)[["strictness_std"]],
    n_obs = m_hed$nobs,
    dep_var_mean = mean(m_hed$custom_data$rent_price, na.rm = TRUE),
    ward_pairs = length(unique(m_hed$custom_data$ward_pair)),
    bandwidth_ft = bw_ft,
    bandwidth_label = bw_label,
    window = window,
    sample_filter = "all",
    cluster_level = cluster_level,
    fe_geo = "segment",
    use_amenity_controls = FALSE
  ),
  tibble(
    specification = "amenity_hedonic",
    sample_name = "hedonic_plus_amenity_complete_case",
    estimate = coef(m_amenity)[["strictness_std"]],
    std_error = se(m_amenity)[["strictness_std"]],
    p_value = pvalue(m_amenity)[["strictness_std"]],
    n_obs = m_amenity$nobs,
    dep_var_mean = mean(m_amenity$custom_data$rent_price, na.rm = TRUE),
    ward_pairs = length(unique(m_amenity$custom_data$ward_pair)),
    bandwidth_ft = bw_ft,
    bandwidth_label = bw_label,
    window = window,
    sample_filter = "all",
    cluster_level = cluster_level,
    fe_geo = "segment",
    use_amenity_controls = TRUE
  )
)

write_csv(coef_tbl, output_csv)

message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))
