source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--window", type = "character", default = "pre_covid"),
  make_option("--sample_filter", type = "character", default = "all"),
  make_option("--output_tex", type = "character", default = "../output/fe_table_rental_bw1000_pre_covid_all.tex"),
  make_option("--output_csv", type = "character", default = "../output/fe_table_rental_bw1000_pre_covid_all.csv"),
  make_option("--output_year_diag", type = "character", default = "../output/year_diagnostics_bw1000_pre_covid_all.csv")
)
opt <- parse_args(OptionParser(option_list = option_list))

if (!opt$window %in% c("full", "pre_covid", "pre_2021", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, drop_mid", call. = FALSE)
}
if (!opt$sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}
if (!is.finite(opt$bw_ft) || opt$bw_ft <= 0) {
  stop("--bw_ft must be a positive integer", call. = FALSE)
}

window_rule <- function(df, window_name) {
  if (window_name == "full") {
    return(df)
  }
  if (window_name == "pre_covid") {
    return(df %>% filter(year <= 2019))
  }
  if (window_name == "pre_2021") {
    return(df %>% filter(year <= 2020))
  }
  if (window_name == "drop_mid") {
    return(df %>% filter(year <= 2020 | year >= 2024))
  }
  df
}

window_label <- c(
  full = "All years (2014-2025)",
  pre_covid = "Pre-COVID (2014-2019)",
  pre_2021 = "Through 2020 (2014-2020)",
  drop_mid = "Skip 2021-2023"
)

message("=== Rental Border Pair FE ===")
message(sprintf("Input: %s", opt$input))
message(sprintf("Bandwidth: %d ft", opt$bw_ft))
message(sprintf("Window: %s", window_label[[opt$window]]))
message(sprintf("Sample filter: %s", opt$sample_filter))

rent_raw <- read_parquet(opt$input) %>%
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
    abs(signed_dist) <= opt$bw_ft,
    !is.na(strictness_own)
  )

year_diag <- rent_raw %>%
  mutate(in_window = case_when(
    opt$window == "full" ~ TRUE,
    opt$window == "pre_covid" ~ year <= 2019,
    opt$window == "pre_2021" ~ year <= 2020,
    opt$window == "drop_mid" ~ (year <= 2020 | year >= 2024),
    TRUE ~ FALSE
  )) %>%
  group_by(year) %>%
  summarise(
    n = n(),
    in_window = first(in_window),
    median_rent = median(rent_price, na.rm = TRUE),
    mean_rent = mean(rent_price, na.rm = TRUE),
    share_multifamily = mean(building_type_clean == "multi_family", na.rm = TRUE),
    coverage_sqft = mean(!is.na(sqft) & sqft > 0),
    coverage_beds = mean(!is.na(beds) & beds > 0),
    coverage_baths = mean(!is.na(baths) & baths > 0),
    coverage_available_date = mean(!is.na(available_date)),
    .groups = "drop"
  )

write_csv(year_diag, opt$output_year_diag)

rent <- window_rule(rent_raw, opt$window)
if (opt$sample_filter == "multifamily_only") {
  rent <- rent %>% filter(building_type_clean == "multi_family")
}

if (nrow(rent) == 0) {
  stop("No observations after filtering.", call. = FALSE)
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

if (nrow(rent_hedonics) == 0) {
  stop("No complete-case rows for hedonic model.", call. = FALSE)
}

mean_y_level <- function(x) {
  dat <- x$custom_data
  sprintf("%.0f", mean(dat$rent_price, na.rm = TRUE))
}
fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")

n_ward_pairs <- function(x) {
  dat <- x$custom_data
  length(unique(stats::na.omit(dat$ward_pair)))
}
fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")

m_no_hed <- feols(
  log(rent_price) ~ strictness_std | ward_pair^year_month,
  data = rent,
  cluster = ~ward_pair
)
m_no_hed$custom_data <- rent

n_type_levels <- n_distinct(rent_hedonics$building_type_factor)
hedonic_rhs <- "strictness_std + log_sqft + log_beds + log_baths"
if (n_type_levels >= 2) {
  hedonic_rhs <- paste0(hedonic_rhs, " + building_type_factor")
}

m_hed <- feols(
  as.formula(paste0("log(rent_price) ~ ", hedonic_rhs, " | ward_pair^year_month")),
  data = rent_hedonics,
  cluster = ~ward_pair
)
m_hed$custom_data <- rent_hedonics

setFixest_dict(c(
  strictness_std = "Strictness Score",
  ward_pair = "Ward Pair",
  year_month = "Year-Month",
  log_sqft = "Log Sqft",
  log_beds = "Log Beds",
  log_baths = "Log Baths"
))

etable(
  list(m_no_hed, m_hed),
  keep = "Strictness Score",
  fitstat = ~ n + myo + nwp,
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
    "_Hedonic Controls" = c("", "$\\checkmark$"),
    "_Ward-Pair $\\times$ Year-Month FE" = c("$\\checkmark$", "$\\checkmark$"),
    "_Window" = c(window_label[[opt$window]], window_label[[opt$window]]),
    "_Sample" = c(opt$sample_filter, opt$sample_filter)
  ),
  file = opt$output_tex,
  replace = TRUE
)

coef_tbl <- tibble(
  specification = c("no_hedonics", "with_hedonics"),
  estimate = c(coef(m_no_hed)[["strictness_std"]], coef(m_hed)[["strictness_std"]]),
  std_error = c(se(m_no_hed)[["strictness_std"]], se(m_hed)[["strictness_std"]]),
  p_value = c(pvalue(m_no_hed)[["strictness_std"]], pvalue(m_hed)[["strictness_std"]]),
  n_obs = c(m_no_hed$nobs, m_hed$nobs),
  dep_var_mean = c(mean(rent$rent_price, na.rm = TRUE), mean(rent_hedonics$rent_price, na.rm = TRUE)),
  ward_pairs = c(length(unique(rent$ward_pair)), length(unique(rent_hedonics$ward_pair))),
  bandwidth_ft = opt$bw_ft,
  window = opt$window,
  sample_filter = opt$sample_filter
)

write_csv(coef_tbl, opt$output_csv)

message(sprintf("Saved table: %s", opt$output_tex))
message(sprintf("Saved coefficients: %s", opt$output_csv))
message(sprintf("Saved year diagnostics: %s", opt$output_year_diag))
