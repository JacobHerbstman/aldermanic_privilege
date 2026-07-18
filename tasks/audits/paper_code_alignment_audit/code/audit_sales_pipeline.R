# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

sales_raw <- fread(
  "../../../download_parcel_sales_data/output/parcel_sales_city.csv",
  colClasses = list(character = c("pin", "sale_date", "sale_price", "row_id"))
) %>%
  as_tibble() %>%
  mutate(
    sale_price_nominal = as.numeric(gsub("[$,]", "", sale_price)),
    year_source = suppressWarnings(as.integer(year)),
    pin = gsub("[^0-9]", "", trimws(pin)),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin),
    sale_date_parsed = coalesce(
      as.Date(as.character(sale_date), format = "%B %d, %Y"),
      as.Date(substr(as.character(sale_date), 1, 10), format = "%Y-%m-%d")
    ),
    sale_date_use = coalesce(sale_date_parsed, as.Date(paste0(year_source, "-06-15"))),
    seller_invalid = is.na(sale_seller_name) |
      sale_seller_name %in% c("", "-", "UNKNOWN", ".."),
    buyer_invalid = is.na(sale_buyer_name) |
      sale_buyer_name %in% c("", "-", "UNKNOWN", "..")
  )

stage_rules <- list(
  "Downloaded Chicago-township sales" = rep(TRUE, nrow(sales_raw)),
  "Residential classes 202-211" = sales_raw$class %in% 202:211,
  "Nominal sale price above $10,000 and nonmissing source year" =
    is.finite(sales_raw$sale_price_nominal) & sales_raw$sale_price_nominal > 10000 &
      !is.na(sales_raw$year_source),
  "Source years 1999-2025" = sales_raw$year_source >= 1999 & sales_raw$year_source <= 2025,
  "Warranty or trustee deed" = sales_raw$sale_deed_type %in% c("Warranty", "Trustee"),
  "Not coded as a land sale" = !is.na(sales_raw$sale_type) & sales_raw$sale_type != "LAND",
  "Seller name passes production screen" = !sales_raw$seller_invalid,
  "Seller and buyer names are not exactly identical" =
    !is.na(sales_raw$sale_buyer_name) & sales_raw$sale_seller_name != sales_raw$sale_buyer_name,
  "Single-parcel sale" = sales_raw$num_parcels_sale == 1
)

keep <- rep(TRUE, nrow(sales_raw))
attrition_raw <- list()
previous_n <- nrow(sales_raw)
for (stage_name in names(stage_rules)) {
  keep <- keep & coalesce(stage_rules[[stage_name]], FALSE)
  current_n <- sum(keep)
  attrition_raw[[length(attrition_raw) + 1L]] <- tibble(
    stage = stage_name,
    n = current_n,
    dropped_from_previous = previous_n - current_n
  )
  previous_n <- current_n
}
eligible <- sales_raw[keep, , drop = FALSE]

pre_scores <- read_csv(
  "../../../calculate_sale_distances/output/sales_pre_scores.csv",
  col_types = cols(pin = col_character(), .default = col_guess()),
  show_col_types = FALSE
) %>%
  mutate(pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin))
scored <- read_csv(
  "../../../merge_event_study_scores/output/sales_with_ward_distances_through2022.csv",
  col_types = cols(pin = col_character(), .default = col_guess()),
  show_col_types = FALSE
) %>%
  mutate(pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin))
hedonics <- read_parquet("../../../prep_sales_border_data/output/sales_with_hedonics.parquet") %>%
  as_tibble()
sales <- read_parquet("../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()

attrition <- bind_rows(
  bind_rows(attrition_raw),
  tibble(
    stage = c(
      "Ward and boundary assigned",
      "Own and neighboring aldermen have unequal through-2022 scores",
      "Sale year at least 2006 and same-year assessor join attempted",
      "Amenity distances attached"
    ),
    n = c(nrow(pre_scores), nrow(scored), nrow(hedonics), nrow(sales)),
    dropped_from_previous = c(
      nrow(eligible) - nrow(pre_scores),
      nrow(pre_scores) - nrow(scored),
      nrow(scored) - nrow(hedonics),
      nrow(hedonics) - nrow(sales)
    )
  )
)

sales <- sales %>%
  mutate(
    sale_date = as.Date(sale_date),
    ward_pair = as.character(ward_pair_id),
    signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
    right = as.integer(signed_dist_ft >= 0)
  )

base_ids <- sales %>%
  mutate(audit_row_id = row_number()) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006,
    year <= 2022,
    !is.na(ward_pair),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor)
  ) %>%
  pull(audit_row_id)

sales <- sales %>% mutate(audit_row_id = row_number())
base_data <- sales %>% filter(audit_row_id %in% base_ids)

hedonic_controls <- c(
  "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage"
)
amenity_controls <- c(
  "nearest_school_dist_ft", "nearest_park_dist_ft", "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
)
model_data <- base_data %>%
  filter(if_all(all_of(c(hedonic_controls, amenity_controls)), ~ !is.na(.x)))

attrition <- bind_rows(
  attrition,
  tibble(
    stage = c("Within 500 feet with valid treatment and segment", "Complete hedonic and amenity controls"),
    n = c(nrow(base_data), nrow(model_data)),
    dropped_from_previous = c(nrow(sales) - nrow(base_data), nrow(base_data) - nrow(model_data))
  )
)
write_csv(attrition, "../output/sales_sample_attrition.csv")

rhs <- paste(c("right", hedonic_controls, amenity_controls), collapse = " + ")
model <- feols(
  as.formula(paste0("log(sale_price) ~ ", rhs, " | segment_id^year_quarter")),
  data = model_data,
  cluster = ~segment_id
)
model_ct <- coeftable(model)
model_sample <- model_data[obs(model), , drop = FALSE]
cell_support <- model_sample %>%
  count(segment_id, year_quarter, right, name = "n") %>%
  count(segment_id, year_quarter, name = "sides")

model_summary <- tibble(
  estimate = unname(model_ct["right", "Estimate"]),
  se = unname(model_ct["right", "Std. Error"]),
  p_value = unname(model_ct["right", "Pr(>|t|)"]),
  percent_effect = 100 * expm1(unname(coef(model)["right"])),
  n = nobs(model),
  segments = n_distinct(model_sample$segment_id),
  segment_quarter_cells = nrow(cell_support),
  two_sided_segment_quarter_cells = sum(cell_support$sides == 2),
  one_sided_segment_quarter_cells = sum(cell_support$sides == 1),
  same_year_assessor_matches = sum(model_sample$hedonic_tax_year == model_sample$sale_year, na.rm = TRUE),
  missing_assessor_matches = sum(is.na(model_sample$hedonic_tax_year))
)
write_csv(model_summary, "../output/sales_model_reproduction.csv")

current_parcel_pins <- fread(
  "../../../download_parcel_universe_data/output/parcel_universe_2025_city.csv",
  select = "pin",
  colClasses = list(character = "pin")
) %>%
  as_tibble() %>%
  mutate(
    pin = gsub("[^0-9]", "", trimws(pin)),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin)
  ) %>%
  distinct(pin)
historical_keys <- fread(
  "../../../download_historical_sale_parcel_coordinates/output/historical_sale_parcel_coordinates_1999_2025.csv",
  colClasses = list(character = "pin")
) %>%
  as_tibble() %>%
  transmute(pin, year = as.integer(year)) %>%
  distinct()

coordinate_sources <- bind_rows(
  pre_scores %>%
    filter(year >= 2006, year <= 2022) %>%
    mutate(sample = "boundary_assigned_2006_2022"),
  model_sample %>% mutate(sample = "regression_sample")
) %>%
  mutate(
    current_2025_full_pin = pin %in% current_parcel_pins$pin,
    historical_exact_pin_year = paste(pin, year, sep = "\r") %in%
      paste(historical_keys$pin, historical_keys$year, sep = "\r"),
    coordinate_source = case_when(
      current_2025_full_pin ~ "2025_parcel_universe",
      historical_exact_pin_year ~ "historical_exact_pin_year",
      TRUE ~ "unverified"
    )
  ) %>%
  count(sample, coordinate_source, name = "n")
write_csv(coordinate_sources, "../output/sales_coordinate_source_summary.csv")

restriction_summary <- eligible %>%
  summarise(
    eligible_sales = n(),
    buyer_name_invalid_under_stated_rule = sum(buyer_invalid),
    seller_buyer_exactly_identical = sum(sale_seller_name == sale_buyer_name, na.rm = TRUE),
    raw_sale_date_missing_and_june15_imputed = sum(is.na(sale_date_parsed)),
    source_year_differs_from_sale_date_year = sum(
      !is.na(sale_date_parsed) & year_source != lubridate::year(sale_date_parsed)
    ),
    ccao_same_sale_within_365_flag = sum(as.logical(sale_filter_same_sale_within_365), na.rm = TRUE),
    ccao_less_than_10k_flag = sum(as.logical(sale_filter_less_than_10k), na.rm = TRUE),
    ccao_deed_type_flag = sum(as.logical(sale_filter_deed_type), na.rm = TRUE),
    duplicate_row_ids = n() - n_distinct(row_id),
    duplicate_pin_date_price_rows = n() - n_distinct(paste(pin, sale_date_use, sale_price_nominal, sep = "\r"))
  )
write_csv(restriction_summary, "../output/sales_restriction_validation.csv")

eligible_lookup <- eligible %>%
  transmute(
    pin,
    sale_date = sale_date_use,
    sale_price_nominal,
    buyer_invalid,
    ccao_same_sale_within_365_flag = coalesce(as.logical(sale_filter_same_sale_within_365), FALSE),
    ccao_less_than_10k_flag = coalesce(as.logical(sale_filter_less_than_10k), FALSE),
    ccao_deed_type_flag = coalesce(as.logical(sale_filter_deed_type), FALSE)
  )
if (anyDuplicated(eligible_lookup[c("pin", "sale_date", "sale_price_nominal")]) > 0) {
  stop("Eligible raw sales are not unique by PIN, sale date, and nominal price.", call. = FALSE)
}

arm_length_data <- model_data %>%
  left_join(
    eligible_lookup,
    by = c("pin", "sale_date", "sale_price_nominal"),
    relationship = "many-to-one"
  )
if (any(is.na(arm_length_data$buyer_invalid))) {
  stop("Some sales model rows could not be linked back to raw arm's-length flags.", call. = FALSE)
}

arm_length_specs <- list(
  production = rep(TRUE, nrow(arm_length_data)),
  valid_buyer_name = !arm_length_data$buyer_invalid,
  no_ccao_same_sale_within_365_flag = !arm_length_data$ccao_same_sale_within_365_flag,
  valid_buyer_and_no_ccao_same_sale_flag =
    !arm_length_data$buyer_invalid & !arm_length_data$ccao_same_sale_within_365_flag
)
arm_length_rows <- list()
for (spec_name in names(arm_length_specs)) {
  spec_data <- arm_length_data[coalesce(arm_length_specs[[spec_name]], FALSE), , drop = FALSE]
  spec_model <- feols(
    as.formula(paste0("log(sale_price) ~ ", rhs, " | segment_id^year_quarter")),
    data = spec_data,
    cluster = ~segment_id
  )
  spec_ct <- coeftable(spec_model)
  arm_length_rows[[length(arm_length_rows) + 1L]] <- tibble(
    specification = spec_name,
    input_n = nrow(spec_data),
    invalid_buyer_rows = sum(spec_data$buyer_invalid),
    ccao_same_sale_flag_rows = sum(spec_data$ccao_same_sale_within_365_flag),
    estimate = unname(spec_ct["right", "Estimate"]),
    se = unname(spec_ct["right", "Std. Error"]),
    p_value = unname(spec_ct["right", "Pr(>|t|)"]),
    n = nobs(spec_model)
  )
}
write_csv(bind_rows(arm_length_rows), "../output/sales_arm_length_sensitivity.csv")

write_csv(
  eligible %>% count(class, name = "n") %>% arrange(class),
  "../output/sales_class_counts.csv"
)

control_attrition <- bind_rows(lapply(
  c(
    "hedonic_tax_year", "log_sqft", "log_land_sqft", "log_building_age",
    "log_bedrooms", "log_baths", "has_garage", amenity_controls
  ),
  function(control_name) {
    tibble(
      control = control_name,
      missing_in_500ft_base = sum(is.na(base_data[[control_name]])),
      base_n = nrow(base_data)
    )
  }
))
write_csv(control_attrition, "../output/sales_control_missingness.csv")

write_csv(
  model_sample %>%
    summarise(
      n = n(),
      building_age_zero = sum(building_age == 0, na.rm = TRUE),
      bedrooms_zero = sum(num_bedrooms == 0, na.rm = TRUE),
      garage_size_missing_but_coded_no_garage = sum(is.na(garage_size) & has_garage == 0),
      half_baths_missing_treated_as_zero = sum(is.na(num_half_baths), na.rm = TRUE)
    ),
  "../output/sales_hedonic_coding_summary.csv"
)

analysis_raw_price <- sales %>%
  filter(year >= 2006, year <= 2022, is.finite(sale_price_real_2022_raw)) %>%
  pull(sale_price_real_2022_raw)
analysis_p01 <- quantile(analysis_raw_price, 0.01, na.rm = TRUE)
analysis_p99 <- quantile(analysis_raw_price, 0.99, na.rm = TRUE)

robustness_data <- model_data %>%
  mutate(
    sale_price_no_winsor = sale_price_real_2022_raw,
    sale_price_analysis_winsor = pmin(pmax(sale_price_real_2022_raw, analysis_p01), analysis_p99)
  )
price_specs <- c(
  production_global_winsor = "sale_price",
  no_winsorization = "sale_price_no_winsor",
  analysis_period_winsorization = "sale_price_analysis_winsor"
)
price_rows <- list()
for (spec_name in names(price_specs)) {
  outcome_name <- price_specs[[spec_name]]
  spec_model <- feols(
    as.formula(paste0("log(", outcome_name, ") ~ ", rhs, " | segment_id^year_quarter")),
    data = robustness_data,
    cluster = ~segment_id
  )
  spec_ct <- coeftable(spec_model)
  price_rows[[length(price_rows) + 1L]] <- tibble(
    specification = spec_name,
    p01 = if (spec_name == "analysis_period_winsorization") analysis_p01 else NA_real_,
    p99 = if (spec_name == "analysis_period_winsorization") analysis_p99 else NA_real_,
    estimate = unname(spec_ct["right", "Estimate"]),
    se = unname(spec_ct["right", "Std. Error"]),
    p_value = unname(spec_ct["right", "Pr(>|t|)"]),
    n = nobs(spec_model)
  )
}
write_csv(bind_rows(price_rows), "../output/sales_price_robustness.csv")

scores <- read_csv(
  "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, june_score = uncertainty_index)
aldermen <- read_csv(
  "../../../create_alderman_data/output/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  mutate(month_date = as.Date(paste("01", month), format = "%d %b %Y")) %>%
  filter(month_date == as.Date("2015-06-01")) %>%
  select(ward, alderman) %>%
  left_join(scores, by = "alderman", relationship = "many-to-one")

transition_data <- model_data %>%
  left_join(
    aldermen %>% rename(ward = ward, june_alderman_own = alderman, june_score_own = june_score),
    by = "ward",
    relationship = "many-to-one"
  ) %>%
  left_join(
    aldermen %>% rename(neighbor_ward = ward, june_alderman_neighbor = alderman, june_score_neighbor = june_score),
    by = "neighbor_ward",
    relationship = "many-to-one"
  ) %>%
  mutate(
    transition_window = sale_date >= as.Date("2015-05-18") & sale_date <= as.Date("2015-05-31"),
    right_corrected = if_else(
      transition_window,
      as.integer(june_score_own > june_score_neighbor),
      right
    ),
    treatment_changed = transition_window & right_corrected != right
  )

transition_rhs <- sub("right", "right_corrected", rhs, fixed = TRUE)
transition_model <- feols(
  as.formula(paste0("log(sale_price) ~ ", transition_rhs, " | segment_id^year_quarter")),
  data = transition_data,
  cluster = ~segment_id
)
transition_ct <- coeftable(transition_model)
write_csv(
  tibble(
    transition_window_model_rows = sum(transition_data$transition_window),
    rows_with_changed_treatment_side = sum(transition_data$treatment_changed, na.rm = TRUE),
    production_estimate = unname(model_ct["right", "Estimate"]),
    production_se = unname(model_ct["right", "Std. Error"]),
    corrected_estimate = unname(transition_ct["right_corrected", "Estimate"]),
    corrected_se = unname(transition_ct["right_corrected", "Std. Error"]),
    corrected_n = nobs(transition_model)
  ),
  "../output/sales_may2015_timing_sensitivity.csv"
)
