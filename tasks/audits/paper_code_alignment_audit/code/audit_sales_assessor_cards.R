# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

sales <- read_parquet("../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    pin = as.character(pin),
    signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
    right = as.integer(signed_dist_ft >= 0)
  ) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006,
    year <= 2022,
    !is.na(ward_pair_id),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    if_all(
      all_of(c(
        "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage",
        "nearest_school_dist_ft", "nearest_park_dist_ft", "nearest_major_road_dist_ft",
        "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
      )),
      ~ !is.na(.x)
    )
  )

model_keys <- sales %>%
  distinct(pin, tax_year) %>%
  mutate(tax_year = as.integer(tax_year))

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
invisible(dbExecute(con, "PRAGMA threads=4"))
duckdb::duckdb_register(con, "model_keys", model_keys)

card_aggregates <- dbGetQuery(
  con,
  "
  WITH raw AS (
    SELECT
      TRIM(pin) AS pin,
      TRY_CAST(REGEXP_REPLACE(CAST(year AS VARCHAR), '[^0-9.-]', '', 'g') AS INTEGER) AS tax_year,
      TRY_CAST(REGEXP_REPLACE(CAST(char_bldg_sf AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS building_sqft,
      TRY_CAST(REGEXP_REPLACE(CAST(char_land_sf AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS land_sqft,
      TRY_CAST(REGEXP_REPLACE(CAST(char_beds AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS bedrooms,
      TRY_CAST(REGEXP_REPLACE(CAST(char_fbath AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS full_baths,
      TRY_CAST(REGEXP_REPLACE(CAST(char_hbath AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS half_baths,
      TRY_CAST(REGEXP_REPLACE(CAST(char_gar1_size AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS garage_size
    FROM read_csv(
      '../../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv',
      ignore_errors = true,
      all_varchar = true,
      header = true,
      auto_detect = true,
      max_line_size = 10000000
    )
  )
  SELECT
    r.pin,
    r.tax_year,
    COUNT(*) AS card_rows,
    COUNT(*) FILTER (WHERE r.building_sqft > 0) AS cards_with_positive_sqft,
    SUM(r.building_sqft) FILTER (WHERE r.building_sqft > 0) AS total_building_sqft,
    MAX(r.building_sqft) FILTER (WHERE r.building_sqft > 0) AS largest_building_sqft,
    MAX(r.land_sqft) FILTER (WHERE r.land_sqft > 0) AS maximum_land_sqft,
    SUM(r.bedrooms) FILTER (WHERE r.bedrooms >= 0) AS total_bedrooms,
    SUM(r.full_baths) FILTER (WHERE r.full_baths >= 0) AS total_full_baths,
    SUM(r.half_baths) FILTER (WHERE r.half_baths >= 0) AS total_half_baths,
    SUM(r.garage_size) FILTER (WHERE r.garage_size >= 0) AS total_garage_size
  FROM raw r
  INNER JOIN model_keys k
    ON r.pin = k.pin
    AND r.tax_year = k.tax_year
  GROUP BY 1, 2
  "
) %>%
  as_tibble()

duckdb::duckdb_unregister(con, "model_keys")

sales_cards <- sales %>%
  left_join(card_aggregates, by = c("pin", "tax_year"), relationship = "many-to-one") %>%
  mutate(
    multi_card = card_rows > 1,
    total_to_selected_sqft_ratio = total_building_sqft / building_sqft,
    log_total_sqft = if_else(total_building_sqft > 0, log(total_building_sqft), NA_real_),
    log_total_bedrooms = if_else(total_bedrooms > 0, log(total_bedrooms), NA_real_),
    total_baths = total_full_baths + 0.5 * coalesce(total_half_baths, 0),
    log_total_baths = if_else(total_baths > 0, log(total_baths), NA_real_),
    has_any_garage = as.integer(total_garage_size > 0)
  )

if (any(is.na(sales_cards$card_rows))) {
  stop("Some final sales model rows could not be linked to raw assessor cards.", call. = FALSE)
}

write_csv(
  sales_cards %>%
    summarise(
      model_rows = n(),
      unique_pin_years = n_distinct(paste(pin, tax_year, sep = "\r")),
      multi_card_rows = sum(multi_card),
      multi_card_pin_years = n_distinct(paste(pin[multi_card], tax_year[multi_card], sep = "\r")),
      rows_selected_sqft_below_total = sum(total_building_sqft > building_sqft + 1e-8),
      median_total_to_selected_sqft_ratio_multi_card = median(
        total_to_selected_sqft_ratio[multi_card],
        na.rm = TRUE
      ),
      mean_total_to_selected_sqft_ratio_multi_card = mean(
        total_to_selected_sqft_ratio[multi_card],
        na.rm = TRUE
      ),
      max_total_to_selected_sqft_ratio_multi_card = max(
        total_to_selected_sqft_ratio[multi_card],
        na.rm = TRUE
      )
    ),
  "../output/sales_assessor_card_summary.csv"
)

write_csv(
  sales_cards %>%
    count(class, multi_card, name = "n") %>%
    arrange(class, multi_card),
  "../output/sales_assessor_card_class_counts.csv"
)

common_controls <- paste(
  c(
    "log_land_sqft", "log_building_age",
    "nearest_school_dist_ft", "nearest_park_dist_ft", "nearest_major_road_dist_ft",
    "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
  ),
  collapse = " + "
)
formulas <- list(
  production_largest_card = as.formula(paste0(
    "log(sale_price) ~ right + log_sqft + log_bedrooms + log_baths + has_garage + ",
    common_controls,
    " | segment_id^year_quarter"
  )),
  summed_across_cards = as.formula(paste0(
    "log(sale_price) ~ right + log_total_sqft + log_total_bedrooms + log_total_baths + has_any_garage + ",
    common_controls,
    " | segment_id^year_quarter"
  ))
)

model_rows <- list()
for (spec_name in names(formulas)) {
  spec_model <- feols(formulas[[spec_name]], data = sales_cards, cluster = ~segment_id)
  spec_ct <- coeftable(spec_model)
  model_rows[[length(model_rows) + 1L]] <- tibble(
    specification = spec_name,
    estimate = unname(spec_ct["right", "Estimate"]),
    se = unname(spec_ct["right", "Std. Error"]),
    p_value = unname(spec_ct["right", "Pr(>|t|)"]),
    n = nobs(spec_model)
  )
}
write_csv(bind_rows(model_rows), "../output/sales_assessor_card_sensitivity.csv")
