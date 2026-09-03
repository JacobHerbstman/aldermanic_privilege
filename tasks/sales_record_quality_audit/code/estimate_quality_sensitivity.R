# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_record_quality_audit/code")
# price_per_sqft_cutoff <- 2000
# outlier_row_id <- "96240099"

source("../../setup_environment/code/packages.R")
args <- if (interactive()) c(price_per_sqft_cutoff, outlier_row_id) else commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, is.finite(as.numeric(args[1])), as.numeric(args[1]) > 0)
price_per_sqft_cutoff <- as.numeric(args[1])
outlier_row_id <- args[2]

# The upstream enriched panel precedes the approved property-quality screen.
# This preserves a reproducible before/after comparison without freezing a copy.
sales <- as.data.table(read_parquet("../output/quality_records.parquet"))[in_main_rd == TRUE]
stopifnot(!anyDuplicated(sales$row_id), sum(sales$row_id == outlier_row_id) == 1L)
sales[, `:=`(
  property_class_factor = factor(class),
  distance_bin = cut(signed_dist_ft, breaks = seq(-500, 500, 100),
                     labels = sprintf("bin_%02d", 1:10), right = FALSE, include.lowest = TRUE)
)]
specifications <- data.table(
  specification = c("Before property-quality exclusions", "Exclude only flagged price record",
                    "Exclude bedrooms above rooms", "Exclude prices above cutoff",
                    "Exclude bedrooms above rooms and flagged price record",
                    "Exclude bedrooms above rooms and prices above cutoff"),
  drop_rooms = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE),
  drop_outlier = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
  cap_ppsf = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
)
fits <- list()
for (i in seq_len(nrow(specifications))) {
  spec <- specifications[i]
  keep <- (!spec$drop_rooms | !sales$flag_rooms_below_bedrooms) &
    (!spec$drop_outlier | sales$row_id != outlier_row_id) &
    (!spec$cap_ppsf | sales$nominal_ppsf <= price_per_sqft_cutoff)
  model <- feols(
    log(sale_price) ~ i(distance_bin, ref = "bin_05") +
      log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage +
      nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft +
      nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor |
      segment_id^year_quarter,
    data = sales[keep], cluster = ~ward_pair_id, notes = FALSE, warn = FALSE
  )
  if (i == 1L) baseline_coefficients <- coef(model)
  results <- coeftable(model)["distance_bin::bin_06", ]
  interval <- confint(model, parm = "distance_bin::bin_06")
  fits[[i]] <- data.table(
    specification = spec$specification, price_per_sqft_cutoff = price_per_sqft_cutoff,
    observations = nobs(model), excluded = nrow(sales) - nobs(model),
    estimate = unname(results[1]), std_error = unname(results[2]), p_value = unname(results[4]),
    percent_difference = 100 * expm1(unname(results[1])),
    percent_ci_low = 100 * expm1(interval[1]), percent_ci_high = 100 * expm1(interval[2]),
    max_coefficient_change = max(abs(coef(model) - baseline_coefficients[names(coef(model))]))
  )
}
results <- rbindlist(fits)
official <- fread("../input/price_boundary_property_type_fe_estimates.csv")[market == "sales" & property_type_fe]
approved <- results[specification == "Exclude bedrooms above rooms"]
stopifnot(approved$observations == official$n,
          abs(approved$estimate - official$estimate) < 1e-8,
          abs(approved$std_error - official$std_error) < 1e-8)

outlier <- sales[row_id == outlier_row_id]
outlier[, segment_quarter_observations := sales[
  segment_id == outlier$segment_id & year_quarter == outlier$year_quarter, .N
]]
fwrite(outlier[, .(row_id, sale_price_nominal, nominal_ppsf, signed_dist_ft,
                   segment_id, year_quarter, segment_quarter_observations)],
       "../output/flagged_price_influence.csv")
fwrite(results, "../output/quality_rd_sensitivity.csv")
