# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_price_tail_audit/code")
# bandwidth_ft <- 500

source("../../setup_environment/code/packages.R")
args <- if (interactive()) c(bandwidth_ft) else commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
bandwidth_ft <- as.numeric(args[1])
stopifnot(is.finite(bandwidth_ft), bandwidth_ft >= 100, bandwidth_ft %% 100 == 0, bandwidth_ft <= 1500)
sales <- as.data.table(read_parquet("../input/quality_records.parquet"))
review <- fread("../output/tail_review.csv", colClasses = list(character = "row_id"))
stopifnot(!anyDuplicated(sales$row_id), !anyDuplicated(review$row_id))
stopifnot(uniqueN(review$tail_probability) == 1L)
sales <- sales[flag_rooms_below_bedrooms == FALSE &
  is.finite(sale_price) & sale_price > 0 & year >= 2006 & year <= 2022 &
  is.finite(signed_dist_ft) & abs(signed_dist_ft) < bandwidth_ft &
  is.finite(strictness_own) & is.finite(strictness_neighbor) & strictness_own != strictness_neighbor &
  !is.na(segment_id) & segment_id != "" & !is.na(ward_pair_id) & ward_pair_id != "" &
  !is.na(era) & is.finite(longitude) & is.finite(latitude) & complete_controls]
sales[, `:=`(property_class_factor = factor(class),
  distance_bin = cut(signed_dist_ft, breaks = seq(-bandwidth_ft, bandwidth_ft, 100),
                    labels = as.character(seq(-bandwidth_ft, bandwidth_ft - 100, 100)),
                    right = FALSE, include.lowest = TRUE))]
fits <- list()
for (rule in c("No price trimming", "Pooled real percentile", "Annual real percentile")) {
  excluded <- switch(rule, "No price trimming" = character(),
    "Pooled real percentile" = review[flag_pooled == TRUE, row_id],
    "Annual real percentile" = review[flag_annual == TRUE, row_id])
  model <- feols(log(sale_price) ~ i(distance_bin, ref = "-100") +
    log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage +
    nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft +
    nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor |
    segment_id^year_quarter, data = sales[!row_id %in% excluded],
    cluster = ~ward_pair_id, notes = FALSE, warn = FALSE)
  coefficient <- coeftable(model)["distance_bin::0", ]
  fits[[rule]] <- data.table(rule = rule, tail_probability = review$tail_probability[1],
    bandwidth_ft = bandwidth_ft, n = nobs(model),
    excluded = nrow(sales) - nobs(model), estimate = unname(coefficient[1]),
    std_error = unname(coefficient[2]), p_value = unname(coefficient[4]),
    percent_difference = 100 * expm1(unname(coefficient[1])))
}
results <- rbindlist(fits)
if (bandwidth_ft == 500) {
  official <- fread("../input/price_boundary_property_type_fe_estimates.csv")[market == "sales" & property_type_fe]
  baseline <- results[rule == "No price trimming"]
  stopifnot(baseline$n == official$n, abs(baseline$estimate - official$estimate) < 1e-8,
            abs(baseline$std_error - official$std_error) < 1e-8)
}
fwrite(results, sprintf("../output/tail_rd_bw%d.csv", bandwidth_ft))
