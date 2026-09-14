# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/working_paper_release_audit/code")
suppressPackageStartupMessages({library(arrow); library(dplyr); library(readr); library(fixest)})
rent <- read_parquet("../input/rents.parquet") |>
  filter(is.finite(signed_dist), abs(signed_dist) < 500, rent_price > 0,
         flag_clean_location_sample, beds >= 0, sqft > 0, baths > 0,
         is.finite(strictness_own), is.finite(strictness_neighbor), strictness_own != strictness_neighbor,
         !is.na(segment_id), segment_id != "", !is.na(ward_pair_id), ward_pair_id != "",
         if_all(c(longitude, latitude, nearest_school_dist_kft, nearest_park_dist_kft,
                  nearest_major_road_dist_kft, nearest_cta_stop_dist_kft, lake_michigan_dist_kft), is.finite))
stopifnot(nrow(rent) == 240713)
duplicates <- rent |>
  count(address_norm, longitude, latitude, file_date, beds, baths, sqft, building_type_clean) |>
  filter(n > 1)
write_csv(duplicates, "../output/rental_floorplan_duplicate_groups.csv")
rent <- rent |>
  add_count(address_norm, longitude, latitude, file_date, beds, baths, sqft, building_type_clean, name = "floorplan_rows") |>
  mutate(log_rent = log(rent_price), log_sqft = log(sqft), log_baths = log(baths),
         beds_factor = factor(beds), building_type_factor = factor(coalesce(building_type_clean, "other")),
         distance_bin = floor(signed_dist / 100), year_month = format(as.Date(file_date), "%Y-%m"))
weighted_results <- list()
for (check in c("baseline", "equal_corrected_floorplan_weight")) {
  rent$analysis_weight <- if (check == "baseline") 1 else 1 / rent$floorplan_rows
  fit <- feols(log_rent ~ i(distance_bin, ref = -1) + log_sqft + beds_factor + log_baths +
                 nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft +
                 nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor |
                 segment_id^year_month, data = rent, weights = ~analysis_weight, cluster = ~ward_pair_id, notes = FALSE)
  ct <- coeftable(fit)["distance_bin::0", ]
  weighted_results[[check]] <- tibble(check, estimate = ct[1], se = ct[2], p = ct[4], n = nobs(fit))
}
write_csv(bind_rows(weighted_results), "../output/rental_duplicate_weight_sensitivity.csv")
rent_spreads <- rent |>
  mutate(era = if_else(as.Date(assignment_date) < as.Date("2015-05-18"), "old", "new")) |>
  summarise(spread_ft = max(abs(signed_dist)) - min(abs(signed_dist)),
            .by = c(longitude, latitude, era, ward_pair_id, segment_id))
sales <- read_parquet("../input/sales.parquet") |>
  filter(is.finite(signed_dist_m), abs(signed_dist_m) / 0.3048 < 500, sale_price > 0,
         is.finite(strictness_own), is.finite(strictness_neighbor), strictness_own != strictness_neighbor,
         !is.na(segment_id), segment_id != "", !is.na(ward_pair_id), ward_pair_id != "",
         if_all(c(longitude, latitude, log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths,
                  has_garage, nearest_school_dist_ft, nearest_park_dist_ft, nearest_major_road_dist_ft,
                  nearest_cta_stop_dist_ft, lake_michigan_dist_ft), is.finite))
stopifnot(nrow(sales) == 58468)
write_csv(sales |> filter(card_proration_rate > 0, card_proration_rate < 1) |>
            select(pin, year, sale_document_num, card_proration_rate, tieback_proration_rate, num_buildings, building_sqft),
          "../output/fractional_card_proration_sales.csv")
sales_spreads <- sales |>
  mutate(era = if_else(as.Date(sale_date) < as.Date("2015-05-18"), "old", "new")) |>
  summarise(spread_ft = (max(abs(signed_dist_m)) - min(abs(signed_dist_m))) / 0.3048,
            .by = c(longitude, latitude, era, ward_pair_id, segment_id))
support <- sales |>
  mutate(distance_bin = floor(signed_dist_m / 0.3048 / 100)) |>
  summarise(n = n(), adjacent_minus = any(distance_bin == -1), adjacent_plus = any(distance_bin == 0),
            .by = c(segment_id, year_quarter))
projects <- read_csv("../input/projects.csv", show_col_types = FALSE)
write_csv(tibble(check = c("rent_duplicate_panel_ids", "rent_nonmissing_unit_ids", "rent_duplicate_floorplan_groups",
                           "rent_duplicate_floorplan_excess_rows", "rent_distance_spread_over_1ft",
                           "sales_duplicate_row_ids", "sales_hedonic_year_mismatch", "sales_coordinate_year_mismatch",
                           "sales_fractional_card_proration", "sales_distance_spread_over_1ft",
                           "sales_adjacent_bin_groups", "sales_rows_in_adjacent_bin_groups",
                           "density_duplicate_projects", "density_far_formula_mismatch", "density_dupac_formula_mismatch"),
                 value = c(anyDuplicated(rent$rent_panel_id), sum(!is.na(rent$unit_id) & rent$unit_id != ""), nrow(duplicates),
                           sum(duplicates$n - 1), sum(rent_spreads$spread_ft > 1), anyDuplicated(sales$row_id),
                           sum(sales$hedonic_tax_year != sales$year), sum(sales$coordinate_year != sales$year),
                           sum(sales$card_proration_rate > 0 & sales$card_proration_rate < 1, na.rm = TRUE), sum(sales_spreads$spread_ft > 1),
                           sum(support$adjacent_minus & support$adjacent_plus), sum(support$n[support$adjacent_minus & support$adjacent_plus]),
                           anyDuplicated(projects$project_id),
                           sum(abs(projects$density_far - projects$building_sqft / projects$land_sqft) > 1e-6, na.rm = TRUE),
                           sum(abs(projects$density_dupac - projects$dwelling_units / projects$land_sqft * 43560) > 1e-6, na.rm = TRUE))),
          "../output/data_checks.csv")
writeLines(capture.output(sessionInfo()), "../output/R_session_info.txt")
