# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_record_quality_audit/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")
sales <- as.data.table(read_parquet("../output/quality_records.parquet"))
stopifnot(!anyDuplicated(sales$row_id), !anyNA(sales$is_mydec_date))
terms <- fread("../input/chicago_alderman_terms.csv")
terms[, `:=`(start_date = as.Date(start_date), end_date = as.Date(end_date))]
setorder(terms, ward, start_date)
terms[, next_start := shift(start_date, type = "lead"), by = ward]
stopifnot(!any(terms$next_start <= terms$end_date, na.rm = TRUE))
scores <- fread("../input/aldermen_uncertainty_scores.csv")
stopifnot(!anyDuplicated(scores$alderman))

# Conservatively check the whole month without imputing a recording day.
# An unrefined date is not proof of truncation when its displayed day is not 1.
review <- sales[is_mydec_date == FALSE]
review[, `:=`(
  earliest_date = as.Date(floor_date(sale_date, "month")),
  latest_date = as.Date(ceiling_date(sale_date, "month") - days(1))
)]
review[, map_ambiguous :=
  canonical_era_from_date(earliest_date) != canonical_era_from_date(latest_date)]

terms <- terms[end_date >= min(review$earliest_date) & start_date <= max(review$latest_date)]
terms[, `:=`(start_date = pmax(start_date, min(review$earliest_date)),
             end_date = pmin(end_date, max(review$latest_date)))]
daily <- terms[, .(date = seq(start_date, end_date, by = "day")),
               by = .(ward, alderman, start_date, end_date)]
stopifnot(!anyDuplicated(daily[, .(ward, date)]))
daily[, key := paste(ward, date)]

# In non-redistricting months, changes can only occur at term boundaries.
# Include every such date as well as the month endpoints, so brief terms or
# vacancies inside a month cannot be missed by an endpoints-only comparison.
change_dates <- sort(unique(c(terms$start_date, terms$end_date + 1)))
possibilities <- vector("list", 0)
for (month_i in sort(unique(format(review$earliest_date, "%Y-%m")))) {
  month_rows <- review[format(earliest_date, "%Y-%m") == month_i]
  first <- month_rows$earliest_date[1]
  last <- month_rows$latest_date[1]
  test_dates <- sort(unique(c(first, last, change_dates[change_dates > first & change_dates <= last],
                              if (any(month_rows$map_ambiguous)) as.Date("2015-05-18") else as.Date(character()))))
  for (date_i in as.character(test_dates)) {
    candidate <- month_rows[, .(row_id, pin, in_main_rd, in_current_main_rd, sale_date, longitude, latitude,
                                 complete_controls, ward, neighbor_ward, ward_pair_id,
                                 dist_m, era, map_ambiguous)]
    candidate[, possible_date := as.Date(date_i)]
    candidate[, possible_era := canonical_era_from_date(possible_date)]
    possibilities[[length(possibilities) + 1L]] <- candidate
  }
}
possibilities <- rbindlist(possibilities)
stopifnot(!anyDuplicated(possibilities[, .(row_id, possible_date)]))

reassign <- which(possibilities$era != possibilities$possible_era)
if (length(reassign) > 0L) {
  ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) |> st_transform(3435)
  maps <- load_canonical_ward_maps(ward_panel, c("2003_2014", "2015_2023"))
  boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", c("2003_2014", "2015_2023"))
  points <- st_as_sf(as.data.frame(possibilities[reassign]),
                     coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
  assignments <- as.data.table(assign_points_to_boundaries(
    points, points$possible_era, maps, boundaries, chunk_n = 5000L))
  stopifnot(nrow(assignments) == length(reassign))
  possibilities[reassign, `:=`(ward = assignments$ward,
                              neighbor_ward = assignments$neighbor_ward,
                              ward_pair_id = assignments$ward_pair_id,
                              dist_m = assignments$dist_m)]
}

possibilities[, own_alderman := daily$alderman[match(paste(ward, possible_date), daily$key)]]
possibilities[, neighbor_alderman := daily$alderman[match(paste(neighbor_ward, possible_date), daily$key)]]
possibilities[, own_score := scores$uncertainty_index[match(own_alderman, scores$alderman)]]
possibilities[, neighbor_score := scores$uncertainty_index[match(neighbor_alderman, scores$alderman)]]
possibilities[, possible_sign := sign(own_score - neighbor_score)]
possibilities[, score_eligible := is.finite(own_score) & is.finite(neighbor_score) & possible_sign != 0]
possibilities[, assigned_pair := paste(ward, neighbor_ward, sep = "/")]

date_cases <- possibilities[, .(
  in_main_rd = first(in_main_rd),
  in_current_main_rd = first(in_current_main_rd),
  sale_date = first(sale_date),
  pin = first(pin),
  own_alderman_changes = uniqueN(own_alderman, na.rm = FALSE) > 1L,
  neighbor_alderman_changes = uniqueN(neighbor_alderman, na.rm = FALSE) > 1L,
  map_ambiguous = first(map_ambiguous),
  ward_assignment_changes = uniqueN(assigned_pair, na.rm = FALSE) > 1L,
  score_eligibility_changes = uniqueN(score_eligible, na.rm = FALSE) > 1L,
  sign_changes_with_both_eligible = uniqueN(possible_sign[score_eligible]) > 1L,
  any_missing_term = any(is.na(own_alderman) | is.na(neighbor_alderman)),
  possible_own_aldermen = paste(unique(own_alderman), collapse = "; "),
  possible_neighbor_aldermen = paste(unique(neighbor_alderman), collapse = "; "),
  possible_signs = paste(unique(possible_sign), collapse = "; "),
  min_boundary_distance_ft = min(dist_m / 0.3048, na.rm = TRUE),
  max_boundary_distance_ft = max(dist_m / 0.3048, na.rm = TRUE)
), by = row_id]
date_cases[, affects_rd_assignment := map_ambiguous | ward_assignment_changes |
             score_eligibility_changes | sign_changes_with_both_eligible]
date_cases[, any_person_change := own_alderman_changes | neighbor_alderman_changes]
fwrite(date_cases[any_person_change | map_ambiguous | affects_rd_assignment | any_missing_term],
       "../output/date_assignment_review.csv")
row_review <- possibilities[row_id %in% date_cases[
  any_person_change | map_ambiguous | affects_rd_assignment | any_missing_term, row_id]]
setorder(row_review, row_id, possible_date)
row_review[, interval_end := shift(possible_date, type = "lead") - 1, by = row_id]
row_review[is.na(interval_end), interval_end := as.Date(ceiling_date(possible_date, "month") - days(1))]
fwrite(row_review[, .(row_id, pin, sale_date, in_main_rd, in_current_main_rd,
                      interval_start = possible_date, interval_end, ward, neighbor_ward,
                      own_alderman, neighbor_alderman, own_score, neighbor_score,
                      possible_sign, score_eligible, dist_m, map_ambiguous)],
       "../output/date_assignment_possibilities.csv")

# CTA proximity is the other production variable that depends on the day.
cta <- st_read("../input/cta_stops.gpkg", quiet = TRUE) |> st_transform(3435)
cta$active_from_date <- as.Date(cta$active_from_date)
cta$active_to_date <- as.Date(cta$active_to_date)
cta_changes <- sort(unique(c(cta$active_from_date, cta$active_to_date + 1)))
cta_changes <- cta_changes[!is.na(cta_changes) &
                           cta_changes >= min(review$earliest_date) &
                           cta_changes <= max(review$latest_date)]
cta_cases <- list()
for (date_i in as.character(cta_changes)) {
  change <- as.Date(date_i)
  candidates <- review[earliest_date < change & latest_date >= change]
  if (nrow(candidates) == 0L) next
  points <- st_as_sf(as.data.frame(candidates),
                     coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
  distances <- list()
  for (offset in c(-1L, 0L)) {
    on_date <- change + offset
    active <- cta[cta$active_from_date <= on_date &
                    (is.na(cta$active_to_date) | cta$active_to_date >= on_date), ]
    stopifnot(nrow(active) > 0L)
    nearest <- st_nearest_feature(points, active)
    distances[[length(distances) + 1L]] <- as.numeric(st_distance(points, active[nearest, ], by_element = TRUE))
  }
  candidates[, `:=`(network_change_date = change,
                     cta_distance_before_ft = distances[[1]],
                     cta_distance_after_ft = distances[[2]])]
  cta_cases[[length(cta_cases) + 1L]] <- candidates[
    abs(cta_distance_after_ft - cta_distance_before_ft) > 1e-6,
    .(row_id, pin, sale_date, in_main_rd, in_current_main_rd, network_change_date,
      cta_distance_before_ft, cta_distance_after_ft)]
}
cta_cases <- rbindlist(cta_cases)
fwrite(cta_cases, "../output/date_cta_review.csv")
date_flags <- c("any_person_change", "map_ambiguous", "ward_assignment_changes",
                "score_eligibility_changes", "sign_changes_with_both_eligible",
                "affects_rd_assignment", "any_missing_term")
summary <- rbindlist(lapply(date_flags, function(flag) data.table(
  issue = flag,
  panel_sales = sum(date_cases[[flag]]),
  main_rd_sales = sum(date_cases[[flag]] & date_cases$in_main_rd),
  current_main_rd_sales = sum(date_cases[[flag]] & date_cases$in_current_main_rd)
)))
summary <- rbind(data.table(issue = "No IDOR-refined date (conservative month review)", panel_sales = nrow(review),
                           main_rd_sales = sum(review$in_main_rd),
                           current_main_rd_sales = sum(review$in_current_main_rd)), summary)
summary <- rbind(summary, data.table(issue = "CTA distance can change within month",
                                     panel_sales = uniqueN(cta_cases$row_id),
                                     main_rd_sales = uniqueN(cta_cases[in_main_rd == TRUE, row_id]),
                                     current_main_rd_sales = uniqueN(cta_cases[in_current_main_rd == TRUE, row_id])))
fwrite(summary, "../output/date_summary.csv")
