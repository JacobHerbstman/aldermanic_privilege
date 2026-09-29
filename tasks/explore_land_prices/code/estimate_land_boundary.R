# setwd("tasks/explore_land_prices/code")
# Exploratory: is land cheaper on the stricter side of ward boundaries? Land prices capitalize the development
# an alderman is expected to allow, so unlike the density of what gets built they do not depend on which projects
# go ahead. For each boundary and pair of aldermen serving its two sides on the sale date (a cell), log price per
# square foot of lot is compared across the boundary with cell, sale-year and zoning-group fixed effects, clustered by
# ward pair, for vacant-lot sales, teardown sales (all, and split by whether a new building followed) and both kinds
# (with a fixed effect for the kind of sale). Two designs, for each measure in tasks/explore_alderman_measures (standardized across aldermen):
#   gap: the price difference per standard deviation by which the side's alderman is stricter than the other's;
#   stricter_side: the difference between the stricter and the more lenient side, as in the paper's boundary designs.
# Both are repeated at placebo lines 500, 750 and 1,000 ft inside either ward, where both sides have the same
# alderman and the side nearer the real boundary is given the other ward's alderman.
measure_names <- c("processing_time_index", "stall_rate", "stall_rate_away", "days_to_passage",
  "fewer_high_discretion_permits", "fewer_low_discretion_permits")
# Teardowns are also split by whether a new building followed (build_land_sales.R).
samples <- c("vacant", "teardown", "teardown_redeveloped", "teardown_not_redeveloped", "all")
in_sample <- function(data, sample) switch(sample,
  vacant = data$sale_kind == "vacant", teardown = data$sale_kind == "teardown",
  teardown_redeveloped = data$sale_kind == "teardown" & data$redeveloped %in% TRUE,
  teardown_not_redeveloped = data$sale_kind == "teardown" & data$redeveloped %in% FALSE,
  all = rep(TRUE, nrow(data)))
half_widths_ft <- c(500, 250)
offsets_ft <- c(-1000, -750, -500, 0, 500, 750, 1000)

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

measures <- read_csv("../input/alderman_measures.csv", show_col_types = FALSE) |>
  left_join(read_csv("../input/stall_rate_away_from_boundaries.csv", show_col_types = FALSE) |>
    select(alderman, stall_rate_away), by = "alderman", relationship = "one-to-one") |>
  select(alderman, all_of(measure_names)) |>
  tidyr::pivot_longer(-alderman, names_to = "measure", values_to = "value") |>
  filter(is.finite(value)) |>
  mutate(value = (value - mean(value)) / sd(value), .by = measure)

# Signed distance is positive inside the pair's lower-numbered (first) ward.
sales <- read_csv("../output/land_sales.csv", show_col_types = FALSE) |>
  filter(distance_ft < 1500, !is.na(alderman), !is.na(neighbor_alderman), !is.na(zone_group),
    is.finite(log_price_per_sqft)) |>
  mutate(ward_pair = paste(map_year, pmin(ward, neighbor_ward), pmax(ward, neighbor_ward)),
    cell = paste(ward_pair, pmin(alderman, neighbor_alderman), pmax(alderman, neighbor_alderman), sep = ":"),
    first_alderman = if_else(ward < neighbor_ward, alderman, neighbor_alderman),
    second_alderman = if_else(ward < neighbor_ward, neighbor_alderman, alderman),
    signed_ft = if_else(ward < neighbor_ward, 1, -1) * distance_ft)

results <- tidyr::expand_grid(sample = samples, measure = measure_names, within_ft = half_widths_ft,
    offset_ft = offsets_ft) |>
  mutate(result = purrr::pmap(list(sample, measure, within_ft, offset_ft), function(k, m, width, offset) {
    scored <- filter(measures, measure == m)
    data <- sales |>
      filter(in_sample(sales, k), abs(signed_ft - offset) < width) |>
      mutate(side = if_else(signed_ft >= offset, first_alderman, second_alderman),
        other = if_else(signed_ft >= offset, second_alderman, first_alderman)) |>
      inner_join(select(scored, side = alderman, side_value = value), by = "side", relationship = "many-to-one") |>
      inner_join(select(scored, other = alderman, other_value = value), by = "other", relationship = "many-to-one") |>
      mutate(stricter_by = side_value - other_value, stricter_side = as.numeric(stricter_by > 0))
    bind_rows(lapply(c("stricter_by", "stricter_side"), function(x) {
      fit <- fixest::feols(stats::as.formula(paste("log_price_per_sqft ~", x,
        "| cell + year + zone_group + sale_kind")), data = data, cluster = ~ward_pair, notes = FALSE, warn = FALSE)
      tibble(design = if_else(x == "stricter_by", "gap", "stricter_side"), estimate = coef(fit)[[x]],
        std_error = fixest::se(fit)[[x]], p_value = fixest::pvalue(fit)[[x]], sales = stats::nobs(fit),
        ward_pairs = n_distinct(data$ward_pair[fixest::obs(fit)]))
    }))
  })) |>
  tidyr::unnest(result)
SaveData(select(results, sample, measure, design, within_ft, offset_ft, everything()),
  c("sample", "measure", "design", "within_ft", "offset_ft"), "../output/land_boundary_estimates.csv")
