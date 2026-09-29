# setwd("tasks/explore_land_prices/code")
# Exploratory: is a teardown less likely to be followed by a new building on the stricter side of a ward boundary?
# Teardown sales whose redevelopment is known (build_land_sales.R: a new-construction permit naming the parcel within
# three years of the demolition, for demolitions through 2019), within 500 and 250 ft of a boundary. Linear
# probability of redevelopment on an indicator for the stricter side by each measure, with cell (boundary and pair of
# serving aldermen), sale-year and zoning-group fixed effects, clustered by ward pair.
measure_names <- c("processing_time_index", "stall_rate", "stall_rate_away", "days_to_passage")
half_widths_ft <- c(500, 250)

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

measures <- read_csv("../input/alderman_measures.csv", show_col_types = FALSE) |>
  left_join(read_csv("../input/stall_rate_away_from_boundaries.csv", show_col_types = FALSE) |>
    select(alderman, stall_rate_away), by = "alderman", relationship = "one-to-one")
teardowns <- read_csv("../output/land_sales.csv", show_col_types = FALSE) |>
  filter(sale_kind == "teardown", !is.na(redeveloped), !is.na(alderman), !is.na(neighbor_alderman),
    !is.na(zone_group)) |>
  mutate(ward_pair = paste(map_year, pmin(ward, neighbor_ward), pmax(ward, neighbor_ward)),
    cell = paste(ward_pair, pmin(alderman, neighbor_alderman), pmax(alderman, neighbor_alderman), sep = ":"),
    redeveloped = as.numeric(redeveloped))

results <- tidyr::expand_grid(measure = measure_names, within_ft = half_widths_ft) |>
  mutate(result = purrr::map2(measure, within_ft, function(m, width) {
    score <- setNames(measures[[m]], measures$alderman)
    data <- teardowns |>
      filter(distance_ft < width) |>
      mutate(stricter_side = as.numeric(score[alderman] > score[neighbor_alderman])) |>
      filter(!is.na(stricter_side))
    fit <- fixest::feols(redeveloped ~ stricter_side | cell + year + zone_group, data = data, cluster = ~ward_pair,
      notes = FALSE, warn = FALSE)
    tibble(estimate = coef(fit)[["stricter_side"]], std_error = fixest::se(fit)[["stricter_side"]],
      p_value = fixest::pvalue(fit)[["stricter_side"]], teardowns = stats::nobs(fit),
      redeveloped_share = mean(data$redeveloped))
  })) |>
  tidyr::unnest(result)
SaveData(results, c("measure", "within_ft"), "../output/teardown_redevelopment_by_side.csv")
