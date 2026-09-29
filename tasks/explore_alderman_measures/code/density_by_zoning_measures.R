# setwd("tasks/explore_alderman_measures/code")
# Exploratory: the paper's density boundary design (tasks/shared/code/density_boundary_helpers.R) with each boundary's
# more-stringent side decided by the zoning and permit measures in alderman_measures.csv and the stall rate from
# applications away from boundaries (boundary_counts.R), in place of the processing-time index. The zoning measures
# start in 2010, so aldermen who left before then have none; each ordering is compared with the paper's on the same
# buildings.
orderings <- c("stall_rate", "stall_rate_away", "days_to_passage", "fewer_high_discretion_permits",
  "fewer_low_discretion_permits")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/density_boundary_helpers.R")

measures <- read_csv("../output/alderman_measures.csv", show_col_types = FALSE) |>
  left_join(read_csv("../output/stall_rate_away_from_boundaries.csv", show_col_types = FALSE) |>
    select(alderman, stall_rate_away), by = "alderman", relationship = "one-to-one")
buildings <- read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  mutate(running_distance_ft = signed_distance_m / 0.3048) |>
  filter(abs(running_distance_ft) < density_bandwidth_ft)

average_difference <- function(data, sample) {
  data <- filter_density_sample(bin_running_distance(data), sample)
  fit <- fixest::feols(stats::as.formula(sprintf("log(density_dupac) ~ stricter_side + %s | %s", density_controls,
    density_fixed_effects)), data = data, cluster = ~ward_pair, warn = FALSE, notes = FALSE)
  tibble(estimate = coef(fit)[["stricter_side"]], std_error = fixest::se(fit)[["stricter_side"]],
    p_value = fixest::pvalue(fit)[["stricter_side"]], buildings = stats::nobs(fit),
    ward_pairs = n_distinct(data$ward_pair))
}
results <- bind_rows(lapply(orderings, function(m) {
  score <- setNames(measures[[m]], measures$alderman)
  side <- sign(score[buildings$alderman_own] - score[buildings$alderman_neighbor])
  covered <- buildings[!is.na(side) & side != 0, ]
  reordered <- mutate(covered, running_distance_ft = abs(running_distance_ft) * side[!is.na(side) & side != 0])
  bind_rows(lapply(density_samples$sample, function(s) bind_rows(
    average_difference(reordered, s) |> mutate(ordering = m),
    average_difference(covered, s) |> mutate(ordering = "processing_time_index_same_buildings")
  ) |> mutate(measure = m, sample = s, share_same_order = mean(sign(reordered$running_distance_ft) ==
    sign(covered$running_distance_ft)))))
}))
SaveData(select(results, measure, sample, ordering, everything()), c("measure", "sample", "ordering"),
  "../output/density_by_zoning_measures.csv")
