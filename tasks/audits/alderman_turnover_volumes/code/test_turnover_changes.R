# setwd("tasks/audits/alderman_turnover_volumes/code")
# Whether a ward's zoning amendments, permits and new buildings change more across an election when its alderman
# changes than when the same alderman stays (count_ward_terms.R). The elections compared are those between two terms
# on one ward map: 2007 and 2011 (2003 map) and 2019 (2015 map). The alderman changes if the one who held the ward
# for most of the term after differs from the one before. For each ward, election and measure, the count after is
# compared with its share of the ward's counts before and after under the citywide split (or the split of the ward's
# side of the city), in standard deviations of counting noise: z = (after - n p) / sqrt(s p (1 - p)), where n is the
# ward's count before and after, p the share after among all wards of the election (or side), and s the sum of the
# squared sizes of the items counted: n for counts, and for dwelling units the sum of each new building's units
# squared, since units arrive a building at a time. Without ward-specific change z squared averages 1; its mean over
# wards measures how much wards' counts shift beyond counting noise. One row per ward, election and measure
# (ward_election_changes.csv), and per measure and comparison the mean z squared of wards whose alderman changed and
# of those whose alderman stayed, with the share of permutation_draws random reassignments of the changes among the
# wards of each election (and side) giving a gap at least as large (turnover_change_tests.csv). Wards with no counts
# in either term are left out.
same_map_elections <- as.Date(c("2007-05-21", "2011-05-16", "2019-05-20"))
permutation_draws <- 5000
permutation_seed <- 20260929

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

ward_terms <- read_csv("../output/ward_term_counts.csv", show_col_types = FALSE)
measures <- c("applications", "own_amendments", "own_downzonings", "permits", "new_construction", "new_buildings",
  "new_units")
before <- ward_terms |>
  mutate(election = lead(term_start), .by = ward) |>
  filter(election %in% same_map_elections)
after <- ward_terms |>
  filter(term_start %in% same_map_elections) |>
  rename(election = term_start)
pairs <- inner_join(before, after, by = c("election", "ward"), suffix = c("_before", "_after"),
  relationship = "one-to-one")
stopifnot(nrow(pairs) == 150, all(pairs$map_version_before == pairs$map_version_after),
  all(pairs$side_before == pairs$side_after))

changes <- bind_rows(lapply(measures, function(m) {
  squares <- if (m == "new_units") pairs$new_units_squared_before + pairs$new_units_squared_after else
    pairs[[paste0(m, "_before")]] + pairs[[paste0(m, "_after")]]
  pairs |>
    transmute(election, ward, side = side_before, alderman_before, alderman_after,
      turnover = alderman_before != alderman_after, measure = m, count_before = .data[[paste0(m, "_before")]],
      count_after = .data[[paste0(m, "_after")]], squares = squares)
})) |>
  mutate(n = count_before + count_after) |>
  mutate(share_after_citywide = sum(count_after) / sum(n), .by = c(measure, election)) |>
  mutate(share_after_side = sum(count_after) / sum(n), .by = c(measure, election, side)) |>
  mutate(
    z_citywide = (count_after - n * share_after_citywide) / sqrt(squares * share_after_citywide *
      (1 - share_after_citywide)),
    z_side = (count_after - n * share_after_side) / sqrt(squares * share_after_side * (1 - share_after_side)))
SaveData(select(changes, -n, -squares), c("measure", "election", "ward"), "../output/ward_election_changes.csv")

set.seed(permutation_seed)
tests <- bind_rows(lapply(measures, function(m) bind_rows(lapply(c("citywide", "side"), function(comparison) {
  d <- changes |>
    filter(measure == m, n > 0) |>
    mutate(z2 = .data[[paste0("z_", comparison)]]^2, stratum = if (comparison == "side") paste(election, side) else
      as.character(election)) |>
    filter(is.finite(z2))
  gap <- function(turnover) mean(d$z2[turnover]) - mean(d$z2[!turnover])
  draws <- replicate(permutation_draws, gap(as.logical(ave(d$turnover, d$stratum, FUN = sample))))
  tibble(measure = m, comparison, wards_turnover = sum(d$turnover), wards_same = sum(!d$turnover),
    z2_turnover = mean(d$z2[d$turnover]), z2_same = mean(d$z2[!d$turnover]),
    permutation_p = mean(draws >= gap(d$turnover)))
}))))
SaveData(tests, c("measure", "comparison"), "../output/turnover_change_tests.csv")
