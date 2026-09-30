# setwd("tasks/audits/zoning_stalls_and_delays/code")
# Whether aldermen differ in how substitute ordinances change the projects in their wards more than chance allows. The
# applications are those of summarize_substitute_changes.R, of aldermen with at least min_compared compared. For each
# outcome (made smaller, made larger, changed at all) the statistic is the sum over aldermen of their applications
# times the squared difference between their share and the overall share; it is compared with permutation_draws
# reassignments of the applications among aldermen at random within strata of kind (planned development or not) and
# period of introduction. One row per outcome: aldermen, applications, the statistic, its mean under reassignment and
# the share of reassignments at least as large.
min_compared <- 10
periods <- c(2009, 2014, 2018, 2022, 2026)
permutation_draws <- 5000
permutation_seed <- 20260930

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

passed <- read_csv("../output/zoning_amendments.csv", show_col_types = FALSE) |>
  filter(source == "elms", filer == "applicant", outcome == "passed", !is.na(alderman)) |>
  select(matter_id = amendment, alderman, introduction_year)
applications <- read_csv("../input/substitute_changes.csv", show_col_types = FALSE) |>
  filter(!is.na(change)) |>
  inner_join(passed, by = "matter_id", relationship = "many-to-one") |>
  summarise(alderman = dplyr::first(alderman), introduction_year = dplyr::first(introduction_year),
    planned_development = dplyr::first(direction) == "to_planned_development",
    smaller = any(change == "down") & !any(change == "up"), larger = any(change == "up") & !any(change == "down"),
    changed = any(change != "same"), .by = matter_id) |>
  filter(n() >= min_compared, .by = alderman) |>
  mutate(stratum = paste(planned_development, cut(introduction_year, periods)))

spread <- function(alderman, y) {
  share <- tapply(y, alderman, mean)
  sum(tapply(y, alderman, length) * (share - mean(y))^2)
}
set.seed(permutation_seed)
substitute_change_tests <- bind_rows(lapply(c("smaller", "larger", "changed"), function(outcome) {
  y <- applications[[outcome]]
  draws <- replicate(permutation_draws, spread(ave(applications$alderman, applications$stratum, FUN = sample), y))
  observed <- spread(applications$alderman, y)
  tibble(outcome, aldermen = n_distinct(applications$alderman), applications = nrow(applications),
    overall_share = mean(y), statistic = observed, mean_under_reassignment = mean(draws),
    permutation_p = mean(draws >= observed))
}))
SaveData(substitute_change_tests, "outcome", "../output/substitute_change_tests.csv")
