# setwd("tasks/estimate_alderman_zoning_measures/code")
# Alderman-level measures of zoning behavior from the zoning map amendments of 2010-2026
# (tasks/clean_zoning_map_amendments, wards and aldermen from tasks/assign_zoning_amendment_wards):
#   stall: whether a decided application (one not filed by an alderman) in the alderman's ward stalled, lapsing
#     without a vote;
#   days to passage: log days from introduction to passage of passed applications;
#   downzonings: downzoning amendments the alderman filed, per year in office.
# The first two follow the stringency index: each application is adjusted for introduction year, the kind of change
# (up, down, same floor-area ratio, to a planned development, unknown) and the time left in the council term when it
# was introduced (applications introduced in a term's last months lapse with it far more often), the adjusted values
# are averaged by alderman, and each average is shrunk toward zero by empirical Bayes according to its sampling
# variance. Whether an application stalled is known only for those introduced before the current term, so the stall
# measure uses those. Downzoning rates are shrunk toward the mean rate in the same way. Estimates are also made from
# the earlier and later halves of each alderman's applications, to check whether they persist.
data_start <- as.Date("2010-02-01")
data_end <- as.Date("2026-09-27")
current_term_start <- as.Date("2023-05-15")
# Council terms begin on these dates; months left in the term are grouped as below.
term_starts <- as.Date(c("2011-05-16", "2015-05-18", "2019-05-20", "2023-05-15", "2027-05-17"))
months_left_breaks <- c(0, 6, 12, 24, 48)

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

amendments <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  inner_join(read_csv("../input/zoning_amendment_wards.csv", show_col_types = FALSE) |> select(matter_id, ward, alderman),
    by = "matter_id", relationship = "one-to-one") |>
  mutate(introduction_year = as.integer(format(introduction_date, "%Y")),
    next_term_start = term_starts[findInterval(introduction_date, term_starts) + 1L],
    months_left_in_term = cut(as.numeric(next_term_start - introduction_date) / 30.44, months_left_breaks))
stopifnot(!anyNA(amendments$months_left_in_term))
applications <- amendments |>
  filter(!filed_by_alderman, !is.na(alderman), outcome != "pending") |>
  mutate(stalled = if_else(introduction_date < current_term_start, as.numeric(outcome == "stalled"), NA_real_),
    log_days_to_passage = if_else(outcome == "passed" & days_to_passage > 0, log(pmax(days_to_passage, 1)), NA_real_))

shrink <- function(estimate, variance) {
  signal_variance <- max(stats::var(estimate) - mean(variance), 0)
  signal_variance / (signal_variance + variance) * estimate
}
# Mean adjusted value by alderman and its shrunk version; the residual variance is pooled across aldermen.
alderman_effects <- function(data, outcome) {
  data <- filter(data, is.finite(.data[[outcome]]))
  adjustment <- fixest::feols(stats::as.formula(paste(outcome, "~ 1 | introduction_year + direction + months_left_in_term")),
    data = data, notes = FALSE)
  data$adjusted <- stats::residuals(adjustment)
  data |>
    summarise(applications = n(), effect = mean(adjusted), .by = alderman) |>
    mutate(variance = stats::var(data$adjusted) / applications, shrunk_effect = shrink(effect, variance))
}

stall <- alderman_effects(applications, "stalled")
days <- alderman_effects(applications, "log_days_to_passage")

# Downzonings each alderman filed, per year served within the data window.
terms <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE) |>
  mutate(days_in_window = as.numeric(pmin(end_date, data_end) - pmax(start_date, data_start)) + 1) |>
  filter(days_in_window > 0) |>
  summarise(years_in_office = sum(days_in_window) / 365.25, .by = alderman)
downzonings <- terms |>
  left_join(amendments |> filter(filed_by_alderman, direction == "down") |> count(alderman, name = "downzonings"),
    by = "alderman", relationship = "one-to-one") |>
  mutate(downzonings = coalesce(downzonings, 0L), rate = downzonings / years_in_office)
mean_rate <- sum(downzonings$downzonings) / sum(downzonings$years_in_office)
downzonings <- downzonings |>
  mutate(shrunk_rate = mean_rate + shrink(rate - mean_rate, mean_rate / years_in_office))
stopifnot(all(amendments$alderman[amendments$filed_by_alderman & amendments$direction == "down"] %in%
  c(terms$alderman, NA)))

measures <- terms |>
  select(alderman, years_in_office) |>
  left_join(stall |> transmute(alderman, applications_decided = applications, stall_effect = effect,
    stall_se = sqrt(variance), stall_shrunk = shrunk_effect), by = "alderman", relationship = "one-to-one") |>
  left_join(days |> transmute(alderman, applications_passed = applications, days_effect = effect,
    days_se = sqrt(variance), days_shrunk = shrunk_effect), by = "alderman", relationship = "one-to-one") |>
  left_join(downzonings |> select(alderman, downzonings, downzoning_rate = rate, downzoning_rate_shrunk = shrunk_rate),
    by = "alderman", relationship = "one-to-one")
SaveData(measures, "alderman", "../output/alderman_zoning_measures.csv")

# Persistence: the same measures from the earlier and later half of each alderman's applications by date.
applications <- applications |>
  arrange(alderman, introduction_date, matter_id) |>
  mutate(half = if_else(row_number() <= n() / 2, "earlier", "later"), .by = alderman)
halves <- bind_rows(lapply(c(earlier = "earlier", later = "later"), function(which_half) {
  half <- filter(applications, half == which_half)
  bind_rows(
    alderman_effects(half, "stalled") |> mutate(measure = "stall"),
    alderman_effects(half, "log_days_to_passage") |> mutate(measure = "days_to_passage")
  ) |>
    select(measure, alderman, applications, effect)
}), .id = "years") |>
  tidyr::pivot_wider(names_from = years, values_from = c(applications, effect))
SaveData(halves, c("measure", "alderman"), "../output/alderman_zoning_measures_by_half.csv")
