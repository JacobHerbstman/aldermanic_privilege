# setwd("tasks/audits/zoning_stalls_and_delays/code")
# Stalls and days to passage of zoning map amendment applications by the alderman of the ward they lie in
# (combine_amendments.R, applications only), one row per alderman and period: the Journals for introductions of
# 2000--2010, eLMS for 2011 to September 2026 (so that 2011 is counted once), and the two combined. Stalls are known
# only for applications introduced before the current term (May 15, 2023); the share not passed within the delay
# window of combine_amendments.R covers applications introduced through the first half of 2026, including the current
# term's. Rates are raw shares, with the binomial standard error of the stall rate. stall_vs_citywide is the
# alderman's stall rate less the citywide rate of the same years, weighting each year by the alderman's applications
# in it (stalls were more common in the 2000s); days_vs_citywide is the same comparison of log days to passage, as a
# percentage. For the Journals, the applications and stall rate by the alderman of the 2003 ward map, in which
# aldermen filed from its adoption on December 19, 2001, are also given (the other columns follow the map in force at
# introduction). The adjusted measures follow tasks/estimate_alderman_zoning_measures, within each period: whether an
# application stalled, whether it was not passed within the window (window_adjusted), and its log days to passage if
# it passed, are adjusted for its year of introduction, the kind of change (direction, combine_amendments.R) and the
# months left in the council term when it was introduced; the adjusted values are averaged by alderman
# (stall_adjusted, in shares, and days_adjusted, in log days, with standard errors from the pooled residual variance),
# and each average is shrunk toward zero by empirical Bayes according to its sampling variance; the variance of the
# true effects is estimated from, and shrunk measures given for, the aldermen with at least min_applications
# applications. The kind of change is interacted with the source, since the two sources read districts differently
# (the kind is unknown far more often in eLMS); in the combined period the years absorb the difference in stall rates
# between the 2000s and the 2010s.
journal_years <- 2000:2010
term_starts <- as.Date(c("1999-05-03", "2003-05-05", "2007-05-21", "2011-05-16", "2015-05-18", "2019-05-20",
  "2023-05-15", "2027-05-17"))
months_left_breaks <- c(0, 6, 12, 24, Inf)
min_applications <- 20

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

applications <- read_csv("../output/zoning_amendments.csv", show_col_types = FALSE) |>
  filter(filer == "applicant") |>
  filter(source == "elms" | introduction_year %in% journal_years) |>
  mutate(period = if_else(source == "journals", "2000-2010 Journals", "2011-2026 eLMS"))
applications <- bind_rows(applications, mutate(applications, period = "2000-2026 combined")) |>
  mutate(log_days = log(days_to_passage),
    months_left_in_term = cut(as.numeric(term_starts[findInterval(introduction_date, term_starts) + 1L] -
      introduction_date) / 30.44, months_left_breaks)) |>
  mutate(citywide_stall_rate = mean(stalled, na.rm = TRUE), citywide_log_days = mean(log_days, na.rm = TRUE),
    .by = c(period, introduction_year))
stopifnot(!anyNA(applications$months_left_in_term))

# The variance of the aldermen's true effects is estimated from those with shrunk measures (used): aldermen with a few
# applications at the edge of a period, often lapsing with a term, would otherwise inflate it.
shrink <- function(estimate, variance, used) {
  signal_variance <- max(stats::var(estimate[used]) - mean(variance[used]), 0)
  if_else(used, signal_variance / (signal_variance + variance) * estimate, NA_real_)
}
# Mean adjusted value by alderman (column by) within each period, and its shrunk version.
alderman_effects <- function(outcome, by) {
  bind_rows(lapply(split(applications, applications$period), function(data) {
    data <- filter(data, is.finite(.data[[outcome]]), !is.na(.data[[by]]))
    adjustment <- fixest::feols(stats::as.formula(paste(outcome,
      "~ 1 | introduction_year + direction^source + months_left_in_term")), data = data, notes = FALSE)
    data$adjusted <- stats::residuals(adjustment)
    data |>
      summarise(applications = n(), effect = mean(adjusted), .by = c(period, all_of(by))) |>
      mutate(se = sqrt(stats::var(data$adjusted) / applications),
        shrunk = shrink(effect, se^2, applications >= min_applications)) |>
      rename(alderman = all_of(by))
  }))
}
applications <- mutate(applications, stalled = as.numeric(stalled),
  not_passed_in_window = as.numeric(not_passed_in_window))
stall <- alderman_effects("stalled", "alderman")
window <- alderman_effects("not_passed_in_window", "alderman")
days <- alderman_effects("log_days", "alderman")
stall_redrawn <- alderman_effects("stalled", "redrawn_alderman") |> filter(period == "2000-2010 Journals")

by_alderman <- applications |>
  filter(!is.na(alderman)) |>
  summarise(wards = paste(sort(unique(ward)), collapse = ";"), first_year = min(introduction_year),
    last_year = max(introduction_year), applications = n(), stall_applications = sum(!is.na(stalled)),
    stall_rate = mean(stalled, na.rm = TRUE), stall_se = sqrt(stall_rate * (1 - stall_rate) / stall_applications),
    stall_vs_citywide = mean(stalled - citywide_stall_rate, na.rm = TRUE),
    stalled_for_good_rate = mean(stalled_for_good, na.rm = TRUE),
    not_passed_in_window_rate = mean(not_passed_in_window, na.rm = TRUE),
    passed = sum(!is.na(days_to_passage)), median_days = median(days_to_passage, na.rm = TRUE),
    days_vs_citywide = exp(mean(log_days - citywide_log_days, na.rm = TRUE)) - 1, .by = c(period, alderman))
redrawn <- applications |>
  filter(period == "2000-2010 Journals", !is.na(redrawn_alderman)) |>
  summarise(applications_redrawn_map = n(), stall_rate_redrawn_map = mean(stalled),
    .by = c(period, redrawn_alderman)) |>
  rename(alderman = redrawn_alderman)
by_alderman <- by_alderman |>
  left_join(stall |>
    transmute(period, alderman, stall_adjusted = effect, stall_adjusted_se = se, stall_shrunk = shrunk),
    by = c("period", "alderman"), relationship = "one-to-one") |>
  left_join(window |> transmute(period, alderman, window_adjusted = effect, window_adjusted_se = se,
    window_shrunk = shrunk), by = c("period", "alderman"), relationship = "one-to-one") |>
  left_join(days |> transmute(period, alderman, days_adjusted = effect, days_adjusted_se = se, days_shrunk = shrunk),
    by = c("period", "alderman"), relationship = "one-to-one") |>
  full_join(redrawn |> left_join(stall_redrawn |> transmute(period, alderman, stall_shrunk_redrawn_map = shrunk),
    by = c("period", "alderman"), relationship = "one-to-one"), by = c("period", "alderman"),
    relationship = "one-to-one") |>
  arrange(period, desc(stall_vs_citywide))
SaveData(by_alderman, c("period", "alderman"), "../output/stalls_and_delays_by_alderman.csv")
