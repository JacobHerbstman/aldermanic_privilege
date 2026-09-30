# setwd("tasks/audits/zoning_stalls_and_delays/code")
# Stalls and delays of zoning map amendment applications by source and year of introduction (combine_amendments.R,
# applications only): the share that stalled and that stalled for good (no refiling passed), for years whose
# applications' fates are all known (through 2022); the share not passed within the delay window, among applications
# with that much follow-up (through the first half of 2026); both with 95 percent binomial intervals; and the mean,
# median and quartiles of days from introduction to passage among those that passed, for applications with a year of
# follow-up.

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

applications <- read_csv("../output/zoning_amendments.csv", show_col_types = FALSE) |>
  filter(filer == "applicant")
by_year <- applications |>
  summarise(applications = n(), stall_rate = mean(stalled), stalled_for_good_rate = mean(stalled_for_good),
    followed = sum(!is.na(not_passed_in_window)), not_passed_in_window_rate = mean(not_passed_in_window, na.rm = TRUE),
    passed = sum(!is.na(days_to_passage)), mean_days = mean(days_to_passage, na.rm = TRUE),
    median_days = median(days_to_passage, na.rm = TRUE), days_q25 = quantile(days_to_passage, 0.25, na.rm = TRUE),
    days_q75 = quantile(days_to_passage, 0.75, na.rm = TRUE), .by = c(source, introduction_year)) |>
  mutate(stall_ci_low = pmax(stall_rate - 1.96 * sqrt(stall_rate * (1 - stall_rate) / applications), 0),
    stall_ci_high = stall_rate + 1.96 * sqrt(stall_rate * (1 - stall_rate) / applications),
    window_ci_low = not_passed_in_window_rate - 1.96 * sqrt(not_passed_in_window_rate *
      (1 - not_passed_in_window_rate) / followed),
    window_ci_high = not_passed_in_window_rate + 1.96 * sqrt(not_passed_in_window_rate *
      (1 - not_passed_in_window_rate) / followed)) |>
  arrange(source, introduction_year)
SaveData(by_year, c("source", "introduction_year"), "../output/stalls_and_delays_by_year.csv")
