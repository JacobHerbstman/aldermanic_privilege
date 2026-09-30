# setwd("tasks/audits/zoning_stalls_and_delays/code")
# How the substitute ordinances of passed eLMS applications changed the projects introduced
# (tasks/compare_substitute_ordinances), by the alderman of the ward the site lies in (combine_amendments.R), one row
# per alderman. An application counts if the introduced and substitute versions are compared on at least one of the
# district after's highest floor-area ratio and the project's stated floor-area ratio, dwelling units and height. It
# was made smaller if some compared field fell and none rose, larger if some rose and none fell, mixed if some did
# each, and kept the same otherwise. For each alderman: applications passed, those compared (and of them planned
# developments), the counts and shares made smaller and larger with the shares' standard errors, and the mean log
# change of dwelling units, height and stated floor-area ratio over the applications where each is compared.

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

passed <- read_csv("../output/zoning_amendments.csv", show_col_types = FALSE) |>
  filter(source == "elms", filer == "applicant", outcome == "passed", !is.na(alderman)) |>
  select(matter_id = amendment, alderman)
changes <- read_csv("../input/substitute_changes.csv", show_col_types = FALSE) |>
  filter(!is.na(change)) |>
  inner_join(passed, by = "matter_id", relationship = "many-to-one") |>
  mutate(log_change = if_else(introduced_value > 0 & substitute_value > 0, log(substitute_value / introduced_value),
    NA_real_))
by_application <- changes |>
  summarise(alderman = dplyr::first(alderman),
    planned_development = dplyr::first(direction) == "to_planned_development",
    project_change = case_when(any(change == "down") & any(change == "up") ~ "mixed", any(change == "down") ~ "smaller",
      any(change == "up") ~ "larger", TRUE ~ "same"),
    log_units = dplyr::first(log_change[field == "units"], default = NA),
    log_height = dplyr::first(log_change[field == "height"], default = NA),
    log_far = dplyr::first(log_change[field == "far"], default = NA), .by = matter_id)
stopifnot(!anyDuplicated(by_application$matter_id))

share_se <- function(k, n) sqrt((k / n) * (1 - k / n) / n)
substitute_changes_by_alderman <- passed |>
  count(alderman, name = "applications_passed") |>
  left_join(by_application |>
    summarise(compared = n(), planned_developments = sum(planned_development),
      smaller = sum(project_change == "smaller"), larger = sum(project_change == "larger"),
      mixed = sum(project_change == "mixed"),
      mean_log_units = mean(log_units, na.rm = TRUE), mean_log_height = mean(log_height, na.rm = TRUE),
      mean_log_far = mean(log_far, na.rm = TRUE), .by = alderman),
    by = "alderman", relationship = "one-to-one") |>
  mutate(compared = coalesce(compared, 0L),
    share_smaller = smaller / compared, share_smaller_se = share_se(smaller, compared),
    share_larger = larger / compared, share_larger_se = share_se(larger, compared),
    across(starts_with("mean_log"), ~ if_else(is.nan(.x), NA_real_, .x))) |>
  arrange(desc(compared))
SaveData(substitute_changes_by_alderman, "alderman", "../output/substitute_changes_by_alderman.csv")
