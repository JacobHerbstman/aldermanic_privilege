# setwd("tasks/audits/zoning_record_validation/code")
# The comparison of introduced and substitute ordinances (tasks/compare_substitute_ordinances) against hand reads of
# the pages drawn by draw_substitute_holdout.R (adjudication/substitute_holdout_reads.csv, one row per amendment and
# version, read from the page images only). One row per amendment and field (floor-area ratio, dwelling units,
# height): each version's value as read and as parsed, whether they agree (within agree_within; heights read in
# inches and feet), the change from introduced to substitute by each (down, same or up, with the comparison's
# thresholds), and whether the parser compared the pair and why not.
agree_within <- c(far = 0.005, units = 0, height = 0.1)
same_within <- c(far = 0.05, units = 0, height = 1)

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

reads <- read_csv("../adjudication/substitute_holdout_reads.csv", show_col_types = FALSE) |>
  rename(units = dwelling_units, height = height_ft) |>
  tidyr::pivot_longer(c(far, units, height), names_to = "field", values_to = "read") |>
  select(record_number, version, field, read, clear) |>
  tidyr::pivot_wider(names_from = version, values_from = c(read, clear), names_glue = "{version}_{.value}")
parsed <- read_csv("../input/substitute_changes.csv", show_col_types = FALSE) |>
  filter(field %in% names(agree_within)) |>
  select(record_number, field, introduced_parsed = introduced_value, substitute_parsed = substitute_value,
    parsed_change = change, not_compared)
stopifnot(n_distinct(reads$record_number) == n_distinct(read_csv("../output/substitute_holdout_pages.csv",
  show_col_types = FALSE)$record_number))

score <- function(read, parsed, field) {
  case_when(is.na(read) & is.na(parsed) ~ "neither", is.na(read) ~ "parsed_not_stated", is.na(parsed) ~ "parser_blank",
    abs(read - parsed) <= agree_within[field] ~ "agrees", TRUE ~ "differs")
}
change_of <- function(introduced, substitute, field) {
  case_when(is.na(introduced) | is.na(substitute) ~ NA_character_,
    substitute < introduced - same_within[field] ~ "down", substitute > introduced + same_within[field] ~ "up",
    TRUE ~ "same")
}
substitute_holdout_scores <- reads |>
  left_join(parsed, by = c("record_number", "field"), relationship = "one-to-one") |>
  mutate(introduced_score = score(introduced_read, introduced_parsed, field),
    substitute_score = score(substitute_read, substitute_parsed, field),
    read_change = change_of(introduced_read, substitute_read, field),
    change_agrees = if_else(is.na(parsed_change), NA, parsed_change == coalesce(read_change, "not read"))) |>
  arrange(record_number, field)
SaveData(substitute_holdout_scores, c("record_number", "field"), "../output/substitute_holdout_scores.csv")
