# setwd("tasks/audits/zoning_record_validation/code")
# The Journals' outcomes of zoning map amendments (tasks/link_journal_zoning_outcomes) against eLMS's
# (tasks/clean_zoning_map_amendments), for amendments introduced from November 2010, when eLMS begins, through 2011.
# An introduction and an eLMS amendment are the same if the record number the Journal prints matches one of the
# amendment's records (a substitute, "SO2010-5177", keeps its original's number). The Journals read end in December
# 2011, so eLMS's outcome is taken as of the last meeting read: passed if it passed by then, withdrawn, placed on file
# or failed if its final action came by then, and otherwise undecided, as are the Journals' stalled and pending
# introductions. One row per Journal introduction of the period.
first_date <- as.Date("2010-11-01")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

journal <- read_csv("../input/journal_zoning_outcomes.csv", col_types = cols(.default = col_character())) |>
  mutate(introduction_date = as.Date(introduction_date), outcome_date = as.Date(outcome_date))
last_meeting <- max(journal$introduction_date, journal$outcome_date, na.rm = TRUE)
journal <- journal |>
  filter(introduction_date >= first_date) |>
  transmute(file, position, introduction_date, filer, record_number, record = str_remove(record_number, "^S"),
    journal_outcome = if_else(outcome %in% c("stalled", "pending"), "undecided", outcome),
    journal_date = outcome_date, link_basis)
elms <- read_csv("../input/zoning_map_amendments.csv", col_types = cols(.default = col_character())) |>
  mutate(introduction_date = as.Date(introduction_date), final_action_date = as.Date(final_action_date),
    passed_date = as.Date(passed_date))
records <- elms |>
  separate_longer_delim(record_numbers, ";") |>
  transmute(record = str_remove(record_numbers, "^S"), matter_id) |>
  distinct()
stopifnot(!anyDuplicated(records$record))
elms <- elms |>
  transmute(matter_id, elms_record = record_number, elms_introduction_date = introduction_date,
    elms_outcome = case_when(
      outcome == "passed" & passed_date <= last_meeting ~ "passed",
      outcome %in% c("withdrawn", "placed_on_file", "failed") & final_action_date <= last_meeting ~ outcome,
      TRUE ~ "undecided"),
    elms_date = case_when(elms_outcome == "passed" ~ passed_date, elms_outcome != "undecided" ~ final_action_date))

comparison <- journal |>
  left_join(records, by = "record", relationship = "many-to-one") |>
  left_join(elms, by = "matter_id", relationship = "many-to-one") |>
  mutate(introduction = paste0(file, "#", position), status = if_else(is.na(matter_id), "journal_only", "matched"),
    outcome_agrees = if_else(is.na(matter_id), NA, journal_outcome == elms_outcome),
    date_agrees = if_else(is.na(matter_id) | is.na(journal_date) | is.na(elms_date), NA, journal_date == elms_date)) |>
  select(introduction, status, file, position, introduction_date, filer, record_number, matter_id, elms_record,
    elms_introduction_date, journal_outcome, elms_outcome, journal_date, elms_date, link_basis, outcome_agrees,
    date_agrees) |>
  arrange(status, introduction_date, file, as.integer(position))
stopifnot(!anyDuplicated(comparison$matter_id[!is.na(comparison$matter_id)]))
SaveData(comparison, "introduction", "../output/outcomes_vs_elms.csv")
