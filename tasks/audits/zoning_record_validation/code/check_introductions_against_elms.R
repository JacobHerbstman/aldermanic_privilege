# setwd("tasks/audits/zoning_record_validation/code")
# The Journals' introductions of zoning map amendments (tasks/parse_journal_zoning_amendments) against eLMS's amendments
# (tasks/clean_zoning_map_amendments), from November 2010, when eLMS begins to record the Council's matters, through
# 2011. An introduction and an amendment are the same if the record number the Journal prints matches one of the
# amendment's records; a substitute ordinance ("SO2010-5177") keeps its original's number. For matched pairs the
# Journal's meeting date, application number, map sheet and districts are compared with eLMS's introduction date and
# readings. One row per Journal introduction and per eLMS amendment introduced in the period that no introduction
# matches.
first_date <- as.Date("2010-11-01")
last_date <- as.Date("2011-12-31")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

journal <- bind_rows(
  read_csv("../input/journal_introductions_2010.csv", col_types = cols(.default = col_character())),
  read_csv("../input/journal_introductions_2011.csv", col_types = cols(.default = col_character()))
) |>
  mutate(meeting_date = as.Date(meeting_date)) |>
  filter(meeting_date >= first_date, meeting_date <= last_date) |>
  transmute(file, position, meeting_date, journal_page = page, filer, record_number, application_number,
    journal_map = map_number, journal_from = from_districts, journal_to = to_districts,
    record = str_remove(record_number, "^S"))
elms <- read_csv("../input/zoning_map_amendments.csv", col_types = cols(.default = col_character())) |>
  mutate(introduction_date = as.Date(introduction_date))
records <- elms |>
  separate_longer_delim(record_numbers, ";") |>
  transmute(record = str_remove(record_numbers, "^S"), matter_id) |>
  distinct()
stopifnot(!anyDuplicated(records$record))
elms <- elms |>
  transmute(matter_id, elms_record = record_number, introduction_date, elms_application = application_number,
    elms_map = map_number, elms_from = from_districts, elms_to = to_districts)

matched <- journal |>
  inner_join(records, by = "record", relationship = "many-to-one") |>
  inner_join(elms, by = "matter_id", relationship = "many-to-one")
stopifnot(!anyDuplicated(matched$matter_id))
agrees <- function(journal_value, elms_value) if_else(is.na(journal_value) | is.na(elms_value), NA,
  journal_value == elms_value)
comparison <- bind_rows(
  matched |> mutate(status = "matched"),
  anti_join(journal, matched, by = c("file", "position")) |> mutate(status = "journal_only"),
  elms |> filter(introduction_date >= first_date, introduction_date <= last_date) |>
    anti_join(matched, by = "matter_id") |> mutate(status = "elms_only")
) |>
  mutate(ordinance = coalesce(matter_id, paste0(file, "#", position)),
    date_agrees = agrees(as.character(meeting_date), as.character(introduction_date)),
    application_agrees = agrees(application_number, elms_application), map_agrees = agrees(journal_map, elms_map),
    from_agrees = agrees(journal_from, elms_from), to_agrees = agrees(journal_to, elms_to)) |>
  select(ordinance, status, file, position, journal_page, filer, matter_id, record_number, elms_record, meeting_date,
    introduction_date, application_number, elms_application, journal_map, elms_map, journal_from, elms_from,
    journal_to, elms_to, date_agrees, application_agrees, map_agrees, from_agrees, to_agrees) |>
  arrange(status, coalesce(meeting_date, introduction_date), file, as.integer(position))
SaveData(comparison, "ordinance", "../output/introductions_vs_elms.csv")
