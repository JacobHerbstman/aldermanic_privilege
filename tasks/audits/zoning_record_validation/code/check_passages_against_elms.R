# setwd("tasks/audits/zoning_record_validation/code")
# The Journals' passed zoning ordinances (tasks/parse_journal_zoning_amendments, the ordinances the Council passed on
# reports of the Committee on Zoning) against eLMS's passed amendments
# (tasks/clean_zoning_map_amendments), from November 2010, when eLMS begins to record the Council's matters, through
# 2011. A Journal passage and an eLMS amendment are the same ordinance if the record number the Journal prints matches
# one of the amendment's records (a substitute, "SO2010-5177", keeps its original's number), or else if they carry the
# same numeric application number; the remaining passages are matched on the meeting date and map sheet where exactly
# one of each side has them. For matched ordinances the Journal's date, map and districts are
# compared with eLMS's passage date, map and ordinance sentences (the passed ordinance's own text, as the Journal
# prints it; eLMS codes a final planned development separately). One row per matched pair and per unmatched
# ordinance on either side, keyed by the eLMS matter or, for a Journal passage eLMS lacks, its file and position.
first_date <- as.Date("2010-11-01")
last_date <- as.Date("2011-12-31")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

journal <- bind_rows(
  read_csv("../input/journal_ordinances_2010.csv", col_types = cols(.default = col_character())),
  read_csv("../input/journal_ordinances_2011.csv", col_types = cols(.default = col_character()))
) |>
  filter(action == "passed") |>
  mutate(meeting_date = as.Date(meeting_date)) |>
  filter(meeting_date >= first_date, meeting_date <= last_date) |>
  transmute(file, position, meeting_date, journal_page = page, application_number, journal_map = map_number,
    journal_from = from_districts, journal_to = to_districts, record = str_remove(record_number, "^S"))
elms <- read_csv("../input/zoning_map_amendments.csv", col_types = cols(.default = col_character())) |>
  mutate(passed_date = as.Date(passed_date))
records <- elms |>
  separate_longer_delim(record_numbers, ";") |>
  transmute(record = str_remove(record_numbers, "^S"), matter_id) |>
  distinct()
stopifnot(!anyDuplicated(records$record))
elms <- elms |>
  filter(passed_date >= first_date, passed_date <= last_date) |>
  transmute(matter_id, record_number, passed_date, elms_application = application_number, elms_map = map_number,
    elms_from = sentence_from,
    elms_to = if_else(to_planned_development %in% "TRUE", "PD", sentence_to))

# 1. Record numbers.
by_record <- journal |>
  inner_join(records, by = "record", relationship = "many-to-one") |>
  inner_join(elms, by = "matter_id", relationship = "many-to-one") |>
  mutate(match_rule = "record_number")
stopifnot(!anyDuplicated(by_record$matter_id))

# 2. Numeric application numbers, unique on each side.
numeric_journal <- journal |>
  anti_join(by_record, by = c("file", "position")) |>
  filter(str_detect(application_number, "^[0-9]+$"))
stopifnot(!anyDuplicated(numeric_journal$application_number), !anyDuplicated(na.omit(elms$elms_application)))
by_number <- inner_join(numeric_journal, filter(anti_join(elms, by_record, by = "matter_id"), !is.na(elms_application)),
  by = c(application_number = "elms_application"), relationship = "one-to-one") |>
  mutate(elms_application = application_number, match_rule = "application_number")

# 3. The rest, on meeting date and map sheet where that pair is unique on both sides.
rest_journal <- anti_join(journal, bind_rows(by_record, by_number), by = c("file", "position")) |>
  filter(!is.na(journal_map)) |>
  add_count(meeting_date, journal_map) |> filter(n == 1) |> select(-n)
rest_elms <- anti_join(elms, bind_rows(by_record, by_number), by = "matter_id") |> filter(!is.na(elms_map)) |>
  add_count(passed_date, elms_map) |> filter(n == 1) |> select(-n)
by_date_map <- inner_join(rest_journal, rest_elms, by = c(meeting_date = "passed_date", journal_map = "elms_map"),
  relationship = "one-to-one") |>
  mutate(passed_date = meeting_date, elms_map = journal_map, match_rule = "date_and_map")

matched <- bind_rows(by_record, by_number, by_date_map)
agrees <- function(journal_value, elms_value) if_else(is.na(journal_value) | is.na(elms_value), NA,
  journal_value == elms_value)
comparison <- bind_rows(
  matched |> mutate(status = "matched"),
  anti_join(journal, matched, by = c("file", "position")) |> mutate(status = "journal_only"),
  anti_join(elms, matched, by = "matter_id") |> mutate(status = "elms_only")
) |>
  mutate(date_agrees = agrees(as.character(meeting_date), as.character(passed_date)),
    map_agrees = agrees(journal_map, elms_map), from_agrees = agrees(journal_from, elms_from),
    to_agrees = agrees(journal_to, elms_to)) |>
  mutate(ordinance = coalesce(matter_id, paste0(file, "#", position))) |>
  select(ordinance, status, match_rule, file, position, journal_page, matter_id, record_number, meeting_date,
    passed_date,
    application_number, elms_application, journal_map, elms_map, journal_from, elms_from, journal_to, elms_to,
    date_agrees, map_agrees, from_agrees, to_agrees) |>
  arrange(status, coalesce(meeting_date, passed_date), file, as.integer(position))
stopifnot(nrow(filter(comparison, status == "matched")) == nrow(matched))
SaveData(comparison, "ordinance", "../output/passages_vs_elms.csv")
