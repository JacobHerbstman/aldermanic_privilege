# setwd("tasks/audits/zoning_record_validation/code")
# The Journals' withdrawals of zoning map amendments, from November 2010 through 2011
# (tasks/parse_journal_zoning_amendments: ordinances the Council withdrew on reports of the Committee on Zoning, and
# applications a report notes as withdrawn), against the outcome eLMS records for the same amendment
# (tasks/clean_zoning_map_amendments). An amendment is found in eLMS by its numeric application number, its record
# number, or an aldermen's "A-" number printed in its eLMS title. One row per Journal withdrawal.
first_date <- as.Date("2010-11-01")
last_date <- as.Date("2011-12-31")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

withdrawals <- bind_rows(
  bind_rows(
    read_csv("../input/journal_ordinances_2010.csv", col_types = cols(.default = col_character())),
    read_csv("../input/journal_ordinances_2011.csv", col_types = cols(.default = col_character()))
  ) |>
    filter(action == "withdrawn") |>
    transmute(meeting_date, file, application_number, record_number, source = "withdrawn ordinance"),
  bind_rows(
    read_csv("../input/journal_report_notes_2010.csv", col_types = cols(.default = col_character())),
    read_csv("../input/journal_report_notes_2011.csv", col_types = cols(.default = col_character()))
  ) |>
    filter(note == "withdrawn") |>
    transmute(meeting_date, file, application_number, record_number = NA_character_, source = "report note")
) |>
  mutate(meeting_date = as.Date(meeting_date)) |>
  filter(meeting_date >= first_date, meeting_date <= last_date) |>
  # A withdrawal both noted and printed is kept once, as the printed ordinance, which carries its record number.
  arrange(meeting_date, application_number, source != "withdrawn ordinance") |>
  distinct(meeting_date, application_number, .keep_all = TRUE)

elms <- read_csv("../input/zoning_map_amendments.csv", col_types = cols(.default = col_character())) |>
  transmute(matter_id, elms_record = record_number, record_numbers, title, elms_application = application_number,
    introduction_date = as.Date(introduction_date), final_action_date, outcome)
# An application number can recur when an application is filed again: the amendment introduced last before the
# meeting is the one withdrawn.
by_number <- elms |> filter(!is.na(elms_application)) |>
  select(elms_application, elms_introduced = introduction_date, by_application = matter_id)
by_record <- elms |> separate_longer_delim(record_numbers, ";") |>
  transmute(record = str_remove(record_numbers, "^S"), by_record = matter_id)
# An aldermen's "A-" number names a series shared by several ordinances, so only one printed in a single eLMS title
# is used.
by_title <- elms |> mutate(a_number = str_extract(title, "A-[0-9]{4}")) |> filter(!is.na(a_number)) |>
  add_count(a_number) |> filter(n == 1) |> select(a_number, by_title = matter_id)
stopifnot(!anyDuplicated(by_record$record))

comparison <- withdrawals |>
  mutate(record = str_remove(record_number, "^S")) |>
  left_join(by_number, by = join_by(application_number == elms_application, closest(meeting_date >= elms_introduced)),
    relationship = "many-to-one") |>
  left_join(by_record, by = "record", relationship = "many-to-one") |>
  left_join(by_title, by = c(application_number = "a_number"), relationship = "many-to-one") |>
  mutate(matter_id = coalesce(by_application, by_record, by_title),
    match_rule = case_when(!is.na(by_application) ~ "application_number", !is.na(by_record) ~ "record_number",
      !is.na(by_title) ~ "title_a_number")) |>
  select(-record, -elms_introduced, -by_application, -by_record, -by_title) |>
  left_join(elms |> select(matter_id, elms_record, introduction_date, final_action_date, outcome), by = "matter_id",
    relationship = "many-to-one") |>
  mutate(ordinance = paste(file, application_number, sep = "#"))
SaveData(comparison, "ordinance", "../output/withdrawals_vs_elms.csv")
