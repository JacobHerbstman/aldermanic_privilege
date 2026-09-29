# setwd("tasks/audits/zoning_record_validation/code")
# Whether eLMS (tasks/download_elms_matters) holds the zoning amendments the Journals list before November 2010, or
# our selection of zoning matters missed them: every matter of any kind in the eLMS index by month of introduction,
# 2010-2011, and, by month, how many of the record numbers the Journals print for introductions
# (tasks/parse_journal_zoning_amendments) are anywhere in the index, under any title or category, as a record number
# or a legacy record number (a substitute ordinance's "SO" number counts for its original's "O" number).
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

field <- function(matter, name) matter[[name]] %||% NA_character_
index <- unlist(lapply(readLines("../input/elms_matter_index.jsonl"), function(page) {
  jsonlite::fromJSON(page, simplifyVector = FALSE)$data
}), recursive = FALSE)
matters <- tibble(record_number = vapply(index, field, character(1), name = "recordNumber"),
  legacy_record_number = vapply(index, field, character(1), name = "legacyRecordNumber"),
  introduction_date = as.Date(substr(vapply(index, field, character(1), name = "introductionDate"), 1, 10)))
index_numbers <- str_remove(toupper(na.omit(c(matters$record_number, matters$legacy_record_number))), "^S")

journal <- bind_rows(
  read_csv("../input/journal_introductions_2010.csv", col_types = cols(.default = col_character())),
  read_csv("../input/journal_introductions_2011.csv", col_types = cols(.default = col_character()))
) |>
  mutate(month = format(as.Date(meeting_date), "%Y-%m"),
    in_elms = str_remove(toupper(record_number), "^S") %in% index_numbers)
coverage <- matters |>
  filter(introduction_date >= as.Date("2010-01-01"), introduction_date < as.Date("2012-01-01")) |>
  count(month = format(introduction_date, "%Y-%m"), name = "elms_matters") |>
  full_join(journal |> summarise(journal_introductions = n(), with_record_number = sum(!is.na(record_number)),
    record_number_in_elms = sum(in_elms), .by = month), by = "month", relationship = "one-to-one") |>
  mutate(across(-month, \(x) coalesce(x, 0L))) |>
  arrange(month)
SaveData(coverage, "month", "../output/elms_coverage_by_month.csv")
