# setwd("tasks/audits/zoning_record_validation/code")
# Two checks of the links between the Journals' ordinances and introductions (tasks/link_journal_zoning_outcomes),
# one row per ordinance:
#   - boundary_agrees: for an ordinance linked by record or application number, whether its best boundary candidate
#     is the same introduction, so that the boundary rule is tested where the numbers give the answer.
#   - number_in_order: for a linked ordinance with an application number, whether the number lies between the median
#     numbers of the meetings before and after its introduction's. Applications (and, in a separate series,
#     aldermen's "A-" amendments) are numbered in order and each meeting's take the next run, so a number out of order
#     marks a wrong link, a misread number or an amendment introduced again. Medians are taken over the numbers known
#     for each meeting's introductions, printed or taken from their linked ordinances.
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

links <- read_csv("../input/journal_ordinance_links.csv", col_types = cols(.default = col_character()))
outcomes <- read_csv("../input/journal_zoning_outcomes.csv", col_types = cols(.default = col_character())) |>
  transmute(introduction = paste0(file, "#", position), introduction_date = as.Date(introduction_date),
    known_number = application_number)

numbered <- outcomes |>
  mutate(series = if_else(str_detect(known_number, "^A-"), "alderman", "applicant"),
    number = as.integer(str_match(known_number, "^(?:A-)?([0-9]+)$")[, 2])) |>
  filter(!is.na(number))
meeting_medians <- numbered |>
  summarise(median_number = median(number), .by = c(series, introduction_date)) |>
  arrange(series, introduction_date) |>
  mutate(previous_median = lag(median_number), next_median = lead(median_number), .by = series)

checks <- links |>
  transmute(ordinance, meeting_date, action, link_basis, application_number, introduction,
    number_introduction = coalesce(record_introduction, application_introduction), boundary_introduction,
    boundary_similarity) |>
  left_join(select(outcomes, introduction, introduction_date), by = "introduction", relationship = "many-to-one") |>
  mutate(series = case_when(str_detect(application_number, "^A-[0-9]+$") ~ "alderman",
      str_detect(application_number, "^[0-9]+$") ~ "applicant"),
    number = as.integer(str_match(application_number, "^(?:A-)?([0-9]+)$")[, 2])) |>
  left_join(select(meeting_medians, series, introduction_date, previous_median, next_median),
    by = c("series", "introduction_date"), relationship = "many-to-one") |>
  mutate(boundary_agrees = if_else(is.na(number_introduction) | is.na(boundary_introduction), NA,
      boundary_introduction == number_introduction),
    number_in_order = if_else(is.na(introduction) | is.na(number) | is.na(previous_median) | is.na(next_median), NA,
      number > previous_median & number < next_median)) |>
  select(ordinance, meeting_date, action, link_basis, application_number, introduction, introduction_date,
    number_introduction, boundary_introduction, boundary_similarity, boundary_agrees, previous_median, next_median,
    number_in_order)
SaveData(checks, "ordinance", "../output/journal_link_checks.csv")
