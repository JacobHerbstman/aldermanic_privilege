# setwd("tasks/link_journal_zoning_outcomes/code")
# The outcome of each zoning map amendment introduced in the City Council Journals
# (tasks/parse_journal_zoning_amendments): passed, withdrawn, placed on file or failed, or else stalled. Each ordinance
# printed in a report of the Committee on Zoning is linked to the earlier introduction it resolves, in this order:
#   1. by record number, printed with both from November 2010 ("O2010-6361"; a substitute, "SO2010-6361", keeps its
#      original's number). Introductions before then carry proposed-ordinance numbers ("PO2009-2094") that their
#      ordinances do not repeat.
#   2. by application number, printed with applications from April 22, 2009, if only one introduction carries it (OCR
#      reads the odd number as another application's).
#   3. by boundary, to an earlier introduction by the same kind of filer (aldermen's amendments carry "A-" numbers)
#      whose boundary shares at least minimum_boundary_similarity of its words with the ordinance's (the share of words
#      in either boundary that are in both; directions, street types and connecting words are not counted). An
#      introduction whose districts before share a district with the ordinance's comes first, then the most similar
#      boundary, then an introduction whose districts after share a district and whose map sheet agrees, then the
#      latest. The districts before come first because a parcel can be rezoned and later rezoned back within the same
#      boundary; an introduction whose districts before differ is linked only if its boundary is nearly the same
#      (identical_boundary_similarity), since an amended ordinance can differ from its introduction. The districts
#      after and the map sheet only break ties, since a request can change before it passes and OCR misreads map
#      sheets. An introduction resolves one ordinance per meeting, and ordinances of different meetings only if they
#      carry the same application number (a deferral and the later passage); pairs are taken in the order above,
#      after the links by number.
#   4. by application number range, for an applicant's ordinance still unlinked: to an introduction without a known
#      number, at the meetings where its number was assigned, with the same map sheet and a district before in common
#      and the most similar boundary (step 4 below).
# Each ordinance's best boundary candidate is kept, whatever its link, so that the two can be compared.
# A report's note of a withdrawal ("Application Number A-7371 was withdrawn") is linked to the one earlier
# introduction with its application number, printed or taken from a linked ordinance. Aldermen's "A-" numbers name
# series shared by several amendments and are never printed with an introduction, so their notes rarely link.
# An introduction's outcome is its first passage; else a withdrawal, placing on file or failure; else it stalled if
# introduced at least stall_follow_up_days before the last meeting read, and is pending if introduced later. Matters
# did not lapse with the council term that began on May 16, 2011: amendments introduced before it passed after it.
journal_years <- 2000:2011
minimum_boundary_similarity <- 0.4
identical_boundary_similarity <- 0.9
range_boundary_similarity <- 0.2
stall_follow_up_days <- 365
boundary_stopwords <- c("a", "the", "of", "and", "to", "line", "feet", "parallel", "next", "alley", "street", "avenue",
  "north", "south", "east", "west", "said", "thereof", "point", "along", "at", "on", "by", "in", "from", "which", "is",
  "as", "or", "public")
outcome_order <- c("passed", "withdrawn", "placed_on_file", "failed")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

read_journal <- function(table) bind_rows(lapply(journal_years, function(year)
  read_csv(sprintf("../input/journal_%s_%d.csv", table, year), col_types = cols(.default = col_character()))))
introductions <- read_journal("introductions") |>
  mutate(introduction = paste0(file, "#", position), introduction_date = as.Date(meeting_date))
ordinances <- read_journal("ordinances") |>
  mutate(ordinance = paste0(file, "#", position), meeting_date = as.Date(meeting_date),
    filer = case_when(str_detect(application_number, "^A-") ~ "alderman", !is.na(application_number) ~ "applicant"))
report_notes <- read_journal("report_notes") |>
  mutate(meeting_date = as.Date(meeting_date), note = recode(note, "placed on file" = "placed_on_file"))
stopifnot(!anyDuplicated(introductions$introduction), !anyDuplicated(ordinances$ordinance),
  all(ordinances$action %in% c(outcome_order, "deferred", "re-referred")),
  all(report_notes$note %in% c(outcome_order, "deferred")))

# 1. Links by record number.
introduction_records <- introductions |>
  filter(str_detect(record_number, "^O")) |>
  select(record_introduction = introduction, introduction_date, record = record_number)
stopifnot(!anyDuplicated(introduction_records$record))
by_record <- ordinances |>
  transmute(ordinance, meeting_date, record = str_remove(record_number, "^S")) |>
  inner_join(introduction_records, by = "record", relationship = "many-to-one") |>
  filter(introduction_date < meeting_date) |>
  select(ordinance, record_introduction)

# 2. Links by application number.
by_application <- ordinances |>
  filter(filer == "applicant") |>
  select(ordinance, meeting_date, application_number) |>
  inner_join(introductions |> filter(!is.na(application_number)) |> add_count(application_number) |> filter(n == 1) |>
    select(application_introduction = introduction, introduction_date, application_number),
    by = "application_number", relationship = "many-to-one") |>
  filter(introduction_date < meeting_date) |>
  select(ordinance, application_introduction)

# 3. Links by boundary. District lists are separated by semicolons; an unread list does not rule a pair out. A district
# of the 1957 ordinance ("1957:R3") shares with the districts the 2004 ordinance converted it to, in or outside
# downtown (data_raw/zoning_conversion_2004_crosswalk.csv: R3 became RS3, B4-2 became B3-2), so an application
# introduced under one ordinance and passed under the other keeps its district before.
conversion <- read_csv("../input/zoning_conversion_2004_crosswalk.csv", show_col_types = FALSE) |>
  filter(str_detect(old_code, "^(?:R[1-8]|[BCM][1-7]-[1-7]|C4)$")) |>
  transmute(old = paste0("1957:", old_code), new = str_replace(new_code, "^(RS|RT|RM)(?=[0-9])", "\\1-")) |>
  distinct()
comparable <- function(codes) unique(c(codes, conversion$new[conversion$old %in% codes]))
boundary_words <- function(boundary) {
  words <- str_extract_all(str_replace_all(str_to_lower(coalesce(boundary, "")), "(?<=[0-9]),(?=[0-9])", "."),
    "[a-z0-9]+(?:\\.[0-9]+)?")
  lapply(words, setdiff, boundary_stopwords)
}
share_district <- function(a, b) is.na(a) | is.na(b) |
  map2_lgl(str_split(a, ";"), str_split(b, ";"), function(x, y) length(intersect(comparable(x), comparable(y))) > 0)
pairs <- ordinances |>
  filter(!is.na(boundary)) |>
  transmute(ordinance, meeting_date, filer, application_number, heading_map, change_map, from_districts, to_districts,
    ordinance_words = boundary_words(boundary)) |>
  cross_join(introductions |>
    filter(!is.na(boundary)) |>
    transmute(introduction, introduction_date, introduction_filer = filer, introduction_map = map_number,
      introduction_from = from_districts, introduction_to = to_districts, introduction_words = boundary_words(boundary))) |>
  filter(introduction_date < meeting_date, is.na(filer) | filer == introduction_filer) |>
  mutate(shared = map2_int(ordinance_words, introduction_words, function(x, y) length(intersect(x, y))),
    boundary_similarity = shared / (lengths(ordinance_words) + lengths(introduction_words) - shared)) |>
  filter(boundary_similarity >= range_boundary_similarity) |>
  mutate(from_shared = share_district(from_districts, introduction_from),
    to_shared = share_district(to_districts, introduction_to),
    map_agrees = coalesce(introduction_map == heading_map | introduction_map == change_map, FALSE)) |>
  arrange(desc(from_shared), desc(boundary_similarity), desc(to_shared), desc(map_agrees), desc(introduction_date),
    ordinance, introduction)
boundary_pairs <- pairs |>
  filter(boundary_similarity >= minimum_boundary_similarity,
    from_shared | boundary_similarity >= identical_boundary_similarity)
best_candidates <- boundary_pairs |>
  slice_head(n = 1, by = ordinance) |>
  select(ordinance, boundary_introduction = introduction, boundary_similarity)

# Pairs are taken in order: an introduction resolves one ordinance per meeting, and ordinances of different meetings
# only if they carry the same application number (a deferral and the later passage).
assign_pairs <- function(linked, pairs) {
  for (i in seq_len(nrow(pairs))) {
    pair <- pairs[i, ]
    held <- linked[linked$introduction == pair$introduction, ]
    if (!pair$ordinance %in% linked$ordinance && all(held$application_number %in% pair$application_number &
      held$meeting_date != pair$meeting_date)) {
      linked <- bind_rows(linked, select(pair, ordinance, introduction, meeting_date, application_number))
    }
  }
  linked
}
number_links <- full_join(by_record, by_application, by = "ordinance", relationship = "one-to-one")
stopifnot(with(number_links, is.na(record_introduction) | is.na(application_introduction) |
  record_introduction == application_introduction))
linked <- number_links |>
  transmute(ordinance, introduction = coalesce(record_introduction, application_introduction)) |>
  left_join(select(ordinances, ordinance, meeting_date, application_number), by = "ordinance",
    relationship = "one-to-one") |>
  assign_pairs(boundary_pairs)
boundary_linked <- setdiff(linked$ordinance, number_links$ordinance)

# 4. Links by application number range. Applications are numbered in order and each meeting's take the next run of
# numbers, so an applicant ordinance still unlinked was introduced between the meetings of the nearest known numbers
# below and above its own (printed with an introduction or taken from its linked ordinance). It is linked to an
# introduction of those meetings with no known number whose map sheet agrees, whose districts before share a
# district, and whose boundary is the most similar (at least range_boundary_similarity; the boundary may have been
# amended).
known_numbers <- introductions |>
  filter(filer == "applicant") |>
  select(introduction, introduction_date, printed_number = application_number) |>
  left_join(linked |> filter(str_detect(application_number, "^[0-9]+$")) |>
    distinct(introduction, linked_number = application_number), by = "introduction", relationship = "one-to-one") |>
  transmute(introduction, introduction_date, number = as.integer(coalesce(printed_number, linked_number))) |>
  filter(!is.na(number))
number_meetings <- known_numbers |>
  summarise(first_meeting = min(introduction_date), last_meeting = max(introduction_date), .by = number)
number_windows <- ordinances |>
  filter(filer == "applicant", str_detect(application_number, "^[0-9]+$"), !ordinance %in% linked$ordinance) |>
  transmute(ordinance, number = as.integer(application_number)) |>
  left_join(transmute(number_meetings, lower = number, window_start = first_meeting),
    join_by(closest(number >= lower)), relationship = "many-to-one") |>
  left_join(transmute(number_meetings, upper = number, window_end = last_meeting),
    join_by(closest(number <= upper)), relationship = "many-to-one") |>
  filter(window_start <= window_end)
range_pairs <- pairs |>
  inner_join(select(number_windows, ordinance, window_start, window_end), by = "ordinance",
    relationship = "many-to-one") |>
  filter(introduction_date >= window_start, introduction_date <= window_end, map_agrees, from_shared,
    !introduction %in% c(known_numbers$introduction, linked$introduction)) |>
  arrange(desc(boundary_similarity), ordinance, introduction)
linked <- assign_pairs(linked, range_pairs)

links <- ordinances |>
  select(ordinance, meeting_date, file, position, page, journal_page, action, application_number, record_number,
    map_number, from_districts, to_districts) |>
  left_join(select(linked, ordinance, introduction), by = "ordinance", relationship = "one-to-one") |>
  left_join(number_links, by = "ordinance", relationship = "one-to-one") |>
  left_join(best_candidates, by = "ordinance", relationship = "one-to-one") |>
  mutate(link_basis = case_when(!is.na(record_introduction) ~ "record_number",
    !is.na(application_introduction) ~ "application_number", ordinance %in% boundary_linked ~ "boundary",
    !is.na(introduction) ~ "application_number_range", TRUE ~ "none"))

# 5. Notes of withdrawals, placings on file and deferrals, by application number.
linked_numbers <- links |>
  filter(!is.na(introduction), !is.na(application_number)) |>
  distinct(introduction, linked_number = application_number)
stopifnot(!anyDuplicated(linked_numbers$introduction))
numbered <- introductions |>
  select(introduction, introduction_date, printed_number = application_number) |>
  left_join(linked_numbers, by = "introduction", relationship = "one-to-one") |>
  mutate(application_number = coalesce(printed_number, linked_number),
    application_source = case_when(!is.na(printed_number) ~ "introduction", !is.na(linked_number) ~ "ordinance"))
note_links <- report_notes |>
  inner_join(numbered |> filter(!is.na(application_number)) |> add_count(application_number) |> filter(n == 1) |>
    select(introduction, introduction_date, application_number), by = "application_number",
    relationship = "many-to-one") |>
  filter(introduction_date < meeting_date)

# 6. Outcomes. An introduction takes the first event of the first kind in outcome_order.
last_meeting <- max(introductions$introduction_date, ordinances$meeting_date)
events <- bind_rows(
  links |> filter(!is.na(introduction)) |> transmute(introduction, outcome = action, outcome_date = meeting_date,
    ordinance, link_basis),
  note_links |> transmute(introduction, outcome = note, outcome_date = meeting_date, link_basis = "report_note"))
decisions <- events |>
  filter(outcome %in% outcome_order) |>
  arrange(introduction, match(outcome, outcome_order), outcome_date) |>
  slice_head(n = 1, by = introduction)
outcomes <- introductions |>
  select(-application_number) |>
  left_join(select(numbered, introduction, application_number, application_source), by = "introduction",
    relationship = "one-to-one") |>
  left_join(decisions, by = "introduction", relationship = "one-to-one") |>
  left_join(count(filter(links, !is.na(introduction)), introduction, name = "ordinances"), by = "introduction",
    relationship = "one-to-one") |>
  mutate(outcome = coalesce(outcome, if_else(introduction_date <= last_meeting - stall_follow_up_days, "stalled",
      "pending")),
    days_to_passage = if_else(outcome == "passed", as.integer(outcome_date - introduction_date), NA_integer_),
    ordinances = coalesce(ordinances, 0L)) |>
  select(introduction_date, file, position, page, journal_page, filer, name, ward, application_number,
    application_source, record_number, map_number, from_districts, to_districts, common_address, outcome, outcome_date,
    days_to_passage, link_basis, ordinance, ordinances)

SaveData(links, "ordinance", "../output/journal_ordinance_links.csv")
SaveData(outcomes, c("file", "position"), "../output/journal_zoning_outcomes.csv")
