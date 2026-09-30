# setwd("tasks/audits/zoning_stalls_and_delays/code")
# Zoning map amendments introduced from 2000 to the end of the eLMS download (September 2026), both applications
# (filer "applicant", transmitted through the Zoning Administrator) and aldermen's own amendments (filer "alderman"),
# from two sources, one row per amendment and source: the City Council Journals for introductions of 2000--2011
# (tasks/follow_journal_zoning_amendments, with wards and aldermen from tasks/place_journal_zoning_amendments) and
# eLMS for introductions from 2011 (tasks/clean_zoning_map_amendments, wards and aldermen from
# tasks/assign_zoning_amendment_wards, refilings from tasks/link_zoning_refilings). The two overlap in 2011, which
# checks one against the other. An amendment stalled if it lapsed with a council term without passing, failing or
# being withdrawn; this is known only for applications introduced before the current term (May 15, 2023), and is
# missing for later ones, which may still be pending. stalled_for_good excludes stalls whose refiling passed.
# not_passed_in_window (delay_window_days) is observable for every application introduced at least that long before
# the end of the eLMS follow-up (its last recorded action), and missing for later ones. days_to_passage runs from
# introduction to passage; eLMS's 28 passages dated on or before their introduction are left without one, and so are
# passages of applications introduced less than days_follow_up_days before the end of the follow-up, of which only the
# quick ones have passed (2.4 percent of passages take longer). An application's ward is the ward its site lies in
# (for the Journals on the ward map in force at introduction and, for 2001-12-19 to 2003-05-04, also the 2003 map in
# which aldermen then filed, redrawn_alderman); an alderman's amendment is credited to the ward it was filed from and
# its alderman on the introduction date (create_alderman_data/adjudication/alderman_terms.csv), though its site may
# lie in a neighboring ward (5 percent do). direction is the kind of change, as for eLMS: to a planned development,
# else up, down or the same by the highest allowed floor-area ratio among the districts before and after
# (tasks/parse_journal_zoning_amendments), else unknown. A district of the 2004 ordinance has the ratio of Second City
# Zoning's table; a district of the 1957 ordinance, in force until November 1, 2004, has the ratio of the 2004
# district it was converted to outside downtown, or downtown where the city's conversion table
# (data_raw/zoning_conversion_2004_crosswalk.csv) gives only that.
journal_years <- 2000:2011
elms_years <- 2011:2026
current_term_start <- as.Date("2023-05-15")
delay_window_days <- 90
days_follow_up_days <- 365

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

far_2004 <- read_csv("../input/zoning-code-summary-district-types.csv", show_col_types = FALSE) |>
  transmute(district = district_type_code, floor_area_ratio = suppressWarnings(as.numeric(floor_area_ratio)))
far_1957 <- read_csv("../input/zoning_conversion_2004_crosswalk.csv", show_col_types = FALSE) |>
  filter(str_detect(old_code, "^(?:R[1-8]|[BCM][1-7]-[1-7]|C4)$")) |>
  arrange(old_code, conversion_scope != "outside_downtown") |>
  slice_head(n = 1, by = old_code) |>
  transmute(district = paste0("1957:", old_code),
    converted_to = str_replace(str_extract(new_code, "^\\S+"), "^(R[STM])(?=[0-9])", "\\1-")) |>
  left_join(far_2004, by = c(converted_to = "district"), relationship = "many-to-one")
stopifnot(!anyNA(far_1957$floor_area_ratio))
districts <- bind_rows(far_2004, select(far_1957, district, floor_area_ratio))
stopifnot(!anyDuplicated(districts$district))
max_far <- function(codes) {
  vapply(strsplit(codes, ";", fixed = TRUE), function(x) {
    far <- districts$floor_area_ratio[match(x, districts$district)]
    if (length(far) == 0 || all(is.na(far))) NA_real_ else max(far, na.rm = TRUE)
  }, numeric(1))
}
journal_districts <- bind_rows(lapply(journal_years, function(year) {
  read_csv(sprintf("../input/journal_introductions_%d.csv", year), col_types = cols(.default = col_character()))
})) |>
  transmute(introduction = paste0(file, "#", position), from_far = max_far(coalesce(from_districts, "")),
    to_far = max_far(coalesce(to_districts, "")),
    direction = case_when(str_detect(coalesce(to_districts, ""), "(?:^|;)PD(?:;|$)") ~ "to_planned_development",
      is.na(from_far) | is.na(to_far) ~ "unknown",
      to_far > from_far ~ "up", to_far < from_far ~ "down", TRUE ~ "same_far"))

terms <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE) |>
  transmute(filing_ward = ward, filing_alderman = alderman, start_date, end_date)
filing_alderman <- function(data) {
  left_join(data, terms, by = join_by(filing_ward, between(introduction_date, start_date, end_date)),
    relationship = "many-to-one")
}

journal_places <- read_csv("../input/journal_amendment_places.csv", col_types = cols(.default = col_character())) |>
  transmute(introduction = paste0(file, "#", position), ward = as.numeric(ward), alderman, redrawn_alderman,
    filing_ward = as.numeric(filing_ward))
journal <- read_csv("../input/journal_amendment_follow_up.csv", col_types = cols(.default = col_character())) |>
  select(-ward) |>
  left_join(journal_places, by = "introduction", relationship = "one-to-one") |>
  left_join(journal_districts, by = "introduction", relationship = "one-to-one") |>
  mutate(introduction_date = as.Date(introduction_date)) |>
  filing_alderman() |>
  transmute(source = "journals", amendment = introduction, filer, introduction_date,
    ward = if_else(filer == "alderman", filing_ward, ward),
    alderman = if_else(filer == "alderman", filing_alderman, alderman),
    redrawn_alderman = if_else(filer == "alderman", filing_alderman, redrawn_alderman), direction,
    outcome = follow_up_outcome,
    stalled = outcome == "stalled", stalled_for_good = stalled & !refiling_outcome %in% "passed",
    days_to_passage = if_else(outcome == "passed", as.numeric(as.Date(follow_up_date) - introduction_date), NA_real_),
    not_passed_in_window = !coalesce(days_to_passage <= delay_window_days, FALSE))
stopifnot(!anyNA(journal$outcome), !anyNA(journal$direction),
  !anyNA(journal$days_to_passage[journal$outcome == "passed"]))

elms_wards <- read_csv("../input/zoning_amendment_wards.csv", col_types = cols(.default = col_character())) |>
  transmute(matter_id, ward = as.numeric(ward), alderman)
elms_refilings <- read_csv("../input/zoning_refilings.csv", col_types = cols(.default = col_character())) |>
  select(matter_id, refiling_outcome)
elms_amendments <- read_csv("../input/zoning_map_amendments.csv", col_types = cols(.default = col_character()))
elms_end <- max(as.Date(c(elms_amendments$introduction_date, elms_amendments$final_action_date)), na.rm = TRUE)
elms <- elms_amendments |>
  left_join(elms_wards, by = "matter_id", relationship = "one-to-one") |>
  left_join(elms_refilings, by = "matter_id", relationship = "one-to-one") |>
  mutate(introduction_date = as.Date(introduction_date), days = as.numeric(days_to_passage),
    followed_days = as.numeric(elms_end - introduction_date), filing_ward = as.numeric(filing_ward),
    filer = if_else(filed_by_alderman == "TRUE", "alderman", "applicant")) |>
  filing_alderman() |>
  transmute(source = "elms", amendment = matter_id, filer, introduction_date,
    ward = if_else(filer == "alderman", filing_ward, ward),
    alderman = if_else(filer == "alderman", filing_alderman, alderman), redrawn_alderman = alderman, direction,
    outcome,
    stalled = if_else(introduction_date < current_term_start, outcome == "stalled", NA),
    stalled_for_good = stalled & !refiling_outcome %in% "passed",
    days_to_passage = if_else(outcome == "passed" & days > 0 & followed_days >= days_follow_up_days, days, NA_real_),
    not_passed_in_window = if_else(followed_days >= delay_window_days,
      !(outcome == "passed" & days <= delay_window_days), NA))

amendments <- bind_rows(journal, elms) |>
  mutate(introduction_year = as.integer(format(introduction_date, "%Y"))) |>
  filter(source == "journals" & introduction_year %in% journal_years |
    source == "elms" & introduction_year %in% elms_years)
stopifnot(!anyDuplicated(amendments[c("source", "amendment")]), all(amendments$filer %in% c("applicant", "alderman")))
SaveData(amendments, c("source", "amendment"), "../output/zoning_amendments.csv")
