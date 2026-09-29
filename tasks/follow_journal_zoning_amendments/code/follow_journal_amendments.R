# setwd("tasks/follow_journal_zoning_amendments/code")
# The zoning map amendments introduced in the City Council Journals of 2000--2011 (tasks/link_journal_zoning_outcomes),
# followed past the last Journal read, and their refilings. One row per introduction.
#   1. Follow-up. The Journals are read through December 14, 2011, so an amendment undecided then may have passed
#      later. eLMS, the City's legislative database (tasks/clean_zoning_map_amendments), holds every amendment
#      introduced from November 2010 and the passage of some older ones. An introduction undecided in the Journals takes
#      the outcome of the eLMS matter with its record number ("O2011-2263"; the Journals print proposed-ordinance
#      numbers, "PO2011-2263", and substitutes, "SO2011-2263", keep the number), or else of the one eLMS matter with its
#      application number. follow_up_source records which, and follow_up_outcome is the Journal's outcome otherwise.
#   2. Refilings. An amendment that did not pass is refiled by the first later amendment of the same site, by the same
#      kind of filer (an applicant or an alderman) and introduced within refiling_window_days (one council term, as for
#      eLMS in tasks/link_zoning_refilings), whose districts before share a district with the earlier one's (the site's
#      zoning is unchanged while the request waits; a district of the 1957 ordinance shares with those the 2004
#      ordinance converted it to, data_raw/zoning_conversion_2004_crosswalk.csv; an unread list does not rule a pair
#      out) and whose site is the same by its boundary or place: a later Journal introduction whose boundary shares at
#      least same_site_similarity of its words with the earlier one's (as in tasks/link_journal_zoning_outcomes), or at
#      least near_site_similarity with the two placed points (tasks/place_journal_zoning_amendments) within
#      near_site_feet, or an eLMS matter introduced after the last Journal read whose located title address
#      (tasks/assign_zoning_amendment_wards) lies within near_site_feet of the placed point. A later amendment also
#      refiles one with the same application number, or one whose common address (printed from July 2008) shares a
#      street segment with its own common address or eLMS title address (tasks/shared/code/address_segments.R, as for
#      eLMS; the Journals' spaced ranges, "3201 - 3345", are closed up, and OCR marks and ordinal suffixes after a street
#      number dropped, "West 31° Street" and "W 31st St"). An
#      eLMS matter that is the introduction's own follow-up is not its refiling. Every refiling
#      chosen has been read by hand (adjudication/journal_refiling_reviews.csv, one row per pair, with a decision and a
#      reason), and the build stops if one has not; a pair judged to be different projects is not a refiling, and the
#      next candidate is taken. The refiling's outcome is its own follow-up outcome.
refiling_window_days <- 1461
same_site_similarity <- 0.7
near_site_similarity <- 0.5
near_site_feet <- 300
journal_years <- 2000:2011
boundary_stopwords <- c("a", "the", "of", "and", "to", "line", "feet", "parallel", "next", "alley", "street", "avenue",
  "north", "south", "east", "west", "said", "thereof", "point", "along", "at", "on", "by", "in", "from", "which", "is",
  "as", "or", "public")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/normalize_chicago_address.R")
source("../../shared/code/address_segments.R")

conversion <- read_csv("../input/zoning_conversion_2004_crosswalk.csv", show_col_types = FALSE) |>
  filter(str_detect(old_code, "^(?:R[1-8]|[BCM][1-7]-[1-7]|C4)$")) |>
  transmute(old = paste0("1957:", old_code), new = str_replace(new_code, "^(RS|RT|RM)(?=[0-9])", "\\1-")) |>
  distinct()
comparable <- function(codes) unique(c(codes, conversion$new[conversion$old %in% codes]))
share_district <- function(a, b) is.na(a) | is.na(b) |
  map2_lgl(str_split(a, ";"), str_split(b, ";"), function(x, y) length(intersect(comparable(x), comparable(y))) > 0)
boundary_words <- function(boundary) {
  words <- str_extract_all(str_replace_all(str_to_lower(coalesce(boundary, "")), "(?<=[0-9]),(?=[0-9])", "."),
    "[a-z0-9]+(?:\\.[0-9]+)?")
  lapply(words, setdiff, boundary_stopwords)
}

boundaries <- bind_rows(lapply(journal_years, function(year) {
  read_csv(sprintf("../input/journal_introductions_%d.csv", year), col_types = cols(.default = col_character()))
})) |>
  transmute(introduction = paste0(file, "#", position), boundary)
points <- read_csv("../input/journal_amendment_places.csv", col_types = cols(.default = col_character())) |>
  filter(!is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3435)
introductions <- read_csv("../input/journal_zoning_outcomes.csv", col_types = cols(.default = col_character())) |>
  mutate(introduction = paste0(file, "#", position), introduction_date = as.Date(introduction_date),
    outcome_date = as.Date(outcome_date)) |>
  left_join(boundaries, by = "introduction", relationship = "one-to-one") |>
  left_join(tibble(introduction = paste0(points$file, "#", points$position), x = st_coordinates(points)[, 1],
    y = st_coordinates(points)[, 2]), by = "introduction", relationship = "one-to-one")
last_journal_meeting <- max(introductions$introduction_date)
stopifnot(!anyDuplicated(introductions$introduction))

# 1. Follow-up in eLMS.
elms <- read_csv("../input/zoning_map_amendments.csv", col_types = cols(.default = col_character())) |>
  mutate(introduction_date = as.Date(introduction_date), final_action_date = as.Date(final_action_date),
    filer = if_else(filed_by_alderman == "TRUE", "alderman", "applicant"))
elms_records <- elms |>
  transmute(matter_id, record = str_split(record_numbers, ";")) |>
  unnest_longer(record) |>
  mutate(record = str_remove(str_squish(record), "^S")) |>
  distinct()
stopifnot(!anyDuplicated(elms_records$record))
elms_applications <- elms |>
  filter(!is.na(application_number)) |>
  add_count(application_number) |>
  filter(n == 1) |>
  select(application_number, application_matter_id = matter_id)
undecided <- introductions |>
  filter(outcome %in% c("stalled", "pending")) |>
  transmute(introduction, record = str_remove(record_number, "^[PS]+(?=O)"), application_number) |>
  left_join(rename(elms_records, record_matter_id = matter_id), by = "record", relationship = "many-to-one") |>
  left_join(elms_applications, by = "application_number", relationship = "many-to-one") |>
  transmute(introduction, elms_matter_id = coalesce(record_matter_id, application_matter_id),
    follow_up_source = case_when(!is.na(record_matter_id) ~ "elms_record_number",
      !is.na(application_matter_id) ~ "elms_application_number")) |>
  filter(!is.na(elms_matter_id))
followed <- introductions |>
  left_join(undecided, by = "introduction", relationship = "one-to-one") |>
  left_join(select(elms, elms_matter_id = matter_id, elms_outcome = outcome, elms_date = final_action_date),
    by = "elms_matter_id", relationship = "many-to-one") |>
  mutate(follow_up_source = coalesce(follow_up_source, if_else(outcome %in% c("stalled", "pending"), "none",
      "journals")),
    follow_up_outcome = if_else(is.na(elms_matter_id), outcome, elms_outcome),
    follow_up_date = if_else(is.na(elms_matter_id), outcome_date, elms_date))
stopifnot(!anyDuplicated(na.omit(followed$elms_matter_id)))

# 2. Refilings: candidate pairs, then the first reviewed one for each amendment.
not_passed <- followed |>
  filter(follow_up_outcome %in% c("stalled", "withdrawn", "placed_on_file", "failed")) |>
  mutate(words = boundary_words(boundary))
journal_candidates <- not_passed |>
  select(introduction, filer, introduction_date, from_districts, words, x, y) |>
  cross_join(followed |>
    transmute(later = introduction, later_filer = filer, later_date = introduction_date,
      later_from = from_districts, later_words = boundary_words(boundary), later_x = x, later_y = y)) |>
  filter(later_filer == filer, later_date > introduction_date,
    as.numeric(later_date - introduction_date) <= refiling_window_days) |>
  mutate(shared = map2_int(words, later_words, function(a, b) length(intersect(a, b))),
    boundary_similarity = shared / (lengths(words) + lengths(later_words) - shared),
    feet = sqrt((x - later_x)^2 + (y - later_y)^2)) |>
  filter(share_district(from_districts, later_from), boundary_similarity >= same_site_similarity |
    boundary_similarity >= near_site_similarity & coalesce(feet, Inf) <= near_site_feet) |>
  transmute(introduction, later, later_source = "journals", later_date,
    refiling_basis = if_else(boundary_similarity >= same_site_similarity, "boundary", "boundary_and_place"))
geocoded <- read_csv("../input/zoning_amendment_wards.csv", col_types = cols(.default = col_character())) |>
  filter(!is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3435)
later_elms <- elms |>
  filter(introduction_date > last_journal_meeting, !matter_id %in% followed$elms_matter_id) |>
  left_join(tibble(matter_id = geocoded$matter_id, later_x = st_coordinates(geocoded)[, 1],
    later_y = st_coordinates(geocoded)[, 2]), by = "matter_id", relationship = "one-to-one") |>
  transmute(later = matter_id, later_filer = filer, later_date = introduction_date,
    later_application = application_number, later_from = from_districts, later_address = address, later_x, later_y)
elms_candidates <- not_passed |>
  select(introduction, filer, introduction_date, application_number, from_districts, x, y) |>
  cross_join(later_elms) |>
  filter(later_filer == filer, as.numeric(later_date - introduction_date) <= refiling_window_days,
    share_district(from_districts, later_from)) |>
  mutate(same_application = coalesce(application_number == later_application, FALSE),
    feet = sqrt((x - later_x)^2 + (y - later_y)^2)) |>
  filter(same_application | coalesce(feet, Inf) <= near_site_feet) |>
  transmute(introduction, later, refiling_basis = if_else(same_application, "application_number", "place"))
later_amendments <- bind_rows(
  transmute(followed, later = introduction, later_source = "journals", later_filer = filer,
    later_date = introduction_date, later_application = application_number, later_address = common_address),
  transmute(later_elms, later, later_source = "elms", later_filer, later_date, later_application, later_address))
number_candidates <- not_passed |>
  filter(!is.na(application_number)) |>
  select(introduction, application_number) |>
  inner_join(later_amendments |> filter(!is.na(later_application)) |> select(later, later_application) |>
    tidyr::nest(laters = -later_application), by = c(application_number = "later_application"),
    relationship = "many-to-one") |>
  tidyr::unnest(laters) |>
  transmute(introduction, later, refiling_basis = "application_number")
site_address <- function(address) str_replace_all(address, c("(?<=[0-9])\\s*-\\s*\\.?(?=[0-9])" = "-",
  "(?<=[0-9])[^A-Za-z0-9\\s,;/&-]+" = "", "(?i)(?<=[0-9])(?:st|nd|rd|th)\\b" = ""))
address_candidates <- overlapping_segments(
  address_segments(not_passed$introduction, site_address(not_passed$common_address)),
  address_segments(later_amendments$later, site_address(later_amendments$later_address))) |>
  transmute(introduction = id, later = later_id, refiling_basis = "address")
candidates <- bind_rows(select(journal_candidates, introduction, later, refiling_basis), elms_candidates,
    number_candidates, address_candidates) |>
  summarise(refiling_basis = paste(sort(unique(refiling_basis)), collapse = "+"), .by = c(introduction, later)) |>
  inner_join(select(not_passed, introduction, filer, introduction_date), by = "introduction",
    relationship = "many-to-one") |>
  inner_join(select(later_amendments, later, later_source, later_filer, later_date), by = "later",
    relationship = "many-to-one") |>
  filter(later_filer == filer, later_date > introduction_date,
    as.numeric(later_date - introduction_date) <= refiling_window_days) |>
  select(introduction, later, later_source, later_date, refiling_basis)

reviews <- read_csv("../adjudication/journal_refiling_reviews.csv", col_types = cols(.default = col_character()))
stopifnot(!anyDuplicated(reviews[c("introduction", "later")]),
  all(reviews$decision %in% c("same_project", "different_project")), !anyNA(reviews$reason),
  nrow(semi_join(reviews, candidates, by = c("introduction", "later"))) == nrow(reviews))
refilings <- candidates |>
  anti_join(filter(reviews, decision == "different_project"), by = c("introduction", "later")) |>
  slice_min(later_date, n = 1, with_ties = FALSE, by = introduction)
unreviewed <- anti_join(refilings, reviews, by = c("introduction", "later"))
if (nrow(unreviewed) > 0) print(unreviewed, n = Inf)
stopifnot(nrow(unreviewed) == 0)
later_outcomes <- bind_rows(
  transmute(followed, later = introduction, refiling_outcome = follow_up_outcome),
  transmute(elms, later = matter_id, refiling_outcome = outcome))
refilings <- refilings |>
  left_join(later_outcomes, by = "later", relationship = "many-to-one") |>
  transmute(introduction, refiling = later, refiling_source = later_source, refiling_date = later_date, refiling_basis,
    refiling_outcome)

follow_up <- followed |>
  left_join(refilings, by = "introduction", relationship = "one-to-one") |>
  transmute(introduction, file, position, introduction_date, filer, name, ward, application_number, record_number,
    journal_outcome = outcome, journal_outcome_date = outcome_date, follow_up_source, elms_matter_id,
    follow_up_outcome, follow_up_date, refiling, refiling_source, refiling_date,
    days_to_refiling = as.integer(refiling_date - introduction_date), refiling_basis, refiling_outcome)
SaveData(follow_up, "introduction", "../output/journal_amendment_follow_up.csv")
