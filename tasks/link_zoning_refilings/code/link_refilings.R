# setwd("tasks/link_zoning_refilings/code")
# Zoning map amendments that did not pass and were filed again (tasks/clean_zoning_map_amendments). A matter that
# stalled, was withdrawn or failed is refiled by the first later matter, by the same kind of filer (an applicant or an
# alderman) and introduced within refiling_window_days, that carries the same application number or shares a street
# segment of the title address: the same street name (and direction, where both give one) with overlapping house
# numbers on the same side of the street. A segment whose end numbers are both odd or both even lies on one side
# (Chicago numbers the two sides of a street odd and even); one with an odd and an even end spans both. Title
# addresses list segments separated by commas, "and", slashes or semicolons ("158-182 N Green St, 833-857 W Lake St");
# an abbreviated upper number ("5689-93") takes the lower number's leading digits. Every refiling chosen has been
# reviewed by hand (adjudication/refiling_reviews.csv); a pair found to be different projects is not a refiling, and
# the next candidate is taken in its place.
refiling_window_days <- 1461
street_types <- "AVE|AV|ST|RD|BLVD|DR|PL|CT|PKWY|PKY|TER|HWY|LN|WAY|SQ|CIR|BROADWAY"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/normalize_chicago_address.R")

amendments <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  select(matter_id, record_number, introduction_date, filed_by_alderman, outcome, application_number, address)

# Street segments of each title address, one row per segment.
segments <- amendments |>
  filter(!is.na(address)) |>
  mutate(address = normalize_address(address) |>
    str_remove(" - .*$") |>
    str_replace_all("\\b(?:DR )?(?:MARTIN L(?:UTHER)?|M L) KING(?: JR)?\\b", "KING") |>
    str_replace_all(paste0("\\b(", street_types, ") (?=[0-9])"), "\\1; ")) |>
  mutate(segment = strsplit(address, "\\s*(?:;|/| AND | AMD )\\s*")) |>
  tidyr::unnest_longer(segment) |>
  mutate(parts = str_match(segment, paste0("^0*([0-9]+)(?:\\s*-\\s*([0-9]+))?\\s+(?:([NSEW])\\s+)?(.+?)",
    "(?:\\s+(?:", street_types, "))?$"))) |>
  transmute(matter_id, low = as.integer(parts[, 2]), high_written = parts[, 3], direction = parts[, 4],
    street = parts[, 5]) |>
  filter(!is.na(low), !is.na(street)) |>
  mutate(high_written = coalesce(high_written, as.character(low)),
    high = as.integer(if_else(nchar(high_written) < nchar(low),
      paste0(substr(low, 1, nchar(low) - nchar(high_written)), high_written), high_written))) |>
  select(matter_id, low, high, direction, street)
# A range written high to low is read as the same range.
segments <- segments |> mutate(low_number = pmin(low, high), high = pmax(low, high), low = low_number) |>
  select(-low_number) |>
  mutate(side = case_when(low %% 2 != high %% 2 ~ "both", low %% 2 == 1 ~ "odd", TRUE ~ "even"))

# Candidate refilings: later matters of the same kind of filer, within the window, sharing an application number or a
# street segment.
not_passed <- amendments |> filter(outcome %in% c("stalled", "withdrawn", "failed"))
later <- amendments |>
  select(later_id = matter_id, later_record = record_number, later_date = introduction_date,
    later_alderman = filed_by_alderman, later_outcome = outcome, later_application = application_number)
by_application <- not_passed |>
  filter(!is.na(application_number)) |>
  inner_join(later, by = c(application_number = "later_application"), relationship = "many-to-many") |>
  transmute(matter_id, later_id, basis = "application_number")
by_address <- segments |>
  semi_join(not_passed, by = "matter_id") |>
  inner_join(rename(segments, later_id = matter_id, later_low = low, later_high = high, later_direction = direction,
    later_side = side), by = "street", relationship = "many-to-many") |>
  filter(matter_id != later_id, low <= later_high, later_low <= high,
    is.na(direction) | is.na(later_direction) | direction == later_direction,
    side == "both" | later_side == "both" | side == later_side) |>
  distinct(matter_id, later_id) |>
  mutate(basis = "address")
candidates <- bind_rows(by_application, by_address) |>
  summarise(basis = paste(sort(unique(basis)), collapse = "+"), .by = c(matter_id, later_id)) |>
  inner_join(not_passed |> select(matter_id, record_number, introduction_date, filed_by_alderman, outcome),
    by = "matter_id", relationship = "many-to-one") |>
  inner_join(later, by = "later_id", relationship = "many-to-one") |>
  filter(later_date > introduction_date, as.numeric(later_date - introduction_date) <= refiling_window_days,
    later_alderman == filed_by_alderman)

# Hand review: pairs judged to be different projects are dropped.
reviews <- read_csv("../adjudication/refiling_reviews.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(reviews[c("matter_id", "later_id")]),
  all(reviews$decision %in% c("same_project", "different_project")), !anyNA(reviews$reason),
  nrow(semi_join(reviews, candidates, by = c("matter_id", "later_id"))) == nrow(reviews))
refilings <- candidates |>
  anti_join(filter(reviews, decision == "different_project"), by = c("matter_id", "later_id")) |>
  slice_min(later_date, n = 1, with_ties = FALSE, by = matter_id) |>
  transmute(matter_id, record_number, outcome, refiling_matter_id = later_id, refiling_record_number = later_record,
    refiling_introduction_date = later_date, days_to_refiling = as.integer(later_date - introduction_date),
    refiling_outcome = later_outcome, match_basis = basis)
stopifnot(nrow(semi_join(refilings, filter(reviews, decision == "same_project"),
  by = c("matter_id", refiling_matter_id = "later_id"))) == nrow(refilings))
SaveData(refilings, "matter_id", "../output/zoning_refilings.csv")
