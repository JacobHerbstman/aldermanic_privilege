# setwd("tasks/link_zoning_refilings/code")
# Zoning map amendments that did not pass and were filed again (tasks/clean_zoning_map_amendments). A matter that
# stalled, was withdrawn or failed is refiled by the first later matter, by the same kind of filer (an applicant or an
# alderman) and introduced within refiling_window_days, that carries the same application number or shares a street
# segment of the title address: the same street name (and direction, where both give one) with overlapping house
# numbers on the same side of the street (tasks/shared/code/address_segments.R). Every refiling chosen has been
# reviewed by hand (adjudication/refiling_reviews.csv); a pair found to be different projects is not a refiling, and
# the next candidate is taken in its place.
refiling_window_days <- 1461

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/normalize_chicago_address.R")
source("../../shared/code/address_segments.R")

amendments <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  select(matter_id, record_number, introduction_date, filed_by_alderman, outcome, application_number, address)

segments <- address_segments(amendments$matter_id, amendments$address)

# Candidate refilings: later matters of the same kind of filer, within the window, sharing an application number or a
# street segment.
not_passed <- amendments |> filter(outcome %in% c("stalled", "withdrawn", "failed"))
later <- amendments |>
  select(later_id = matter_id, later_record = record_number, later_date = introduction_date,
    later_alderman = filed_by_alderman, later_outcome = outcome, later_application = application_number)
by_application <- not_passed |>
  filter(!is.na(application_number)) |>
  inner_join(later |> filter(!is.na(later_application)) |> tidyr::nest(later = -later_application),
    by = c(application_number = "later_application"), relationship = "many-to-one") |>
  tidyr::unnest(later) |>
  transmute(matter_id, later_id, basis = "application_number")
by_address <- overlapping_segments(filter(segments, id %in% not_passed$matter_id), segments) |>
  transmute(matter_id = id, later_id, basis = "address")
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
