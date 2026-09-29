# setwd("tasks/explore_alderman_measures/code")
# Exploratory: the path of each zoning map amendment application (not filed by an alderman) through the council,
# from the Clerk's action histories (tasks/download_elms_matters): introduction and referral to the Committee on
# Zoning, the committee's recommendation to pass after its hearing, and passage by the council. Applications of the
# three council terms that have ended since the records begin (2011-2015, 2015-2019, 2019-2023) are followed to the end
# of their term, when those not passed lapse (stall). Holds in committee are not used: the Clerk recorded them as named
# actions through 2017, as unnamed committee entries in 2018-2023, and not at all from 2024.
#   1. Share passed by months since introduction, by term.
#   2. Where the time goes: days from introduction to the committee's recommendation and from it to passage.
#   3. Aldermen with at least min_applications applications: stall rate, and days to passage of passed applications.
#   4. Lame ducks: stall rates of aldermen who left the council at the end of the term against those who stayed.
#   5. Refilings: stalled applications later filed again for the same geocoded address, and what became of them.
term_starts <- as.Date(c("2011-05-16", "2015-05-18", "2019-05-20", "2023-05-15"))
term_labels <- c("2011-2015", "2015-2019", "2019-2023")
min_applications <- 20
months_left_breaks <- c(0, 6, 12, 24, 48)

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

matters <- lapply(readLines("../input/elms_zoning_matter_details.jsonl"), jsonlite::fromJSON, simplifyVector = FALSE)
committee_dates <- bind_rows(lapply(matters, function(m) {
  names <- vapply(m$actions, function(a) a$actionName %||% "", character(1))
  dates <- as.Date(substr(vapply(m$actions, function(a) a$actionDate %||% NA_character_, character(1)), 1, 10))
  tibble(matter_id = m$matterId,
    recommended_date = suppressWarnings(min(dates[names == "Recommended to Pass"])))
})) |>
  mutate(recommended_date = if_else(is.finite(recommended_date), recommended_date, as.Date(NA)))
stopifnot(!anyDuplicated(committee_dates$matter_id))

applications <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  filter(!filed_by_alderman, introduction_date >= term_starts[1], introduction_date < term_starts[4]) |>
  inner_join(read_csv("../input/zoning_amendment_wards.csv", show_col_types = FALSE) |>
    select(matter_id, matched_address, alderman), by = "matter_id", relationship = "one-to-one") |>
  left_join(committee_dates, by = "matter_id", relationship = "one-to-one") |>
  mutate(term = term_labels[findInterval(introduction_date, term_starts)],
    term_end = term_starts[findInterval(introduction_date, term_starts) + 1L],
    months_left = cut(as.numeric(term_end - introduction_date) / 30.44, months_left_breaks, right = FALSE),
    stalled = outcome == "stalled",
    days_to_committee = as.integer(recommended_date - introduction_date),
    committee_to_passage = as.integer(passed_date - recommended_date))
stopifnot(!anyNA(applications$term), all(applications$outcome != "pending"))

# 5 (built first, used below). A stall is refiled when a later matter carries the same geocoded address.
later_filings <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  inner_join(read_csv("../input/zoning_amendment_wards.csv", show_col_types = FALSE) |>
    select(matter_id, matched_address), by = "matter_id", relationship = "one-to-one") |>
  filter(!is.na(matched_address)) |>
  # Filings for the same address on the same day are one refiling, passed if any of them passed.
  summarise(later_outcome = if_else(any(outcome == "passed"), "passed", dplyr::first(outcome)),
    later_passed = suppressWarnings(min(passed_date, na.rm = TRUE)), .by = c(matched_address, introduction_date)) |>
  transmute(matched_address, later_date = introduction_date, later_outcome,
    later_passed = if_else(is.finite(later_passed), later_passed, as.Date(NA)))
refilings <- applications |>
  filter(stalled, !is.na(matched_address)) |>
  select(matter_id, matched_address, introduction_date) |>
  inner_join(later_filings, by = join_by(matched_address, closest(introduction_date < later_date)),
    relationship = "many-to-one") |>
  transmute(matter_id, refiled_date = later_date, refiling_outcome = later_outcome, refiling_passed = later_passed)
applications <- applications |>
  left_join(refilings, by = "matter_id", relationship = "one-to-one") |>
  mutate(refiled = !is.na(refiled_date))
SaveData(applications |> select(matter_id, record_number, term, alderman, introduction_date, months_left, direction,
    outcome, stalled, recommended_date, passed_date, days_to_committee, committee_to_passage, days_to_passage,
    refiled, refiled_date, refiling_outcome, refiling_passed),
  "matter_id", "../output/application_timeline.csv")

# 1. Share passed by months since introduction.
passage_curves <- tidyr::expand_grid(term = term_labels, months = 0:48) |>
  mutate(share_passed = purrr::map2_dbl(term, months, function(t, m) {
    x <- applications[applications$term == t, ]
    mean(!is.na(x$passed_date) & x$days_to_passage <= m * 30.44)
  }), applications = as.integer(table(applications$term)[term]))
SaveData(passage_curves, c("term", "months"), "../output/passage_curves.csv")

# 2. Where the time goes, for passed applications.
timing <- applications |>
  filter(outcome == "passed") |>
  summarise(passed = n(), with_recommendation = sum(!is.na(recommended_date)),
    median_days_to_committee = median(days_to_committee, na.rm = TRUE),
    p90_days_to_committee = quantile(days_to_committee, 0.9, na.rm = TRUE),
    median_committee_to_passage = median(committee_to_passage, na.rm = TRUE),
    median_days_to_passage = median(days_to_passage), p90_days_to_passage = quantile(days_to_passage, 0.9),
    .by = c(term, direction)) |>
  bind_rows(applications |> filter(outcome == "passed") |>
    summarise(passed = n(), with_recommendation = sum(!is.na(recommended_date)),
      median_days_to_committee = median(days_to_committee, na.rm = TRUE),
      p90_days_to_committee = quantile(days_to_committee, 0.9, na.rm = TRUE),
      median_committee_to_passage = median(committee_to_passage, na.rm = TRUE),
      median_days_to_passage = median(days_to_passage), p90_days_to_passage = quantile(days_to_passage, 0.9),
      .by = term) |> mutate(direction = "all")) |>
  arrange(term, direction)
SaveData(timing, c("term", "direction"), "../output/application_timing.csv")

# 3. Aldermen: stall rate and time to passage, raw, for aldermen with enough applications.
aldermen <- applications |>
  filter(!is.na(alderman)) |>
  summarise(applications = n(), stalls = sum(stalled), stall_rate = mean(stalled),
    stall_rate_refilings_excluded = mean(stalled & !refiled), passed = sum(outcome == "passed"),
    median_days_to_passage = median(days_to_passage, na.rm = TRUE),
    share_passed_within_90_days = mean(days_to_passage[outcome == "passed"] <= 90),
    first_term = min(term), last_term = max(term), .by = alderman) |>
  filter(applications >= min_applications) |>
  arrange(desc(stall_rate))
SaveData(aldermen, "alderman", "../output/alderman_stall_and_time.csv")

# 4. Lame ducks: aldermen serving at the end of each term who did not serve in the next.
terms <- read_csv("../input/alderman_terms.csv", show_col_types = FALSE)
lame_ducks <- bind_rows(lapply(seq_along(term_labels), function(i) {
  end <- term_starts[i + 1]
  serving_at_end <- terms$alderman[terms$start_date <= end - 1 & terms$end_date >= end - 1]
  serving_after <- terms$alderman[terms$start_date <= end + 30 & terms$end_date >= end + 30]
  applications |>
    filter(term == term_labels[i], alderman %in% serving_at_end) |>
    mutate(leaving = !alderman %in% serving_after)
})) |>
  summarise(aldermen = n_distinct(alderman), applications = n(), stall_rate = mean(stalled),
    .by = c(term, months_left, leaving)) |>
  arrange(term, months_left, leaving)
SaveData(lame_ducks, c("term", "months_left", "leaving"), "../output/lame_duck_stall_rates.csv")

refiling_summary <- applications |>
  filter(stalled) |>
  summarise(stalls = n(), refiled = sum(refiled), refilings_passed = sum(refiling_outcome %in% "passed"),
    median_days_to_refiling = median(as.integer(refiled_date - introduction_date), na.rm = TRUE),
    median_days_first_filing_to_passage = median(as.integer(refiling_passed - introduction_date), na.rm = TRUE),
    .by = term) |>
  arrange(term)
SaveData(refiling_summary, "term", "../output/refiling_summary.csv")
