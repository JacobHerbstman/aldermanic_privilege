# setwd("tasks/clean_zoning_map_amendments/code")
# One row per zoning map amendment in the City Clerk's eLMS records (tasks/download_elms_matters), 2010-2026: dates,
# outcome, who filed it, the address and application number from its title, and the zoning districts before and
# after the change, read from its legislation text (tasks/extract_zoning_legislation_text).
# Matters not acted on lapse at the end of a council term; a matter never passed and introduced before the current
# term began is coded stalled, one introduced during the current term and not yet decided is pending.
current_term_start <- as.Date("2023-05-15")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

# The zoning ordinance's district codes and their allowed floor-area ratios (City of Chicago zoning summary).
districts <- read_csv("../input/zoning-code-summary-district-types.csv", show_col_types = FALSE) |>
  transmute(district = district_type_code, floor_area_ratio = suppressWarnings(as.numeric(floor_area_ratio)))
stopifnot(!anyDuplicated(districts$district))

# 1. The matter records.
matters <- lapply(readLines("../input/elms_zoning_matter_details.jsonl"), jsonlite::fromJSON, simplifyVector = FALSE)
amendments <- bind_rows(lapply(matters, function(m) {
  action_names <- vapply(m$actions, function(a) a$actionName %||% "", character(1))
  action_dates <- as.Date(substr(vapply(m$actions, function(a) a$actionDate %||% NA_character_, character(1)), 1, 10))
  tibble(matter_id = m$matterId, record_number = m$recordNumber, title = m$title,
    introduction_date = as.Date(substr(m$introductionDate, 1, 10)),
    final_action_date = as.Date(substr(m$finalActionDate %||% NA_character_, 1, 10)),
    sub_status = m$subStatus, filing_office = m$filingOffice %||% NA_character_,
    filing_sponsor = m$filingSponsor %||% NA_character_,
    passed_date = suppressWarnings(min(action_dates[action_names %in% c("Passed", "Passed as Substitute")])),
    held_in_committee = any(action_names == "Held in Committee"),
    roll_calls = sum(vapply(m$actions, function(a) length(a$votes) > 0, logical(1))))
})) |>
  mutate(
    passed_date = if_else(is.finite(passed_date), passed_date, as.Date(NA)),
    filed_by_alderman = grepl("^[0-9]+$", filing_office),
    filing_ward = if_else(filed_by_alderman, suppressWarnings(as.integer(filing_office)), NA_integer_),
    outcome = case_when(
      sub_status %in% c("Passed", "Passed as Substitute") ~ "passed",
      sub_status == "Withdrawn" ~ "withdrawn",
      sub_status == "Placed on File" ~ "placed_on_file",
      sub_status == "Failed to Pass" ~ "failed",
      introduction_date < current_term_start ~ "stalled",
      TRUE ~ "pending"),
    days_to_passage = as.integer(passed_date - introduction_date),
    application_number = str_match(title, regex("App(?:lication)?\\.?\\s*No\\.?\\s*([0-9]+)", ignore_case = TRUE))[, 2],
    map_number = str_match(title, regex("Map\\s*No\\.?\\s*([0-9]{1,2}-[A-Z])", ignore_case = TRUE))[, 2],
    address = str_trim(str_match(title, regex("\\bat\\s+(.+?)(?:\\s*-\\s*App.*)?$", ignore_case = TRUE))[, 2]))
stopifnot(!anyDuplicated(amendments$matter_id), !anyNA(amendments$introduction_date))

# 2. Districts before and after. Ordinances state each change as "changing all the <from> District symbols and
# indications as shown on Map No. ... to those of a <to> District"; a change in steps (for example to a district and
# then to a planned development) has several such sentences. Applications also fill in "Present Zoning District" and
# "Proposed Zoning District". OCR confusions in codes are repaired (l or I for 1, O for 0, spaces around hyphens) and
# only codes in the zoning ordinance are kept.
district_codes <- function(clause) {
  if (is.na(clause)) return(character())
  found <- str_match_all(clause, paste0("\\b(RS|RT|RM|B[1-3lI]|C[1-3lI]|M[1-3lI]|DC|DX|DR|DS|POS|PMD)\\s?[-–]?\\s?",
    "([0-9lIO]{1,2}(?:\\.[0-9])?A?)\\b"))[[1]]
  prefix <- str_replace_all(found[, 2], c("l" = "1", "I" = "1"))
  code <- paste0(prefix, "-", str_replace_all(found[, 3], c("l" = "1", "I" = "1", "O" = "0")))
  code[prefix == "PMD"] <- "PMD"
  unique(code[code %in% districts$district])
}
change_sentences <- function(text) {
  str_match_all(str_squish(text), regex(paste0("changing all (?:of )?the (.{0,250}?)symbols(?: and indications)?",
    ".{0,8000}?to those of (?:a |an |the )?(.{0,200}?)(?:district|which|and a corresponding|\\.(?:\\s|$))"),
    ignore_case = TRUE))[[1]]
}
application_fields <- function(text) {
  text <- str_squish(text)
  c(from = str_match(text, regex("present zoning(?: district)?:?\\s+(.{0,80}?)\\s+(?:proposed zoning|\\d+\\.|lot size)",
      ignore_case = TRUE))[, 2],
    to = str_match(text, regex("proposed zoning(?: district)?:?\\s+(.{0,80}?)\\s+(?:\\d+\\.|lot size|current use)",
      ignore_case = TRUE))[, 2])
}
texts <- arrow::read_parquet("../input/zoning_legislation_text.parquet")
file_districts <- bind_rows(lapply(seq_len(nrow(texts)), function(i) {
  sentences <- change_sentences(texts$text[i])
  form <- application_fields(texts$text[i])
  tibble(matter_id = texts$matter_id[i], file_name = texts$file_name[i],
    substitute = startsWith(texts$file_name[i], "SO"),
    change_sentences = nrow(sentences),
    sentence_from = if (nrow(sentences) > 0) paste(district_codes(sentences[1, 2]), collapse = ";") else "",
    sentence_to = if (nrow(sentences) > 0) paste(district_codes(sentences[nrow(sentences), 3]), collapse = ";") else "",
    to_planned_development = nrow(sentences) > 0 &&
      grepl("planned development", sentences[nrow(sentences), 3], ignore.case = TRUE),
    form_from = paste(district_codes(form[["from"]]), collapse = ";"),
    form_to = paste(district_codes(form[["to"]]), collapse = ";"))
}))

# Each matter uses its substitute ordinance where one exists (the version passed), otherwise the introduced one. The
# district before is the application's present district, or else the first sentence's; the district after is the last
# sentence's, or else the application's proposed district.
matter_districts <- file_districts |>
  arrange(matter_id, desc(substitute)) |>
  summarise(
    sentence_from = dplyr::first(sentence_from[sentence_from != ""], default = ""),
    sentence_to = dplyr::first(sentence_to[sentence_to != ""], default = ""),
    change_sentences = dplyr::first(change_sentences[change_sentences > 0], default = 0L),
    to_planned_development = dplyr::first(to_planned_development[sentence_to != "" | to_planned_development],
      default = FALSE),
    form_from = dplyr::first(form_from[form_from != ""], default = ""),
    form_to = dplyr::first(form_to[form_to != ""], default = ""),
    .by = matter_id) |>
  mutate(
    from_districts = if_else(form_from != "", form_from, sentence_from),
    to_districts = if_else(sentence_to != "", sentence_to, form_to),
    from_source = case_when(form_from != "" ~ "application_form", sentence_from != "" ~ "ordinance_sentence",
      TRUE ~ "not_found"),
    to_source = case_when(sentence_to != "" ~ "ordinance_sentence", form_to != "" ~ "application_form",
      to_planned_development ~ "ordinance_sentence", TRUE ~ "not_found"),
    in_steps = change_sentences > 1,
    from_sources_disagree = sentence_from != "" & form_from != "" & sentence_from != form_from,
    to_sources_disagree = sentence_to != "" & form_to != "" & sentence_to != form_to)
stopifnot(nrow(matter_districts) == n_distinct(file_districts$matter_id))

# 3. Direction of the change, by the highest allowed floor-area ratio among the districts before and after.
max_far <- function(codes) {
  vapply(strsplit(codes, ";", fixed = TRUE), function(x) {
    far <- districts$floor_area_ratio[match(x, districts$district)]
    if (length(far) == 0 || all(is.na(far))) NA_real_ else max(far, na.rm = TRUE)
  }, numeric(1))
}
amendments <- amendments |>
  left_join(matter_districts, by = "matter_id", relationship = "one-to-one") |>
  mutate(from_source = coalesce(from_source, "no_legislation_text"), to_source = coalesce(to_source, "no_legislation_text"),
    from_far = max_far(coalesce(from_districts, "")), to_far = max_far(coalesce(to_districts, "")),
    direction = case_when(to_planned_development ~ "to_planned_development",
      is.na(from_far) | is.na(to_far) ~ "unknown",
      to_far > from_far ~ "up", to_far < from_far ~ "down", TRUE ~ "same_far"))

SaveData(amendments, "matter_id", "../output/zoning_map_amendments.csv")
