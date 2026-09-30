# setwd("tasks/clean_zoning_map_amendments/code")
# One row per zoning map amendment in the City Clerk's eLMS records (tasks/download_elms_matters), 2010-2026: dates,
# outcome, who filed it, the address and application number from its title, and the zoning districts before and
# after the change, read from its legislation text (tasks/extract_zoning_legislation_text).
# Matters not acted on lapse at the end of a council term; a matter never passed and introduced before the current
# term began is coded stalled, one introduced during the current term and not yet decided is pending. Some matters
# have two records (step 1b); each matter is one row.
current_term_start <- as.Date("2023-05-15")
# A matter with several records takes the outcome of the first record in this order.
outcome_order <- c("passed", "withdrawn", "placed_on_file", "failed", "pending", "stalled")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

# The zoning ordinance's district codes and their allowed floor-area ratios (City of Chicago zoning summary).
districts <- read_csv("../input/zoning-code-summary-district-types.csv", show_col_types = FALSE) |>
  transmute(district = district_type_code, floor_area_ratio = suppressWarnings(as.numeric(floor_area_ratio)))
stopifnot(!anyDuplicated(districts$district))

# 1. The matter records. The download also holds 28 communications (residents' and aldermen's objections to or notices
# about rezonings) and 6 resolutions on zoning exceptions filed under the zoning category; only ordinances are map
# amendments.
matters <- lapply(readLines("../input/elms_zoning_matter_details.jsonl"), jsonlite::fromJSON, simplifyVector = FALSE)
matters <- Filter(function(m) identical(m$type, "Ordinance"), matters)
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
    # Applicants' application numbers are numeric ("App No. 17212", "App 20897"); "A" numbers (A8496) name a series of
    # aldermen's amendments and are shared by different ordinances.
    application_number = str_match(title, regex("App(?:lication)?\\.?\\s*(?:No\\.?)?\\s*([0-9]{4,})",
      ignore_case = TRUE))[, 2],
    map_number = str_match(title, regex("Map\\s*No\\.?\\s*([0-9]{1,2}-[A-Z])", ignore_case = TRUE))[, 2],
    address = str_trim(str_match(title, regex("\\bat\\s+(.+?)(?:\\s*-\\s*App.*)?$", ignore_case = TRUE))[, 2]))
stopifnot(!anyDuplicated(amendments$matter_id), !anyNA(amendments$introduction_date))

# 1b. Records of the same matter. When the City Clerk moved to its new system in 2023, matters still pending were
# given a second record, with the original introduction date, on which later actions were recorded; a few ordinances
# were also entered twice. Records introduced the same day with the same title or the same application number are one
# matter. Groups are formed from the title and merged where records share an application number. The matter is
# represented by the record on which it was resolved (by outcome_order, then the later final action); all its record
# numbers are listed.
same_title <- paste(amendments$introduction_date, str_to_lower(str_squish(amendments$title)))
same_application <- if_else(is.na(amendments$application_number), amendments$matter_id,
  paste(amendments$introduction_date, amendments$application_number))
record_group <- match(same_title, same_title)
repeat {
  merged <- as.integer(ave(as.integer(ave(record_group, same_application, FUN = min)), same_title, FUN = min))
  if (identical(merged, record_group)) break
  record_group <- merged
}
records <- amendments |>
  mutate(record_group = record_group) |>
  arrange(record_group, match(outcome, outcome_order), desc(final_action_date), record_number) |>
  mutate(kept_matter_id = dplyr::first(matter_id), record_numbers = paste(sort(record_number), collapse = ";"),
    records = n(), .by = record_group)
stopifnot(all(records$outcome %in% outcome_order))
amendments <- records |>
  filter(matter_id == kept_matter_id) |>
  select(-record_group, -kept_matter_id)

# 2. Districts before and after. Ordinances state each change as "changing all the <from> District symbols and
# indications as shown on Map No. ... to those of a <to> District"; a change in steps (for example to a district and
# then to a planned development) has several such sentences. Applications also fill in "Present Zoning District" and
# "Proposed Zoning District". OCR confusions in codes are repaired (l or I for 1, O for 0, 8 for B before a digit
# and hyphen as in "83-2", S for 5 in "RMS", spaces between letters and around hyphens as in "R M 5" and "B l - l")
# and only codes in the zoning ordinance are kept.
district_codes <- function(clause) {
  if (is.na(clause)) return(character())
  clause <- str_replace_all(clause, c("\\b8([1-3])\\s?-\\s?(?=[0-9lI])" = "B\\1-", "\\bRMS(?=\\b|\\.)" = "RM5",
    "\\bR\\s([MTS])\\s?(?=[0-9])" = "R\\1", "\\b([BCM])\\s([1-3lI])\\s?-\\s?" = "\\1\\2-"))
  found <- str_match_all(clause, paste0("\\b(RS|RT|RM|B[1-3lI]|C[1-3lI]|M[1-3lI]|DC|DX|DR|DS|POS|PMD)\\s?[-–]?\\s?",
    "([0-9lIO]{1,2}(?:\\.[0-9])?A?)\\b"))[[1]]
  prefix <- str_replace_all(found[, 2], c("l" = "1", "I" = "1"))
  code <- paste0(prefix, "-", str_replace_all(found[, 3], c("l" = "1", "I" = "1", "O" = "0")))
  code[prefix == "PMD"] <- "PMD"
  unique(code[code %in% districts$district])
}
# The sentence's wording varies and OCR mangles it: "changing all of the", "changing all ofthe", "changing the",
# "changing ail", "changingill" or "changuig all" before the district; "symbols" misread ("s3aTibols"), singular or
# left out ("District and indications", "District, as shown on Map"); "to those of", "to the designation of", "lo" or
# "10 those of", "tho.se", "thosc", "tiiose" or "o f", with the article run on ("ofthe", "ofa"). The new district ends
# at "District", "which" or "is hereby" (after "which" misread: "vvhich", "wfiich").
change_sentences <- function(text) {
  str_match_all(str_squish(text), regex(paste0(
    "chang(?:ing|uig)\\s*(?:a\\s?[il1]\\s?[il1]|ill)?\\s*(?:o\\s?f\\s*)?(?:the\\s*)?(.{0,250}?)",
    "(?:\\bs\\w{3,6}ls\\b(?: and indications)?|\\bsymbol\\b|\\band indications\\b|\\bdesignations?\\b|",
    ",?\\s*(?:as\\s+)?shown\\s+on\\s+map).{0,8000}?",
    "(?:to|\\blo|\\b10)\\s+(?:t(?:h|ii|li)\\W?o\\W?s\\W?[ec]|the\\s+designations?)\\s*o\\s?[f!]\\s*",
    "(?:(?:a|an|the)\\b\\s*)?(.{0,200}?)(?:district|which|\\bis hereby\\b|and a corresponding|\\.(?:\\s|$))"),
    ignore_case = TRUE))[[1]]
}
# "Planned Development" as OCR reads it ("Plarmed", "Plaimed", "Developmenl", "FJevelopment").
planned_development <- "p\\s?la\\w{1,4}ed\\W{0,2}\\w{0,2}evel"
# Every ordinance establishing or amending a planned development attaches its statements ("PLANNED DEVELOPMENT NO.
# 1230, AS AMENDED PLANNED DEVELOPMENT STATEMENTS", "PLANNED DEVELOPMENT NO. PLAN OF DEVELOPMENT STATEMENTS").
planned_development_statements <- paste0(planned_development, "\\w*\\W{0,3}(?:no\\.?|number)?[\\s\\d_\\[\\]#.]{0,12}",
  "\\W{0,3}statements|plan\\s+of\\s+develop\\w*\\W{0,3}statements")
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
    # A substitute's file name starts with SO, or S0 as some are typed ("S02023-0002715 Final Ordinance.pdf").
    substitute = grepl("^S[O0]", texts$file_name[i]),
    change_sentences = nrow(sentences),
    sentence_from = if (nrow(sentences) > 0) paste(district_codes(sentences[1, 2]), collapse = ";") else "",
    sentence_to = if (nrow(sentences) > 0) paste(district_codes(sentences[nrow(sentences), 3]), collapse = ";") else "",
    # A change to a planned development is its last step, but a file may print its ordinance twice, so any step to a
    # planned development counts, as do the planned development's statements where no step is read as one.
    to_planned_development = (nrow(sentences) > 0 &&
      any(grepl(planned_development, sentences[, 3], ignore.case = TRUE, perl = TRUE))) ||
      grepl(planned_development_statements, str_squish(texts$text[i]), ignore.case = TRUE, perl = TRUE),
    form_from = paste(district_codes(form[["from"]]), collapse = ";"),
    form_to = paste(district_codes(form[["to"]]), collapse = ";"))
}))

# Each matter uses its substitute ordinance where one exists (the version passed), otherwise the introduced one, from
# the files of all its records. The district before is the application's present district, or else the first
# sentence's; the district after is the last sentence's, or else the application's proposed district. The district
# after as each version states it, and whether each version is to a planned development (missing where the matter has
# no file of that version), are kept for comparing the two.
matter_districts <- file_districts |>
  inner_join(select(records, matter_id, kept_matter_id), by = "matter_id", relationship = "many-to-one") |>
  mutate(matter_id = kept_matter_id, read = sentence_to != "" | to_planned_development) |>
  arrange(matter_id, desc(substitute)) |>
  summarise(
    introduced_to_districts = dplyr::first(sentence_to[!substitute & sentence_to != ""], default = ""),
    substitute_to_districts = dplyr::first(sentence_to[substitute & sentence_to != ""], default = ""),
    introduced_to_planned_development = if (any(!substitute)) any(to_planned_development[!substitute]) else NA,
    substitute_to_planned_development = if (any(substitute)) any(to_planned_development[substitute]) else NA,
    sentence_from = dplyr::first(sentence_from[sentence_from != ""], default = ""),
    sentence_to = dplyr::first(sentence_to[sentence_to != ""], default = ""),
    change_sentences = dplyr::first(change_sentences[change_sentences > 0], default = 0L),
    to_planned_development = dplyr::first(to_planned_development[read], default = FALSE),
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
stopifnot(!anyDuplicated(matter_districts$matter_id), all(matter_districts$matter_id %in% amendments$matter_id))

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
