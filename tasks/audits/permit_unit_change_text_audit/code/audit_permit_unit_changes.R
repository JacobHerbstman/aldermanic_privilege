# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/permit_unit_change_text_audit/code")
# uncertainty_spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(uncertainty_spec)
}
if (length(cli_args) != 1) {
  stop("Script requires one argument: <uncertainty_spec>.", call. = FALSE)
}
uncertainty_spec <- cli_args[1]

permits <- data.table::fread(
  cmd = "gzip -dc ../input/building_permits_text_features.csv.gz",
  select = c(
    "id", "permit", "permit_type_clean", "application_start_date", "ward",
    "project_id", "work_description", "permit_condition", "desc_clean",
    "condition_clean", "flag_residential", "flag_deconversion", "flag_reduction",
    "flag_increase", "flag_adu", "flag_demolition_phrase", "from_to_pair_n",
    "units_from_to_last", "units_to_last", "units_delta_last", "unit_change_text",
    "unit_change_confidence", "unit_change_signal"
  )
)

permits[, application_start_date := as.Date(application_start_date)]
permits[, ward := as.character(suppressWarnings(as.integer(as.character(ward))))]
permits[ward == "NA", ward := NA_character_]
permits[, original_source := data.table::fcase(
  !is.na(units_delta_last), "numeric_from_to",
  flag_deconversion == 1L, "deconversion_fallback",
  flag_adu == 1L, "adu_fallback",
  flag_reduction == 1L & flag_residential == 1L, "reduction_fallback",
  flag_increase == 1L & flag_residential == 1L, "increase_fallback",
  default = "none"
)]

full_scope_summary <- data.table::rbindlist(list(
  permits[, .(
    category = "full_text_feature_file",
    metric = c("permits", "unique_permit_ids", "signals"),
    value = c(.N, data.table::uniqueN(id), sum(unit_change_signal == 1L))
  )],
  permits[
    permit_type_clean %chin% c("PERMIT - NEW CONSTRUCTION", "PERMIT - RENOVATION/ALTERATION"),
    .(
      category = paste0("full_curated_", original_source),
      metric = c("permits", "signals", "positive_signals"),
      value = c(.N, sum(unit_change_signal == 1L), sum(!is.na(unit_change_text) & unit_change_text > 0))
    ),
    by = original_source
  ][, original_source := NULL]
), use.names = TRUE)

permits <- permits[
  application_start_date <= as.Date("2022-12-31") &
    permit_type_clean %chin% c(
      "PERMIT - NEW CONSTRUCTION",
      "PERMIT - RENOVATION/ALTERATION",
      "PERMIT - WRECKING/DEMOLITION",
      "PERMIT - PORCH CONSTRUCTION",
      "PERMIT - REINSTATE REVOKED PMT"
    ) &
    !is.na(application_start_date) &
    !is.na(ward)
]

permits[, text := fifelse(is.na(desc_clean), "", desc_clean)]
compound_tens <- c(
  TWENTY = 20L, THIRTY = 30L, FORTY = 40L, FIFTY = 50L,
  SIXTY = 60L, SEVENTY = 70L, EIGHTY = 80L, NINETY = 90L
)
compound_ones <- c(ONE = 1L, TWO = 2L, THREE = 3L, FOUR = 4L, FIVE = 5L, SIX = 6L, SEVEN = 7L, EIGHT = 8L, NINE = 9L)
for (tens_word in names(compound_tens)) {
  for (ones_word in names(compound_ones)) {
    permits[, text := stringr::str_replace_all(
      text,
      paste0("\\b", tens_word, "\\s+", ones_word, "\\b"),
      as.character(compound_tens[[tens_word]] + compound_ones[[ones_word]])
    )]
  }
}
number_words <- c(
  ONE = 1L, TWO = 2L, THREE = 3L, FOUR = 4L, FIVE = 5L,
  SIX = 6L, SEVEN = 7L, EIGHT = 8L, NINE = 9L, TEN = 10L,
  ELEVEN = 11L, TWELVE = 12L, THIRTEEN = 13L, FOURTEEN = 14L,
  FIFTEEN = 15L, SIXTEEN = 16L, SEVENTEEN = 17L, EIGHTEEN = 18L,
  NINETEEN = 19L, TWENTY = 20L, THIRTY = 30L, FORTY = 40L,
  FIFTY = 50L, SIXTY = 60L, SEVENTY = 70L, EIGHTY = 80L, NINETY = 90L
)
for (number_word in names(number_words)) {
  permits[, text := stringr::str_replace_all(
    text,
    paste0("\\b", number_word, "\\b"),
    as.character(number_words[[number_word]])
  )]
}
permits[, text := stringr::str_replace_all(text, "\\b(D\\s*U\\s*S?)(TO|INTO|NEW)\\b", "\\1 \\2")]
permits[, text := stringr::str_replace_all(text, "\\b(UNITS?)(TO|INTO|NEW)\\b", "\\1 \\2")]
permits[, text := stringr::str_replace_all(text, "(\\d)(D\\s*U\\s*S?|DWELLING|RESIDENTIAL|UNITS?|APTS?)\\b", "\\1 \\2")]
permits[, text := stringr::str_replace_all(text, "\\b(FROM|TO|INTO)(?=\\d)", "\\1 ")]
permits[, text := stringr::str_replace(text, "\\b(?:ORIGINAL\\s+PERMIT\\s+SCOPE|SEE\\s+(?:REVISION|PERMIT|-)).*$", "")]
unit_token <- "(?:D\\.?\\s*U\\.?\\s*S?|DWELLING\\s+UNITS?|DWELLINGS?|RESIDENTIAL\\s+UNITS?|RESI\\s+UNITS?|APARTMENT\\s+UNITS?|APARTMENTS?|APTS?|FLATS?|RESIDENCES?|CONDOS?|CONDOMINIUMS?|SRO\\s+UNITS?|UNITS?)"
unit_modifiers <- "(?:(?:EXISTING|LEGAL|TOTAL|ORIGINAL|NEW|RESIDENTIAL|DWELLING|APARTMENT|SRO|DUPLEX)\\s+){0,4}"
building_modifiers <- "(?:(?:MASONRY|BRICK|FRAME|RESIDENTIAL|APARTMENT|BUILDING|BLDG|RESIDENCE|HOUSE|WITH|BASEMENT|EXISTING|STORY|STORIES|DUPLEX|MULTI[- ]?FAMILY)\\s+){0,6}"

old_unit_token <- "(?:D\\.?\\s*U\\.?|DWELLING(?:\\s+UNITS?)?|UNITS?|APTS?|APARTMENTS?|FLATS?|RESIDENCES?|CONDOS?|CONDOMINIUMS?)"
old_pattern_a <- paste0(
  "\\b(?:FROM|CONVERT(?:ED|ING)?\\s+FROM|DECONVERT(?:ED|ING)?\\s+FROM)\\s*(\\d{1,4})\\s*(?:",
  old_unit_token,
  ")?\\s*(?:TO|INTO)\\s*(\\d{1,4})\\s*(?:",
  old_unit_token,
  ")?\\b"
)
old_pattern_b <- paste0(
  "\\b(\\d{1,4})\\s*",
  old_unit_token,
  "\\s*(?:TO|INTO)\\s*(\\d{1,4})\\s*",
  old_unit_token,
  "\\b"
)
permits[, original_numeric_in_description := stringr::str_detect(text, old_pattern_a) |
  stringr::str_detect(text, old_pattern_b)]
permits[, original_numeric_in_condition := stringr::str_detect(
  fifelse(is.na(condition_clean), "", condition_clean),
  old_pattern_a
) | stringr::str_detect(
  fifelse(is.na(condition_clean), "", condition_clean),
  old_pattern_b
)]

match_direct <- stringr::str_match(
  permits$text,
  paste0("\\b(\\d{1,3})\\s*-?\\s*", unit_token, "\\s+", building_modifiers, "(?:TO|INTO)\\s+(\\d{1,3})\\s*-?\\s*", unit_modifiers, unit_token, "\\b")
)
match_from_trailing <- stringr::str_match(
  permits$text,
  paste0("\\bFROM\\s+(\\d{1,3})\\s+(?:TO|INTO)\\s+(\\d{1,3})\\s*-?\\s*", unit_modifiers, unit_token, "\\b")
)
match_unit_count <- stringr::str_match(
  permits$text,
  "\\b(?:DWELLING\\s+)?UNIT\\s+COUNT\\s+FROM\\s+(\\d{1,3})\\s+(?:TO|INTO)\\s+(\\d{1,3})\\b"
)
match_units_before_from <- stringr::str_match(
  permits$text,
  paste0(
    "\\b(?:NUMBER\\s+OF\\s+|AMOUNT\\s+OF\\s+|TOTAL\\s+)?",
    unit_token,
    "\\s+(?:TO\\s+GO\\s+)?FROM\\s+(\\d{1,3})\\s+(?:TO|INTO)\\s+(\\d{1,3})\\b"
  )
)
match_unit_to_unlabeled_count <- stringr::str_match(
  permits$text,
  paste0(
    "\\b(?:FROM\\s+)?(\\d{1,3})\\s*-?\\s*",
    unit_token,
    "\\s+(?:TO|INTO)\\s+(\\d{1,3})(?=\\s+(?:IN\\s+(?:AN?\\s+)?|FOR\\s+|AS\\s+|WITH\\s+|TOTAL\\b|PER\\s+PLANS)|$)"
  )
)
match_conversion_from_to <- stringr::str_match(
  permits$text,
  paste0(
    "\\b(?:DE[- ]?CONVERT|DECONVERSION|CONVERT|CONVERSION)(?:ED|ING)?",
    ".{0,35}?\\bFROM\\s+(\\d{1,3})\\s+(?:TO|INTO)\\s*(\\d{1,3})",
    "(?!\\s*(?:STOR|CAR|PARK|SPACE|FEET|FOOT|OCCUPANCY|COMMERCIAL))"
  )
)
match_context_from_to <- stringr::str_match(
  permits$text,
  paste0(
    "\\b(?:UNIT\\s+COUNT|(?:NET\\s+)?(?:REDUCTION|INCREASE|CHANGE)\\s+OF\\s+\\d{1,3}\\s+",
    unit_token,
    ").{0,30}\\bFROM\\s+(\\d{1,3})\\s+(?:TO|INTO)\\s+(\\d{1,3})\\b"
  )
)
match_to_single <- stringr::str_match(
  permits$text,
  paste0("\\b(\\d{1,3})\\s*-?\\s*", unit_token, ".{0,80}\\b(?:TO|INTO)\\s+(?:(?:1|A)\\s+)?(?:\\d+\\s+STORY\\s+)?(?:SINGLE\\s+FAMILY|SFR)\\b")
)
match_from_single <- stringr::str_match(
  permits$text,
  paste0("\\bSINGLE\\s+FAMILY.{0,40}\\b(?:TO|INTO)\\s+(\\d{1,3})\\s*-?\\s*", unit_token, "\\b")
)

permits[, `:=`(
  candidate_from = NA_real_,
  candidate_to = NA_real_,
  candidate_source = NA_character_
)]

candidate_patterns <- list(
  list(matches = match_unit_count, source = "unit_count_from_to", reverse = FALSE),
  list(matches = match_context_from_to, source = "unit_context_from_to", reverse = FALSE),
  list(matches = match_units_before_from, source = "units_before_from_to", reverse = FALSE),
  list(matches = match_unit_to_unlabeled_count, source = "unit_to_unlabeled_count", reverse = FALSE),
  list(matches = match_conversion_from_to, source = "conversion_from_to", reverse = FALSE),
  list(matches = match_direct, source = "unit_labeled_from_to", reverse = FALSE),
  list(matches = match_from_trailing, source = "from_to_trailing_unit", reverse = FALSE)
)

for (candidate_pattern in candidate_patterns) {
  candidate_rows <- which(
    is.na(permits$candidate_source) &
      !is.na(candidate_pattern$matches[, 2]) &
      !is.na(candidate_pattern$matches[, 3])
  )
  if (length(candidate_rows) > 0) {
    first_value <- suppressWarnings(as.numeric(candidate_pattern$matches[candidate_rows, 2]))
    second_value <- suppressWarnings(as.numeric(candidate_pattern$matches[candidate_rows, 3]))
    if (candidate_pattern$reverse) {
      permits[candidate_rows, `:=`(
        candidate_from = second_value,
        candidate_to = first_value,
        candidate_source = candidate_pattern$source
      )]
    } else {
      permits[candidate_rows, `:=`(
        candidate_from = first_value,
        candidate_to = second_value,
        candidate_source = candidate_pattern$source
      )]
    }
  }
}

candidate_rows <- which(is.na(permits$candidate_source) & !is.na(match_to_single[, 2]))
if (length(candidate_rows) > 0) {
  permits[candidate_rows, `:=`(
    candidate_from = suppressWarnings(as.numeric(match_to_single[candidate_rows, 2])),
    candidate_to = 1,
    candidate_source = "units_to_single_family"
  )]
}

housing_context <- stringr::str_detect(
  permits$text,
  "\\b(D\\s*U|DWELLING|RESIDENTIAL|APARTMENTS?|APTS?|FLATS?|RESIDENCES?|CONDOS?|CONDOMINIUMS?|SRO)\\b"
)
hotel_only_rows <- which(
  !is.na(permits$candidate_source) &
    stringr::str_detect(permits$text, "\\b(HOTEL|GUEST\\s*ROOMS?)\\b") &
    !housing_context
)
if (length(hotel_only_rows) > 0) {
  permits[hotel_only_rows, `:=`(
    candidate_from = NA_real_,
    candidate_to = NA_real_,
    candidate_source = NA_character_
  )]
}

candidate_rows <- which(is.na(permits$candidate_source) & !is.na(match_from_single[, 2]))
if (length(candidate_rows) > 0) {
  permits[candidate_rows, `:=`(
    candidate_from = 1,
    candidate_to = suppressWarnings(as.numeric(match_from_single[candidate_rows, 2])),
    candidate_source = "single_family_to_units"
  )]
}

explicit_add_one <- stringr::str_detect(
  permits$text,
  "\\b(ADD|ADDING|ADDED|CREATE|CREATING|ESTABLISH|ESTABLISHING)\\b.{0,30}\\b(?:A|ONE|1)\\s+(?:NEW\\s+)?(?:DWELLING|RESIDENTIAL)\\s+UNIT\\b"
)
explicit_addition_count_match <- stringr::str_match(
  permits$text,
  "\\b(?:ADD|ADDING|CREATE|CREATING|ESTABLISH|ESTABLISHING)\\b.{0,30}\\b(\\d{1,2})\\s+(?:NEW\\s+)?(?:D\\s*U|DWELLING\\s+UNITS?|RESIDENTIAL\\s+UNITS?)\\b"
)
explicit_remove_one <- stringr::str_detect(
  permits$text,
  "\\b(REMOVE|REMOVING|ELIMINATE|ELIMINATING|DECONVERT)\\b.{0,30}\\b(?:A|ONE|1|ILLEGAL)\\s+(?:BASEMENT\\s+)?(?:DWELLING\\s+UNIT|APARTMENT)\\b"
)

add_one_rows <- which(is.na(permits$candidate_source) & explicit_add_one)
if (length(add_one_rows) > 0) {
  permits[add_one_rows, `:=`(
    candidate_from = NA_real_,
    candidate_to = NA_real_,
    candidate_source = "explicit_one_unit_addition"
  )]
}
addition_count_rows <- which(
  is.na(permits$candidate_source) &
    !is.na(explicit_addition_count_match[, 2])
)
if (length(addition_count_rows) > 0) {
  permits[addition_count_rows, candidate_source := "explicit_unit_addition_count"]
}
remove_one_rows <- which(is.na(permits$candidate_source) & explicit_remove_one)
if (length(remove_one_rows) > 0) {
  permits[remove_one_rows, `:=`(
    candidate_from = NA_real_,
    candidate_to = NA_real_,
    candidate_source = "explicit_one_unit_removal"
  )]
}

directional_deconversion <- stringr::str_detect(
  permits$text,
  "\\b(?:DE[- ]?CONVERT|DECONVERSION|DECONVERTION)(?:ED|ING|ION)?\\b.{0,50}\\b(BASEMENT|BSMT|ATTIC|APARTMENT|DWELLING\\s+UNIT|UNIT\\b|ORIGINAL\\s+USE|SINGLE\\s+FAMILY|SFR)\\b|\\b(BASEMENT|BSMT|ATTIC).{0,20}\\b(?:DECONVERSION|DECONVERTION)\\b"
)
directional_adu_addition <- stringr::str_detect(
  permits$text,
  "\\b(ADD|CREATE|ESTABLISH|CONSTRUCT|ERECT|BUILD|PROPOSED|REQUEST\\s+FOR|LEGALIZE)\\b.{0,50}\\b(ADU|ACCESSORY\\s+DWELLING|GARDEN\\s+UNIT|COACH\\s+HOUSE)\\b|\\bNEW\\s+(?:BASEMENT\\s+)?ADU\\b|\\bADU\\s+IN\\s+BASEMENT\\b"
)
directional_unit_addition <- stringr::str_detect(
  permits$text,
  "\\b(ADD|ADDING|ADDED|CREATE|CREATING|ESTABLISH|ESTABLISHING)\\b.{0,40}\\b(DWELLING\\s+UNIT|RESIDENTIAL\\s+UNIT|D\\s*U)\\b|\\bADDITION\\s+OF\\s+\\d+(?:ST|ND|RD|TH)\\s+(?:D\\s*U|DWELLING\\s+UNIT|RESIDENTIAL\\s+UNIT)\\b"
)

deconversion_rows <- which(is.na(permits$candidate_source) & directional_deconversion)
if (length(deconversion_rows) > 0) {
  permits[deconversion_rows, candidate_source := "direction_only_deconversion"]
}
adu_rows <- which(is.na(permits$candidate_source) & directional_adu_addition)
if (length(adu_rows) > 0) {
  permits[adu_rows, candidate_source := "direction_only_adu_addition"]
}
unit_addition_rows <- which(is.na(permits$candidate_source) & directional_unit_addition)
if (length(unit_addition_rows) > 0) {
  permits[unit_addition_rows, candidate_source := "direction_only_unit_addition"]
}
permits[, candidate_exact_change := candidate_to - candidate_from]
permits[candidate_source == "explicit_one_unit_addition", candidate_exact_change := 1]
permits[candidate_source == "explicit_one_unit_removal", candidate_exact_change := -1]
permits[addition_count_rows, candidate_exact_change :=
  suppressWarnings(as.numeric(explicit_addition_count_match[addition_count_rows, 2]))]
permits[, candidate_direction := data.table::fcase(
  !is.na(candidate_exact_change) & candidate_exact_change > 0, 1L,
  !is.na(candidate_exact_change) & candidate_exact_change < 0, -1L,
  candidate_source == "direction_only_deconversion", -1L,
  candidate_source == "direction_only_adu_addition", 1L,
  candidate_source == "direction_only_unit_addition", 1L,
  default = 0L
)]
permits[, candidate_change_signal := as.integer(candidate_direction != 0L)]
permits[, candidate_exact_signal := as.integer(!is.na(candidate_exact_change) & candidate_exact_change != 0)]
permits[, candidate_direction_only_signal := as.integer(
  candidate_change_signal == 1L & candidate_exact_signal == 0L
)]
permits[, candidate_units_reduced := data.table::fifelse(
  !is.na(candidate_exact_change) & candidate_exact_change < 0,
  -candidate_exact_change,
  0
)]
permits[, original_units_reduced := data.table::fifelse(
  !is.na(unit_change_text) & unit_change_text < 0,
  -unit_change_text,
  0
)]

summary_rows <- data.table::rbindlist(list(
  full_scope_summary,
  permits[, .(
    category = "sample",
    metric = c("permits", "unique_projects", "missing_descriptions"),
    value = c(.N, data.table::uniqueN(project_id), sum(is.na(work_description) | trimws(work_description) == ""))
  )],
  permits[, .(
    category = paste0("original_", original_source),
    metric = c("permits", "signals", "units_reduced"),
    value = c(.N, sum(unit_change_signal == 1L), sum(original_units_reduced))
  ), by = original_source][, original_source := NULL],
  permits[, .(
    category = paste0("candidate_", data.table::fifelse(is.na(candidate_source), "none", candidate_source)),
    metric = c("permits", "signals", "units_reduced"),
    value = c(.N, sum(candidate_change_signal), sum(candidate_units_reduced))
  ), by = candidate_source][, candidate_source := NULL],
  permits[, .(
    category = paste0("candidate_permit_type_", tolower(gsub("[^A-Z]+", "_", permit_type_clean))),
    metric = c("permits", "signals", "exact_signals", "direction_only_signals", "units_added", "units_reduced"),
    value = c(
      .N,
      sum(candidate_change_signal),
      sum(candidate_exact_signal),
      sum(candidate_direction_only_signal),
      sum(data.table::fifelse(candidate_exact_change > 0, candidate_exact_change, 0), na.rm = TRUE),
      sum(candidate_units_reduced)
    )
  ), by = permit_type_clean][, permit_type_clean := NULL],
  data.table::data.table(
    category = "comparison",
    metric = c(
      "original_signals", "candidate_signals", "original_units_reduced",
      "candidate_units_reduced", "original_numeric_rejected_by_candidate",
      "candidate_signals_missed_by_original", "original_adu_demolition_positive",
      "original_numeric_zero_coded_as_change",
      "original_numeric_condition_only", "original_signal_share",
      "candidate_signal_share", "candidate_exact_signals",
      "candidate_direction_only_signals", "candidate_increase_signals",
      "candidate_reduction_signals", "candidate_exact_units_added",
      "candidate_exact_increase_signals", "candidate_exact_reduction_signals",
      "candidate_mean_exact_units_added", "candidate_median_exact_units_added",
      "candidate_mean_exact_units_reduced", "candidate_median_exact_units_reduced",
      "candidate_mean_exact_change", "candidate_median_exact_change",
      "candidate_p90_absolute_exact_change", "candidate_max_absolute_exact_change",
      "original_signal_projects", "candidate_signal_projects",
      "candidate_signal_permits_beyond_first_project_signal"
    ),
    value = c(
      permits[, sum(unit_change_signal == 1L)],
      permits[, sum(candidate_change_signal == 1L)],
      permits[, sum(original_units_reduced)],
      permits[, sum(candidate_units_reduced)],
      permits[!is.na(units_delta_last) & candidate_change_signal == 0L, .N],
      permits[candidate_change_signal == 1L & unit_change_signal == 0L, .N],
      permits[original_source == "adu_fallback" & flag_demolition_phrase == 1L & unit_change_text > 0, .N],
      permits[unit_change_signal == 1L & unit_change_text == 0, .N],
      permits[!is.na(units_delta_last) & !original_numeric_in_description & original_numeric_in_condition, .N],
      permits[, mean(unit_change_signal == 1L)],
      permits[, mean(candidate_change_signal == 1L)],
      permits[, sum(candidate_exact_signal)],
      permits[, sum(candidate_direction_only_signal)],
      permits[, sum(candidate_direction == 1L)],
      permits[, sum(candidate_direction == -1L)],
      permits[, sum(data.table::fifelse(candidate_exact_change > 0, candidate_exact_change, 0), na.rm = TRUE)],
      permits[candidate_exact_change > 0, .N],
      permits[candidate_exact_change < 0, .N],
      permits[candidate_exact_change > 0, mean(candidate_exact_change)],
      permits[candidate_exact_change > 0, stats::median(candidate_exact_change)],
      permits[candidate_exact_change < 0, mean(-candidate_exact_change)],
      permits[candidate_exact_change < 0, stats::median(-candidate_exact_change)],
      permits[candidate_exact_signal == 1L, mean(candidate_exact_change)],
      permits[candidate_exact_signal == 1L, stats::median(candidate_exact_change)],
      permits[candidate_exact_signal == 1L, stats::quantile(abs(candidate_exact_change), 0.9, names = FALSE)],
      permits[candidate_exact_signal == 1L, max(abs(candidate_exact_change))],
      permits[unit_change_signal == 1L, data.table::uniqueN(project_id, na.rm = TRUE)],
      permits[candidate_change_signal == 1L, data.table::uniqueN(project_id, na.rm = TRUE)],
      permits[, sum(candidate_change_signal == 1L) - data.table::uniqueN(project_id[candidate_change_signal == 1L], na.rm = TRUE)]
    )
  )
), use.names = TRUE, fill = TRUE)
summary_rows <- summary_rows[, .(category, metric, value)]

set.seed(20260713)
permits[, review_group := data.table::fcase(
  !is.na(units_delta_last) & !original_numeric_in_description & original_numeric_in_condition, "original_numeric_condition_only",
  !is.na(units_delta_last) & candidate_change_signal == 0L, "original_numeric_rejected_by_candidate",
  candidate_change_signal == 1L & unit_change_signal == 0L, "candidate_missed_by_original",
  original_source == "reduction_fallback", "original_reduction_fallback",
  original_source == "increase_fallback", "original_increase_fallback",
  original_source == "adu_fallback", "original_adu_fallback",
  original_source == "deconversion_fallback", "original_deconversion_fallback",
  original_source == "numeric_from_to", "original_numeric_retained_or_changed",
  default = NA_character_
)]

review_sample <- permits[!is.na(review_group), {
  selected_rows <- sample.int(.N, min(.N, 30L))
  .SD[selected_rows]
}, by = review_group]
largest_change_review <- head(
  permits[candidate_exact_signal == 1L][order(-abs(candidate_exact_change))],
  30L
)
largest_change_review[, review_group := "candidate_largest_absolute_exact_change"]
review_sample <- data.table::rbindlist(
  list(review_sample, largest_change_review),
  use.names = TRUE,
  fill = TRUE
)
review_sample <- review_sample[, .(
  review_group,
  id,
  permit,
  permit_type_clean,
  application_start_date,
  ward,
  original_source,
  units_from_to_last,
  units_to_last,
  units_delta_last,
  unit_change_text,
  candidate_source,
  candidate_from,
  candidate_to,
  candidate_exact_change,
  candidate_direction,
  work_description,
  permit_condition
)]

permits[, month := zoo::as.yearmon(application_start_date)]
alderman_panel <- data.table::fread("../input/chicago_alderman_panel.csv")
alderman_panel[, ward := as.character(suppressWarnings(as.integer(as.character(ward))))]
alderman_panel[ward == "NA", ward := NA_character_]
alderman_panel[, month := zoo::as.yearmon(month, "%b %Y")]
alderman_panel <- unique(alderman_panel[!is.na(ward) & !is.na(month) & !is.na(alderman), .(ward, month, alderman)])
if (anyDuplicated(alderman_panel, by = c("ward", "month")) > 0) {
  stop("Alderman panel is not unique by ward-month.", call. = FALSE)
}

permits <- merge(permits, alderman_panel, by = c("ward", "month"), all.x = TRUE)
permits <- permits[!is.na(alderman)]
permits[, alderman_key := toupper(trimws(iconv(as.character(alderman), to = "ASCII//TRANSLIT")))]
permits[, alderman_key := trimws(gsub("\\s+", " ", gsub("[^A-Z ]+", " ", alderman_key)))]

scores <- data.table::fread(sprintf("../input/alderman_uncertainty_index_%s.csv", uncertainty_spec))
scores[, alderman_key := toupper(trimws(iconv(as.character(alderman), to = "ASCII//TRANSLIT")))]
scores[, alderman_key := trimws(gsub("\\s+", " ", gsub("[^A-Z ]+", " ", alderman_key)))]
scores[, uncertainty_index := suppressWarnings(as.numeric(uncertainty_index))]
scores <- scores[!is.na(uncertainty_index), .(alderman_key, uncertainty_index)]
if (anyDuplicated(scores, by = "alderman_key") > 0) {
  stop("Stringency scores are not unique by alderman.", call. = FALSE)
}

permits <- merge(permits, scores, by = "alderman_key", all.x = TRUE)
permits <- permits[!is.na(uncertainty_index)]

ward_month <- permits[, .(
  n_permits = .N,
  original_share_change = mean(unit_change_signal == 1L),
  candidate_share_change = mean(candidate_change_signal == 1L),
  candidate_exact_share_change = mean(candidate_exact_signal == 1L),
  candidate_direction_only_share_change = mean(candidate_direction_only_signal == 1L),
  original_units_reduced_permit = sum(original_units_reduced) / .N,
  candidate_units_reduced_permit = sum(candidate_units_reduced) / .N,
  uncertainty_index = unique(uncertainty_index),
  alderman_key = unique(alderman_key)
), by = .(ward, month)]

model_outcomes <- c(
  "n_permits",
  "original_share_change",
  "candidate_share_change",
  "candidate_exact_share_change",
  "candidate_direction_only_share_change",
  "original_units_reduced_permit",
  "candidate_units_reduced_permit"
)
model_rows <- vector("list", length(model_outcomes))
for (i in seq_along(model_outcomes)) {
  outcome_i <- model_outcomes[i]
  formula_i <- as.formula(paste0(outcome_i, " ~ uncertainty_index"))
  model_iid <- fixest::feols(formula_i, data = ward_month, weights = ~n_permits, warn = FALSE)
  model_cluster <- fixest::feols(formula_i, data = ward_month, weights = ~n_permits, cluster = ~alderman_key, warn = FALSE)
  iid_table <- fixest::coeftable(model_iid)
  cluster_table <- fixest::coeftable(model_cluster)
  model_rows[[i]] <- data.table::data.table(
    outcome = outcome_i,
    estimate = iid_table["uncertainty_index", "Estimate"],
    iid_se = iid_table["uncertainty_index", "Std. Error"],
    iid_p_value = iid_table["uncertainty_index", "Pr(>|t|)"],
    alderman_clustered_se = cluster_table["uncertainty_index", "Std. Error"],
    alderman_clustered_p_value = cluster_table["uncertainty_index", "Pr(>|t|)"],
    ward_months = stats::nobs(model_iid),
    aldermen = data.table::uniqueN(ward_month$alderman_key)
  )
}
model_comparison <- data.table::rbindlist(model_rows)

data.table::fwrite(summary_rows, "../output/permit_unit_change_text_audit_summary.csv")
data.table::fwrite(review_sample, "../output/permit_unit_change_text_review_sample.csv")
data.table::fwrite(model_comparison, "../output/permit_unit_change_model_comparison.csv")
