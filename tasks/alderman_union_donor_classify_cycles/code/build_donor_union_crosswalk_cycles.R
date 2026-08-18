# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/alderman_union_donor_classify_cycles/code")
# high_dollar_donor_min <- 10000

source("../../setup_environment/code/packages.R")

donor_categories <- c("construction_trades", "teacher_education", "public_sector_service", "generic_labor")

category_patterns <- data.table::data.table(
  donor_category = c(
    rep("teacher_education", 12),
    rep("construction_trades", 21),
    rep("public_sector_service", 12),
    rep("generic_labor", 7)
  ),
  pattern = c(
    "\\bchicago teachers union\\b",
    "\\bctu\\b",
    "\\bamerican federation of teachers\\b",
    "\\baft\\b",
    "\\billinois federation of teachers\\b",
    "\\bift\\b",
    "\\billinois education association\\b",
    "\\biea\\b",
    "\\bipace\\b",
    "\\bi p a c e\\b",
    "\\bteachers union\\b",
    "\\bteacher union\\b",
    "\\blaborers\\b",
    "\\bliuna\\b",
    "\\boperating engineers\\b",
    "\\blocal 150\\b",
    "\\bcarpenters\\b",
    "\\bibew\\b",
    "\\belectrical workers\\b",
    "\\bplumbers\\b",
    "\\bpipefitters\\b",
    "\\bpipe fitters\\b",
    "\\bsheet metal\\b",
    "\\biron workers\\b",
    "\\bironworkers\\b",
    "\\bpainters district council\\b",
    "\\broofers\\b",
    "\\bsprinkler fitters\\b",
    "\\bbricklayers\\b",
    "\\bcement masons\\b",
    "\\belevator constructors\\b",
    "\\bbuilding trades\\b",
    "\\bheat and frost\\b",
    "\\bseiu\\b",
    "\\bservice employees\\b",
    "\\bafscme\\b",
    "\\bufcw\\b",
    "\\bunite here\\b",
    "\\bhotel employees\\b",
    "\\bnurses association\\b",
    "\\bfire fighters\\b",
    "\\bfirefighters\\b",
    "\\bmunicipal employees\\b",
    "\\bstate county and municipal employees\\b",
    "\\bstate county municipal employees\\b",
    "\\bafl cio\\b",
    "\\bfederation of labor\\b",
    "\\blabor council\\b",
    "\\bcentral labor\\b",
    "\\bcope\\b",
    "\\bunion pac\\b",
    "\\bworking families\\b"
  )
)
category_patterns[, pattern_order := .I]

normalize_text <- function(value) {
  text <- ifelse(is.na(value), "", as.character(value))
  text <- iconv(text, from = "", to = "UTF-8", sub = " ")
  text <- tolower(gsub("&", " and ", text, fixed = TRUE))
  text <- gsub("[^a-z0-9 ]+", " ", text)
  trimws(gsub("\\s+", " ", text))
}

parse_bool <- function(value) {
  tolower(trimws(as.character(value))) == "true"
}

money_column <- function(value, label) {
  text <- as.character(value)
  text[is.na(text)] <- ""
  text <- trimws(text)
  cleaned <- gsub(",", "", text, fixed = TRUE)
  cleaned <- gsub("$", "", cleaned, fixed = TRUE)
  parenthetical_negative <- grepl("^\\(.*\\)$", cleaned)
  cleaned <- gsub("^\\((.*)\\)$", "\\1", cleaned)
  out <- suppressWarnings(as.numeric(cleaned))
  bad <- nzchar(cleaned) & is.na(out)
  if (any(bad)) {
    stop(sprintf("Could not parse %s values: %s", label, paste(head(unique(text[bad]), 20), collapse = "; ")), call. = FALSE)
  }
  out[is.na(out)] <- 0
  out[parenthetical_negative] <- -out[parenthetical_negative]
  out
}

donor_display_name <- function(first_name, last_name) {
  first_name <- as.character(first_name)
  last_name <- as.character(last_name)
  first_name[is.na(first_name)] <- ""
  last_name[is.na(last_name)] <- ""
  first_name <- trimws(iconv(first_name, from = "", to = "UTF-8", sub = " "))
  last_name <- trimws(iconv(last_name, from = "", to = "UTF-8", sub = " "))
  out <- ifelse(nzchar(first_name), paste(first_name, last_name), last_name)
  trimws(out)
}

validate_unique <- function(df, keys, label) {
  if (anyDuplicated(df, by = keys) > 0) {
    stop(sprintf("%s must be unique by %s.", label, paste(keys, collapse = ", ")), call. = FALSE)
  }
}

ensure_columns <- function(df, columns) {
  for (column in columns) {
    if (!(column %in% names(df))) {
      df[, (column) := ""]
    }
  }
  df
}

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(high_dollar_donor_min)
}
if (length(cli_args) != 1) {
  stop("Expected argument: high_dollar_donor_min.", call. = FALSE)
}
high_dollar_donor_min <- suppressWarnings(as.numeric(cli_args[1]))
if (is.na(high_dollar_donor_min) || high_dollar_donor_min < 0) {
  stop("high_dollar_donor_min must be nonnegative.", call. = FALSE)
}

receipts <- data.table::fread(
  "../input/isbe_receipts_full_cycles_raw.tsv",
  sep = "\t",
  select = c("cycle_year", "CommitteeID", "D2Part", "Archived", "LastOnlyName", "FirstName", "Amount", "Occupation", "Employer", "Description"),
  colClasses = "character",
  na.strings = "",
  encoding = "UTF-8",
  showProgress = TRUE
)
committee_crosswalk <- data.table::fread("../input/alderman_committee_crosswalk_cycles.csv", colClasses = "character")

included <- committee_crosswalk[
  parse_bool(include_strict_candidate) |
    parse_bool(include_main_named_recipient) |
    parse_bool(include_candidate_clean),
  .(cycle_year = trimws(cycle_year), committee_id = trimws(committee_id))
]
validate_unique(included, c("cycle_year", "committee_id"), "included committee crosswalk")
included[, recipient_key := paste(cycle_year, committee_id, sep = "|")]

receipts[, cycle_year := trimws(cycle_year)]
receipts[, committee_id := trimws(CommitteeID)]
receipts[, archived := parse_bool(Archived)]
receipts[, d2part_code := trimws(D2Part)]
receipts <- receipts[!archived & d2part_code %in% c("1A", "2A", "5A")]
receipts[, recipient_key := paste(cycle_year, committee_id, sep = "|")]
receipts <- receipts[recipient_key %in% included$recipient_key]
receipts[, donor_name := donor_display_name(FirstName, LastOnlyName)]
receipts[, donor_norm := normalize_text(donor_name)]
receipts <- receipts[donor_norm != ""]
receipts[, amount_numeric := money_column(Amount, "receipt Amount")]

crosswalk <- receipts[, .(
  donor_name = min(donor_name),
  raw_name_count = data.table::uniqueN(donor_name),
  receipt_count = .N,
  cycle_count = data.table::uniqueN(cycle_year),
  total_amount_full_cycles = round(sum(amount_numeric, na.rm = TRUE), 2)
), by = donor_norm]
data.table::setorder(crosswalk, donor_norm)
crosswalk[, donor_category := ""]
crosswalk[, match_reason := ""]
crosswalk[, confidence := ""]
crosswalk[, review_flag := FALSE]

for (i in seq_len(nrow(category_patterns))) {
  pattern_i <- category_patterns$pattern[i]
  category_i <- category_patterns$donor_category[i]
  matched <- crosswalk$donor_category == "" & grepl(pattern_i, crosswalk$donor_norm, perl = TRUE)
  crosswalk[matched, donor_category := category_i]
  crosswalk[matched, match_reason := pattern_i]
  crosswalk[matched, confidence := "high"]
  crosswalk[matched, review_flag := category_i == "generic_labor"]
}

receipts[, secondary_norm := normalize_text(paste(Occupation, Employer, Description))]
secondary_candidates <- unique(receipts[nzchar(secondary_norm), .(donor_norm, secondary_norm)])
for (i in seq_len(nrow(category_patterns))) {
  pattern_i <- category_patterns$pattern[i]
  category_i <- category_patterns$donor_category[i]
  matched_norms <- secondary_candidates[grepl(pattern_i, secondary_norm, perl = TRUE), unique(donor_norm)]
  matched <- crosswalk$donor_category == "" & crosswalk$donor_norm %in% matched_norms
  crosswalk[matched, donor_category := category_i]
  crosswalk[matched, match_reason := pattern_i]
  crosswalk[matched, confidence := "medium"]
  crosswalk[matched, review_flag := TRUE]
}

validate_unique(crosswalk, "donor_norm", "donor union crosswalk")
crosswalk[, auto_donor_category := donor_category]
crosswalk[, auto_match_reason := match_reason]
crosswalk[, auto_confidence := confidence]
crosswalk[, manual_review_required := donor_category != "" & (review_flag | total_amount_full_cycles >= high_dollar_donor_min)]
crosswalk[, manual_review_reason := trimws(paste(
  ifelse(review_flag, "classifier_review_flag", ""),
  ifelse(total_amount_full_cycles >= high_dollar_donor_min, "high_dollar_donor", "")
))]
data.table::fwrite(
  crosswalk[, .(
    donor_norm,
    donor_name,
    donor_category,
    match_reason,
    confidence,
    review_flag,
    manual_review_required,
    manual_review_reason,
    raw_name_count,
    receipt_count,
    cycle_count,
    total_amount_full_cycles
  )],
  "../output/donor_union_crosswalk_auto_cycles.csv"
)

manual_decisions <- data.table::fread("donor_manual_review_decisions_cycles.csv", colClasses = "character", na.strings = "")
manual_decisions <- ensure_columns(manual_decisions, c(
  "donor_norm",
  "manual_include_union",
  "manual_donor_category",
  "manual_decision_status",
  "manual_decision_reason",
  "chatgpt_include_union",
  "chatgpt_donor_category",
  "chatgpt_confidence",
  "chatgpt_needs_jacob_review",
  "chatgpt_reason",
  "chatgpt_review_date",
  "chatgpt_review_url"
))
manual_decisions[, donor_norm := trimws(donor_norm)]
manual_decisions[, manual_include_union := toupper(trimws(manual_include_union))]
manual_decisions[, manual_donor_category := trimws(manual_donor_category)]
manual_decisions[, manual_decision_status := trimws(manual_decision_status)]
manual_decisions[, manual_decision_reason := trimws(manual_decision_reason)]
manual_decisions[, chatgpt_include_union := toupper(trimws(chatgpt_include_union))]
manual_decisions[, chatgpt_donor_category := trimws(chatgpt_donor_category)]
manual_decisions[, chatgpt_confidence := trimws(chatgpt_confidence)]
manual_decisions[, chatgpt_needs_jacob_review := toupper(trimws(chatgpt_needs_jacob_review))]
manual_decisions[, chatgpt_reason := trimws(chatgpt_reason)]
manual_decisions[, chatgpt_review_date := trimws(chatgpt_review_date)]
manual_decisions[, chatgpt_review_url := trimws(chatgpt_review_url)]
manual_decisions <- manual_decisions[nzchar(donor_norm)]
validate_unique(manual_decisions, "donor_norm", "donor manual review decisions")

unknown_decisions <- manual_decisions[!(donor_norm %in% crosswalk$donor_norm)]
if (nrow(unknown_decisions) > 0) {
  stop(sprintf(
    "Manual donor decision file contains donors absent from current crosswalk: %s",
    paste(head(unknown_decisions$donor_norm, 20), collapse = "; ")
  ), call. = FALSE)
}

required_decisions <- crosswalk[manual_review_required == TRUE, donor_norm]
missing_decisions <- required_decisions[!(required_decisions %in% manual_decisions$donor_norm)]
if (length(missing_decisions) > 0) {
  stop(sprintf(
    "Missing manual donor decisions for %s review-required donors, including: %s",
    length(missing_decisions),
    paste(head(missing_decisions, 20), collapse = "; ")
  ), call. = FALSE)
}

bad_include <- manual_decisions[!(manual_include_union %in% c("TRUE", "FALSE"))]
if (nrow(bad_include) > 0) {
  stop("manual_include_union must be TRUE or FALSE for every manual donor decision.", call. = FALSE)
}
allowed_categories <- donor_categories
bad_categories <- manual_decisions[
  manual_include_union == "TRUE" & !(manual_donor_category %in% allowed_categories)
]
if (nrow(bad_categories) > 0) {
  stop(sprintf(
    "Manual included donors must use one of these categories: %s",
    paste(donor_categories, collapse = ", ")
  ), call. = FALSE)
}
bad_excluded_categories <- manual_decisions[manual_include_union == "FALSE" & nzchar(manual_donor_category)]
if (nrow(bad_excluded_categories) > 0) {
  stop("Manual excluded donors must have blank manual_donor_category.", call. = FALSE)
}
bad_chatgpt_include <- manual_decisions[nzchar(chatgpt_include_union) & !(chatgpt_include_union %in% c("TRUE", "FALSE", "UNSURE"))]
if (nrow(bad_chatgpt_include) > 0) {
  stop("chatgpt_include_union must be TRUE, FALSE, UNSURE, or blank.", call. = FALSE)
}
bad_chatgpt_categories <- manual_decisions[
  nzchar(chatgpt_donor_category) & !(chatgpt_donor_category %in% allowed_categories)
]
if (nrow(bad_chatgpt_categories) > 0) {
  stop("chatgpt_donor_category must be blank or a valid donor category.", call. = FALSE)
}
bad_chatgpt_review <- manual_decisions[nzchar(chatgpt_needs_jacob_review) & !(chatgpt_needs_jacob_review %in% c("TRUE", "FALSE"))]
if (nrow(bad_chatgpt_review) > 0) {
  stop("chatgpt_needs_jacob_review must be TRUE, FALSE, or blank.", call. = FALSE)
}
missing_status <- manual_decisions[!nzchar(manual_decision_status)]
if (nrow(missing_status) > 0) {
  stop("manual_decision_status is required for every manual donor decision.", call. = FALSE)
}

crosswalk <- merge(
  crosswalk,
  manual_decisions[, .(
    donor_norm,
    manual_include_union,
    manual_donor_category,
    manual_decision_status,
    manual_decision_reason,
    chatgpt_include_union,
    chatgpt_donor_category,
    chatgpt_confidence,
    chatgpt_needs_jacob_review,
    chatgpt_reason,
    chatgpt_review_date,
    chatgpt_review_url
  )],
  by = "donor_norm",
  all.x = TRUE,
  sort = FALSE
)
crosswalk[is.na(manual_include_union), manual_include_union := ""]
crosswalk[is.na(manual_donor_category), manual_donor_category := ""]
crosswalk[is.na(manual_decision_status), manual_decision_status := ""]
crosswalk[is.na(manual_decision_reason), manual_decision_reason := ""]
crosswalk[is.na(chatgpt_include_union), chatgpt_include_union := ""]
crosswalk[is.na(chatgpt_donor_category), chatgpt_donor_category := ""]
crosswalk[is.na(chatgpt_confidence), chatgpt_confidence := ""]
crosswalk[is.na(chatgpt_needs_jacob_review), chatgpt_needs_jacob_review := ""]
crosswalk[is.na(chatgpt_reason), chatgpt_reason := ""]
crosswalk[is.na(chatgpt_review_date), chatgpt_review_date := ""]
crosswalk[is.na(chatgpt_review_url), chatgpt_review_url := ""]
crosswalk[manual_review_required == TRUE & manual_include_union == "TRUE", donor_category := manual_donor_category]
crosswalk[manual_review_required == TRUE & manual_include_union == "FALSE", donor_category := ""]
crosswalk[manual_review_required == TRUE & manual_include_union == "TRUE", confidence := "manual"]
crosswalk[manual_review_required == TRUE & manual_include_union == "FALSE", confidence := "manual_excluded"]
crosswalk[manual_review_required == TRUE, match_reason := manual_decision_reason]
crosswalk[manual_review_required == FALSE, manual_include_union := ifelse(donor_category == "", "FALSE", "")]
crosswalk[manual_review_required == FALSE, manual_donor_category := ""]
crosswalk[manual_review_required == FALSE, manual_decision_status := ""]
crosswalk[manual_review_required == FALSE, manual_decision_reason := ""]

validate_unique(crosswalk, "donor_norm", "final donor union crosswalk")
data.table::fwrite(
  crosswalk[, .(
    donor_norm,
    donor_name,
    donor_category,
    match_reason,
    confidence,
    review_flag,
    manual_review_required,
    manual_review_reason,
    manual_include_union,
    manual_donor_category,
    manual_decision_status,
    manual_decision_reason,
    chatgpt_include_union,
    chatgpt_donor_category,
    chatgpt_confidence,
    chatgpt_needs_jacob_review,
    chatgpt_reason,
    chatgpt_review_date,
    chatgpt_review_url,
    auto_donor_category,
    auto_match_reason,
    auto_confidence,
    raw_name_count,
    receipt_count,
    cycle_count,
    total_amount_full_cycles
  )],
  "../output/donor_union_crosswalk_cycles.csv"
)
