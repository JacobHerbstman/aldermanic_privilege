# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/alderman_union_donations_cycles/code")
# high_dollar_donor_min <- 10000

source("../../../setup_environment/code/packages.R")

donor_categories <- c("construction_trades", "teacher_education", "public_sector_service", "generic_labor")
old_output_root <- "/Users/jacobherbstman/Desktop/alderman_data/tasks"

parse_bool <- function(value) {
  tolower(trimws(as.character(value))) == "true"
}

money_column <- function(value, label = "money") {
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

normalize_text <- function(value) {
  text <- ifelse(is.na(value), "", as.character(value))
  text <- iconv(text, from = "", to = "UTF-8", sub = " ")
  text <- tolower(gsub("&", " and ", text, fixed = TRUE))
  text <- gsub("[^a-z0-9 ]+", " ", text)
  trimws(gsub("\\s+", " ", text))
}

donor_display_name <- function(first_name, last_name) {
  first_name <- as.character(first_name)
  last_name <- as.character(last_name)
  first_name[is.na(first_name)] <- ""
  last_name[is.na(last_name)] <- ""
  first_name <- trimws(iconv(first_name, from = "", to = "UTF-8", sub = " "))
  last_name <- trimws(iconv(last_name, from = "", to = "UTF-8", sub = " "))
  trimws(ifelse(nzchar(first_name), paste(first_name, last_name), last_name))
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

top_recipients <- function(group) {
  if (nrow(group) == 0) {
    return("")
  }
  totals <- group[, .(amount = sum(amount, na.rm = TRUE)), by = .(ward_number, full_name)]
  data.table::setorder(totals, -amount, ward_number)
  totals <- totals[seq_len(min(5L, nrow(totals)))]
  paste(sprintf("Ward %s %s: %.2f", totals$ward_number, totals$full_name, totals$amount), collapse = "; ")
}

numeric_like <- function(values) {
  values <- as.character(values)
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    return(FALSE)
  }
  all(grepl("^-?[0-9]+(\\.[0-9]+)?$", values))
}

compare_csv_pair <- function(label, new_path, old_path) {
  if (!file.exists(new_path)) {
    stop(sprintf("Missing new parity file: %s", new_path), call. = FALSE)
  }
  if (!file.exists(old_path)) {
    stop(sprintf("Missing old parity file: %s", old_path), call. = FALSE)
  }

  new <- data.table::fread(new_path, colClasses = "character", na.strings = "")
  old <- data.table::fread(old_path, colClasses = "character", na.strings = "")
  same_nrow <- nrow(new) == nrow(old)
  same_ncol <- ncol(new) == ncol(old)
  same_columns <- identical(names(new), names(old))
  same_column_set <- setequal(names(new), names(old))
  unequal_cells <- NA_integer_
  max_numeric_abs_diff <- NA_real_

  if (same_nrow && same_columns) {
    unequal_cells <- 0L
    max_numeric_abs_diff <- 0
    for (column in names(new)) {
      new_values <- as.character(new[[column]])
      old_values <- as.character(old[[column]])
      new_values[is.na(new_values)] <- ""
      old_values[is.na(old_values)] <- ""
      if (numeric_like(c(new_values, old_values))) {
        diff_i <- max(abs(money_column(new_values) - money_column(old_values)), na.rm = TRUE)
        max_numeric_abs_diff <- max(max_numeric_abs_diff, diff_i)
        unequal_cells <- unequal_cells + sum(abs(money_column(new_values) - money_column(old_values)) > 1e-7)
      } else {
        unequal_cells <- unequal_cells + sum(new_values != old_values)
      }
    }
  }

  data.table::data.table(
    output_name = label,
    new_path = new_path,
    old_path = old_path,
    new_rows = nrow(new),
    old_rows = nrow(old),
    new_columns = ncol(new),
    old_columns = ncol(old),
    same_nrow = same_nrow,
    same_ncol = same_ncol,
    same_columns = same_columns,
    same_column_set = same_column_set,
    unequal_cells = unequal_cells,
    max_numeric_abs_diff = max_numeric_abs_diff,
    parity_ok = same_nrow && same_ncol && same_columns && !is.na(unequal_cells) && unequal_cells == 0L
  )
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
  colClasses = "character",
  na.strings = "",
  encoding = "UTF-8",
  showProgress = TRUE
)
crosswalk <- data.table::fread("../input/alderman_committee_crosswalk_cycles.csv", colClasses = "character")
donor_crosswalk <- data.table::fread("../input/donor_union_crosswalk_cycles.csv", colClasses = "character")
donor_crosswalk <- ensure_columns(donor_crosswalk, c(
  "manual_review_required",
  "manual_review_reason",
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
  "chatgpt_review_url",
  "auto_donor_category",
  "auto_match_reason",
  "auto_confidence"
))
targets <- data.table::fread("../input/alderman_cycle_targets.csv", colClasses = "character")
cycle_panel_main <- data.table::fread("../input/alderman_cycle_union_donations_reviewed_named_recipient.csv", colClasses = "character")
cycle_panel_strict <- data.table::fread("../input/alderman_cycle_union_donations_strict_candidate.csv", colClasses = "character")
candidate_total_main <- data.table::fread("../input/alderman_candidate_union_category_totals_reviewed_named_recipient.csv", colClasses = "character")
candidate_total_strict <- data.table::fread("../input/alderman_candidate_union_category_totals_strict_candidate.csv", colClasses = "character")

required_receipt_columns <- c("cycle_year", "CommitteeID", "D2Part", "Archived", "Amount", "FirstName", "LastOnlyName", "RcvDate", "FiledDocID")
if (!all(required_receipt_columns %in% names(receipts))) {
  stop("ISBE receipts file is missing required audit columns.", call. = FALSE)
}
validate_unique(crosswalk, c("cycle_year", "committee_id"), "committee crosswalk")
validate_unique(donor_crosswalk, "donor_norm", "donor crosswalk")
validate_unique(targets, c("cycle_year", "alderman_id"), "alderman cycle targets")
validate_unique(cycle_panel_main, c("cycle_year", "alderman_id"), "main cycle panel")
validate_unique(cycle_panel_strict, c("cycle_year", "alderman_id"), "strict cycle panel")
validate_unique(candidate_total_main, "alderman_id", "main candidate totals")
validate_unique(candidate_total_strict, "alderman_id", "strict candidate totals")

receipts[, cycle_year := as.integer(cycle_year)]
receipts[, committee_id := trimws(CommitteeID)]
receipts[, archived := parse_bool(Archived)]
receipts[, d2part_code := trimws(D2Part)]
receipts[, amount := money_column(Amount, "receipt Amount")]
receipts[, received_date := as.Date(substr(RcvDate, 1, 10))]
receipts[, donor_name := donor_display_name(FirstName, LastOnlyName)]
receipts[, donor_norm := normalize_text(donor_name)]
receipts[, filed_doc_id := trimws(FiledDocID)]

receipt_summary <- receipts[, .(
  receipt_rows = .N,
  total_amount = round(sum(amount, na.rm = TRUE), 2),
  negative_amount_rows = sum(amount < 0, na.rm = TRUE),
  zero_amount_rows = sum(amount == 0, na.rm = TRUE),
  missing_committee_id_rows = sum(!nzchar(committee_id)),
  missing_donor_name_rows = sum(!nzchar(donor_norm)),
  min_received_date = as.character(min(received_date, na.rm = TRUE)),
  max_received_date = as.character(max(received_date, na.rm = TRUE))
), by = .(cycle_year, Archived, D2Part, d2part_code)]
data.table::setorder(receipt_summary, cycle_year, Archived, D2Part)

crosswalk[, cycle_year := as.integer(cycle_year)]
crosswalk[, ward_number := as.integer(ward_number)]
included_main <- crosswalk[parse_bool(include_main_named_recipient), .(
  cycle_year,
  committee_id = trimws(committee_id),
  alderman_id,
  ward_number,
  full_name,
  committee_scope,
  review_flag
)]
included_strict <- crosswalk[parse_bool(include_strict_candidate), .(
  cycle_year,
  committee_id = trimws(committee_id),
  alderman_id
)]
validate_unique(included_main, c("cycle_year", "committee_id"), "main included committees")
validate_unique(included_strict, c("cycle_year", "committee_id"), "strict included committees")

main_receipts <- receipts[!archived & d2part_code %in% c("1A", "2A", "5A")]
main_receipts <- merge(
  main_receipts,
  included_main,
  by = c("cycle_year", "committee_id"),
  all = FALSE,
  sort = FALSE
)
main_receipts <- merge(
  main_receipts,
  donor_crosswalk[, .(donor_norm, donor_category, match_reason, confidence)],
  by = "donor_norm",
  all.x = TRUE,
  sort = FALSE
)
main_receipts[is.na(donor_category), donor_category := ""]

strict_receipts <- receipts[!archived & d2part_code %in% c("1A", "2A", "5A")]
strict_receipts <- merge(
  strict_receipts,
  included_strict,
  by = c("cycle_year", "committee_id"),
  all = FALSE,
  sort = FALSE
)

main_donor_totals <- main_receipts[, .(
  reviewed_named_recipient_amount = round(sum(amount, na.rm = TRUE), 2),
  reviewed_named_recipient_receipt_count = .N,
  alderman_count = data.table::uniqueN(alderman_id),
  committee_count = data.table::uniqueN(committee_id),
  top_recipients = top_recipients(.SD)
), by = donor_norm]
strict_donor_totals <- strict_receipts[, .(
  strict_candidate_amount = round(sum(amount, na.rm = TRUE), 2),
  strict_candidate_receipt_count = .N
), by = donor_norm]

donor_review <- donor_crosswalk[parse_bool(manual_review_required) | donor_category %in% donor_categories]
donor_review <- merge(donor_review, main_donor_totals, by = "donor_norm", all.x = TRUE, sort = FALSE)
donor_review <- merge(donor_review, strict_donor_totals, by = "donor_norm", all.x = TRUE, sort = FALSE)
for (column in c(
  "reviewed_named_recipient_amount", "reviewed_named_recipient_receipt_count",
  "strict_candidate_amount", "strict_candidate_receipt_count",
  "alderman_count", "committee_count"
)) {
  donor_review[, (column) := money_column(get(column))]
}
donor_review[is.na(top_recipients), top_recipients := ""]
donor_review[, high_dollar_review_flag := reviewed_named_recipient_amount >= high_dollar_donor_min]
donor_review[, audit_reason := trimws(paste(
  ifelse(parse_bool(review_flag), "classifier_review_flag", ""),
  ifelse(confidence != "high", "non_high_confidence", ""),
  ifelse(donor_category == "generic_labor", "generic_labor_category", ""),
  ifelse(high_dollar_review_flag, "high_dollar_donor", "")
))]
donor_review <- donor_review[nzchar(audit_reason)]
data.table::setorder(donor_review, -reviewed_named_recipient_amount, -strict_candidate_amount, donor_category, donor_name)

manual_decision_summary <- donor_crosswalk[, .(
  donor_rows = .N,
  union_donor_rows = sum(donor_category %in% donor_categories),
  manual_review_required_rows = sum(parse_bool(manual_review_required)),
  manual_seeded_accept_rows = sum(parse_bool(manual_review_required) & manual_decision_status == "seeded_accept_auto_needs_jacob_review"),
  manual_confirmed_rows = sum(parse_bool(manual_review_required) & manual_decision_status == "confirmed"),
  manual_excluded_rows = sum(parse_bool(manual_review_required) & manual_include_union == "FALSE"),
  manual_missing_decision_rows = sum(parse_bool(manual_review_required) & !nzchar(manual_decision_status)),
  chatgpt_pre_reviewed_rows = sum(parse_bool(manual_review_required) & nzchar(chatgpt_include_union)),
  chatgpt_clear_accept_rows = sum(parse_bool(manual_review_required) & chatgpt_include_union == "TRUE" & chatgpt_needs_jacob_review == "FALSE"),
  chatgpt_clear_exclude_rows = sum(parse_bool(manual_review_required) & chatgpt_include_union == "FALSE" & chatgpt_needs_jacob_review == "FALSE"),
  chatgpt_jacob_review_rows = sum(parse_bool(manual_review_required) & chatgpt_needs_jacob_review == "TRUE")
)]

d2_columns <- names(data.table::fread(
  "../output/isbe_d2totals_raw.tsv",
  sep = "\t",
  quote = "",
  colClasses = "character",
  na.strings = "",
  encoding = "UTF-8",
  nrows = 0
))
d2totals <- data.table::fread(
  "../output/isbe_d2totals_raw.tsv",
  sep = "\t",
  quote = "",
  fill = Inf,
  colClasses = "character",
  na.strings = "",
  encoding = "UTF-8"
)
d2_extra_columns <- setdiff(names(d2totals), d2_columns)
if (length(d2_extra_columns) > 0) {
  d2totals[, (d2_extra_columns) := NULL]
}
filed_doc_columns <- names(data.table::fread(
  "../output/isbe_filed_docs_raw.tsv",
  sep = "\t",
  quote = "",
  colClasses = "character",
  na.strings = "",
  encoding = "UTF-8",
  nrows = 0
))
filed_docs <- data.table::fread(
  "../output/isbe_filed_docs_raw.tsv",
  sep = "\t",
  quote = "",
  fill = Inf,
  colClasses = "character",
  na.strings = "",
  encoding = "UTF-8"
)
filed_doc_extra_columns <- setdiff(names(filed_docs), filed_doc_columns)
if (length(filed_doc_extra_columns) > 0) {
  filed_docs[, (filed_doc_extra_columns) := NULL]
}
required_d2_columns <- c("CommitteeID", "FiledDocID", "Archived", "IndivContribI", "IndivContribNI", "XferInI", "XferInNI", "LoanRcvI", "LoanRcvNI", "OtherRctI", "OtherRctNI", "InKindI", "InKindNI")
required_filed_doc_columns <- c("ID", "CommitteeID", "DocName", "RptPdBegDate", "RptPdEndDate")
if (!all(required_d2_columns %in% names(d2totals))) {
  stop("D2Totals file is missing required reconciliation columns.", call. = FALSE)
}
if (!all(required_filed_doc_columns %in% names(filed_docs))) {
  stop("FiledDocs file is missing required reconciliation columns.", call. = FALSE)
}

d2totals[, committee_id := trimws(CommitteeID)]
d2totals[, filed_doc_id := trimws(FiledDocID)]
d2totals <- d2totals[!parse_bool(Archived)]
for (column in required_d2_columns[4:length(required_d2_columns)]) {
  d2totals[, (column) := money_column(get(column))]
}
d2totals <- d2totals[, .(
  IndivContribI = sum(IndivContribI),
  IndivContribNI = sum(IndivContribNI),
  XferInI = sum(XferInI),
  XferInNI = sum(XferInNI),
  LoanRcvI = sum(LoanRcvI),
  LoanRcvNI = sum(LoanRcvNI),
  OtherRctI = sum(OtherRctI),
  OtherRctNI = sum(OtherRctNI),
  InKindI = sum(InKindI),
  InKindNI = sum(InKindNI)
), by = .(committee_id, filed_doc_id)]

filed_docs[, filed_doc_id := trimws(ID)]
filed_docs[, filed_committee_id := trimws(CommitteeID)]
filed_docs[, report_begin_date := as.Date(substr(RptPdBegDate, 1, 10))]
filed_docs[, report_end_date := as.Date(substr(RptPdEndDate, 1, 10))]
validate_unique(filed_docs, "filed_doc_id", "FiledDocs")
d2totals <- merge(
  d2totals,
  filed_docs[, .(filed_doc_id, filed_committee_id, DocName, report_begin_date, report_end_date)],
  by = "filed_doc_id",
  all.x = TRUE,
  sort = FALSE
)
d2totals[, committee_id_mismatch := nzchar(filed_committee_id) & filed_committee_id != committee_id]
d2totals[, d2_itemized_main_total := IndivContribI + XferInI + InKindI]
d2totals[, d2_nonitemized_main_total := IndivContribNI + XferInNI + InKindNI]
d2totals[, d2_main_total := d2_itemized_main_total + d2_nonitemized_main_total]
d2totals[, d2_excluded_loans_other_total := LoanRcvI + LoanRcvNI + OtherRctI + OtherRctNI]

receipt_by_doc <- main_receipts[, .(
  receipt_main_amount = sum(amount, na.rm = TRUE),
  receipt_main_count = .N
), by = .(cycle_year, committee_id, filed_doc_id)]

d2_cycle_rows <- list()
d2_cycle_index <- 0L
cycle_windows <- unique(targets[, .(
  cycle_year = as.integer(cycle_year),
  cycle_start_date = as.Date(cycle_start_date),
  cycle_end_date = as.Date(cycle_end_date)
)])
data.table::setorder(cycle_windows, cycle_year)
for (i in seq_len(nrow(cycle_windows))) {
  cycle <- cycle_windows[i]
  included_cycle <- included_main[cycle_year == cycle$cycle_year]
  d2_cycle <- d2totals[
    committee_id %in% included_cycle$committee_id &
      (is.na(report_begin_date) | report_begin_date <= cycle$cycle_end_date) &
      (is.na(report_end_date) | report_end_date >= cycle$cycle_start_date)
  ]
  if (nrow(d2_cycle) == 0L) {
    next
  }
  d2_cycle <- merge(
    d2_cycle,
    included_cycle[, .(committee_id, alderman_id, full_name, ward_number)],
    by = "committee_id",
    all.x = TRUE,
    sort = FALSE
  )
  d2_cycle[, cycle_year := cycle$cycle_year]
  d2_cycle[, period_inside_cycle := !is.na(report_begin_date) & !is.na(report_end_date) &
    report_begin_date >= cycle$cycle_start_date & report_end_date <= cycle$cycle_end_date]
  d2_cycle[, period_overlaps_cycle := is.na(report_begin_date) | is.na(report_end_date) |
    (report_begin_date <= cycle$cycle_end_date & report_end_date >= cycle$cycle_start_date)]
  d2_cycle_index <- d2_cycle_index + 1L
  d2_cycle_rows[[d2_cycle_index]] <- d2_cycle
}
d2_cycle <- data.table::rbindlist(d2_cycle_rows, fill = TRUE)
d2_cycle <- merge(
  d2_cycle,
  receipt_by_doc,
  by = c("cycle_year", "committee_id", "filed_doc_id"),
  all.x = TRUE,
  sort = FALSE
)
d2_cycle[is.na(receipt_main_amount), receipt_main_amount := 0]
d2_cycle[is.na(receipt_main_count), receipt_main_count := 0]

receipt_keys <- unique(receipt_by_doc[, .(cycle_year, committee_id, filed_doc_id)])
d2_keys <- unique(d2_cycle[, .(cycle_year, committee_id, filed_doc_id)])
receipt_missing_d2 <- merge(
  receipt_keys,
  d2_keys[, missing_d2_marker := FALSE],
  by = c("cycle_year", "committee_id", "filed_doc_id"),
  all.x = TRUE,
  sort = FALSE
)
receipt_missing_d2[is.na(missing_d2_marker), missing_d2_marker := TRUE]

d2_summary_rows <- list()
d2_summary_index <- 0L
for (cycle_year_i in sort(unique(cycle_windows$cycle_year))) {
  receipts_i <- main_receipts[cycle_year == cycle_year_i]
  d2_i <- d2_cycle[cycle_year == cycle_year_i]
  inside_i <- d2_i[period_inside_cycle == TRUE]
  missing_i <- receipt_missing_d2[cycle_year == cycle_year_i & missing_d2_marker == TRUE]
  metric_rows <- data.table::data.table(
    row_type = "summary",
    cycle_year = cycle_year_i,
    metric = c(
      "reviewed_named_recipient_receipt_main_amount_exact_window",
      "reviewed_named_recipient_receipt_main_rows_exact_window",
      "d2_overlap_filing_count",
      "d2_overlap_main_total",
      "d2_overlap_itemized_main_total",
      "d2_overlap_nonitemized_main_total",
      "d2_overlap_nonitemized_share",
      "d2_overlap_excluded_loans_other_total",
      "d2_inside_filing_count",
      "d2_inside_main_total",
      "d2_filed_docs_committee_mismatch_count",
      "receipt_filing_ids_missing_d2_count"
    ),
    value = c(
      sum(receipts_i$amount, na.rm = TRUE),
      nrow(receipts_i),
      nrow(d2_i),
      sum(d2_i$d2_main_total, na.rm = TRUE),
      sum(d2_i$d2_itemized_main_total, na.rm = TRUE),
      sum(d2_i$d2_nonitemized_main_total, na.rm = TRUE),
      ifelse(sum(d2_i$d2_main_total, na.rm = TRUE) == 0, 0, sum(d2_i$d2_nonitemized_main_total, na.rm = TRUE) / sum(d2_i$d2_main_total, na.rm = TRUE)),
      sum(d2_i$d2_excluded_loans_other_total, na.rm = TRUE),
      nrow(inside_i),
      sum(inside_i$d2_main_total, na.rm = TRUE),
      sum(d2_i$committee_id_mismatch, na.rm = TRUE),
      nrow(missing_i)
    ),
    committee_id = "",
    alderman_id = "",
    full_name = "",
    ward_number = "",
    filed_doc_id = "",
    doc_name = "",
    report_begin_date = "",
    report_end_date = "",
    receipt_main_amount = "",
    receipt_main_count = "",
    d2_itemized_main_total = "",
    d2_nonitemized_main_total = "",
    d2_main_total = "",
    d2_excluded_loans_other_total = "",
    note = ""
  )
  d2_summary_index <- d2_summary_index + 1L
  d2_summary_rows[[d2_summary_index]] <- metric_rows
}

top_nonitemized <- d2_cycle[order(-d2_nonitemized_main_total, -d2_main_total)][seq_len(min(100L, nrow(d2_cycle)))]
top_nonitemized_rows <- top_nonitemized[, .(
  row_type = "top_overlap_nonitemized_filing",
  cycle_year,
  metric = "",
  value = "",
  committee_id,
  alderman_id,
  full_name,
  ward_number,
  filed_doc_id,
  doc_name = DocName,
  report_begin_date = as.character(report_begin_date),
  report_end_date = as.character(report_end_date),
  receipt_main_amount = as.character(round(receipt_main_amount, 2)),
  receipt_main_count = as.character(receipt_main_count),
  d2_itemized_main_total = as.character(round(d2_itemized_main_total, 2)),
  d2_nonitemized_main_total = as.character(round(d2_nonitemized_main_total, 2)),
  d2_main_total = as.character(round(d2_main_total, 2)),
  d2_excluded_loans_other_total = as.character(round(d2_excluded_loans_other_total, 2)),
  note = "Large non-itemized D-2 total on a filing overlapping the cycle window."
)]

committee_d2_summary <- d2_cycle[, .(
  receipt_main_amount = sum(receipt_main_amount, na.rm = TRUE),
  receipt_main_count = sum(receipt_main_count, na.rm = TRUE),
  d2_itemized_main_total = sum(d2_itemized_main_total, na.rm = TRUE),
  d2_nonitemized_main_total = sum(d2_nonitemized_main_total, na.rm = TRUE),
  d2_main_total = sum(d2_main_total, na.rm = TRUE),
  d2_excluded_loans_other_total = sum(d2_excluded_loans_other_total, na.rm = TRUE)
), by = .(cycle_year, committee_id, alderman_id, full_name, ward_number)]
committee_d2_rows <- committee_d2_summary[, .(
  row_type = "committee_overlap_summary",
  cycle_year,
  metric = "",
  value = "",
  committee_id,
  alderman_id,
  full_name,
  ward_number,
  filed_doc_id = "",
  doc_name = "",
  report_begin_date = "",
  report_end_date = "",
  receipt_main_amount = as.character(round(receipt_main_amount, 2)),
  receipt_main_count = as.character(receipt_main_count),
  d2_itemized_main_total = as.character(round(d2_itemized_main_total, 2)),
  d2_nonitemized_main_total = as.character(round(d2_nonitemized_main_total, 2)),
  d2_main_total = as.character(round(d2_main_total, 2)),
  d2_excluded_loans_other_total = as.character(round(d2_excluded_loans_other_total, 2)),
  note = ""
)]
data.table::setorder(committee_d2_rows, cycle_year, -d2_nonitemized_main_total, -d2_main_total)
d2_reconciliation <- data.table::rbindlist(c(d2_summary_rows, list(top_nonitemized_rows, committee_d2_rows)), fill = TRUE)

parity_pairs <- data.table::data.table(
  output_name = c(
    "alderman_committee_crosswalk_cycles",
    "donor_union_crosswalk_cycles",
    "alderman_cycle_union_donations_reviewed_named_recipient",
    "alderman_cycle_union_donations_strict_candidate",
    "alderman_candidate_union_category_totals_reviewed_named_recipient",
    "alderman_candidate_union_category_totals_strict_candidate",
    "alderman_cycle_union_category_aggregates_reviewed_named_recipient",
    "alderman_cycle_union_category_aggregates_strict_candidate",
    "alderman_union_donations_main"
  ),
  new_path = c(
    "../../../alderman_union_committee_crosswalk_cycles/output/alderman_committee_crosswalk_cycles.csv",
    "../../../alderman_union_donor_classify_cycles/output/donor_union_crosswalk_cycles.csv",
    "../../../alderman_union_donations_cycles/output/alderman_cycle_union_donations_reviewed_named_recipient.csv",
    "../../../alderman_union_donations_cycles/output/alderman_cycle_union_donations_strict_candidate.csv",
    "../../../alderman_union_candidate_aggregates_cycles/output/alderman_candidate_union_category_totals_reviewed_named_recipient.csv",
    "../../../alderman_union_candidate_aggregates_cycles/output/alderman_candidate_union_category_totals_strict_candidate.csv",
    "../../../alderman_union_candidate_aggregates_cycles/output/alderman_cycle_union_category_aggregates_reviewed_named_recipient.csv",
    "../../../alderman_union_candidate_aggregates_cycles/output/alderman_cycle_union_category_aggregates_strict_candidate.csv",
    "../../../alderman_union_candidate_aggregates_cycles/output/alderman_union_donations_main.csv"
  ),
  old_path = c(
    file.path(old_output_root, "alderman_union_committee_crosswalk_cycles/output/alderman_committee_crosswalk_cycles.csv"),
    file.path(old_output_root, "alderman_union_donor_classify_cycles/output/donor_union_crosswalk_cycles.csv"),
    file.path(old_output_root, "alderman_union_donations_cycles/output/alderman_cycle_union_donations_reviewed_named_recipient.csv"),
    file.path(old_output_root, "alderman_union_donations_cycles/output/alderman_cycle_union_donations_strict_candidate.csv"),
    file.path(old_output_root, "alderman_union_candidate_aggregates_cycles/output/alderman_candidate_union_category_totals_reviewed_named_recipient.csv"),
    file.path(old_output_root, "alderman_union_candidate_aggregates_cycles/output/alderman_candidate_union_category_totals_strict_candidate.csv"),
    file.path(old_output_root, "alderman_union_candidate_aggregates_cycles/output/alderman_cycle_union_category_aggregates_reviewed_named_recipient.csv"),
    file.path(old_output_root, "alderman_union_candidate_aggregates_cycles/output/alderman_cycle_union_category_aggregates_strict_candidate.csv"),
    file.path(old_output_root, "alderman_union_candidate_aggregates_cycles/output/alderman_union_donations_main.csv")
  )
)
parity_report <- data.table::rbindlist(lapply(seq_len(nrow(parity_pairs)), function(i) {
  compare_csv_pair(parity_pairs$output_name[i], parity_pairs$new_path[i], parity_pairs$old_path[i])
}), fill = TRUE)
main_target <- Sys.readlink("../../../alderman_union_candidate_aggregates_cycles/output/alderman_union_donations_main.csv")
parity_report <- data.table::rbindlist(list(
  parity_report,
  data.table::data.table(
    output_name = "main_symlink_target",
    new_path = "../../../alderman_union_candidate_aggregates_cycles/output/alderman_union_donations_main.csv",
    old_path = "",
    new_rows = NA_integer_,
    old_rows = NA_integer_,
    new_columns = NA_integer_,
    old_columns = NA_integer_,
    same_nrow = NA,
    same_ncol = NA,
    same_columns = NA,
    same_column_set = NA,
    unequal_cells = NA_integer_,
    max_numeric_abs_diff = NA_real_,
    parity_ok = identical(main_target, "alderman_candidate_union_category_totals_reviewed_named_recipient.csv"),
    note = paste0("readlink target: ", main_target)
  )
), fill = TRUE)
parity_report <- ensure_columns(parity_report, "note")
parity_report[is.na(note), note := ""]
parity_report[!same_columns & same_column_set == TRUE, note := "Column set matches; order differs from prototype."]
parity_report[
  output_name == "donor_union_crosswalk_cycles" & parity_ok == FALSE,
  note := "Current R port adds a manual donor decision layer, treats missing first names as blank, and builds donor classifications on the final non-archived D2Part 1A/2A/5A analytic receipt universe."
]
parity_report[
  output_name %in% c(
    "alderman_cycle_union_donations_reviewed_named_recipient",
    "alderman_cycle_union_donations_strict_candidate",
    "alderman_candidate_union_category_totals_reviewed_named_recipient",
    "alderman_candidate_union_category_totals_strict_candidate",
    "alderman_cycle_union_category_aggregates_reviewed_named_recipient",
    "alderman_cycle_union_category_aggregates_strict_candidate",
    "alderman_union_donations_main"
  ) & parity_ok == FALSE,
  note := "Remaining data differences trace to documented changes: 66 2012 receipt rows omitted by the prototype extract, plus removal of one 300 dollar medium-confidence Jerry Morrison donor classification induced outside the final analytic receipt universe."
]

packet_lines <- c(
  "# Union Donations Second-Opinion Packet",
  "",
  "This packet summarizes the R port and the main subjective choices for outside review in Browser/ChatGPT.",
  "",
  "## Production Scripts",
  "",
  "- `tasks/isbe_campaign_disclosure_cycle_extract/code/extract_receipts_full_cycles.R`: keeps receipts dated 1999-06-01 through 2023-05-31 and assigns aldermanic election cycles.",
  "- `tasks/create_alderman_cycle_targets/code/build_alderman_cycle_targets.R`: uses the June 1 post-election panel month for 2003, 2007, 2011, 2015, 2019, and 2023.",
  "- `tasks/alderman_union_committee_crosswalk_cycles/code/build_committee_crosswalk_cycles.R`: automatically matches active committees by candidate name first, then ward name/purpose only when no candidate match is found.",
  "- `tasks/alderman_union_committee_crosswalk_cycles/code/apply_committee_manual_decisions_cycles.R`: applies the reviewed committee decisions and creates strict candidate and reviewed named-recipient inclusion flags.",
  "- `tasks/alderman_union_donor_classify_cycles/code/build_donor_union_crosswalk_cycles.R`: classifies donors into construction trades, teacher/education, public-sector/service, and generic labor by ordered text patterns, writes an auto crosswalk, and applies tracked manual donor decisions to produce the final donor crosswalk.",
  "- `tasks/alderman_union_donations_cycles/code/build_union_donation_panel_cycles.R`: counts non-archived D2Part 1A, 2A, and 5A receipt rows for included alderman committees.",
  "- `tasks/alderman_union_candidate_aggregates_cycles/code/build_union_candidate_aggregates_cycles.R`: creates cycle and candidate-total outputs; the main CSV is the reviewed named-recipient candidate-total output.",
  "",
  "## Subjective Choices To Review",
  "",
  "- Cycle windows are June 1 after one aldermanic election through May 31 before the next target term: 1999-06-01 to 2023-05-31 for 2003-2023 cycles.",
  "- Committee inclusion has two definitions: `strict_candidate` for high-confidence candidate committees and `reviewed_named_recipient` for the main measure after manual review.",
  "- The main receipt definition excludes archived rows and keeps exact D2Part codes 1A individual contributions, 2A transfers in, and 5A in-kind receipts.",
  "- Donor classification is text-based and requires tracked manual decisions for generic labor, secondary-text matches, and high-dollar union donors before those classifications appear in the final donor crosswalk.",
  "- D2Totals reconciliation is diagnostic only because D2 report periods can overlap cycle windows and include non-itemized totals that cannot be assigned to exact receipt dates.",
  "",
  "## Manual Donor Review Status",
  "",
  sprintf("- Donor crosswalk rows: %s", manual_decision_summary$donor_rows),
  sprintf("- Final union donor rows: %s", manual_decision_summary$union_donor_rows),
  sprintf("- Manual-review-required donor rows: %s", manual_decision_summary$manual_review_required_rows),
  sprintf("- Seeded accept-auto rows still needing Jacob confirmation: %s", manual_decision_summary$manual_seeded_accept_rows),
  sprintf("- Confirmed manual donor rows: %s", manual_decision_summary$manual_confirmed_rows),
  sprintf("- Manually excluded donor rows: %s", manual_decision_summary$manual_excluded_rows),
  sprintf("- ChatGPT pre-reviewed donor rows: %s", manual_decision_summary$chatgpt_pre_reviewed_rows),
  sprintf("- ChatGPT clear accept suggestions: %s", manual_decision_summary$chatgpt_clear_accept_rows),
  sprintf("- ChatGPT clear exclude suggestions: %s", manual_decision_summary$chatgpt_clear_exclude_rows),
  sprintf("- ChatGPT rows left for Jacob review: %s", manual_decision_summary$chatgpt_jacob_review_rows),
  "",
  "## Known Parity Exceptions",
  "",
  "- The current R extract includes 66 non-archived D2Part 1A, 2A, and 5A receipt rows from 2012 FiledDocIDs 479572, 479582, and 479607 that were absent from the prototype extract.",
  "- Those rows flow through the reviewed named-recipient and strict candidate panel outputs; they add 53,750 dollars of total receipts and 750 dollars of union receipts to the reviewed named-recipient main output.",
  "- The current donor classifier is restricted to the final analytic receipt universe; this removes one 300 dollar medium-confidence public-sector/service classification for Jerry Morrison to Ricardo Munoz that the prototype induced from a non-analytic receipt row.",
  "- The net reviewed named-recipient main union total differs from the prototype by +450 dollars.",
  "- These are treated as identified prototype issues and hardening changes rather than unresolved porting regressions.",
  "",
  "## Current Audit Counts",
  "",
  sprintf("- Receipt summary rows: %s", nrow(receipt_summary)),
  sprintf("- Donor review rows: %s", nrow(donor_review)),
  sprintf("- D2 reconciliation rows: %s", nrow(d2_reconciliation)),
  sprintf("- Parity rows passing: %s of %s", sum(parity_report$parity_ok, na.rm = TRUE), nrow(parity_report)),
  "",
  "## Browser Review Prompt",
  "",
  "Please review whether these choices are internally consistent and sufficiently conservative for reporting union contributions by Chicago aldermanic cycle. Pay special attention to committee inclusion definitions, exact D2Part filtering, treatment of archived/amended rows, and the text-pattern donor classification categories."
)

data.table::fwrite(receipt_summary, "../output/isbe_receipts_extract_summary_cycles.csv")
data.table::fwrite(donor_review, "../output/donor_union_review_cycles.csv")
data.table::fwrite(d2_reconciliation, "../output/isbe_d2totals_reconciliation_cycles.csv")
data.table::fwrite(parity_report, "../output/union_donation_parity_report.csv")
writeLines(packet_lines, "../output/second_opinion_packet.md")
