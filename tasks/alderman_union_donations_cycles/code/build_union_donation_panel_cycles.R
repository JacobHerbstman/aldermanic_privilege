# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/alderman_union_donations_cycles/code")
# committee_definition <- "reviewed_named_recipient"

source("../../setup_environment/code/packages.R")

donor_categories <- c("construction_trades", "teacher_education", "public_sector_service", "generic_labor")
committee_definitions <- c("strict_candidate", "reviewed_named_recipient")

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

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(committee_definition)
}
if (length(cli_args) != 1) {
  stop("Expected argument: committee_definition.", call. = FALSE)
}
committee_definition <- cli_args[1]
if (!(committee_definition %in% committee_definitions)) {
  stop(sprintf("Unknown committee definition: %s.", committee_definition), call. = FALSE)
}

receipts <- data.table::fread(
  "../input/isbe_receipts_full_cycles_raw.tsv",
  sep = "\t",
  select = c("cycle_year", "CommitteeID", "D2Part", "Archived", "Amount", "FirstName", "LastOnlyName"),
  colClasses = "character",
  na.strings = "",
  encoding = "UTF-8",
  showProgress = TRUE
)
crosswalk <- data.table::fread("../input/alderman_committee_crosswalk_cycles.csv", colClasses = "character")
donor_crosswalk <- data.table::fread("../input/donor_union_crosswalk_cycles.csv", colClasses = "character")
targets <- data.table::fread("../input/alderman_cycle_targets.csv", colClasses = "character")

validate_unique(crosswalk, c("cycle_year", "committee_id"), "committee crosswalk")
validate_unique(donor_crosswalk, "donor_norm", "donor crosswalk")
validate_unique(targets, c("cycle_year", "alderman_id"), "alderman cycle targets")

if (committee_definition == "strict_candidate") {
  included <- crosswalk[parse_bool(include_strict_candidate)]
} else {
  included <- crosswalk[parse_bool(include_main_named_recipient)]
}
included <- included[, .(
  cycle_year = as.integer(cycle_year),
  committee_id = trimws(committee_id),
  alderman_id,
  committee_scope,
  review_flag
)]
validate_unique(included, c("cycle_year", "committee_id"), "included committee crosswalk")

receipts[, cycle_year := as.integer(cycle_year)]
receipts[, committee_id := trimws(CommitteeID)]
receipts[, archived := parse_bool(Archived)]
receipts[, d2part_code := trimws(D2Part)]
receipts <- receipts[!archived & d2part_code %in% c("1A", "2A", "5A")]
receipts[, donor_name := donor_display_name(FirstName, LastOnlyName)]
receipts[, donor_norm := normalize_text(donor_name)]
receipts[, amount := money_column(Amount, "receipt Amount")]

mapped <- merge(
  receipts,
  included,
  by = c("cycle_year", "committee_id"),
  all = FALSE,
  sort = FALSE
)
mapped <- merge(
  mapped,
  donor_crosswalk[, .(donor_norm, donor_category)],
  by = "donor_norm",
  all.x = TRUE,
  sort = FALSE
)
mapped[is.na(donor_category), donor_category := ""]

panel <- targets[, .(
  cycle_year = as.integer(cycle_year),
  ward_number = as.integer(ward_number),
  alderman_id,
  full_name
)]
panel[, panel_committee_definition := committee_definition]

committee_counts <- included[, .(
  committee_count = data.table::uniqueN(committee_id),
  candidate_committee_count = sum(committee_scope == "candidate"),
  review_committee_count = sum(parse_bool(review_flag))
), by = .(cycle_year, alderman_id)]
panel <- merge(panel, committee_counts, by = c("cycle_year", "alderman_id"), all.x = TRUE, sort = FALSE)

total_receipts <- mapped[, .(
  total_receipts_amount = sum(amount, na.rm = TRUE),
  total_receipts_count = .N,
  total_receipts_donor_count = data.table::uniqueN(donor_norm)
), by = .(cycle_year, alderman_id)]
panel <- merge(panel, total_receipts, by = c("cycle_year", "alderman_id"), all.x = TRUE, sort = FALSE)

union_receipts <- mapped[donor_category %in% donor_categories]
union_totals <- union_receipts[, .(
  union_total_amount = sum(amount, na.rm = TRUE),
  union_total_count = .N,
  union_donor_count = data.table::uniqueN(donor_norm)
), by = .(cycle_year, alderman_id)]
panel <- merge(panel, union_totals, by = c("cycle_year", "alderman_id"), all.x = TRUE, sort = FALSE)

for (category in donor_categories) {
  category_totals <- union_receipts[donor_category == category, .(
    amount = sum(amount, na.rm = TRUE),
    count = .N,
    donor_count = data.table::uniqueN(donor_norm)
  ), by = .(cycle_year, alderman_id)]
  data.table::setnames(
    category_totals,
    c("amount", "count", "donor_count"),
    c(paste0(category, "_amount"), paste0(category, "_count"), paste0(category, "_donor_count"))
  )
  panel <- merge(panel, category_totals, by = c("cycle_year", "alderman_id"), all.x = TRUE, sort = FALSE)
}

numeric_columns <- names(panel)[grepl("(_count|_amount)$", names(panel))]
for (column in numeric_columns) {
  data.table::set(panel, which(is.na(panel[[column]])), column, 0)
}

panel[, union_share_total_receipts := 0]
panel[total_receipts_amount > 0, union_share_total_receipts := union_total_amount / total_receipts_amount]
for (category in donor_categories) {
  amount_col <- paste0(category, "_amount")
  share_col <- paste0(category, "_share_total_receipts")
  panel[, (share_col) := 0]
  panel[total_receipts_amount > 0, (share_col) := get(amount_col) / total_receipts_amount]
}

data.table::setorder(panel, cycle_year, ward_number)
columns <- c(
  "alderman_id", "full_name", "ward_number", "cycle_year", "panel_committee_definition",
  "committee_count", "candidate_committee_count", "review_committee_count",
  "total_receipts_amount", "total_receipts_count", "total_receipts_donor_count",
  "union_total_amount", "union_total_count", "union_donor_count"
)
for (category in donor_categories) {
  columns <- c(columns, paste0(category, "_amount"), paste0(category, "_count"), paste0(category, "_donor_count"))
}
columns <- c(
  columns,
  "construction_trades_share_total_receipts",
  "teacher_education_share_total_receipts",
  "public_sector_service_share_total_receipts",
  "generic_labor_share_total_receipts",
  "union_share_total_receipts"
)
data.table::fwrite(panel[, ..columns], sprintf("../output/alderman_cycle_union_donations_%s.csv", committee_definition))
