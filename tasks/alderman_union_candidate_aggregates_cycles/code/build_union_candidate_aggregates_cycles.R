# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/alderman_union_candidate_aggregates_cycles/code")
# committee_definition <- "reviewed_named_recipient"
# aggregate_level <- "candidate_total"

source("../../setup_environment/code/packages.R")

union_categories <- c("construction_trades", "teacher_education", "public_sector_service", "generic_labor")
committee_definitions <- c("reviewed_named_recipient", "strict_candidate")
aggregate_levels <- c("cycle", "candidate_total")

numeric_column <- function(df, column) {
  if (!(column %in% names(df))) {
    return(rep(0, nrow(df)))
  }
  out <- suppressWarnings(as.numeric(df[[column]]))
  out[is.na(out)] <- 0
  out
}

validate_unique <- function(df, keys, label) {
  if (anyDuplicated(df, by = keys) > 0) {
    stop(sprintf("%s must be unique by %s.", label, paste(keys, collapse = ", ")), call. = FALSE)
  }
}

add_union_category_shares <- function(df) {
  df[, total_receipts_amount := numeric_column(df, "total_receipts_amount")]
  df[, union_total_amount := numeric_column(df, "union_total_amount")]
  df[, union_share_total_receipts := 0]
  df[total_receipts_amount > 0, union_share_total_receipts := union_total_amount / total_receipts_amount]

  for (category in union_categories) {
    amount_col <- paste0(category, "_amount")
    share_receipts_col <- paste0(category, "_share_total_receipts")
    share_union_col <- paste0(category, "_share_union_total")
    df[, (amount_col) := numeric_column(df, amount_col)]
    df[, (share_receipts_col) := 0]
    df[total_receipts_amount > 0, (share_receipts_col) := get(amount_col) / total_receipts_amount]
    df[, (share_union_col) := 0]
    df[union_total_amount > 0, (share_union_col) := get(amount_col) / union_total_amount]
  }
  df
}

cycle_aggregates <- function(panel) {
  panel[, cycle_year := as.integer(cycle_year)]
  panel[, ward_number := as.integer(ward_number)]
  validate_unique(panel, c("cycle_year", "alderman_id"), "alderman-cycle union panel")
  panel <- add_union_category_shares(panel)

  columns <- c(
    "cycle_year", "ward_number", "alderman_id", "full_name", "panel_committee_definition",
    "committee_count", "candidate_committee_count", "review_committee_count",
    "total_receipts_amount", "total_receipts_count", "total_receipts_donor_count",
    "union_total_amount", "union_total_count", "union_donor_count", "union_share_total_receipts"
  )
  for (category in union_categories) {
    columns <- c(
      columns,
      paste0(category, "_amount"),
      paste0(category, "_count"),
      paste0(category, "_donor_count"),
      paste0(category, "_share_total_receipts"),
      paste0(category, "_share_union_total")
    )
  }
  for (column in columns) {
    if (!(column %in% names(panel))) {
      panel[, (column) := ifelse(grepl("(_count|_amount)$", column), 0, "")]
    }
  }
  panel[order(cycle_year, ward_number), ..columns]
}

semicolon_values <- function(values) {
  paste(sort(unique(as.character(values[nzchar(as.character(values))]))), collapse = ";")
}

candidate_total_aggregates <- function(panel) {
  panel <- cycle_aggregates(panel)
  for (column in c(
    "committee_count", "candidate_committee_count", "review_committee_count",
    "total_receipts_amount", "total_receipts_count", "union_total_amount", "union_total_count"
  )) {
    panel[, (column) := numeric_column(panel, column)]
  }
  for (category in union_categories) {
    panel[, (paste0(category, "_amount")) := numeric_column(panel, paste0(category, "_amount"))]
    panel[, (paste0(category, "_count")) := numeric_column(panel, paste0(category, "_count"))]
  }

  grouped <- panel[, .(
    first_cycle_year = min(cycle_year),
    last_cycle_year = max(cycle_year),
    cycles_count = data.table::uniqueN(cycle_year),
    ward_numbers = semicolon_values(ward_number),
    committee_count_sum = sum(committee_count),
    candidate_committee_count_sum = sum(candidate_committee_count),
    review_committee_count_sum = sum(review_committee_count),
    total_receipts_amount = sum(total_receipts_amount),
    total_receipts_count = sum(total_receipts_count),
    union_total_amount = sum(union_total_amount),
    union_total_count = sum(union_total_count)
  ), by = .(alderman_id, full_name, panel_committee_definition)]

  validate_unique(grouped, c("alderman_id", "panel_committee_definition"), "candidate-total aggregates")
  for (category in union_categories) {
    category_totals <- panel[, .(
      category_amount = sum(get(paste0(category, "_amount"))),
      category_count = sum(get(paste0(category, "_count")))
    ), by = .(alderman_id, full_name, panel_committee_definition)]
    validate_unique(category_totals, c("alderman_id", "panel_committee_definition"), "candidate category totals")
    data.table::setnames(
      category_totals,
      c("category_amount", "category_count"),
      c(paste0(category, "_amount"), paste0(category, "_count"))
    )
    grouped <- merge(
      grouped,
      category_totals,
      by = c("alderman_id", "full_name", "panel_committee_definition"),
      all.x = TRUE,
      sort = FALSE
    )
    validate_unique(grouped, c("alderman_id", "panel_committee_definition"), "candidate-total aggregates")
  }

  grouped <- add_union_category_shares(grouped)
  columns <- c(
    "alderman_id", "full_name", "panel_committee_definition", "first_cycle_year",
    "last_cycle_year", "cycles_count", "ward_numbers", "committee_count_sum",
    "candidate_committee_count_sum", "review_committee_count_sum",
    "total_receipts_amount", "total_receipts_count", "union_total_amount",
    "union_total_count", "union_share_total_receipts"
  )
  for (category in union_categories) {
    columns <- c(
      columns,
      paste0(category, "_amount"),
      paste0(category, "_count"),
      paste0(category, "_share_total_receipts"),
      paste0(category, "_share_union_total")
    )
  }
  grouped[order(-union_total_amount, -total_receipts_amount, full_name), ..columns]
}

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(committee_definition, aggregate_level)
}
if (length(cli_args) != 2) {
  stop("Expected arguments: committee_definition aggregate_level.", call. = FALSE)
}
committee_definition <- cli_args[1]
aggregate_level <- cli_args[2]
if (!(committee_definition %in% committee_definitions)) {
  stop(sprintf("Unknown committee definition: %s.", committee_definition), call. = FALSE)
}
if (!(aggregate_level %in% aggregate_levels)) {
  stop(sprintf("Unknown aggregate level: %s.", aggregate_level), call. = FALSE)
}

panel <- data.table::fread(sprintf("../input/alderman_cycle_union_donations_%s.csv", committee_definition), colClasses = "character")
if (aggregate_level == "cycle") {
  output <- cycle_aggregates(panel)
  data.table::fwrite(output, sprintf("../output/alderman_cycle_union_category_aggregates_%s.csv", committee_definition))
} else {
  output <- candidate_total_aggregates(panel)
  data.table::fwrite(output, sprintf("../output/alderman_candidate_union_category_totals_%s.csv", committee_definition))
}
