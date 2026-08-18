# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/alderman_union_committee_crosswalk_cycles/code")

source("../../setup_environment/code/packages.R")

include_decisions <- c("include_main_candidate", "include_candidate_sensitivity_only", "include_named_recipient_committee")

parse_bool <- function(value) {
  tolower(trimws(as.character(value))) == "true"
}

validate_unique <- function(df, keys, label) {
  if (anyDuplicated(df, by = keys) > 0) {
    stop(sprintf("%s must be unique by %s.", label, paste(keys, collapse = ", ")), call. = FALSE)
  }
}

clean_character_columns <- function(df) {
  for (column in names(df)) {
    if (is.character(df[[column]])) {
      df[, (column) := iconv(get(column), from = "", to = "UTF-8", sub = " ")]
    }
  }
  df
}

activity_flags <- function(row, cycle_start_date, cycle_end_date) {
  creation_date <- as.Date(substr(row$committee_creation_date, 1, 10))
  status_date <- as.Date(substr(row$committee_status_date, 1, 10))
  status_upper <- toupper(trimws(row$committee_status))
  creation_after_cycle_end <- !is.na(creation_date) && creation_date > cycle_end_date
  status_before_cycle_start <- status_upper != "A" && !is.na(status_date) && status_date < cycle_start_date
  active_for_cycle_auto <- !creation_after_cycle_end && (
    status_upper == "A" || is.na(status_date) || status_date >= cycle_start_date
  )
  list(
    active_for_cycle_auto = active_for_cycle_auto,
    creation_after_cycle_end = creation_after_cycle_end,
    status_before_cycle_start = status_before_cycle_start,
    activity_window_issue = creation_after_cycle_end || status_before_cycle_start
  )
}

auto_crosswalk <- data.table::fread("../output/alderman_committee_crosswalk_auto_cycles.csv", colClasses = "character")
committee_columns <- names(data.table::fread(
  "../input/isbe_committees_raw.tsv",
  sep = "\t",
  quote = "",
  colClasses = "character",
  na.strings = "",
  encoding = "UTF-8",
  nrows = 0
))
committees <- data.table::fread(
  "../input/isbe_committees_raw.tsv",
  sep = "\t",
  quote = "",
  fill = Inf,
  colClasses = "character",
  na.strings = "",
  encoding = "UTF-8"
)
extra_columns <- setdiff(names(committees), committee_columns)
if (length(extra_columns) > 0) {
  committees[, (extra_columns) := NULL]
}
committees <- clean_character_columns(committees)
committees <- committees[nzchar(trimws(ID))]
targets <- data.table::fread("../input/alderman_cycle_targets.csv", colClasses = "character")
decisions <- data.table::fread("committee_manual_review_decisions_cycles.csv", colClasses = "character")

validate_unique(auto_crosswalk, c("cycle_year", "committee_id"), "automatic committee crosswalk")
validate_unique(decisions, c("cycle_year", "committee_id"), "manual committee decisions")
validate_unique(targets, c("cycle_year", "alderman_id"), "alderman cycle targets")

committee_details <- committees[, .(
  committee_id = trimws(ID),
  committee_type = trimws(TypeOfCommittee),
  committee_name_registry = trimws(Name),
  committee_purpose = trimws(Purpose),
  committee_status = trimws(Status),
  committee_creation_date = trimws(CreationDate),
  committee_status_date = trimws(StatusDate)
)]
validate_unique(committee_details, "committee_id", "ISBE committee registry")

auto_crosswalk[, decision_key := paste(cycle_year, committee_id, sep = "|")]
decisions[, decision_key := paste(cycle_year, committee_id, sep = "|")]
base <- auto_crosswalk[!(decision_key %in% decisions$decision_key)]
base[, decision_key := NULL]
base <- merge(base, committee_details, by = "committee_id", all.x = TRUE, sort = FALSE)

for (column in names(committee_details)) {
  data.table::set(base, which(is.na(base[[column]])), column, "")
}

base_flags <- lapply(seq_len(nrow(base)), function(i) {
  row <- as.list(base[i])
  flags <- activity_flags(row, as.Date(row$cycle_start_date), as.Date(row$cycle_end_date))
  data.table::as.data.table(flags)
})
base <- cbind(base, data.table::rbindlist(base_flags))
base[, manual_decision := ""]
base[, correct_owner_person := ""]
base[, correct_owner_type := ""]
base[, correct_owner_ward := ""]
base[, include_strict_candidate := committee_scope == "candidate" & !parse_bool(review_flag) & active_for_cycle_auto]
base[, include_main_named_recipient := committee_scope == "candidate" & !parse_bool(review_flag)]
base[, include_candidate_clean := include_main_named_recipient]
base[, include_candidate_all := committee_scope == "candidate"]
base[, include_expanded_validated := FALSE]
base[, manual_overrode_activity_window := FALSE]
base[, exclude_reason := ""]
base[, review_notes := ""]
base[, reviewer := ""]
base[, review_date := ""]

manual_rows <- list()
manual_index <- 0L
included_decisions <- decisions[decision %in% include_decisions & (parse_bool(include_candidate_clean) | parse_bool(include_candidate_all))]
for (i in seq_len(nrow(included_decisions))) {
  decision <- included_decisions[i]
  committee <- committee_details[committee_id == trimws(decision$committee_id)]
  if (nrow(committee) != 1L) {
    stop(sprintf("Manual decision committee_id not found or duplicated: %s", decision$committee_id), call. = FALSE)
  }
  target <- targets[cycle_year == decision$cycle_year & alderman_id == decision$matched_target_alderman_id]
  if (nrow(target) != 1L) {
    stop(sprintf("Manual decision target not found: %s %s", decision$cycle_year, decision$matched_target_alderman_id), call. = FALSE)
  }
  flags <- activity_flags(as.list(committee[1]), as.Date(target$cycle_start_date), as.Date(target$cycle_end_date))
  correct_owner_type <- trimws(decision$correct_owner_type)
  include_main_named_recipient <- parse_bool(decision$include_candidate_clean) || parse_bool(decision$include_candidate_all)
  include_strict_candidate <- decision$decision == "include_main_candidate" &&
    correct_owner_type == "candidate" &&
    flags$active_for_cycle_auto

  manual_index <- manual_index + 1L
  manual_rows[[manual_index]] <- data.table::data.table(
    cycle_year = target$cycle_year,
    cycle_start_date = target$cycle_start_date,
    cycle_end_date = target$cycle_end_date,
    alderman_id = target$alderman_id,
    full_name = target$full_name,
    ward_number = target$ward_number,
    committee_id = committee$committee_id,
    committee_name = committee$committee_name_registry,
    committee_scope = ifelse(nzchar(correct_owner_type), correct_owner_type, "manual_review"),
    committee_type = committee$committee_type,
    committee_name_registry = committee$committee_name_registry,
    committee_purpose = committee$committee_purpose,
    committee_status = committee$committee_status,
    committee_creation_date = committee$committee_creation_date,
    committee_status_date = committee$committee_status_date,
    match_rule = paste0("manual_review:", decision$decision),
    confidence = "manual",
    review_flag = !parse_bool(decision$include_candidate_clean),
    manual_decision = decision$decision,
    correct_owner_person = decision$correct_owner_person,
    correct_owner_type = correct_owner_type,
    correct_owner_ward = decision$correct_owner_ward,
    include_strict_candidate = include_strict_candidate,
    include_main_named_recipient = include_main_named_recipient,
    include_candidate_clean = include_main_named_recipient,
    include_candidate_all = parse_bool(decision$include_candidate_all),
    include_expanded_validated = parse_bool(decision$include_expanded_validated),
    active_for_cycle_auto = flags$active_for_cycle_auto,
    creation_after_cycle_end = flags$creation_after_cycle_end,
    status_before_cycle_start = flags$status_before_cycle_start,
    activity_window_issue = flags$activity_window_issue,
    manual_overrode_activity_window = include_main_named_recipient && flags$activity_window_issue,
    exclude_reason = decision$exclude_reason,
    review_notes = decision$review_notes,
    reviewer = decision$reviewer,
    review_date = decision$review_date
  )
}

final <- data.table::rbindlist(c(list(base), manual_rows), fill = TRUE)
final[, cycle_year := as.integer(cycle_year)]
final[, ward_number := as.integer(ward_number)]
validate_unique(final, c("cycle_year", "committee_id"), "final committee crosswalk")

data.table::setorder(final, cycle_year, ward_number, committee_scope, committee_name)
data.table::fwrite(final, "../output/alderman_committee_crosswalk_cycles.csv")
