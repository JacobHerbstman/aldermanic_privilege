# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/alderman_union_donations_cycles/code")

source("../../../setup_environment/code/packages.R")

suffix_tokens <- c("jr", "sr", "ii", "iii", "iv", "v")
office_words <- c("alderman", "alderwoman", "ward", "candidate", "campaign", "elect", "election", "citizens", "friends")

normalize_text <- function(value) {
  text <- ifelse(is.na(value), "", as.character(value))
  text <- iconv(text, from = "", to = "UTF-8", sub = " ")
  text <- tolower(gsub("&", " and ", text, fixed = TRUE))
  text <- gsub("[^a-z0-9 ]+", " ", text)
  trimws(gsub("\\s+", " ", text))
}

name_tokens <- function(full_name) {
  tokens <- strsplit(normalize_text(full_name), " ", fixed = TRUE)[[1]]
  tokens[nzchar(tokens) & !(tokens %in% suffix_tokens)]
}

words_set <- function(text) {
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  words[nzchar(words)]
}

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

semicolon_values <- function(values) {
  values <- as.character(values)
  values <- values[nzchar(values)]
  paste(values, collapse = ";")
}

coverage_quality_tier <- function(strict_rows, reviewed_rows) {
  if (nrow(strict_rows) > 0) {
    return("strict_candidate")
  }
  if (nrow(reviewed_rows) == 0) {
    return("missing")
  }
  if (any(parse_bool(reviewed_rows$activity_window_issue))) {
    return("reviewed_activity_window_override")
  }
  if (any(reviewed_rows$committee_scope != "candidate")) {
    return("reviewed_non_candidate_scope")
  }
  "reviewed_manual_or_flagged_candidate"
}

search_rule_for_target <- function(target, committee) {
  tokens <- name_tokens(target$full_name)
  if (length(tokens) < 2) {
    return(list(search_rule = "", search_score = 0L))
  }

  first_name <- tokens[1]
  last_name <- tokens[length(tokens)]
  full_name <- paste(tokens, collapse = " ")
  name_norm <- committee$committee_name_norm
  purpose_norm <- committee$purpose_norm
  name_words <- words_set(name_norm)
  purpose_words <- words_set(purpose_norm)
  any_office_word <- any(unique(c(name_words, purpose_words)) %in% office_words)

  if (nzchar(full_name) && grepl(full_name, name_norm, fixed = TRUE)) {
    return(list(search_rule = "search_full_name_in_committee_name", search_score = 100L))
  }
  if (nzchar(full_name) && grepl(full_name, purpose_norm, fixed = TRUE)) {
    return(list(search_rule = "search_full_name_in_purpose", search_score = 95L))
  }
  if (first_name %in% name_words && last_name %in% name_words) {
    return(list(search_rule = "search_first_last_in_committee_name", search_score = 90L))
  }
  if (first_name %in% purpose_words && last_name %in% purpose_words) {
    return(list(search_rule = "search_first_last_in_purpose", search_score = 85L))
  }
  if (last_name %in% name_words && any_office_word) {
    return(list(search_rule = "search_last_name_committee_name_with_office_signal", search_score = 70L))
  }
  if (last_name %in% purpose_words && any_office_word) {
    return(list(search_rule = "search_last_name_purpose_with_office_signal", search_score = 65L))
  }
  list(search_rule = "", search_score = 0L)
}

committee_rows_for_cycle <- function(committees, cycle_start_date, cycle_end_date) {
  committees[
    (is.na(creation_date) | creation_date <= cycle_end_date) &
      (status_upper == "A" | is.na(status_date) | status_date >= cycle_start_date)
  ]
}

reason_not_strict <- function(row) {
  if (parse_bool(row$include_strict_candidate)) {
    return("")
  }
  if (!parse_bool(row$include_main_named_recipient)) {
    return("not_in_reviewed_named_recipient")
  }
  if (parse_bool(row$activity_window_issue)) {
    return("activity_window_issue")
  }
  if (trimws(row$committee_scope) != "candidate") {
    return(paste0("non_candidate_scope:", row$committee_scope))
  }
  if (parse_bool(row$review_flag)) {
    return("review_flagged_candidate_match")
  }
  "manual_or_legacy_not_strict"
}

crosswalk <- data.table::fread("../input/alderman_committee_crosswalk_cycles.csv", colClasses = "character")
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

validate_unique(crosswalk, c("cycle_year", "committee_id"), "committee crosswalk")
validate_unique(targets, c("cycle_year", "alderman_id"), "alderman cycle targets")

for (column in c(
  "include_strict_candidate", "include_main_named_recipient", "include_candidate_clean",
  "manual_decision", "correct_owner_type", "committee_type", "committee_status",
  "committee_creation_date", "committee_status_date", "active_for_cycle_auto",
  "creation_after_cycle_end", "status_before_cycle_start", "activity_window_issue",
  "manual_overrode_activity_window", "review_notes"
)) {
  if (!(column %in% names(crosswalk))) {
    crosswalk[, (column) := ""]
  }
}

targets[, cycle_year := as.integer(cycle_year)]
targets[, ward_number := as.integer(ward_number)]
crosswalk[, cycle_year := as.integer(cycle_year)]
crosswalk[, ward_number := as.integer(ward_number)]
committees[, committee_id := trimws(ID)]
committees[, committee_name_norm := normalize_text(Name)]
committees[, purpose_norm := normalize_text(Purpose)]
committees[, creation_date := as.Date(substr(CreationDate, 1, 10))]
committees[, status_date := as.Date(substr(StatusDate, 1, 10))]
committees[, status_upper := toupper(trimws(Status))]

coverage_rows <- list()
coverage_index <- 0L
for (cycle_year_i in sort(unique(targets$cycle_year))) {
  cycle_targets <- targets[cycle_year == cycle_year_i][order(ward_number)]
  cycle_crosswalk <- crosswalk[cycle_year == cycle_year_i]
  strict_candidate <- cycle_crosswalk[parse_bool(include_strict_candidate)]
  reviewed_named <- cycle_crosswalk[parse_bool(include_main_named_recipient)]
  review_candidate <- cycle_crosswalk[committee_scope == "candidate" & parse_bool(review_flag)]
  candidate_all <- cycle_crosswalk[committee_scope == "candidate"]

  for (i in seq_len(nrow(cycle_targets))) {
    target <- cycle_targets[i]
    strict_rows <- strict_candidate[alderman_id == target$alderman_id]
    reviewed_rows <- reviewed_named[alderman_id == target$alderman_id]
    review_rows <- review_candidate[alderman_id == target$alderman_id]
    candidate_rows <- candidate_all[alderman_id == target$alderman_id]
    non_candidate_rows <- reviewed_rows[committee_scope != "candidate"]
    ward_org_rows <- reviewed_rows[committee_scope %in% c("ward", "ward_org", "committeeman")]
    other_office_rows <- reviewed_rows[committee_scope == "other_office"]
    activity_issue_rows <- reviewed_rows[parse_bool(activity_window_issue)]
    manual_override_rows <- reviewed_rows[parse_bool(manual_overrode_activity_window)]

    coverage_index <- coverage_index + 1L
    coverage_rows[[coverage_index]] <- data.table::data.table(
      cycle_year = cycle_year_i,
      ward_number = target$ward_number,
      alderman_id = target$alderman_id,
      full_name = target$full_name,
      strict_candidate_committee_count = data.table::uniqueN(strict_rows$committee_id),
      strict_candidate_committee_ids = semicolon_values(strict_rows$committee_id),
      reviewed_named_recipient_committee_count = data.table::uniqueN(reviewed_rows$committee_id),
      reviewed_named_recipient_committee_ids = semicolon_values(reviewed_rows$committee_id),
      reviewed_non_candidate_committee_count = data.table::uniqueN(non_candidate_rows$committee_id),
      reviewed_non_candidate_committee_ids = semicolon_values(non_candidate_rows$committee_id),
      reviewed_ward_org_committee_count = data.table::uniqueN(ward_org_rows$committee_id),
      reviewed_other_office_committee_count = data.table::uniqueN(other_office_rows$committee_id),
      activity_window_issue_committee_count = data.table::uniqueN(activity_issue_rows$committee_id),
      activity_window_issue_committee_ids = semicolon_values(activity_issue_rows$committee_id),
      manual_activity_override_committee_count = data.table::uniqueN(manual_override_rows$committee_id),
      manual_activity_override_committee_ids = semicolon_values(manual_override_rows$committee_id),
      review_candidate_committee_count = data.table::uniqueN(review_rows$committee_id),
      review_candidate_committee_ids = semicolon_values(review_rows$committee_id),
      candidate_all_committee_count = data.table::uniqueN(candidate_rows$committee_id),
      missing_strict_candidate = nrow(strict_rows) == 0,
      missing_reviewed_named_recipient = nrow(reviewed_rows) == 0,
      coverage_quality_tier = coverage_quality_tier(strict_rows, reviewed_rows)
    )
  }
}
coverage <- data.table::rbindlist(coverage_rows)

review_flagged <- crosswalk[committee_scope == "candidate" & parse_bool(review_flag)]
review_flagged[, review_reason := "review_flagged_candidate_match"]
review_flagged[, search_rule := match_rule]
review_flagged[, search_score := 80L]
review_flagged[, suggested_decision := "review_for_candidate_clean"]

queue_rows <- list()
queue_index <- 0L
existing_keys <- paste(crosswalk$cycle_year, crosswalk$committee_id, sep = "|")
missing <- coverage[missing_strict_candidate == TRUE]
for (i in seq_len(nrow(missing))) {
  target <- missing[i]
  cycle_start_i <- as.Date(targets[cycle_year == target$cycle_year, cycle_start_date][1])
  cycle_end_i <- as.Date(targets[cycle_year == target$cycle_year, cycle_end_date][1])
  active <- committee_rows_for_cycle(committees, cycle_start_i, cycle_end_i)
  tokens <- name_tokens(target$full_name)
  last_name <- if (length(tokens) > 0) tokens[length(tokens)] else ""
  active <- active[grepl(last_name, committee_name_norm, fixed = TRUE) | grepl(last_name, purpose_norm, fixed = TRUE)]
  matches <- list()
  match_index <- 0L

  for (j in seq_len(nrow(active))) {
    committee <- active[j]
    if (paste(target$cycle_year, committee$committee_id, sep = "|") %in% existing_keys) {
      next
    }
    search <- search_rule_for_target(as.list(target), as.list(committee))
    if (!nzchar(search$search_rule)) {
      next
    }
    match_index <- match_index + 1L
    matches[[match_index]] <- data.table::data.table(
      cycle_year = target$cycle_year,
      ward_number = target$ward_number,
      alderman_id = target$alderman_id,
      full_name = target$full_name,
      review_reason = "missing_strict_candidate_name_search",
      committee_id = committee$committee_id,
      committee_name = committee$Name,
      committee_type = committee$TypeOfCommittee,
      committee_purpose = committee$Purpose,
      committee_status = committee$Status,
      committee_creation_date = committee$CreationDate,
      committee_status_date = committee$StatusDate,
      committee_scope = "",
      match_rule = "",
      confidence = "",
      review_flag = "",
      search_rule = search$search_rule,
      search_score = search$search_score,
      suggested_decision = "review_for_candidate_clean"
    )
  }

  if (length(matches) == 0L) {
    queue_index <- queue_index + 1L
    queue_rows[[queue_index]] <- data.table::data.table(
      cycle_year = target$cycle_year,
      ward_number = target$ward_number,
      alderman_id = target$alderman_id,
      full_name = target$full_name,
      review_reason = "missing_strict_candidate_no_name_match",
      committee_id = "",
      committee_name = "",
      committee_type = "",
      committee_purpose = "",
      committee_status = "",
      committee_creation_date = "",
      committee_status_date = "",
      committee_scope = "",
      match_rule = "",
      confidence = "",
      review_flag = "",
      search_rule = "",
      search_score = 0L,
      suggested_decision = "manual_research_needed"
    )
  } else {
    matches <- data.table::rbindlist(matches)
    data.table::setorder(matches, -search_score, committee_name)
    matches <- matches[seq_len(min(15L, nrow(matches)))]
    for (j in seq_len(nrow(matches))) {
      queue_index <- queue_index + 1L
      queue_rows[[queue_index]] <- matches[j]
    }
  }
}

review_columns <- c(
  "cycle_year", "ward_number", "alderman_id", "full_name", "review_reason",
  "committee_id", "committee_name", "committee_type", "committee_purpose",
  "committee_status", "committee_creation_date", "committee_status_date",
  "committee_scope", "match_rule", "confidence", "review_flag", "search_rule",
  "search_score", "suggested_decision"
)
flagged_queue <- review_flagged[, ..review_columns]
missing_queue <- if (length(queue_rows) > 0L) data.table::rbindlist(queue_rows, fill = TRUE) else flagged_queue[0]
queue <- data.table::rbindlist(list(flagged_queue, missing_queue), fill = TRUE)
for (column in c(
  "decision", "correct_owner_person", "correct_owner_type", "correct_owner_ward",
  "matched_target_alderman_id", "include_strict_candidate",
  "include_main_named_recipient", "include_candidate_clean", "include_candidate_all",
  "include_expanded_validated", "exclude_reason", "review_notes", "reviewer", "review_date"
)) {
  queue[, (column) := ""]
}
queue[, search_score := as.integer(search_score)]
data.table::setorder(queue, cycle_year, ward_number, review_reason, -search_score)

bridge <- data.table::copy(crosswalk)
bridge[, include_strict_candidate := parse_bool(include_strict_candidate)]
bridge[, include_main_named_recipient := parse_bool(include_main_named_recipient)]
bridge[, reason_not_strict := vapply(seq_len(.N), function(i) reason_not_strict(as.list(bridge[i])), character(1))]
bridge <- bridge[, .(
  cycle_year,
  ward_number,
  alderman_id,
  full_name,
  committee_id,
  committee_name,
  committee_scope,
  committee_type,
  committee_status,
  committee_creation_date,
  committee_status_date,
  include_strict_candidate,
  include_main_named_recipient,
  reason_not_strict,
  manual_decision,
  correct_owner_type,
  active_for_cycle_auto,
  creation_after_cycle_end,
  status_before_cycle_start,
  activity_window_issue,
  manual_overrode_activity_window,
  review_flag,
  match_rule,
  review_notes
)]
data.table::setorder(bridge, cycle_year, ward_number, include_strict_candidate, committee_name)

packet_lines <- c(
  "# Alderman Committee Coverage Audit",
  "",
  "This packet audits committee-definition coverage before union donation totals are treated as usable.",
  "The main panel uses reviewed named-recipient committees; the strict candidate definition is a sensitivity check.",
  "It uses the cycle target file, ISBE committee registry, and the reviewed committee crosswalk.",
  "",
  "## Cycle Coverage",
  "",
  "| cycle | main reviewed aldermen | strict sensitivity aldermen | strict sensitivity gaps | main-only aldermen |",
  "|---:|---:|---:|---:|---:|"
)
for (cycle_year_i in sort(unique(coverage$cycle_year))) {
  group <- coverage[cycle_year == cycle_year_i]
  strict_count <- sum(!group$missing_strict_candidate)
  reviewed_count <- sum(!group$missing_reviewed_named_recipient)
  missing_strict_count <- sum(group$missing_strict_candidate)
  broad_only_count <- sum(group$missing_strict_candidate) - sum(group$missing_reviewed_named_recipient)
  packet_lines <- c(packet_lines, sprintf("| %s | %s | %s | %s | %s |", cycle_year_i, reviewed_count, strict_count, missing_strict_count, broad_only_count))
}

packet_lines <- c(packet_lines, "", "## Strict Sensitivity Gaps", "")
if (nrow(missing) == 0) {
  packet_lines <- c(packet_lines, "No strict sensitivity gaps.")
} else {
  for (cycle_year_i in sort(unique(missing$cycle_year))) {
    packet_lines <- c(packet_lines, sprintf("### %s", cycle_year_i))
    for (i in seq_len(nrow(missing[cycle_year == cycle_year_i]))) {
      row <- missing[cycle_year == cycle_year_i][i]
      ids <- ifelse(nzchar(row$reviewed_named_recipient_committee_ids), row$reviewed_named_recipient_committee_ids, "none")
      packet_lines <- c(
        packet_lines,
        sprintf(
          "- Ward %s: %s (`%s`), main reviewed IDs: %s; tier `%s`",
          row$ward_number,
          row$full_name,
          row$alderman_id,
          ids,
          row$coverage_quality_tier
        )
      )
    }
    packet_lines <- c(packet_lines, "")
  }
}

packet_lines <- c(
  packet_lines,
  "## Review Queue Notes",
  "",
  "Use `committee_manual_review_queue_cycles.csv` to resolve candidates into `committee_manual_review_decisions_cycles.csv`.",
  "The main reviewed definition allows named-recipient committees when manual review supports that donations are plausibly to the alderman or aldermanic political operation.",
  "The strict sensitivity definition only keeps committees whose registry name or purpose supports the alderman's own candidate committee, not a ward, party, committeeman, slate, other-office, or independent support committee.",
  "Use `committee_definition_bridge_cycles.csv` to inspect every committee counted in the main reviewed definition but not in the strict sensitivity definition.",
  "",
  "Top review candidates by cycle are:"
)
for (cycle_year_i in sort(unique(queue$cycle_year))) {
  packet_lines <- c(packet_lines, sprintf("### %s", cycle_year_i))
  group <- queue[cycle_year == cycle_year_i]
  group <- group[seq_len(min(20L, nrow(group)))]
  for (i in seq_len(nrow(group))) {
    row <- group[i]
    committee_label <- ifelse(nzchar(row$committee_name), row$committee_name, "no candidate found")
    packet_lines <- c(
      packet_lines,
      sprintf(
        "- Ward %s: %s; %s; `%s`; `%s`; score %s",
        row$ward_number,
        row$full_name,
        committee_label,
        row$review_reason,
        row$search_rule,
        row$search_score
      )
    )
  }
  packet_lines <- c(packet_lines, "")
}

data.table::fwrite(coverage, "../output/committee_coverage_status_cycles.csv")
data.table::fwrite(queue, "../output/committee_manual_review_queue_cycles.csv")
data.table::fwrite(bridge, "../output/committee_definition_bridge_cycles.csv")
writeLines(packet_lines, "../output/committee_resolution_packet_cycles.md")
