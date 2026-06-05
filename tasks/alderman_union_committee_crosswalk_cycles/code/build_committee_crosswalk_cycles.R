# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/alderman_union_committee_crosswalk_cycles/code")

source("../../setup_environment/code/packages.R")

suffix_tokens <- c("jr", "sr", "ii", "iii", "iv", "v")
ward_words <- c(
  "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth",
  "ninth", "tenth", "eleventh", "twelfth", "thirteenth", "fourteenth",
  "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth",
  "twentieth", "twenty first", "twenty second", "twenty third", "twenty fourth",
  "twenty fifth", "twenty sixth", "twenty seventh", "twenty eighth",
  "twenty ninth", "thirtieth", "thirty first", "thirty second",
  "thirty third", "thirty fourth", "thirty fifth", "thirty sixth",
  "thirty seventh", "thirty eighth", "thirty ninth", "fortieth",
  "forty first", "forty second", "forty third", "forty fourth",
  "forty fifth", "forty sixth", "forty seventh", "forty eighth",
  "forty ninth", "fiftieth"
)

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

clean_character_columns <- function(df) {
  for (column in names(df)) {
    if (is.character(df[[column]])) {
      df[, (column) := iconv(get(column), from = "", to = "UTF-8", sub = " ")]
    }
  }
  df
}

target_records_for_cycle <- function(targets) {
  lapply(seq_len(nrow(targets)), function(i) {
    tokens <- name_tokens(targets$full_name[i])
    ward_number <- as.integer(targets$ward_number[i])
    list(
      cycle_year = as.integer(targets$cycle_year[i]),
      cycle_start_date = as.character(targets$cycle_start_date[i]),
      cycle_end_date = as.character(targets$cycle_end_date[i]),
      alderman_id = targets$alderman_id[i],
      full_name = targets$full_name[i],
      ward_number = ward_number,
      tokens = tokens,
      first_token = if (length(tokens) > 0) tokens[1] else "",
      last_token = if (length(tokens) > 0) tokens[length(tokens)] else "",
      full_name_norm = paste(tokens, collapse = " "),
      ward_numeric_pattern = sprintf("\\b0*%s(st|nd|rd|th)? ward\\b", ward_number),
      ward_word_pattern = sprintf("\\b%s ward\\b", ward_words[ward_number])
    )
  })
}

candidate_match_record <- function(target, committee_name_norm, committee_words, purpose_words) {
  if (length(target$tokens) < 2) {
    return(list(matched = FALSE, match_rule = "", confidence = "", review_flag = TRUE))
  }
  if (nzchar(target$full_name_norm) && grepl(target$full_name_norm, committee_name_norm, fixed = TRUE)) {
    return(list(matched = TRUE, match_rule = "candidate_full_name", confidence = "high", review_flag = FALSE))
  }
  if (target$first_token %in% committee_words && target$last_token %in% committee_words) {
    return(list(matched = TRUE, match_rule = "candidate_first_last", confidence = "high", review_flag = FALSE))
  }
  if (target$first_token %in% purpose_words && target$last_token %in% purpose_words) {
    return(list(matched = TRUE, match_rule = "candidate_purpose_first_last", confidence = "medium", review_flag = TRUE))
  }
  list(matched = FALSE, match_rule = "", confidence = "", review_flag = TRUE)
}

ward_match_record <- function(target, committee_name_norm, purpose_norm) {
  if (grepl(target$ward_numeric_pattern, committee_name_norm, perl = TRUE) ||
      grepl(target$ward_word_pattern, committee_name_norm, perl = TRUE)) {
    return(list(matched = TRUE, match_rule = "ward_name"))
  }
  if (grepl(target$ward_numeric_pattern, purpose_norm, perl = TRUE) ||
      grepl(target$ward_word_pattern, purpose_norm, perl = TRUE)) {
    return(list(matched = TRUE, match_rule = "ward_purpose"))
  }
  list(matched = FALSE, match_rule = "")
}

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
targets <- data.table::fread("../input/alderman_cycle_targets.csv")

required_committee_columns <- c("ID", "Name", "Purpose", "CreationDate", "StatusDate", "Status")
if (!all(required_committee_columns %in% names(committees))) {
  stop("ISBE committees file is missing required columns.", call. = FALSE)
}
if (anyDuplicated(targets, by = c("cycle_year", "ward_number")) > 0) {
  stop("Alderman cycle targets must be unique by cycle_year and ward_number.", call. = FALSE)
}

committees[, committee_name_norm := normalize_text(Name)]
committees[, purpose_norm := normalize_text(Purpose)]
committees[, creation_date := as.Date(substr(CreationDate, 1, 10))]
committees[, status_date := as.Date(substr(StatusDate, 1, 10))]
committees[, status_upper := toupper(trimws(Status))]

rows <- list()
row_index <- 0L

for (cycle_year_i in sort(unique(targets$cycle_year))) {
  cycle_targets <- targets[cycle_year == cycle_year_i][order(ward_number)]
  if (nrow(cycle_targets) != 50L) {
    stop(sprintf("Expected 50 target aldermen for %s.", cycle_year_i), call. = FALSE)
  }
  cycle_start_i <- as.Date(cycle_targets$cycle_start_date[1])
  cycle_end_i <- as.Date(cycle_targets$cycle_end_date[1])
  target_records <- target_records_for_cycle(cycle_targets)
  active <- committees[
    (is.na(creation_date) | creation_date <= cycle_end_i) &
      (status_upper == "A" | is.na(status_date) | status_date >= cycle_start_i)
  ]

  for (i in seq_len(nrow(active))) {
    committee_id_i <- trimws(active$ID[i])
    committee_name_i <- trimws(active$Name[i])
    committee_name_norm_i <- active$committee_name_norm[i]
    purpose_norm_i <- active$purpose_norm[i]
    committee_words_i <- words_set(committee_name_norm_i)
    purpose_words_i <- words_set(purpose_norm_i)

    matches <- list()
    for (target in target_records) {
      candidate_match <- candidate_match_record(target, committee_name_norm_i, committee_words_i, purpose_words_i)
      if (candidate_match$matched) {
        matches[[length(matches) + 1L]] <- data.table::data.table(
          cycle_year = cycle_year_i,
          cycle_start_date = target$cycle_start_date,
          cycle_end_date = target$cycle_end_date,
          alderman_id = target$alderman_id,
          full_name = target$full_name,
          ward_number = target$ward_number,
          committee_id = committee_id_i,
          committee_name = committee_name_i,
          committee_scope = "candidate",
          match_rule = candidate_match$match_rule,
          confidence = candidate_match$confidence,
          review_flag = candidate_match$review_flag,
          match_priority = ifelse(candidate_match$confidence == "high", 1L, 2L)
        )
      }
    }

    if (length(matches) == 0L) {
      for (target in target_records) {
        ward_match <- ward_match_record(target, committee_name_norm_i, purpose_norm_i)
        if (ward_match$matched) {
          matches[[length(matches) + 1L]] <- data.table::data.table(
            cycle_year = cycle_year_i,
            cycle_start_date = target$cycle_start_date,
            cycle_end_date = target$cycle_end_date,
            alderman_id = target$alderman_id,
            full_name = target$full_name,
            ward_number = target$ward_number,
            committee_id = committee_id_i,
            committee_name = committee_name_i,
            committee_scope = "ward",
            match_rule = ward_match$match_rule,
            confidence = "medium",
            review_flag = TRUE,
            match_priority = 3L
          )
        }
      }
    }

    if (length(matches) > 0L) {
      matched <- data.table::rbindlist(matches)
      data.table::setorder(matched, match_priority, ward_number, alderman_id)
      best <- matched[1]
      if (data.table::uniqueN(matched$alderman_id) > 1L) {
        best[, review_flag := TRUE]
        best[, match_rule := paste0(match_rule, "|multiple_candidate_matches")]
      }
      row_index <- row_index + 1L
      rows[[row_index]] <- best
    }
  }
}

crosswalk <- data.table::rbindlist(rows, fill = TRUE)
crosswalk[, match_priority := NULL]
if (anyDuplicated(crosswalk, by = c("cycle_year", "committee_id")) > 0) {
  stop("Committee IDs are not unique within cycle in automatic crosswalk.", call. = FALSE)
}

data.table::setorder(crosswalk, cycle_year, ward_number, committee_scope, committee_name)
data.table::fwrite(crosswalk, "../output/alderman_committee_crosswalk_auto_cycles.csv")
