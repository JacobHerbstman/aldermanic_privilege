# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/donation_score_robustness/code")
# history_start <- 1994
# history_end <- 2026

library(data.table)
library(zoo)
source("../../../shared/code/save_data.R")

args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 2L)
  history_start <- as.integer(args[1])
  history_end <- as.integer(args[2])
}
stopifnot(history_start < history_end)

# The export mixes UTF-8 with Windows punctuation/accent bytes.
decode_text <- function(x) {
  bad <- !is.na(x) & is.na(iconv(x, from = "UTF-8", to = "UTF-8"))
  x[bad] <- iconv(x[bad], from = "Windows-1252", to = "UTF-8", sub = " ")
  x
}

# The same normalization is used for donors, employers and recorded names.
normalize <- function(x) {
  x[is.na(x)] <- ""
  x <- tolower(iconv(x, from = "", to = "UTF-8", sub = " "))
  trimws(gsub(" +", " ", gsub("[^a-z0-9 ]", " ", gsub("&", " and ", x, fixed = TRUE))))
}

# Read the recorded committees and require one person per included committee.
mapping <- fread("../sources/committee_crosswalk.csv", na.strings = "")
stopifnot(!anyDuplicated(mapping[, .(cycle_year, committee_id)]))
mapping <- mapping[include_strict_candidate | include_main_named_recipient]
stopifnot(all(mapping[, uniqueN(alderman_id), by = committee_id]$V1 == 1L))
owners <- mapping[, .(alderman_id = first(alderman_id), full_name = first(full_name),
                     strict_candidate = any(include_strict_candidate),
                     named_recipient = any(include_main_named_recipient)), by = committee_id]
owners[, ownership_source := "recorded_committee_crosswalk"]
registry <- fread("../sources/committee_registry.tsv", sep = "\t", quote = "", fill = TRUE)
registry[, `:=`(Name = decode_text(Name), ReferName = decode_text(ReferName))]
stopifnot(!anyDuplicated(registry$ID))

# Recover officials omitted by the old election-winner roster only when the
# registry has one Chicago candidate committee containing their exact full name.
scores <- fread("../sources/paper_scores.csv")
missing_names <- setdiff(scores$alderman, owners$full_name)
registry[, candidate_name := normalize(paste(Name, ReferName))]
for (person in missing_names) {
  found <- registry[normalize(City) == "chicago" &
    grepl(paste0("\\b", normalize(person), "\\b"), candidate_name, perl = TRUE) &
    grepl("\\b(friends|citizens|elect)\\b", candidate_name, perl = TRUE)]
  if (nrow(found) == 1L && !found$ID %in% owners$committee_id) {
    owners <- rbind(owners, data.table(committee_id = found$ID,
      alderman_id = gsub(" ", "_", normalize(person)), full_name = person,
      strict_candidate = TRUE, named_recipient = TRUE,
      ownership_source = "unique_full_name_chicago_candidate_committee"))
  }
}
stopifnot(!anyDuplicated(owners$committee_id))

# Malformed physical rows are excluded, not repaired by shifting their fields.
widths <- count.fields("../input/receipts.tsv", sep = "\t", quote = "", comment.char = "", blank.lines.skip = TRUE)
receipts <- fread("../input/receipts.tsv", sep = "\t", quote = "", fill = Inf,
                  select = c("ID", "CommitteeID", "FiledDocID", "ETransID", "RcvDate", "Amount",
                             "D2Part", "Archived", "FirstName", "LastOnlyName", "Occupation", "Employer"),
                  colClasses = "character", na.strings = "", showProgress = FALSE)
stopifnot(nrow(receipts) == length(widths) - 1L, widths[1] == 29L)
receipts[, source_row := .I + 1L]
receipts[, valid_width := widths[-1] == widths[1]]
receipts[, committee_id := suppressWarnings(as.integer(CommitteeID))]
counts <- data.table(stage = c("raw_rows", "malformed_raw_rows", "rows_in_recorded_committees"),
                     rows = c(nrow(receipts), sum(!receipts$valid_width),
                              sum(receipts$committee_id %in% owners$committee_id)))
receipts <- receipts[committee_id %in% owners$committee_id]
for (column in c("FirstName", "LastOnlyName", "Occupation", "Employer")) {
  set(receipts, j = column, value = decode_text(receipts[[column]]))
}
setnames(receipts, c("ID", "FiledDocID", "ETransID", "D2Part"),
         c("receipt_id", "filed_doc_id", "electronic_id", "receipt_type"))
receipts[, received_date := suppressWarnings(as.Date(substr(RcvDate, 1, 10), format = "%Y-%m-%d"))]
receipts[, amount := suppressWarnings(as.numeric(gsub("[$,]", "", Amount)))]
receipts[, exclusion := fcase(
  !valid_width | !grepl("^[0-9]+$", receipt_id), "malformed_row",
  !tolower(Archived) %in% c("true", "false"), "unknown_amendment_status",
  tolower(Archived) == "true", "superseded_by_amendment",
  is.na(received_date), "invalid_receipt_date",
  received_date < as.Date(paste0(history_start, "-01-01")) |
    received_date > as.Date(paste0(history_end, "-12-31")), "outside_recorded_analysis_period",
  !receipt_type %in% c("1A", "2A", "5A"), "loan_or_other_receipt",
  is.na(amount), "unusable_amount",
  amount <= 0, "nonpositive_receipt",
  default = "retained")]
counts <- rbind(counts, receipts[, .(rows = .N), by = .(stage = exclusion)])
receipts <- receipts[exclusion == "retained"]
stopifnot(!anyDuplicated(receipts$receipt_id))
receipts <- merge(receipts, owners, by = "committee_id", all.x = TRUE, sort = FALSE)
stopifnot(!anyNA(receipts$alderman_id))
receipts[, year := as.integer(format(received_date, "%Y"))]
receipts[, month := format(received_date, "%Y-%m")]
receipts[, cycle_year := 2003L + 4L * floor((year - 1999L - as.integer(format(received_date, "%m") < "06")) / 4L)]
cycle_key <- paste(mapping$committee_id, mapping$cycle_year)
receipts[, cycle_match := match(paste(committee_id, cycle_year), cycle_key)]
receipts[, legacy_cycle_included := !is.na(cycle_match) & mapping$include_main_named_recipient[cycle_match]]
receipts[, FirstName := fifelse(is.na(FirstName), "", trimws(FirstName))]
receipts[, LastOnlyName := fifelse(is.na(LastOnlyName), "", trimws(LastOnlyName))]
receipts[, donor_name := trimws(paste(FirstName, LastOnlyName))]
receipts[, donor_norm := normalize(donor_name)]
receipts[, occupation_norm := normalize(Occupation)]
receipts[, employer_norm := normalize(Employer)]

# Use the actual recorded months in office for the 2006--2022 comparison.
office <- fread("../input/alderman_months.csv")
office[, person_key := normalize(alderman)]
office[, month := format(as.Date(as.yearmon(month, "%b %Y")), "%Y-%m")]
receipts[, person_key := normalize(full_name)]
receipts[, in_office := paste(person_key, month) %in% paste(office$person_key, office$month)]
receipts[month < min(office$month) | month > max(office$month), in_office := NA]

# Recognize transfers among the same person's recorded committees by exact names.
aliases <- unique(rbind(
  mapping[, .(alderman_id, alias = normalize(committee_name))],
  merge(owners, registry[, .(committee_id = ID, Name, ReferName)], by = "committee_id")[,
    .(alderman_id, alias = normalize(c(Name, ReferName))), by = committee_id][, .(alderman_id, alias)]))
aliases <- aliases[nzchar(alias)]
receipts[, own_committee_transfer := receipt_type == "2A" &
           paste(alderman_id, donor_norm) %in% paste(aliases$alderman_id, aliases$alias)]

# Recorded donor judgments are applied once. New names use the original ordered rules.
recorded <- fread("../sources/recorded_union_classifications.csv", na.strings = "")
recorded[trimws(donor_category) == "", donor_category := NA_character_]
stopifnot(!anyDuplicated(recorded$donor_norm))
stopifnot(all(na.omit(recorded$donor_category) %in%
  c("construction_trades", "teacher_education", "public_sector_service", "generic_labor")))
receipts <- merge(receipts, recorded[, .(donor_norm, recorded_union = donor_category,
  union_review_status = manual_decision_status, auto_confidence)], by = "donor_norm", all.x = TRUE, sort = FALSE)
receipts[, union_category := recorded_union]
receipts[, union_source := fifelse(!is.na(recorded_union), "recorded_classification", "unclassified")]
rules <- fread("../sources/union_name_rules.csv")
setorder(rules, order)
for (i in seq_len(nrow(rules))) {
  hit <- is.na(receipts$union_category) & grepl(rules$pattern[i], receipts$donor_norm, perl = TRUE)
  # Explicit recorded exclusions stay excluded.
  hit <- hit & !grepl("clear_exclude", fifelse(is.na(receipts$union_review_status), "", receipts$union_review_status))
  receipts[hit, `:=`(union_category = rules$category[i], union_source = "donor_name_rule")]
}
receipts[, union_original_rules := !is.na(union_category)]

# The evidence table adds named organizations omitted by the old vocabulary.
# Match companies to the donor or recorded employer, never to a personal surname.
evidence <- fread("../sources/sector_name_evidence.csv")
stopifnot(!anyDuplicated(evidence$rule_id))
receipts[, `:=`(named_developer = FALSE, named_contractor = FALSE,
                named_estate_donor = FALSE, sector_name_rule = "")]
for (i in seq_len(nrow(evidence))) {
  name_hit <- grepl(evidence$pattern[i], receipts$donor_norm, perl = TRUE)
  employer_hit <- grepl(evidence$pattern[i], receipts$employer_norm, perl = TRUE)
  category <- evidence$category[i]
  hit <- if (grepl("^union_", category)) name_hit & receipts$FirstName == "" else name_hit | employer_hit
  receipts[hit, sector_name_rule := paste0(sector_name_rule, evidence$rule_id[i], ";")]
  if (category %in% c("union_generic", "union_service")) {
    hit <- hit & is.na(receipts$union_category) &
      !grepl("clear_exclude", fifelse(is.na(receipts$union_review_status), "", receipts$union_review_status))
    receipts[hit, `:=`(union_category = if (category == "union_generic") "generic_labor" else "public_sector_service",
                       union_source = "documented_added_name_rule")]
  }
  if (category == "developer") receipts[hit, named_developer := TRUE]
  if (category == "construction") receipts[hit, named_contractor := TRUE]
  if (category == "developer") receipts[name_hit, named_estate_donor := TRUE]
}
union_cue <- "\\b(union|local [0-9]+|pac|political|council|brotherhood|ibew|liuna|seiu|afscme|ufcw|afl cio|teachers|ipace|i p a c e|unite here|federation|teamsters?|fraternal order of police)\\b"
receipts[, union_organization := !is.na(union_category) & FirstName == "" & grepl(union_cue, donor_norm, perl = TRUE)]
stopifnot(!any(receipts$union_category == "", na.rm = TRUE))
receipts[, union_pending := grepl("seeded_accept_auto", fifelse(is.na(union_review_status), "", union_review_status))]

# The fields identify real-estate links, not a claim that every such donor is a developer.
estate <- "\\b(real estate|realty|realtors?|property management|landlord)\\b"
developer <- "\\b((real estate|property|housing|residential|commercial) develop(ment|er|ers)?|home ?builders?)\\b"
broad <- "\\b(development|developers?|devco|properties|apartments?|condominiums?)\\b"
construction <- "\\b(construction|general contractors?|building contractors?|home ?builders?|design build)\\b"
professional <- "\\b(land use|zoning|architects?|urban planning)\\b"
irrelevant <- "\\b(software|web|application|business development|economic development|community development|child development|information technology|cloud|data architect|systems architect)\\b"
receipts[, sector_text := paste(donor_norm, occupation_norm, employer_norm)]
receipts[, real_estate_strict := !union_organization & grepl(estate, sector_text, perl = TRUE)]
receipts[, developer_explicit := !union_organization & grepl(developer, sector_text, perl = TRUE)]
receipts[, real_estate_broad := real_estate_strict | developer_explicit |
  (!union_organization & grepl(broad, sector_text, perl = TRUE) & !grepl(irrelevant, sector_text, perl = TRUE))]
receipts[, nonunion_construction := !union_organization & grepl(construction, sector_text, perl = TRUE) & !grepl(irrelevant, sector_text, perl = TRUE)]
receipts[, land_use_professional := !union_organization & grepl(professional, sector_text, perl = TRUE) & !grepl(irrelevant, sector_text, perl = TRUE)]
receipts[, developer_verified := developer_explicit | (!union_organization & named_developer)]
receipts[, real_estate_verified := real_estate_strict | developer_verified]
receipts[, real_estate_broad := real_estate_broad | real_estate_verified]
receipts[, nonunion_construction := nonunion_construction | (!union_organization & named_contractor)]
receipts[, estate_name_evidence := !union_organization &
  (grepl(paste(estate, developer, sep = "|"), donor_norm, perl = TRUE) | (FirstName == "" & named_estate_donor))]

donors <- receipts[, .(donor_name = first(donor_name), receipt_count = .N, amount = sum(amount),
  union_organization = any(union_organization), union_pending = any(union_pending),
  union_categories = paste(sort(unique(na.omit(union_category))), collapse = ";"),
  real_estate_strict = any(real_estate_strict), developer_explicit = any(developer_explicit),
  real_estate_broad = any(real_estate_broad), nonunion_construction = any(nonunion_construction),
  real_estate_verified = any(real_estate_verified), developer_verified = any(developer_verified),
  sector_name_rules = paste(sort(unique(sector_name_rule[nzchar(sector_name_rule)])), collapse = " "),
  occupations = paste(head(sort(unique(na.omit(Occupation))), 5), collapse = "; "),
  employers = paste(head(sort(unique(na.omit(Employer))), 5), collapse = "; ")),
  by = donor_norm]
setorder(donors, -amount, donor_norm)
coverage <- receipts[, .(receipts = .N, dollars = sum(amount), first_date = min(received_date),
  last_date = max(received_date), in_office_receipts = sum(in_office %in% TRUE),
  own_transfer_dollars = sum(amount[own_committee_transfer]),
  pending_union_dollars = sum(amount[union_pending]),
  missing_job_dollars = sum(amount[occupation_norm == "" & employer_norm == ""])),
  by = .(alderman_id, full_name, year)]
setorder(receipts, received_date, receipt_id)
setorder(coverage, year, alderman_id)
receipts[, c("CommitteeID", "RcvDate", "Amount", "Archived", "valid_width", "exclusion",
             "cycle_match", "sector_text") := NULL]
SaveData(receipts, "receipt_id", "../output/donation_receipts.parquet")
SaveData(counts, "stage", "../output/cleaning_counts.csv")
SaveData(donors, "donor_norm", "../output/donor_classifications.csv")
SaveData(coverage, c("alderman_id", "year"), "../output/committee_coverage.csv")
