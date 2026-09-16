# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/donation_score_robustness/code")
# sample_start <- 2006
# sample_end <- 2022
# split_year <- 2014
# history_start <- 1994
# history_end <- 2026
# dollar_year <- 2022

library(data.table)
library(zoo)
source("../../../shared/code/save_data.R")

args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 6L)
  sample_start <- as.integer(args[1])
  sample_end <- as.integer(args[2])
  split_year <- as.integer(args[3])
  history_start <- as.integer(args[4])
  history_end <- as.integer(args[5])
  dollar_year <- as.integer(args[6])
}

normalize <- function(x) trimws(gsub(" +", " ", gsub("[^a-z0-9 ]", " ", tolower(x))))
receipts <- as.data.table(arrow::read_parquet("../output/donation_receipts.parquet"))
scores <- fread("../input/paper_scores.csv")
scores[, person_key := normalize(alderman)]
setnames(scores, c("uncertainty_index", "alderman_fe_raw"), c("paper_score", "raw_score"))
early <- fread("../input/early_scores.csv")
early[, person_key := normalize(alderman)]
stopifnot(!anyDuplicated(scores$person_key), !anyDuplicated(early$person_key))
scores <- merge(scores, early[, .(person_key, early_score = uncertainty_index)], by = "person_key", all.x = TRUE)
office <- fread("../input/alderman_months.csv")
office[, `:=`(person_key = normalize(alderman), date = as.Date(as.yearmon(month, "%b %Y")))]
office[, year := as.integer(format(date, "%Y"))]
office <- unique(office[, .(person_key, date, year)])
cpi <- fread("../input/chicago_cpi.csv")
cpi[, year := as.integer(substr(observation_date, 1, 4))]
cpi <- cpi[, .(cpi = mean(CUURA207SA0, na.rm = TRUE)), by = year]
stopifnot(!anyDuplicated(cpi$year))
receipts <- merge(receipts, cpi, by = "year", all.x = TRUE, sort = FALSE)
stopifnot(all(is.finite(receipts$cpi)))
dollar_base <- cpi[year == dollar_year, cpi]
stopifnot(length(dollar_base) == 1L)
receipts[, real_amount := amount * dollar_base / cpi]

# Keep organizations, union-linked individuals and real-estate definitions visible.
receipts[, `:=`(
  union_recorded_or_name = !is.na(union_category),
  union_direct = union_organization,
  union_direct_no_pending = union_organization & !union_pending,
  union_individual_or_other = !is.na(union_category) & !union_organization,
  trades_direct = union_organization & union_category == "construction_trades",
  trades_legacy = union_category == "construction_trades",
  education_direct = union_organization & union_category == "teacher_education",
  service_direct = union_organization & union_category == "public_sector_service",
  generic_labor_direct = union_organization & union_category == "generic_labor",
  development_coalition = real_estate_verified | nonunion_construction |
    (union_organization & union_category == "construction_trades"))]
sectors <- c("union_recorded_or_name", "union_direct", "union_direct_no_pending",
             "union_individual_or_other", "trades_direct", "trades_legacy", "education_direct",
             "service_direct", "generic_labor_direct", "real_estate_strict", "developer_explicit",
             "real_estate_broad", "nonunion_construction", "land_use_professional",
             "estate_name_evidence", "development_coalition", "real_estate_verified",
             "developer_verified", "union_original_rules")
for (sector in sectors) set(receipts, which(is.na(receipts[[sector]])), sector, FALSE)

windows <- data.table(
  period = c("main", "early", "late", "before", "after_complete_years", "latest_partial_year", "all_recorded"),
  first_year = c(sample_start, sample_start, split_year, history_start, sample_end + 1L, history_end, history_start),
  last_year = c(sample_end, split_year - 1L, sample_end, sample_start - 1L, history_end - 1L, history_end, history_end))
definitions <- c("personal_in_office", "personal_calendar", "named_in_office", "with_inkind", "gross_transfers", "legacy_cycle")
parts <- list()
for (w in seq_len(nrow(windows))) {
  first_year <- windows$first_year[w]
  last_year <- windows$last_year[w]
  for (definition in definitions) {
    uses_office <- definition %in% c("personal_in_office", "named_in_office", "with_inkind", "gross_transfers")
    # The recorded monthly office roster ends in 2022; wider histories are explicitly calendar-based.
    if (uses_office && !windows$period[w] %in% c("main", "early", "late")) next
    d <- copy(receipts[year >= first_year & year <= last_year])
    if (definition == "legacy_cycle") {
      d <- d[legacy_cycle_included & receipt_type %in% c("1A", "2A", "5A")]
    } else {
      if (definition != "named_in_office") d <- d[strict_candidate == TRUE]
      if (uses_office) d <- d[in_office %in% TRUE]
      if (definition != "with_inkind") d <- d[receipt_type %in% c("1A", "2A")]
      if (definition != "gross_transfers") d <- d[own_committee_transfer == FALSE]
    }
    if (!nrow(d)) next
    tenure <- office[year >= first_year & year <= last_year, .(office_years = .N / 12), by = person_key]
    d <- merge(d, tenure, by = "person_key", all.x = TRUE, sort = FALSE)
    if (!uses_office) d[, office_years := last_year - first_year + 1]
    donor_totals <- d[, .(donor_amount = sum(amount)), by = .(person_key, donor_norm)]
    donor_totals[, largest_donor := donor_amount == max(donor_amount), by = person_key]
    stopifnot(!anyDuplicated(donor_totals[, .(person_key, donor_norm)]))
    d <- merge(d, donor_totals[, .(person_key, donor_norm, largest_donor)], by = c("person_key", "donor_norm"), all.x = TRUE, sort = FALSE)
    for (sector in sectors) {
      d[, sector_flag := get(sector)]
      a <- d[, .(
        full_name = first(full_name), total_dollars = sum(amount), total_receipts = .N,
        observed_years = uniqueN(year), office_years = first(office_years),
        sector_dollars = sum(amount[sector_flag]),
        dollar_share = sum(amount[sector_flag]) / sum(amount),
        real_dollar_share = sum(real_amount[sector_flag]) / sum(real_amount),
        receipt_share = mean(sector_flag),
        donor_name_share = uniqueN(donor_norm[sector_flag & nzchar(donor_norm)]) / uniqueN(donor_norm[nzchar(donor_norm)]),
        real_dollars_per_year = sum(real_amount[sector_flag]) / first(office_years),
        share_without_largest_donor = if (sum(amount[!largest_donor]) > 0)
          sum(amount[sector_flag & !largest_donor]) / sum(amount[!largest_donor]) else NA_real_,
        largest_donor_share = sum(amount[largest_donor]) / sum(amount)), by = person_key]
      a[, `:=`(period = windows$period[w], first_year = first_year, last_year = last_year,
               definition = definition, sector = sector)]
      parts[[length(parts) + 1L]] <- a
    }
  }
}
measures <- rbindlist(parts)
legacy <- fread("../sources/legacy_donation_totals.csv")
legacy <- legacy[committee_count_sum > 0 & total_receipts_amount > 0]
legacy[, person_key := normalize(full_name)]
stopifnot(!anyDuplicated(legacy$person_key))
for (sector in c("union_recorded_or_name", "trades_legacy")) {
  amount_column <- if (sector == "trades_legacy") "construction_trades_amount" else "union_total_amount"
  count_column <- if (sector == "trades_legacy") "construction_trades_count" else "union_total_count"
  old <- legacy[, .(person_key, full_name, total_dollars = total_receipts_amount,
    total_receipts = total_receipts_count, observed_years = cycles_count * 4,
    office_years = cycles_count * 4, sector_dollars = get(amount_column),
    dollar_share = get(amount_column) / total_receipts_amount,
    receipt_share = get(count_column) / total_receipts_count)]
  old[, `:=`(period = "legacy_1999_2023", first_year = 1999L, last_year = 2023L,
              definition = "legacy_pooled", sector = sector)]
  measures <- rbind(measures, old, fill = TRUE)
}
measures <- merge(measures, scores[, .(person_key, paper_score, raw_score, early_score, n_permits)],
                  by = "person_key", all.x = TRUE, sort = FALSE)
setorder(measures, period, definition, sector, person_key)
stopifnot(!anyDuplicated(measures[, .(period, definition, sector, person_key)]))

# Report every declared comparison, including zeros, rather than selecting a best-fitting definition.
baseline_people <- unique(measures[period == "main" & definition == "personal_in_office" &
                                    is.finite(paper_score), person_key])
common_people <- measures[period == "main" & person_key %in% baseline_people,
  .(n_definitions = uniqueN(definition)), by = person_key][n_definitions == length(definitions), person_key]
comparison_measures <- rbind(
  copy(measures)[, cohort := "available"],
  copy(measures[period == "main" & person_key %in% common_people])[, cohort := "common_main"])
metric_names <- c("dollar_share", "real_dollar_share", "receipt_share", "donor_name_share", "real_dollars_per_year", "share_without_largest_donor")
comparisons <- list()
for (metric in metric_names) {
  for (score in c("paper_score", "raw_score", "early_score")) {
    out <- comparison_measures[, {
      ok <- is.finite(get(metric)) & is.finite(get(score))
      x <- get(metric)[ok]; y <- get(score)[ok]
      varied <- length(x) >= 4L && sd(x) > 0 && sd(y) > 0
      pearson <- if (varied) cor(x, y) else NA_real_
      spearman <- if (varied) cor(x, y, method = "spearman") else NA_real_
      .(n = length(x), positive = sum(x > 0), pearson = pearson, spearman = spearman,
        pearson_low = if (varied && abs(pearson) < 1) tanh(atanh(pearson) - 1.96 / sqrt(length(x) - 3)) else NA_real_,
        pearson_high = if (varied && abs(pearson) < 1) tanh(atanh(pearson) + 1.96 / sqrt(length(x) - 3)) else NA_real_)
    }, by = .(cohort, period, definition, sector)]
    out[, `:=`(metric = metric, score = score)]
    comparisons[[length(comparisons) + 1L]] <- out
  }
}
correlations <- rbindlist(comparisons)

# Track influential aldermen for the main dollar-share/rank comparisons.
base <- measures[period == "main" & definition == "personal_in_office" & is.finite(paper_score)]
influence <- base[, {
  x <- dollar_share; y <- paper_score
  varied <- .N >= 5L && sd(x) > 0 && sd(y) > 0
  r <- if (varied) cor(x, y, method = "spearman") else NA_real_
  leave <- if (varied) vapply(seq_along(x), function(i) {
    if (sd(x[-i]) == 0 || sd(y[-i]) == 0) return(NA_real_)
    cor(x[-i], y[-i], method = "spearman")
  }, numeric(1)) else rep(NA_real_, .N)
  .(leave_one_out_low = if (any(is.finite(leave))) min(leave, na.rm = TRUE) else NA_real_,
    leave_one_out_high = if (any(is.finite(leave))) max(leave, na.rm = TRUE) else NA_real_,
    most_influential = if (any(is.finite(leave))) full_name[which.max(abs(leave - r))] else NA_character_)
}, by = sector]
correlations <- merge(correlations, influence, by = "sector", all.x = TRUE, sort = FALSE)
correlations[period != "main" | definition != "personal_in_office" | metric != "dollar_share" | score != "paper_score",
             c("leave_one_out_low", "leave_one_out_high", "most_influential") := list(NA_real_, NA_real_, NA_character_)]

# Same-person stability across the two donation periods; no comparisons of changing cohorts.
halves <- dcast(measures[period %in% c("early", "late") & definition == "personal_in_office"],
                 person_key + sector ~ period, value.var = "dollar_share")
stability <- halves[, {
  keep <- is.finite(early) & is.finite(late)
  x <- early[keep]; y <- late[keep]
  .(left_measure = "2006--2013", right_measure = "2014--2022", n = length(x),
    pearson = if (length(x) > 3 && sd(x) > 0 && sd(y) > 0) cor(x, y) else NA_real_,
    spearman = if (length(x) > 3 && sd(x) > 0 && sd(y) > 0) cor(x, y, method = "spearman") else NA_real_)
}, by = sector]
stability[, comparison := "same_person_over_time"]
wide <- dcast(base, person_key ~ sector, value.var = "dollar_share")
between <- list()
for (a in sectors) for (b in sectors) {
  keep <- is.finite(wide[[a]]) & is.finite(wide[[b]])
  x <- wide[[a]][keep]; y <- wide[[b]][keep]
  between[[length(between) + 1L]] <- data.table(sector = "all", left_measure = a, right_measure = b,
    n = length(x), pearson = if (length(x) > 3 && sd(x) > 0 && sd(y) > 0) cor(x, y) else NA_real_,
    spearman = if (length(x) > 3 && sd(x) > 0 && sd(y) > 0) cor(x, y, method = "spearman") else NA_real_,
    comparison = "between_donation_measures")
}
stability <- rbind(stability, rbindlist(between), fill = TRUE)
# Compare each recipient definition with the baseline on the same observed people.
variants <- dcast(measures[period == "main" & person_key %in% baseline_people],
                  person_key + sector ~ definition, value.var = "dollar_share")
for (variant in setdiff(definitions, "personal_in_office")) {
  agreement <- variants[, {
    keep <- is.finite(personal_in_office) & is.finite(get(variant))
    x <- personal_in_office[keep]; y <- get(variant)[keep]
    .(left_measure = "personal_in_office", right_measure = variant, n = length(x),
      pearson = if (length(x) > 3 && sd(x) > 0 && sd(y) > 0) cor(x, y) else NA_real_,
      spearman = if (length(x) > 3 && sd(x) > 0 && sd(y) > 0) cor(x, y, method = "spearman") else NA_real_)
  }, by = sector]
  agreement[, comparison := "same_people_different_recipient_definition"]
  stability <- rbind(stability, agreement)
}
gaps <- copy(base)
gaps[, `:=`(donation_percentile = frank(dollar_share, ties.method = "average") / .N,
             stringency_percentile = frank(paper_score, ties.method = "average") / .N), by = sector]
gaps[, leniency_percentile := 1 - stringency_percentile]
gaps[, gap_if_donations_signal_leniency := donation_percentile - leniency_percentile]
gaps[, fitted_score := if (sd(dollar_share) > 0) fitted(lm(paper_score ~ dollar_share)) else mean(paper_score), by = sector]
gaps[, score_residual := paper_score - fitted_score]
setorder(gaps, sector, -score_residual)
setorder(correlations, cohort, period, definition, sector, metric, score)
SaveData(measures, c("period", "definition", "sector", "person_key"), "../output/donation_measures.csv")
SaveData(correlations, c("cohort", "period", "definition", "sector", "metric", "score"), "../output/donation_correlations.csv")
SaveData(stability, c("comparison", "sector", "left_measure", "right_measure"), "../output/donation_stability.csv")
SaveData(gaps, c("sector", "person_key"), "../output/donation_rank_gaps.csv")
