# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/permit_stringency_table/code")
# spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"
# max_application_ym <- "2022-12"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(spec, max_application_ym)
}

if (length(cli_args) != 2) {
  stop(
    "FATAL: Script requires 2 args: <spec> <max_application_ym>.",
    call. = FALSE
  )
}

spec <- cli_args[1]
max_application_ym <- cli_args[2]

permits <- data.table::fread(cmd = "gzip -dc ../input/building_permits_text_features.csv.gz")

for (col in intersect(c("unit_change_signal", "unit_change_text"), names(permits))) {
  permits[, (col) := suppressWarnings(as.numeric(as.character(get(col))))]
}

permits[, application_start_date := as.Date(application_start_date)]
bad_dates <- is.na(permits$application_start_date)
if (any(bad_dates)) {
  permits[bad_dates, application_start_date := as.Date(application_start_date, format = "%m/%d/%Y")]
}
permits[, month := as.yearmon(application_start_date)]
permits[, ward := suppressWarnings(as.integer(as.character(ward)))]
permits[, ward := as.character(ward)]
permits[ward == "NA", ward := NA_character_]

permits <- permits[
  permit_type_clean %in% c(
    "PERMIT - NEW CONSTRUCTION",
    "PERMIT - RENOVATION/ALTERATION",
    "PERMIT - WRECKING/DEMOLITION",
    "PERMIT - PORCH CONSTRUCTION",
    "PERMIT - REINSTATE REVOKED PMT"
  ) &
    !is.na(application_start_date) &
    !is.na(month) &
    !is.na(ward) &
    month <= as.yearmon(max_application_ym)
]

alderman_panel <- data.table::fread("../input/chicago_alderman_panel.csv")
alderman_panel[, ward := suppressWarnings(as.integer(as.character(ward)))]
alderman_panel[, ward := as.character(ward)]
alderman_panel[ward == "NA", ward := NA_character_]
alderman_panel[, month_ym := zoo::as.yearmon(month, "%b %Y")]
alderman_panel[is.na(month_ym), month_ym := zoo::as.yearmon(month)]
alderman_panel <- unique(
  alderman_panel[!is.na(ward) & !is.na(month_ym) & !is.na(alderman), .(ward, month = month_ym, alderman)]
)
if (anyDuplicated(alderman_panel, by = c("ward", "month")) > 0) {
  stop("Alderman panel must be unique by ward-month before joining to permits.", call. = FALSE)
}

permits_before_alderman_join <- nrow(permits)
permits <- merge(permits, alderman_panel, by = c("ward", "month"), all.x = TRUE)
if (nrow(permits) != permits_before_alderman_join) {
  stop("Alderman panel join changed permit row count.", call. = FALSE)
}
permits <- permits[!is.na(alderman)]
permits[, alderman_key := toupper(trimws(as.character(alderman)))]
permits[, alderman_key := iconv(alderman_key, to = "ASCII//TRANSLIT")]
permits[is.na(alderman_key), alderman_key := ""]
permits[, alderman_key := gsub("[^A-Z ]+", " ", alderman_key)]
permits[, alderman_key := gsub("\\s+", " ", alderman_key)]
permits[, alderman_key := trimws(alderman_key)]

scores <- data.table::fread(sprintf("../input/alderman_uncertainty_index_%s.csv", spec))
if (!("uncertainty_index" %in% names(scores) && "alderman" %in% names(scores))) {
  stop("Uncertainty score file must include columns `alderman` and `uncertainty_index`.", call. = FALSE)
}
scores[, alderman_key := toupper(trimws(as.character(alderman)))]
scores[, alderman_key := iconv(alderman_key, to = "ASCII//TRANSLIT")]
scores[is.na(alderman_key), alderman_key := ""]
scores[, alderman_key := gsub("[^A-Z ]+", " ", alderman_key)]
scores[, alderman_key := gsub("\\s+", " ", alderman_key)]
scores[, alderman_key := trimws(alderman_key)]
scores[, uncertainty_index := suppressWarnings(as.numeric(as.character(uncertainty_index)))]
scores <- scores[!is.na(uncertainty_index) & alderman_key != "", .(alderman_key, uncertainty_index)]
if (anyDuplicated(scores, by = "alderman_key") > 0) {
  stop("Uncertainty score file must be unique by alderman after key cleaning.", call. = FALSE)
}

permits_before_score_join <- nrow(permits)
permits <- merge(permits, scores, by = "alderman_key", all.x = TRUE)
if (nrow(permits) != permits_before_score_join) {
  stop("Uncertainty score join changed permit row count.", call. = FALSE)
}
permits <- permits[!is.na(uncertainty_index)]

permits[, units_reduced_text := fifelse(!is.na(unit_change_text) & unit_change_text < 0, -unit_change_text, 0)]

ward_month_score_counts <- permits[, .(n_scores = uniqueN(uncertainty_index)), by = .(ward, month)]
if (any(ward_month_score_counts$n_scores > 1)) {
  stop("Ward-month validation cells map to multiple uncertainty scores.", call. = FALSE)
}

ward_month <- permits[, .(
  n_permits = .N,
  share_unit_change = mean(unit_change_signal == 1, na.rm = TRUE),
  units_reduced_text_permit = sum(units_reduced_text, na.rm = TRUE) / .N,
  uncertainty_index = mean(uncertainty_index, na.rm = TRUE)
), by = .(ward, month)]

model_specs <- data.table(
  outcome_key = c("n_permits", "share_unit_change", "units_reduced_text_permit"),
  outcome_label = c(
    "N permits (all high-discretion)",
    "Share with any unit change",
    "Units reduced per permit (text)"
  ),
  digits = c(2L, 4L, 4L)
)

result_rows <- vector("list", nrow(model_specs))
for (i in seq_len(nrow(model_specs))) {
  outcome_name <- model_specs$outcome_key[i]
  digits_i <- model_specs$digits[i]
  dt_i <- ward_month[!is.na(get(outcome_name)) & !is.na(uncertainty_index) & !is.na(n_permits)]
  model_i <- feols(
    as.formula(paste0(outcome_name, " ~ uncertainty_index")),
    data = dt_i,
    weights = ~n_permits,
    warn = FALSE
  )
  coef_table_i <- coeftable(model_i)
  est_i <- as.numeric(coef_table_i["uncertainty_index", "Estimate"])
  se_i <- as.numeric(coef_table_i["uncertainty_index", "Std. Error"])
  p_i <- as.numeric(coef_table_i["uncertainty_index", "Pr(>|t|)"])

  stars_i <- ""
  if (!is.na(p_i) && p_i < 0.01) {
    stars_i <- "***"
  } else if (!is.na(p_i) && p_i < 0.05) {
    stars_i <- "**"
  } else if (!is.na(p_i) && p_i < 0.1) {
    stars_i <- "*"
  }

  result_rows[[i]] <- data.table(
    outcome_key = outcome_name,
    outcome_label = model_specs$outcome_label[i],
    coef_display = paste0(
      formatC(est_i, digits = digits_i, format = "f", big.mark = ","),
      if (nchar(stars_i) == 0) "" else paste0("^{", stars_i, "}")
    ),
    se_display = paste0("(", formatC(se_i, digits = digits_i, format = "f", big.mark = ","), ")"),
    n_obs = nobs(model_i)
  )
}

results <- rbindlist(result_rows)
results <- results[match(
  outcome_key,
  c("n_permits", "share_unit_change", "units_reduced_text_permit")
)]

obs_display <- formatC(results$n_obs, format = "d", big.mark = ",")

writeLines(
  c(
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & \\multicolumn{3}{c}{Dependent variable} \\\\",
    "\\cmidrule(lr){2-4}",
    " & Permits & Any unit change & Units reduced \\\\",
    " & (1) & (2) & (3) \\\\",
    "\\midrule",
    sprintf(
      "Stringency index & $%s$ & $%s$ & $%s$ \\\\",
      results$coef_display[1],
      results$coef_display[2],
      results$coef_display[3]
    ),
    sprintf(
      " & %s & %s & %s \\\\",
      results$se_display[1],
      results$se_display[2],
      results$se_display[3]
    ),
    "\\midrule",
    sprintf("Ward-month observations & %s & %s & %s \\\\", obs_display[1], obs_display[2], obs_display[3]),
    "Permit-count weights & \\multicolumn{3}{c}{Yes} \\\\",
    "Sample & \\multicolumn{3}{c}{High-discretion permits} \\\\",
    "\\bottomrule",
    "\\end{tabular}"
  ),
  con = sprintf("../output/permit_validation_table_uncertainty_%s.tex", spec)
)
