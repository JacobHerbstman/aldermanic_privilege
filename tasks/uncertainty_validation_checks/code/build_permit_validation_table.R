source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/uncertainty_validation_checks/code")
# spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH"
# permits_path <- "../input/building_permits_text_features.csv.gz"
# alderman_panel_path <- "../input/chicago_alderman_panel.csv"
# uncertainty_path <- "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH.csv"
# output_csv <- "../output/permit_validation_results_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH.csv"
# output_tex <- "../output/permit_validation_table_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(spec, permits_path, alderman_panel_path, uncertainty_path, output_csv, output_tex)
}

if (length(args) != 6) {
  stop(
    "Usage: Rscript build_permit_validation_table.R <spec> <permits_csv_gz> <alderman_panel_csv> <uncertainty_csv> <output_csv> <output_tex>",
    call. = FALSE
  )
}

spec <- args[1]
permits_path <- args[2]
alderman_panel_path <- args[3]
uncertainty_path <- args[4]
output_csv <- args[5]
output_tex <- args[6]

cat("=== Permit Validation Table (Stringency) ===\n")
cat("Spec:", spec, "\n")

if (grepl("\\.gz$", permits_path)) {
  permits <- data.table::fread(cmd = paste("gzip -dc", shQuote(permits_path)))
} else {
  permits <- data.table::fread(permits_path)
}
setDT(permits)

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

high_discretion_permits <- c(
  "PERMIT - NEW CONSTRUCTION",
  "PERMIT - RENOVATION/ALTERATION",
  "PERMIT - WRECKING/DEMOLITION",
  "PERMIT - PORCH CONSTRUCTION",
  "PERMIT - REINSTATE REVOKED PMT"
)

permits <- permits[
  permit_type_clean %in% high_discretion_permits &
    !is.na(application_start_date) &
    !is.na(month) &
    !is.na(ward)
]

alderman_panel <- data.table::fread(alderman_panel_path)
setDT(alderman_panel)
alderman_panel[, ward := suppressWarnings(as.integer(as.character(ward)))]
alderman_panel[, ward := as.character(ward)]
alderman_panel[ward == "NA", ward := NA_character_]
alderman_panel[, month_ym := zoo::as.yearmon(month, "%b %Y")]
alderman_panel[is.na(month_ym), month_ym := zoo::as.yearmon(month)]
alderman_panel <- unique(
  alderman_panel[!is.na(ward) & !is.na(month_ym) & !is.na(alderman), .(ward, month = month_ym, alderman)]
)

permits <- merge(permits, alderman_panel, by = c("ward", "month"), all.x = TRUE)
permits <- permits[!is.na(alderman)]
permits[, alderman_key := toupper(trimws(as.character(alderman)))]
permits[, alderman_key := iconv(alderman_key, to = "ASCII//TRANSLIT")]
permits[is.na(alderman_key), alderman_key := ""]
permits[, alderman_key := gsub("[^A-Z ]+", " ", alderman_key)]
permits[, alderman_key := gsub("\\s+", " ", alderman_key)]
permits[, alderman_key := trimws(alderman_key)]

scores <- data.table::fread(uncertainty_path)
setDT(scores)
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
scores <- scores[
  !is.na(uncertainty_index),
  .(uncertainty_index = mean(uncertainty_index, na.rm = TRUE)),
  by = alderman_key
]

permits <- merge(permits, scores, by = "alderman_key", all.x = TRUE)
permits <- permits[!is.na(uncertainty_index)]

permits[, units_reduced_text := fifelse(!is.na(unit_change_text) & unit_change_text < 0, -unit_change_text, 0)]

ward_month <- permits[, .(
  n_permits_all = .N,
  share_unit_change = mean(unit_change_signal == 1, na.rm = TRUE),
  units_reduced_text_permit = sum(units_reduced_text, na.rm = TRUE) / .N,
  n_permits = .N,
  uncertainty_index = mean(uncertainty_index, na.rm = TRUE)
), by = .(ward, month)]

model_specs <- data.table(
  outcome_key = c("n_permits_all", "share_unit_change", "units_reduced_text_permit"),
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

  coef_display_i <- paste0(
    formatC(est_i, digits = digits_i, format = "f", big.mark = ","),
    if (nchar(stars_i) == 0) "" else paste0("^{", stars_i, "}")
  )
  se_display_i <- paste0("(", formatC(se_i, digits = digits_i, format = "f", big.mark = ","), ")")

  result_rows[[i]] <- data.table(
    outcome_key = outcome_name,
    outcome_label = model_specs$outcome_label[i],
    estimate = est_i,
    std_error = se_i,
    p_value = p_i,
    stars = stars_i,
    n_obs = nobs(model_i),
    coef_display = coef_display_i,
    se_display = se_display_i
  )
}

results <- rbindlist(result_rows)
results <- results[match(
  outcome_key,
  c("n_permits_all", "share_unit_change", "units_reduced_text_permit")
)]

readr::write_csv(
  results[, .(outcome_key, outcome_label, estimate, std_error, p_value, stars, n_obs)],
  output_csv
)

n_min <- min(results$n_obs, na.rm = TRUE)
n_max <- max(results$n_obs, na.rm = TRUE)
n_display <- if (n_min == n_max) {
  formatC(n_min, format = "d", big.mark = ",")
} else {
  paste0(formatC(n_min, format = "d", big.mark = ","), "--", formatC(n_max, format = "d", big.mark = ","))
}

tex_lines <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{lc}",
  "\\toprule",
  "Outcome (Ward-Month) & Coef. on Stringency Index \\\\",
  "\\midrule",
  sprintf("%s & $%s$ \\\\", results$outcome_label[1], results$coef_display[1]),
  sprintf(" & %s \\\\", results$se_display[1]),
  sprintf("%s & $%s$ \\\\", results$outcome_label[2], results$coef_display[2]),
  sprintf(" & %s \\\\", results$se_display[2]),
  sprintf("%s & $%s$ \\\\", results$outcome_label[3], results$coef_display[3]),
  sprintf(" & %s \\\\", results$se_display[3]),
  "\\midrule",
  sprintf("N & %s \\\\", n_display),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{threeparttable}"
)

writeLines(tex_lines, con = output_tex)

cat("Saved:\n")
cat(" -", output_csv, "\n")
cat(" -", output_tex, "\n")
