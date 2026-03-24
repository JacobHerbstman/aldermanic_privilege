source("../../setup_environment/code/packages.R")

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/uncertainty_validation_checks/code")
# Rscript build_permit_validation_table.R "ptfeFALSE_rtfeFALSE_porchTRUE_cafeFALSE_2stage" "../input/building_permits_text_features.csv.gz" "../input/chicago_alderman_panel.csv" "../input/alderman_uncertainty_index_ptfeFALSE_rtfeFALSE_porchTRUE_cafeFALSE_2stage.csv" "../output/permit_validation_results_uncertainty_ptfeFALSE_rtfeFALSE_porchTRUE_cafeFALSE_2stage.csv" "../output/permit_validation_table_uncertainty_ptfeFALSE_rtfeFALSE_porchTRUE_cafeFALSE_2stage.tex"

args <- commandArgs(trailingOnly = TRUE)
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

to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

name_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- gsub("[^A-Z ]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

ward_key <- function(x) {
  out <- suppressWarnings(as.integer(as.character(x)))
  out_chr <- as.character(out)
  out_chr[is.na(out)] <- NA_character_
  out_chr
}

read_dt <- function(path) {
  if (grepl("\\.gz$", path)) {
    data.table::fread(cmd = paste("gzip -dc", shQuote(path)))
  } else {
    data.table::fread(path)
  }
}

star_code <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

fmt_num <- function(x, digits) {
  formatC(x, digits = digits, format = "f", big.mark = ",")
}

fmt_coef <- function(x, digits, stars) {
  tex_stars <- if (nchar(stars) == 0) "" else paste0("^{", stars, "}")
  paste0(fmt_num(x, digits), tex_stars)
}

cat("=== Permit Validation Table (Stringency) ===\n")
cat("Spec:", spec, "\n")

permits <- read_dt(permits_path)
setDT(permits)

for (col in intersect(c("unit_change_signal", "unit_change_text"), names(permits))) {
  permits[, (col) := to_numeric(get(col))]
}

permits[, application_start_date := as.Date(application_start_date)]
bad_dates <- is.na(permits$application_start_date)
if (any(bad_dates)) {
  permits[bad_dates, application_start_date := as.Date(application_start_date, format = "%m/%d/%Y")]
}
permits[, month := as.yearmon(application_start_date)]
permits[, ward := ward_key(ward)]

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
alderman_panel[, ward := ward_key(ward)]
alderman_panel[, month_ym := zoo::as.yearmon(month, "%b %Y")]
alderman_panel[is.na(month_ym), month_ym := zoo::as.yearmon(month)]
alderman_panel <- unique(
  alderman_panel[!is.na(ward) & !is.na(month_ym) & !is.na(alderman), .(ward, month = month_ym, alderman)]
)

permits <- merge(permits, alderman_panel, by = c("ward", "month"), all.x = TRUE)
permits <- permits[!is.na(alderman)]
permits[, alderman_key := name_key(alderman)]

scores <- data.table::fread(uncertainty_path)
setDT(scores)
if (!("uncertainty_index" %in% names(scores) && "alderman" %in% names(scores))) {
  stop("Uncertainty score file must include columns `alderman` and `uncertainty_index`.", call. = FALSE)
}
scores[, alderman_key := name_key(alderman)]
scores[, uncertainty_index := to_numeric(uncertainty_index)]
scores <- scores[!is.na(uncertainty_index), .(uncertainty_index = mean(uncertainty_index, na.rm = TRUE)), by = alderman_key]

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

run_model <- function(outcome_name, label, digits) {
  dt <- ward_month[!is.na(get(outcome_name)) & !is.na(uncertainty_index) & !is.na(n_permits)]
  model <- feols(
    as.formula(paste0(outcome_name, " ~ uncertainty_index")),
    data = dt,
    weights = ~n_permits,
    warn = FALSE
  )
  ctab <- coeftable(model)
  est <- as.numeric(ctab["uncertainty_index", "Estimate"])
  se <- as.numeric(ctab["uncertainty_index", "Std. Error"])
  p <- as.numeric(ctab["uncertainty_index", "Pr(>|t|)"])
  stars <- star_code(p)
  data.table(
    outcome_key = outcome_name,
    outcome_label = label,
    estimate = est,
    std_error = se,
    p_value = p,
    stars = stars,
    n_obs = nobs(model),
    coef_display = fmt_coef(est, digits, stars),
    se_display = paste0("(", fmt_num(se, digits), ")")
  )
}

results <- rbindlist(list(
  run_model("n_permits_all", "N permits (all high-discretion)", 2),
  run_model("share_unit_change", "Share with any unit change", 4),
  run_model("units_reduced_text_permit", "Units reduced per permit (text)", 4)
))

results <- results[match(
  outcome_key,
  c("n_permits_all", "share_unit_change", "units_reduced_text_permit")
)]

readr::write_csv(results[, .(outcome_key, outcome_label, estimate, std_error, p_value, stars, n_obs)], output_csv)

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
