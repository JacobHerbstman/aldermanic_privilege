source("../../setup_environment/code/packages.R")

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

permit_file <- "../input/building_permits_text_features.csv.gz"
alderman_panel_file <- "../input/chicago_alderman_panel.csv"
strictness_file <- "../input/alderman_restrictiveness_scores_month_FEs.csv"
ward_month_controls_file <- "../input/ward_monthly_panel_for_alderman_fe.csv"

panel_output <- "../output/ward_month_strictness_validation_panel.csv"
coverage_output <- "../output/ward_month_validation_coverage.csv"
coef_output <- "../output/ward_month_validation_coefficients.csv"
table_output <- "../output/ward_month_validation_regressions.tex"
plot_output <- "../output/ward_month_validation_coefficients.pdf"
cuts_output <- "../output/ward_month_validation_sample_cuts.csv"
permit_volume_output <- "../output/ward_month_validation_permit_volume.csv"

cat("Loading scraped permit features...\n")
permits <- read_dt(permit_file)
setDT(permits)

num_cols <- c(
  "unit_change_signal", "unit_reduction_signal", "unit_increase_signal",
  "flag_deconversion", "units_delta_last", "unit_change_text",
  "stories_first_mention", "stories_last_mention", "from_to_pair_n",
  "flag_revision"
)
for (col in intersect(num_cols, names(permits))) {
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

cat("High-discretion permit rows:", nrow(permits), "\n")

cat("Loading alderman panel...\n")
alderman_panel <- data.table::fread(alderman_panel_file)
setDT(alderman_panel)
alderman_panel[, ward := ward_key(ward)]
alderman_panel[, month_ym := zoo::as.yearmon(month, "%b %Y")]
alderman_panel[is.na(month_ym), month_ym := zoo::as.yearmon(month)]
alderman_panel <- unique(alderman_panel[!is.na(ward) & !is.na(month_ym) & !is.na(alderman), .(ward, month = month_ym, alderman)])

permits <- merge(
  permits,
  alderman_panel,
  by = c("ward", "month"),
  all.x = TRUE
)
permits <- permits[!is.na(alderman)]
permits[, alderman_key := name_key(alderman)]

cat("Loading strictness scores...\n")
strictness <- data.table::fread(strictness_file)
setDT(strictness)
strictness[, strictness_index := to_numeric(strictness_index)]
strictness[, alderman_key := name_key(alderman)]
strictness_lookup <- strictness[!is.na(strictness_index), .(
  strictness_index = mean(strictness_index, na.rm = TRUE)
), by = alderman_key]

permits <- merge(permits, strictness_lookup, by = "alderman_key", all.x = TRUE)
permits <- permits[!is.na(strictness_index)]
cat("Rows after strictness merge:", nrow(permits), "\n")

# Build permit-level derived outcomes.
permits[, story_reduction_signal := as.integer(
  !is.na(stories_first_mention) &
    !is.na(stories_last_mention) &
    stories_last_mention < stories_first_mention
)]
permits[, explicit_unit_reduction_signal := as.integer(
  !is.na(units_delta_last) & units_delta_last < 0
)]
permits[, units_reduced_text := fifelse(!is.na(unit_change_text) & unit_change_text < 0, -unit_change_text, 0)]
permits[, units_reduced_explicit := fifelse(!is.na(units_delta_last) & units_delta_last < 0, -units_delta_last, 0)]

build_ward_month <- function(dt) {
  out <- dt[, .(
    alderman = first(alderman),
    strictness_index = first(strictness_index),
    n_permits = .N,
    n_unit_reduction = sum(unit_reduction_signal == 1, na.rm = TRUE),
    share_unit_reduction = mean(unit_reduction_signal == 1, na.rm = TRUE),
    share_unit_change = mean(unit_change_signal == 1, na.rm = TRUE),
    share_deconversion = mean(flag_deconversion == 1, na.rm = TRUE),
    share_explicit_unit_reduction = mean(explicit_unit_reduction_signal == 1, na.rm = TRUE),
    share_story_reduction = mean(story_reduction_signal == 1, na.rm = TRUE),
    units_reduced_text_permit = sum(units_reduced_text, na.rm = TRUE) / .N,
    units_reduced_explicit_permit = sum(units_reduced_explicit, na.rm = TRUE) / .N,
    n_with_explicit_unit_delta = sum(!is.na(units_delta_last)),
    n_with_story_mentions = sum(!is.na(stories_first_mention) & !is.na(stories_last_mention))
  ), by = .(ward, month)]
  out[, units_reduced_text_given_reduction := fifelse(n_unit_reduction > 0, (units_reduced_text_permit * n_permits) / n_unit_reduction, NA_real_)]
  out[, month_fe := format(as.Date(month), "%Y-%m")]
  out[!is.na(month_fe)]
}

cat("Aggregating to ward-month...\n")
ward_month <- build_ward_month(permits)

write_csv(ward_month, panel_output)
cat("Ward-month rows:", nrow(ward_month), "\n")

coverage <- data.table(
  outcome = c(
    "share_unit_reduction",
    "share_unit_change",
    "share_deconversion",
    "share_explicit_unit_reduction",
    "share_story_reduction",
    "units_reduced_text_permit",
    "units_reduced_explicit_permit",
    "units_reduced_text_given_reduction"
  ),
  mean_outcome = c(
    mean(ward_month$share_unit_reduction, na.rm = TRUE),
    mean(ward_month$share_unit_change, na.rm = TRUE),
    mean(ward_month$share_deconversion, na.rm = TRUE),
    mean(ward_month$share_explicit_unit_reduction, na.rm = TRUE),
    mean(ward_month$share_story_reduction, na.rm = TRUE),
    mean(ward_month$units_reduced_text_permit, na.rm = TRUE),
    mean(ward_month$units_reduced_explicit_permit, na.rm = TRUE),
    mean(ward_month$units_reduced_text_given_reduction, na.rm = TRUE)
  ),
  share_cells_positive = c(
    mean(ward_month$share_unit_reduction > 0, na.rm = TRUE),
    mean(ward_month$share_unit_change > 0, na.rm = TRUE),
    mean(ward_month$share_deconversion > 0, na.rm = TRUE),
    mean(ward_month$share_explicit_unit_reduction > 0, na.rm = TRUE),
    mean(ward_month$share_story_reduction > 0, na.rm = TRUE),
    mean(ward_month$units_reduced_text_permit > 0, na.rm = TRUE),
    mean(ward_month$units_reduced_explicit_permit > 0, na.rm = TRUE),
    mean(ward_month$units_reduced_text_given_reduction > 0, na.rm = TRUE)
  )
)
write_csv(coverage, coverage_output)

outcome_map <- c(
  share_unit_reduction = "Share with Unit Reduction Signal",
  share_deconversion = "Share with Deconversion Signal",
  share_unit_change = "Share with Any Unit Change Signal",
  units_reduced_text_permit = "Units Reduced per Permit (Text Signal)",
  units_reduced_text_given_reduction = "Units Reduced | Reduction (Text Signal)",
  units_reduced_explicit_permit = "Units Reduced per Permit (Explicit From-To)",
  share_story_reduction = "Share with Story Reduction Signal"
)

star_code <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

weighted_cor <- function(x, y, w) {
  keep <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
  x <- x[keep]
  y <- y[keep]
  w <- w[keep]
  if (length(x) < 2) return(NA_real_)
  w <- w / sum(w)
  mx <- sum(w * x)
  my <- sum(w * y)
  vx <- sum(w * (x - mx)^2)
  vy <- sum(w * (y - my)^2)
  if (vx <= 0 || vy <= 0) return(NA_real_)
  cov_xy <- sum(w * (x - mx) * (y - my))
  cov_xy / sqrt(vx * vy)
}

models <- list()
coef_rows <- lapply(names(outcome_map), function(outcome) {
  label <- outcome_map[[outcome]]
  dt <- ward_month[!is.na(get(outcome)) & !is.na(strictness_index) & !is.na(n_permits)]

  # Simple bivariate weighted regression (no FE) for presentation.
  m <- feols(
    as.formula(paste0(outcome, " ~ strictness_index")),
    data = dt,
    weights = ~n_permits,
    warn = FALSE
  )
  models[[label]] <<- m
  ctab <- coeftable(m)
  est <- ctab["strictness_index", "Estimate"]
  se <- ctab["strictness_index", "Std. Error"]
  pval <- ctab["strictness_index", "Pr(>|t|)"]
  data.table(
    outcome = label,
    estimate = est,
    std_error = se,
    p_value = pval,
    stars = star_code(pval),
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se,
    n_obs = nobs(m),
    cor_unweighted = suppressWarnings(cor(dt[[outcome]], dt$strictness_index, use = "complete.obs")),
    cor_weighted = weighted_cor(dt[[outcome]], dt$strictness_index, dt$n_permits),
    mean_outcome = mean(dt[[outcome]], na.rm = TRUE)
  )
})
coef_dt <- rbindlist(coef_rows, fill = TRUE)
write_csv(coef_dt, coef_output)

etable(
  models,
  keep = "%strictness_index",
  dict = c("strictness_index" = "Strictness Score"),
  fitstat = ~n + r2,
  style.tex = style.tex(
    "aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  ),
  depvar = FALSE,
  digits = 3,
  tex = TRUE,
  file = table_output,
  replace = TRUE
)

coef_dt[, outcome := factor(outcome, levels = rev(coef_dt$outcome))]
p <- ggplot(coef_dt, aes(x = cor_weighted, y = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2.5, color = "#1f4f7a") +
  geom_text(
    aes(label = sprintf("r_w = %.3f", cor_weighted)),
    nudge_x = 0.01,
    hjust = 0,
    size = 3.1
  ) +
  labs(
    title = "Ward-Month Correlation with Strictness Score",
    subtitle = "High-discretion permits only; weighted by ward-month permit count",
    x = "Weighted Correlation (r)",
    y = NULL
  ) +
  theme_minimal(base_size = 11)

ggsave(plot_output, plot = p, width = 11, height = 5.5, device = "pdf")

# Sample-cut diagnostics requested for presentation iteration.
sample_cuts <- list(
  all_high_discretion = permits,
  new_construction = permits[permit_type_clean == "PERMIT - NEW CONSTRUCTION"],
  renovations_only = permits[permit_type_clean == "PERMIT - RENOVATION/ALTERATION"],
  high_confidence_only = permits[from_to_pair_n > 0 | unit_change_confidence == "high"],
  standard_plan_review_only = permits[review_type_clean == "STANDARD PLAN REVIEW"],
  revisions_only = permits[flag_revision == 1]
)

cut_outcome_map <- c(
  share_unit_reduction = "Share with Unit Reduction Signal",
  share_unit_change = "Share with Any Unit Change Signal",
  share_deconversion = "Share with Deconversion Signal",
  units_reduced_text_permit = "Units Reduced per Permit (Text Signal)",
  units_reduced_text_given_reduction = "Units Reduced | Reduction (Text Signal)",
  units_reduced_explicit_permit = "Units Reduced per Permit (Explicit From-To)"
)

cut_rows <- list()
idx <- 1L
for (cut_name in names(sample_cuts)) {
  dt_cut <- sample_cuts[[cut_name]]
  wm_cut <- build_ward_month(dt_cut)
  n_permit_rows <- nrow(dt_cut)
  n_cells <- nrow(wm_cut)

  for (outcome in names(cut_outcome_map)) {
    dt <- wm_cut[!is.na(get(outcome)) & !is.na(strictness_index) & !is.na(n_permits)]
    if (nrow(dt) < 50) next
    if (stats::sd(dt[[outcome]], na.rm = TRUE) <= 0) next
    if (stats::sd(dt$strictness_index, na.rm = TRUE) <= 0) next
    fit <- feols(
      as.formula(paste0(outcome, " ~ strictness_index")),
      data = dt,
      weights = ~n_permits,
      warn = FALSE
    )
    ctab <- coeftable(fit)
    est <- ctab["strictness_index", "Estimate"]
    se <- ctab["strictness_index", "Std. Error"]
    pval <- ctab["strictness_index", "Pr(>|t|)"]

    cut_rows[[idx]] <- data.table(
      sample_cut = cut_name,
      outcome = cut_outcome_map[[outcome]],
      n_permit_rows = n_permit_rows,
      n_ward_month_cells = n_cells,
      estimate = est,
      std_error = se,
      p_value = pval,
      stars = star_code(pval),
      cor_unweighted = suppressWarnings(cor(dt[[outcome]], dt$strictness_index, use = "complete.obs")),
      cor_weighted = weighted_cor(dt[[outcome]], dt$strictness_index, dt$n_permits),
      mean_outcome = mean(dt[[outcome]], na.rm = TRUE),
      share_cells_positive = mean(dt[[outcome]] > 0, na.rm = TRUE)
    )
    idx <- idx + 1L
  }
}

cut_dt <- rbindlist(cut_rows, fill = TRUE)
write_csv(cut_dt, cuts_output)

# Permit-volume validation requested: all high-discretion vs new-construction counts.
new_counts <- permits[
  permit_type_clean == "PERMIT - NEW CONSTRUCTION",
  .(n_new_construction = .N),
  by = .(ward, month)
]

controls_dt <- data.table::fread(
  ward_month_controls_file,
  select = c("ward", "month", "pop_total")
)
setDT(controls_dt)
controls_dt[, ward := ward_key(ward)]
controls_dt[, month := as.yearmon(month)]
controls_dt <- unique(controls_dt[!is.na(ward) & !is.na(month), .(ward, month, pop_total = to_numeric(pop_total))])

permit_volume_panel <- ward_month[, .(
  ward, month, strictness_index,
  n_permits_all = n_permits
)]
permit_volume_panel <- merge(
  permit_volume_panel,
  controls_dt,
  by = c("ward", "month"),
  all.x = TRUE
)
permit_volume_panel <- merge(
  permit_volume_panel,
  new_counts,
  by = c("ward", "month"),
  all.x = TRUE
)
permit_volume_panel[is.na(n_new_construction), n_new_construction := 0]
permit_volume_panel[, permits_per_10k_pop_all := fifelse(!is.na(pop_total) & pop_total > 0, n_permits_all / (pop_total / 10000), NA_real_)]
permit_volume_panel[, permits_per_10k_pop_new := fifelse(!is.na(pop_total) & pop_total > 0, n_new_construction / (pop_total / 10000), NA_real_)]
permit_volume_panel[, share_new_construction := fifelse(n_permits_all > 0, n_new_construction / n_permits_all, NA_real_)]

count_specs <- data.table(
  count_var = c(
    "n_permits_all",
    "n_new_construction",
    "permits_per_10k_pop_all",
    "permits_per_10k_pop_new",
    "share_new_construction"
  ),
  label = c(
    "Permits per Ward-Month (All High-Discretion)",
    "Permits per Ward-Month (New Construction)",
    "Permits per 10,000 Residents (All High-Discretion)",
    "Permits per 10,000 Residents (New Construction)",
    "Share New Construction Among High-Discretion Permits"
  )
)

permit_volume_rows <- lapply(seq_len(nrow(count_specs)), function(i) {
  count_var <- count_specs$count_var[i]
  label <- count_specs$label[i]
  dt <- permit_volume_panel[!is.na(strictness_index) & !is.na(get(count_var))]
  if (nrow(dt) < 50 || stats::sd(dt[[count_var]], na.rm = TRUE) <= 0 || stats::sd(dt$strictness_index, na.rm = TRUE) <= 0) {
    return(NULL)
  }

  m_level <- feols(
    as.formula(paste0(count_var, " ~ strictness_index")),
    data = dt,
    weights = ~n_permits_all,
    warn = FALSE
  )
  ctab_level <- coeftable(m_level)

  dt_log <- dt[get(count_var) > 0]
  if (nrow(dt_log) >= 50 && stats::sd(log(dt_log[[count_var]]), na.rm = TRUE) > 0) {
    m_log <- feols(
      as.formula(paste0("log(", count_var, ") ~ strictness_index")),
      data = dt_log,
      warn = FALSE
    )
    ctab_log <- coeftable(m_log)
    log_est <- ctab_log["strictness_index", "Estimate"]
    log_se <- ctab_log["strictness_index", "Std. Error"]
    log_p <- ctab_log["strictness_index", "Pr(>|t|)"]
    log_n <- nobs(m_log)
  } else {
    log_est <- NA_real_
    log_se <- NA_real_
    log_p <- NA_real_
    log_n <- NA_integer_
  }

  data.table(
    outcome = label,
    n_ward_month_cells = nrow(dt),
    mean_count = mean(dt[[count_var]], na.rm = TRUE),
    share_zero = mean(dt[[count_var]] == 0, na.rm = TRUE),
    cor_unweighted = suppressWarnings(cor(dt[[count_var]], dt$strictness_index, use = "complete.obs")),
    cor_weighted = weighted_cor(dt[[count_var]], dt$strictness_index, dt$n_permits_all),
    level_estimate = ctab_level["strictness_index", "Estimate"],
    level_std_error = ctab_level["strictness_index", "Std. Error"],
    level_p_value = ctab_level["strictness_index", "Pr(>|t|)"],
    level_stars = star_code(ctab_level["strictness_index", "Pr(>|t|)"]),
    log_estimate = log_est,
    log_std_error = log_se,
    log_p_value = log_p,
    log_stars = star_code(log_p),
    n_log_sample = log_n
  )
})

permit_volume_dt <- rbindlist(permit_volume_rows, fill = TRUE)
write_csv(permit_volume_dt, permit_volume_output)

cat("Wrote:\n")
cat(" -", panel_output, "\n")
cat(" -", coverage_output, "\n")
cat(" -", coef_output, "\n")
cat(" -", table_output, "\n")
cat(" -", plot_output, "\n")
cat(" -", cuts_output, "\n")
cat(" -", permit_volume_output, "\n")
