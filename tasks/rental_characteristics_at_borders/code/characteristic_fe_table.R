# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# bw_ft <- 500
# window <- "pre_2023"
# cluster_level <- "ward_pair"

source("../../setup_environment/code/packages.R", local = new.env(parent = globalenv()))

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bw_ft, window, cluster_level)
}

if (length(cli_args) == 3) {
  bw_ft <- suppressWarnings(as.integer(cli_args[1]))
  window <- cli_args[2]
  cluster_level <- tolower(cli_args[3])
} else {
  stop("FATAL: Script requires 3 args: <bw_ft> <window> <cluster_level>", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive integer.", call. = FALSE)
}
if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("cluster_level must be one of: segment, ward_pair", call. = FALSE)
}

output_stem <- if (window == "pre_2023") {
  sprintf("../output/char_fe_table_bw%d_clust_%s", bw_ft, cluster_level)
} else {
  sprintf("../output/char_fe_table_bw%d_%s_clust_%s", bw_ft, window, cluster_level)
}

cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair
cluster_label <- if (cluster_level == "segment") "Segment" else "Ward Pair"
treatment_var <- "strictness_std"
treatment_label <- "Stringency Index"

message(sprintf(
  "=== Characteristic FE Table | bw=%d | window=%s | cluster=%s | treatment=stringency ===",
  bw_ft, window, cluster_level
))

dat <- read_parquet("../input/rent_with_ward_distances.parquet") %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    is_multifamily = as.integer(building_type_clean == "multi_family"),
    right = as.integer(signed_dist >= 0)
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(segment_id), segment_id != "",
    !is.na(signed_dist), !is.na(strictness_own),
    abs(signed_dist) <= bw_ft
  )

if (window == "pre_2021") {
  dat <- dat %>% filter(year <= 2020)
} else if (window == "pre_2023") {
  dat <- dat %>% filter(year <= 2022)
} else if (window == "pre_covid") {
  dat <- dat %>% filter(year <= 2019)
}

strictness_sd <- sd(dat$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero or invalid SD in the filtered sample.", call. = FALSE)
}
dat <- dat %>% mutate(strictness_std = strictness_own / strictness_sd)

# ── Outcomes to estimate ──
outcomes <- list(
  list(var = "sqft",           label = "Sqft"),
  list(var = "beds",           label = "Beds"),
  list(var = "baths",          label = "Baths"),
  list(var = "is_multifamily", label = "Multi-Family"),
  list(var = "laundry",        label = "Laundry"),
  list(var = "gym",            label = "Gym")
)

results <- list()
for (oc in outcomes) {
  d <- dat %>%
    mutate(Y = as.numeric(.data[[oc$var]])) %>%
    filter(!is.na(Y))
  if (nrow(d) < 10 || n_distinct(d$segment_id) < 2) {
    message(sprintf("  Skipping %s: too few obs (%d)", oc$var, nrow(d)))
    next
  }
  rhs_formula <- as.formula(paste0("Y ~ ", treatment_var, " | segment_id^year_month"))
  m <- feols(rhs_formula, data = d, cluster = cluster_formula)
  ct <- coeftable(m)
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)
  if (length(p_col) == 0) {
    stop("Could not find p-value column in coeftable output.", call. = FALSE)
  }
  results[[length(results) + 1]] <- tibble(
    label = oc$label,
    estimate = ct[treatment_var, "Estimate"],
    std_error = ct[treatment_var, "Std. Error"],
    p_value = ct[treatment_var, p_col[1]],
    n_obs = nobs(m),
    dep_var_mean = mean(d$Y, na.rm = TRUE)
  )
  message(sprintf("  %s: b=%.4f (SE %.4f, p=%.3f), N=%s",
                  oc$label, ct[treatment_var, "Estimate"], ct[treatment_var, "Std. Error"],
                  ct[treatment_var, p_col[1]], format(nobs(m), big.mark = ",")))
}

coef_tbl <- bind_rows(results)

ncol <- nrow(coef_tbl)
coef_stars <- vapply(coef_tbl$p_value, function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}, character(1))

header <- paste0("\\begingroup\n\\centering\n\\begin{tabular}{l", paste(rep("c", ncol), collapse = ""), "}\n   \\toprule\n")
col_headers <- paste0("   ", paste(sprintf("& %s", coef_tbl$label), collapse = " "), "\\\\  \n")
midrule <- "   \\midrule \n"

coef_row <- paste0("   ", treatment_label, " ",
  paste(sapply(seq_len(ncol), function(i) {
    sprintf("& %.4f%s", coef_tbl$estimate[i], coef_stars[i])
  }), collapse = " "), "\\\\   \n")

se_row <- paste0("   ",
  paste(sapply(seq_len(ncol), function(i) {
    sprintf("& (%.4f)", coef_tbl$std_error[i])
  }), collapse = " "), "\\\\   \n")

blank <- "    \\\\\n"
obs_row <- paste0("   Observations ",
  paste(sapply(coef_tbl$n_obs, function(n) sprintf("& %s", format(n, big.mark = ","))), collapse = " "), "\\\\  \n")
mean_row <- paste0("   Dep. Var. Mean ",
  paste(sapply(coef_tbl$dep_var_mean, function(m) sprintf("& %.2f", m)), collapse = " "), "\\\\  \n")
fe_row <- paste0("   Segment $\\times$ Year-Month FE ",
  paste(rep("& $\\checkmark$", ncol), collapse = " "), "\\\\   \n")
cluster_row <- paste0("   Clustered by ", cluster_label, " ",
  paste(rep("& $\\checkmark$", ncol), collapse = " "), "\\\\   \n")

footer <- "   \\bottomrule\n\\end{tabular}\n\\par\\endgroup\n"

tex <- paste0(header, col_headers, midrule, coef_row, se_row, blank,
              obs_row, mean_row, fe_row, cluster_row, footer)
writeLines(tex, paste0(output_stem, ".tex"))

message(sprintf("Saved: %s.tex", output_stem))
