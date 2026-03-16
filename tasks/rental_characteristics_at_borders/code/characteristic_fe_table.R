source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# output_tex <- NA
# output_csv <- NA
# Rscript characteristic_fe_table.R "../input/rent_with_ward_distances.parquet" 1000 "pre_2021" NA NA ward_pair
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 6) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  cluster_level <- tolower(cli_args[6])
} else if (length(cli_args) >= 5) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("output_tex") || !exists("output_csv") || !exists("cluster_level")) {
    stop("FATAL: Script requires 5 args: <input> <bw_ft> <window> <output_tex> <output_csv> [<cluster_level>]", call. = FALSE)
  }
}

if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("cluster_level must be one of: segment, ward_pair", call. = FALSE)
}

cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair
cluster_label <- if (cluster_level == "segment") "Segment" else "Ward Pair"
treatment_var <- "strictness_std"
treatment_label <- "Stringency Index"

apply_window <- function(df, w) {
  if (w == "full") return(df)
  if (w == "pre_2021") return(df %>% filter(year <= 2020))
  if (w == "pre_2023") return(df %>% filter(year <= 2022))
  if (w == "pre_covid") return(df %>% filter(year <= 2019))
  df
}

window_label <- c(
  full = "All years",
  pre_covid = "Pre-COVID (2014-2019)",
  pre_2021 = "Through 2020",
  pre_2023 = "Through 2022 (2014-2022)"
)

message(sprintf(
  "=== Characteristic FE Table | bw=%d | window=%s | cluster=%s | treatment=stringency ===",
  bw_ft, window, cluster_level
))

# ── Load and filter ──
dat <- read_parquet(input) %>%
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
  ) %>%
  apply_window(window)

strictness_sd <- sd(dat$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero or invalid SD in the filtered sample.", call. = FALSE)
}
dat <- dat %>% mutate(strictness_std = strictness_own / strictness_sd)

# ── Outcomes to estimate ──
outcomes <- list(
  list(name = "sqft",       var = "sqft",           label = "Sqft",            log = FALSE),
  list(name = "beds",       var = "beds",           label = "Beds",            log = FALSE),
  list(name = "baths",      var = "baths",          label = "Baths",           log = FALSE),
  list(name = "multifamily",var = "is_multifamily", label = "Multi-Family",    log = FALSE),
  list(name = "laundry",    var = "laundry",        label = "Laundry",         log = FALSE),
  list(name = "gym",        var = "gym",            label = "Gym",             log = FALSE)
)

results <- list()
for (oc in outcomes) {
  d <- dat
  if (oc$log) {
    d <- d %>% mutate(Y = if_else(!is.na(.data[[oc$var]]) & .data[[oc$var]] > 0,
                                   log(.data[[oc$var]]), NA_real_))
  } else {
  d <- d %>% mutate(Y = as.numeric(.data[[oc$var]]))
  }
  d <- d %>% filter(!is.na(Y))
  if (nrow(d) < 10 || n_distinct(d$segment_id) < 2) {
    message(sprintf("  Skipping %s: too few obs (%d)", oc$name, nrow(d)))
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
    outcome = oc$name,
    label = oc$label,
    estimate = ct[treatment_var, "Estimate"],
    std_error = ct[treatment_var, "Std. Error"],
    p_value = ct[treatment_var, p_col[1]],
    n_obs = nobs(m),
    dep_var_mean = mean(d$Y, na.rm = TRUE),
    segments = n_distinct(d$segment_id[d$segment_id %in% names(which(table(d$segment_id) > 0))]),
    bandwidth_ft = bw_ft,
    window = window,
    cluster_level = cluster_level
  )
  message(sprintf("  %s: b=%.4f (SE %.4f, p=%.3f), N=%s",
                  oc$label, ct[treatment_var, "Estimate"], ct[treatment_var, "Std. Error"],
                  ct[treatment_var, p_col[1]], format(nobs(m), big.mark = ",")))
}

coef_tbl <- bind_rows(results)
write_csv(coef_tbl, output_csv)

# ── LaTeX table ──
stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

ncol <- nrow(coef_tbl)
header <- paste0("\\begingroup\n\\centering\n\\begin{tabular}{l", paste(rep("c", ncol), collapse = ""), "}\n   \\toprule\n")
col_headers <- paste0("   ", paste(sprintf("& %s", coef_tbl$label), collapse = " "), "\\\\  \n")
midrule <- "   \\midrule \n"

coef_row <- paste0("   ", treatment_label, " ",
  paste(sapply(seq_len(ncol), function(i) {
    sprintf("& %.4f%s", coef_tbl$estimate[i], stars(coef_tbl$p_value[i]))
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
writeLines(tex, output_tex)

message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))
