source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../output/rent_with_ward_distances_amenities.parquet"
# bw_arg <- 500
# window <- "pre_2023"
# output_tex <- "../output/amenity_char_fe_table_bw500_pre_2023.tex"
# output_csv <- "../output/amenity_char_fe_table_bw500_pre_2023.csv"
# cluster_level <- "ward_pair"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_arg, window, output_tex, output_csv, cluster_level)
}

if (length(cli_args) >= 6) {
  input <- cli_args[1]
  bw_arg <- cli_args[2]
  window <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  cluster_level <- tolower(cli_args[6])
} else if (length(cli_args) >= 5) {
  input <- cli_args[1]
  bw_arg <- cli_args[2]
  window <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
} else {
  stop("FATAL: Script requires 5 args: <input> <bw_ft> <window> <output_tex> <output_csv> [<cluster_level>]", call. = FALSE)
}

if (length(bw_arg) != 1) {
  stop("bw_ft must be a single value.", call. = FALSE)
}
if (is.character(bw_arg) && str_to_lower(bw_arg) %in% c("all", "full", "none", "inf", "infinity")) {
  bw_ft <- Inf
} else {
  bw_ft <- suppressWarnings(as.numeric(bw_arg))
  if (!is.finite(bw_ft) || bw_ft <= 0) {
    stop("bw_ft must be a positive number or one of: all, full, none, inf.", call. = FALSE)
  }
}
bw_label <- if (is.finite(bw_ft)) as.character(as.integer(round(bw_ft))) else "all"

if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("cluster_level must be one of: segment, ward_pair", call. = FALSE)
}

message(sprintf(
  "=== Amenity Characteristic FE Table | bw=%s | window=%s | cluster=%s | treatment=stringency ===",
  bw_label, window, cluster_level
))

dat <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    right = as.integer(signed_dist >= 0)
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(segment_id), segment_id != "",
    !is.na(signed_dist), !is.na(strictness_own)
  )

if (window == "pre_2021") {
  dat <- dat %>% filter(year <= 2020)
} else if (window == "pre_2023") {
  dat <- dat %>% filter(year <= 2022)
} else if (window == "pre_covid") {
  dat <- dat %>% filter(year <= 2019)
}

if (is.finite(bw_ft)) {
  dat <- dat %>% filter(abs(signed_dist) <= bw_ft)
}

cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair
cluster_label <- if (cluster_level == "segment") "Segment" else "Ward Pair"
treatment_var <- "strictness_std"
treatment_label <- "Stringency Index"

strictness_sd <- sd(dat$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero or invalid SD in the filtered sample.", call. = FALSE)
}
dat <- dat %>% mutate(strictness_std = strictness_own / strictness_sd)

outcomes <- list(
  list(name = "nearest_school_dist_ft", label = "Dist. to School (ft)"),
  list(name = "nearest_park_dist_ft", label = "Dist. to Park (ft)"),
  list(name = "nearest_major_road_dist_ft", label = "Dist. to Major Road (ft)"),
  list(name = "lake_michigan_dist_ft", label = "Dist. to Lake Michigan (ft)")
)

results <- list()
for (oc in outcomes) {
  d <- dat %>%
    mutate(Y = as.numeric(.data[[oc$name]])) %>%
    filter(!is.na(Y))

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
    segments = n_distinct(d$segment_id),
    bandwidth_ft = bw_ft,
    bandwidth_label = bw_label,
    window = window,
    cluster_level = cluster_level
  )
}

coef_tbl <- bind_rows(results)
write_csv(coef_tbl, output_csv)

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
  paste(vapply(seq_len(ncol), function(i) sprintf("& %.2f%s", coef_tbl$estimate[i], coef_stars[i]), character(1)), collapse = " "),
  "\\\\   \n")
se_row <- paste0("   ",
  paste(vapply(seq_len(ncol), function(i) sprintf("& (%.2f)", coef_tbl$std_error[i]), character(1)), collapse = " "),
  "\\\\   \n")
blank <- "    \\\\\n"
obs_row <- paste0("   Observations ",
  paste(vapply(coef_tbl$n_obs, function(n) sprintf("& %s", format(n, big.mark = ",")), character(1)), collapse = " "),
  "\\\\  \n")
mean_row <- paste0("   Dep. Var. Mean ",
  paste(vapply(coef_tbl$dep_var_mean, function(m) sprintf("& %.1f", m), character(1)), collapse = " "),
  "\\\\  \n")
fe_row <- paste0("   Segment $\\times$ Year-Month FE ",
  paste(rep("& $\\checkmark$", ncol), collapse = " "),
  "\\\\   \n")
cluster_row <- paste0("   Clustered by ", cluster_label, " ",
  paste(rep("& $\\checkmark$", ncol), collapse = " "),
  "\\\\   \n")
footer <- "   \\bottomrule\n\\end{tabular}\n\\par\\endgroup\n"

writeLines(
  paste0(header, col_headers, midrule, coef_row, se_row, blank, obs_row, mean_row, fe_row, cluster_row, footer),
  output_tex
)

message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))
