source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# Rscript amenity_characteristic_fe_table.R ../output/rent_with_ward_distances_amenities.parquet 500 pre_2023 ../output/amenity_char_fe_table_bw500_pre_2023.tex ../output/amenity_char_fe_table_bw500_pre_2023.csv
# =======================================================================================

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 5) {
  input <- cli_args[1]
  bw_arg <- cli_args[2]
  window <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
} else {
  if (!exists("input") || !exists("bw_arg") || !exists("window") || !exists("output_tex") || !exists("output_csv")) {
    stop("FATAL: Script requires 5 args: <input> <bw_ft> <window> <output_tex> <output_csv>", call. = FALSE)
  }
}

parse_bw_ft <- function(x) {
  if (length(x) != 1) {
    stop("bw_ft must be a single value.", call. = FALSE)
  }
  if (is.character(x) && str_to_lower(x) %in% c("all", "full", "none", "inf", "infinity")) {
    return(Inf)
  }
  out <- suppressWarnings(as.numeric(x))
  if (!is.finite(out) || out <= 0) {
    stop("bw_ft must be a positive number or one of: all, full, none, inf.", call. = FALSE)
  }
  out
}

apply_window <- function(df, w) {
  if (w == "full") return(df)
  if (w == "pre_2021") return(df %>% filter(year <= 2020))
  if (w == "pre_2023") return(df %>% filter(year <= 2022))
  if (w == "pre_covid") return(df %>% filter(year <= 2019))
  df
}

bw_ft <- parse_bw_ft(bw_arg)
bw_label <- if (is.finite(bw_ft)) as.character(as.integer(round(bw_ft))) else "all"

message(sprintf("=== Amenity Characteristic FE Table | bw=%s | window=%s ===", bw_label, window))

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
  ) %>%
  apply_window(window)

if (is.finite(bw_ft)) {
  dat <- dat %>% filter(abs(signed_dist) <= bw_ft)
}

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

  m <- feols(Y ~ right | segment_id^year_month, data = d, cluster = ~segment_id)
  ct <- coeftable(m)

  results[[length(results) + 1]] <- tibble(
    outcome = oc$name,
    label = oc$label,
    estimate = ct["right", "Estimate"],
    std_error = ct["right", "Std. Error"],
    p_value = ct["right", "Pr(>|t|)"],
    n_obs = nobs(m),
    dep_var_mean = mean(d$Y, na.rm = TRUE),
    segments = n_distinct(d$segment_id),
    bandwidth_ft = bw_ft,
    bandwidth_label = bw_label,
    window = window
  )
}

coef_tbl <- bind_rows(results)
write_csv(coef_tbl, output_csv)

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
coef_row <- paste0("   More Stringent Side ",
  paste(vapply(seq_len(ncol), function(i) sprintf("& %.2f%s", coef_tbl$estimate[i], stars(coef_tbl$p_value[i])), character(1)), collapse = " "),
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
cluster_row <- paste0("   Clustered by Segment ",
  paste(rep("& $\\checkmark$", ncol), collapse = " "),
  "\\\\   \n")
footer <- "   \\bottomrule\n\\end{tabular}\n\\par\\endgroup\n"

writeLines(
  paste0(header, col_headers, midrule, coef_row, se_row, blank, obs_row, mean_row, fe_row, cluster_row, footer),
  output_tex
)

message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))
