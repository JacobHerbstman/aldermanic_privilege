source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 500
# window <- "pre_2023"
# output_tex <- NA
# output_csv <- NA
# Rscript listing_units_ppml_robustness.R "../input/rent_with_ward_distances.parquet" 500 "pre_2023" NA NA
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 5) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("output_tex") || !exists("output_csv")) {
    stop("FATAL: Script requires 5 args: <input> <bw_ft> <window> <output_tex> <output_csv>", call. = FALSE)
  }
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

apply_window <- function(df, w) {
  if (w == "full") return(df)
  if (w == "pre_2021") return(df %>% filter(year <= 2020))
  if (w == "pre_2023") return(df %>% filter(year <= 2022))
  if (w == "pre_covid") return(df %>% filter(year <= 2019))
  df
}

message(sprintf("=== Listing Units PPML Robustness | bw=%d | window=%s ===", bw_ft, window))

base <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    available_date = as.Date(available_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    right = as.integer(signed_dist >= 0)
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
    !is.na(latitude), !is.na(longitude)
  ) %>%
  apply_window(window) %>%
  mutate(
    strict_more = pmax(strictness_own, strictness_neighbor),
    strict_less = pmin(strictness_own, strictness_neighbor),
    loc_key = paste(round(latitude, 5), round(longitude, 5)),
    unit_proxy_key = paste(
      round(latitude, 5),
      round(longitude, 5),
      coalesce(as.character(beds), "NA"),
      coalesce(as.character(baths), "NA"),
      coalesce(as.character(sqft), "NA"),
      sep = "_"
    )
  )

if (!"id" %in% names(base)) {
  stop("Expected listing id column `id` not found.", call. = FALSE)
}

avail_filter <- function(df, mode) {
  if (mode == "all") return(df)
  if (mode == "known_available_date") {
    return(df %>% filter(!is.na(available_date)))
  }
  if (mode == "available_within_30d") {
    return(df %>% filter(!is.na(available_date), available_date <= file_date + 30))
  }
  stop(sprintf("Unknown availability filter: %s", mode), call. = FALSE)
}

unit_key_value <- function(df, unit_def) {
  if (unit_def == "id") return(as.character(df$id))
  if (unit_def == "loc_key") return(df$loc_key)
  if (unit_def == "unit_proxy") return(df$unit_proxy_key)
  stop(sprintf("Unknown unit_def: %s", unit_def), call. = FALSE)
}

run_spec <- function(spec_row) {
  label <- as.character(spec_row$label[[1]])
  unit_def <- as.character(spec_row$unit_def[[1]])
  sample_filter <- as.character(spec_row$sample_filter[[1]])
  bw_ft <- as.integer(spec_row$bw_ft[[1]])
  min_strictness_diff_pctile <- as.integer(spec_row$min_strictness_diff_pctile[[1]])
  avail_mode <- as.character(spec_row$avail_filter[[1]])
  add_covars <- as.logical(spec_row$add_covars[[1]])
  two_sided_only <- as.logical(spec_row$two_sided_only[[1]])

  message(sprintf("  Running: %s", label))

  dat <- base %>%
    filter(abs(signed_dist) <= bw_ft) %>%
    avail_filter(avail_mode)

  if (sample_filter == "multifamily_only") {
    dat <- dat %>% filter(building_type_clean == "multi_family")
  }

  if (min_strictness_diff_pctile > 0) {
    pair_diffs <- dat %>%
      group_by(ward_pair) %>%
      summarise(diff = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE), .groups = "drop")
    cutoff <- quantile(pair_diffs$diff, min_strictness_diff_pctile / 100, na.rm = TRUE)
    keep_pairs <- pair_diffs %>% filter(diff >= cutoff) %>% pull(ward_pair)
    dat <- dat %>% filter(ward_pair %in% keep_pairs)
  }

  if (nrow(dat) == 0) {
    return(tibble(spec = label, estimate = NA_real_, std_error = NA_real_, p_value = NA_real_))
  }

  dat <- dat %>%
    mutate(unit_key = unit_key_value(., unit_def)) %>%
    filter(!is.na(unit_key), unit_key != "")

  pair_month_map <- dat %>%
    group_by(ward_pair, year_month) %>%
    summarise(
      strict_more = max(strict_more, na.rm = TRUE),
      strict_less = min(strict_less, na.rm = TRUE),
      .groups = "drop"
    )

  side_template <- bind_rows(
    pair_month_map %>% transmute(ward_pair, year_month, right = 0L, strictness_own = strict_less),
    pair_month_map %>% transmute(ward_pair, year_month, right = 1L, strictness_own = strict_more)
  )

  side_counts <- dat %>%
    distinct(ward_pair, right, year_month, unit_key) %>%
    count(ward_pair, right, year_month, name = "n_units")

  panel <- side_template %>%
    left_join(side_counts, by = c("ward_pair", "right", "year_month")) %>%
    mutate(n_units = as.integer(coalesce(n_units, 0L)))

  if (isTRUE(two_sided_only)) {
    two_sided_pm <- side_counts %>%
      count(ward_pair, year_month, name = "n_sides_obs") %>%
      filter(n_sides_obs == 2)
    panel <- panel %>%
      semi_join(two_sided_pm, by = c("ward_pair", "year_month"))
  }

  if (nrow(panel) == 0 || n_distinct(panel$ward_pair) < 2) {
    return(tibble(spec = label, estimate = NA_real_, std_error = NA_real_, p_value = NA_real_))
  }

  strictness_sd <- sd(panel$strictness_own, na.rm = TRUE)
  if (!is.finite(strictness_sd) || strictness_sd <= 0) {
    return(tibble(spec = label, estimate = NA_real_, std_error = NA_real_, p_value = NA_real_))
  }

  panel <- panel %>% mutate(strictness_std = strictness_own / strictness_sd)

  rhs <- "strictness_std"
  if (isTRUE(add_covars)) {
    cov_side <- dat %>%
      distinct(ward_pair, right, year_month, unit_key, .keep_all = TRUE) %>%
      group_by(ward_pair, right, year_month) %>%
      summarise(
        mean_log_sqft = mean(if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_), na.rm = TRUE),
        mean_beds = mean(beds, na.rm = TRUE),
        mean_baths = mean(baths, na.rm = TRUE),
        share_multifamily = mean(building_type_clean == "multi_family", na.rm = TRUE),
        share_laundry = mean(laundry == 1, na.rm = TRUE),
        share_gym = mean(gym == 1, na.rm = TRUE),
        .groups = "drop"
      )

    panel <- panel %>% left_join(cov_side, by = c("ward_pair", "right", "year_month"))

    covars <- c(
      "mean_log_sqft", "mean_beds", "mean_baths",
      "share_multifamily", "share_laundry", "share_gym"
    )
    for (v in covars) {
      miss_v <- paste0("miss_", v)
      panel[[miss_v]] <- as.integer(is.na(panel[[v]]))
      panel[[v]][is.na(panel[[v]])] <- 0
    }
    rhs <- paste(
      c("strictness_std", covars, paste0("miss_", covars)),
      collapse = " + "
    )
  }

  fml <- as.formula(paste0("n_units ~ ", rhs, " | ward_pair^year_month"))
  m <- tryCatch(
    fepois(fml, data = panel, cluster = ~ward_pair),
    error = function(e) NULL
  )
  if (is.null(m)) {
    return(tibble(
      spec = label,
      unit_def = unit_def,
      sample_filter = sample_filter,
      bw_ft = bw_ft,
      min_strictness_diff_pctile = min_strictness_diff_pctile,
      avail_filter = avail_mode,
      add_covars = add_covars,
      two_sided_only = two_sided_only,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      implied_pct_change = NA_real_,
      n_obs = NA_integer_,
      ward_pairs = n_distinct(panel$ward_pair),
      pair_month_cells = nrow(pair_month_map),
      share_single_sided_pair_month = NA_real_,
      share_zero_cells = mean(panel$n_units == 0),
      mean_units_per_cell = mean(panel$n_units, na.rm = TRUE)
    ))
  }

  ct <- coeftable(m)
  if (!"strictness_std" %in% rownames(ct)) {
    return(tibble(
      spec = label,
      unit_def = unit_def,
      sample_filter = sample_filter,
      bw_ft = bw_ft,
      min_strictness_diff_pctile = min_strictness_diff_pctile,
      avail_filter = avail_mode,
      add_covars = add_covars,
      two_sided_only = two_sided_only,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      implied_pct_change = NA_real_,
      n_obs = nobs(m),
      ward_pairs = n_distinct(panel$ward_pair),
      pair_month_cells = nrow(pair_month_map),
      share_single_sided_pair_month = NA_real_,
      share_zero_cells = mean(panel$n_units == 0),
      mean_units_per_cell = mean(panel$n_units, na.rm = TRUE)
    ))
  }

  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)
  p_name <- if (length(p_col) == 0) NA_character_ else p_col[1]

  pair_month_obs <- side_counts %>%
    count(ward_pair, year_month, name = "n_sides_obs")

  pair_month_diag <- pair_month_map %>%
    left_join(pair_month_obs, by = c("ward_pair", "year_month")) %>%
    mutate(n_sides_obs = coalesce(n_sides_obs, 0L))

  tibble(
    spec = label,
    unit_def = unit_def,
    sample_filter = sample_filter,
    bw_ft = bw_ft,
    min_strictness_diff_pctile = min_strictness_diff_pctile,
    avail_filter = avail_mode,
    add_covars = add_covars,
    two_sided_only = two_sided_only,
    estimate = ct["strictness_std", "Estimate"],
    std_error = ct["strictness_std", "Std. Error"],
    p_value = if (is.na(p_name)) NA_real_ else ct["strictness_std", p_name],
    implied_pct_change = 100 * (exp(ct["strictness_std", "Estimate"]) - 1),
    n_obs = nobs(m),
    ward_pairs = n_distinct(panel$ward_pair),
    pair_month_cells = nrow(pair_month_map),
    share_single_sided_pair_month = mean(pair_month_diag$n_sides_obs == 1),
    share_zero_cells = mean(panel$n_units == 0),
    mean_units_per_cell = mean(panel$n_units, na.rm = TRUE)
  )
}

specs <- tribble(
  ~label, ~unit_def, ~sample_filter, ~bw_ft, ~min_strictness_diff_pctile, ~avail_filter, ~add_covars, ~two_sided_only,
  "Baseline (unit proxy)", "unit_proxy", "all", bw_ft, 0L, "all", FALSE, FALSE,
  "Add composition controls", "unit_proxy", "all", bw_ft, 0L, "all", TRUE, FALSE,
  "Alt unit key: Listing ID", "id", "all", bw_ft, 0L, "all", FALSE, FALSE,
  "Alt unit key: Location (5dp)", "loc_key", "all", bw_ft, 0L, "all", FALSE, FALSE,
  "Two-sided pair-months only", "unit_proxy", "all", bw_ft, 0L, "all", FALSE, TRUE,
  "Sample: Multifamily only", "unit_proxy", "multifamily_only", bw_ft, 0L, "all", FALSE, FALSE,
  "Sample: BW 250ft", "unit_proxy", "all", 250L, 0L, "all", FALSE, FALSE,
  "Sample: Top 50% uncertainty-gap pairs", "unit_proxy", "all", bw_ft, 50L, "all", FALSE, FALSE,
  "Availability proxy: known available_date", "unit_proxy", "all", bw_ft, 0L, "known_available_date", FALSE, FALSE,
  "Availability proxy: available <= 30d", "unit_proxy", "all", bw_ft, 0L, "available_within_30d", FALSE, FALSE
)

results <- bind_rows(lapply(seq_len(nrow(specs)), function(i) run_spec(specs[i, ])))
write_csv(results, output_csv)

tex_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "  \\toprule",
  "  Specification & Coef. (1 SD) & SE & Implied \\% & N \\\\",
  "  \\midrule"
)

for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  coef_cell <- if (is.na(r$estimate)) "NA" else sprintf("%.4f%s", r$estimate, stars(r$p_value))
  se_cell <- if (is.na(r$std_error)) "NA" else sprintf("(%.4f)", r$std_error)
  imp_cell <- if (is.na(r$implied_pct_change)) "NA" else sprintf("%.2f\\%%", r$implied_pct_change)
  n_cell <- if (is.na(r$n_obs)) "NA" else format(r$n_obs, big.mark = ",")
  tex_lines <- c(
    tex_lines,
    sprintf("  %s & %s & %s & %s & %s \\\\", r$spec, coef_cell, se_cell, imp_cell, n_cell)
  )
}

tex_lines <- c(
  tex_lines,
  "  \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(tex_lines, output_tex)

message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))