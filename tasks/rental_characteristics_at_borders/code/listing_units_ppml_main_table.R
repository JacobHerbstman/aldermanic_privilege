source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 500
# window <- "pre_2023"
# sample_filter <- "all"
# unit_def <- "unit_proxy"
# output_tex <- NA
# output_csv <- NA
# Rscript listing_units_ppml_main_table.R "../input/rent_with_ward_distances.parquet" 500 "pre_2023" "all" "unit_proxy" NA NA
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 7) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  unit_def <- cli_args[5]
  output_tex <- cli_args[6]
  output_csv <- cli_args[7]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") || !exists("unit_def") || !exists("output_tex") || !exists("output_csv")) {
    stop("FATAL: Script requires 7 args: <input> <bw_ft> <window> <sample_filter> <unit_def> <output_tex> <output_csv>", call. = FALSE)
  }
}

if (!unit_def %in% c("id", "loc_key", "unit_proxy")) {
  stop("--unit_def must be one of: id, loc_key, unit_proxy", call. = FALSE)
}

prune_sample_raw <- tolower(Sys.getenv("PRUNE_SAMPLE", "all"))
if (prune_sample_raw %in% c("all", "false", "f", "0", "no", "off")) {
  prune_sample <- "all"
} else if (prune_sample_raw %in% c("pruned", "true", "t", "1", "yes", "on")) {
  prune_sample <- "pruned"
} else {
  stop("PRUNE_SAMPLE must map to one of: all/false/0 or pruned/true/1", call. = FALSE)
}
confound_flags_path <- Sys.getenv("CONFOUND_FLAGS_PATH", "../input/confounded_pair_era_flags.csv")

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  out <- rep(NA_character_, length(x))
  if (!any(ok)) return(out)
  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) return(NA_character_)
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
  out
}

era_from_year <- function(y) {
  y <- as.integer(y)
  ifelse(
    y < 2003L, "1998_2002",
    ifelse(y < 2015L, "2003_2014", ifelse(y < 2023L, "2015_2023", "post_2023"))
  )
}

apply_window <- function(df, w) {
  if (w == "full") return(df)
  if (w == "pre_2021") return(df %>% filter(year <= 2020))
  if (w == "pre_2023") return(df %>% filter(year <= 2022))
  if (w == "pre_covid") return(df %>% filter(year <= 2019))
  df
}

message(sprintf("=== Listing Units PPML Main Table | bw=%d | window=%s | sample=%s | unit_def=%s ===",
                bw_ft, window, sample_filter, unit_def))
message(sprintf("Pruning spec: %s", prune_sample))

dat <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    right = as.integer(signed_dist >= 0),
    listing_id = as.character(id),
    loc_key = paste(round(latitude, 5), round(longitude, 5), sep = "_"),
    unit_proxy_key = paste(
      round(latitude, 5),
      round(longitude, 5),
      coalesce(as.character(beds), "NA"),
      coalesce(as.character(baths), "NA"),
      coalesce(as.character(sqft), "NA"),
      sep = "_"
    ),
    strict_more = pmax(strictness_own, strictness_neighbor),
    strict_less = pmin(strictness_own, strictness_neighbor)
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(segment_id), segment_id != "",
    !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
    !is.na(latitude), !is.na(longitude),
    abs(signed_dist) <= bw_ft
  ) %>%
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

if (prune_sample == "pruned") {
  if (!file.exists(confound_flags_path)) {
    stop(sprintf("Missing confound flags file for pruned run: %s", confound_flags_path), call. = FALSE)
  }

  conf_flags <- read_csv(
    confound_flags_path,
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "drop_confound")
  ) %>%
    transmute(
      pair_dash = normalize_pair_dash(ward_pair_id_dash),
      era = as.character(era),
      keep_pair_era = !as.logical(drop_confound)
    ) %>%
    distinct()

  if (anyNA(conf_flags$pair_dash) || anyNA(conf_flags$era)) {
    stop("Confound flags have invalid pair/era keys.", call. = FALSE)
  }
  if (anyDuplicated(conf_flags[, c("pair_dash", "era")]) > 0) {
    stop("Confound flags contain duplicate pair-era keys.", call. = FALSE)
  }

  dat <- dat %>%
    mutate(
      pair_dash = normalize_pair_dash(ward_pair),
      era = era_from_year(year)
    ) %>%
    left_join(conf_flags, by = c("pair_dash", "era"))

  n_missing <- sum(is.na(dat$keep_pair_era))
  if (n_missing > 0) {
    message(sprintf(
      "Pruned run: %d observations have no pair-era pruning flag and will be dropped.",
      n_missing
    ))
    dat <- dat %>% mutate(keep_pair_era = if_else(is.na(keep_pair_era), FALSE, keep_pair_era))
  }

  n_before_prune <- nrow(dat)
  dat <- dat %>% filter(keep_pair_era)
  message(sprintf("Observations after pair-era pruning: %d -> %d", n_before_prune, nrow(dat)))
}

dat <- dat %>%
  mutate(unit_key = case_when(
    unit_def == "id" ~ listing_id,
    unit_def == "loc_key" ~ loc_key,
    unit_def == "unit_proxy" ~ unit_proxy_key,
    TRUE ~ listing_id
  )) %>%
  filter(!is.na(unit_key), unit_key != "")

pair_month_map <- dat %>%
  group_by(segment_id, year_month) %>%
  summarise(
    strict_more = max(strict_more, na.rm = TRUE),
    strict_less = min(strict_less, na.rm = TRUE),
    .groups = "drop"
  )

side_template <- bind_rows(
  pair_month_map %>% transmute(segment_id, year_month, right = 0L, strictness_own = strict_less),
  pair_month_map %>% transmute(segment_id, year_month, right = 1L, strictness_own = strict_more)
)

side_counts <- dat %>%
  distinct(segment_id, right, year_month, unit_key) %>%
  count(segment_id, right, year_month, name = "n_units")

panel <- side_template %>%
  left_join(side_counts, by = c("segment_id", "right", "year_month")) %>%
  mutate(n_units = as.integer(coalesce(n_units, 0L)))

cov_side <- dat %>%
  distinct(segment_id, right, year_month, unit_key, .keep_all = TRUE) %>%
  group_by(segment_id, right, year_month) %>%
  summarise(
    mean_log_sqft = mean(if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_), na.rm = TRUE),
    mean_beds = mean(beds, na.rm = TRUE),
    mean_baths = mean(baths, na.rm = TRUE),
    share_multifamily = mean(building_type_clean == "multi_family", na.rm = TRUE),
    share_laundry = mean(laundry == 1, na.rm = TRUE),
    share_gym = mean(gym == 1, na.rm = TRUE),
    .groups = "drop"
  )

panel <- panel %>% left_join(cov_side, by = c("segment_id", "right", "year_month"))

covars <- c("mean_log_sqft", "mean_beds", "mean_baths", "share_multifamily", "share_laundry", "share_gym")
for (v in covars) {
  miss_v <- paste0("miss_", v)
  panel[[miss_v]] <- as.integer(is.na(panel[[v]]))
  panel[[v]][is.na(panel[[v]])] <- 0
}

strictness_sd <- sd(panel$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero/invalid SD in this sample.", call. = FALSE)
}
panel <- panel %>% mutate(strictness_std = strictness_own / strictness_sd)

rhs <- paste(c("strictness_std", covars, paste0("miss_", covars)), collapse = " + ")
fml <- as.formula(paste0("n_units ~ ", rhs, " | segment_id^year_month"))
m <- fepois(fml, data = panel, cluster = ~segment_id)
ct <- coeftable(m)
p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)
if (length(p_col) == 0) {
  stop("Could not find p-value column in coeftable output.", call. = FALSE)
}

pair_month_obs <- side_counts %>%
  count(segment_id, year_month, name = "n_sides_obs")
pair_month_diag <- pair_month_map %>%
  left_join(pair_month_obs, by = c("segment_id", "year_month")) %>%
  mutate(n_sides_obs = coalesce(n_sides_obs, 0L))

out <- tibble(
  estimate = ct["strictness_std", "Estimate"],
  std_error = ct["strictness_std", "Std. Error"],
  p_value = ct["strictness_std", p_col[1]],
  implied_pct_change = 100 * (exp(ct["strictness_std", "Estimate"]) - 1),
  n_obs = nobs(m),
  segments = n_distinct(panel$segment_id),
  pair_month_cells = nrow(pair_month_map),
  share_single_sided_pair_month = mean(pair_month_diag$n_sides_obs == 1),
  share_zero_cells = mean(panel$n_units == 0),
  mean_units_per_cell = mean(panel$n_units, na.rm = TRUE),
  strictness_sd = strictness_sd,
  unit_def = unit_def,
  bandwidth_ft = bw_ft,
  window = window,
  sample_filter = sample_filter
)
write_csv(out, output_csv)

coef_str <- sprintf("%.4f%s", out$estimate, stars(out$p_value))
se_str <- sprintf("(%.4f)", out$std_error)

tex <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{@{}lc@{}}",
  "  \\toprule",
  "  & PPML: Distinct Units \\\\",
  "  \\midrule",
  sprintf("  Uncertainty Index & %s \\\\", coef_str),
  sprintf("  & %s \\\\", se_str),
  "  \\\\",
  sprintf("  Obs. & %s \\\\", format(out$n_obs, big.mark = ",")),
  "  Hedonic Controls & $\\checkmark$ \\\\",
  "  Segment $\\times$ Month FE & $\\checkmark$ \\\\",
  "  Clustered by Segment & $\\checkmark$ \\\\",
  "  \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)
writeLines(tex, output_tex)

message(sprintf("  b=%.4f (SE %.4f, p=%.3f), implied=%.2f%%",
                out$estimate, out$std_error, out$p_value, out$implied_pct_change))
message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))
