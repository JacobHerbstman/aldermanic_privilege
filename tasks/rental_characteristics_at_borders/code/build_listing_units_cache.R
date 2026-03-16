source("../../setup_environment/code/packages.R")

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# Rscript build_listing_units_cache.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy 0 8 all NA ../output/listing_units_side_panel_bw500_pre_2023_all_pct0_unit_proxy_all.parquet ../output/listing_units_bin_cells_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.parquet ../output/listing_units_cache_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.csv

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 12) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  unit_def <- cli_args[5]
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[6]))
  bins_per_side <- suppressWarnings(as.integer(cli_args[7]))
  prune_sample <- tolower(cli_args[8])
  confound_flags_path <- cli_args[9]
  output_side_panel <- cli_args[10]
  output_bin_cells <- cli_args[11]
  output_meta <- cli_args[12]
} else {
  stop(
    paste(
      "FATAL: Script requires 12 args:",
      "<input> <bw_ft> <window> <sample_filter> <unit_def>",
      "<min_strictness_diff_pctile> <bins_per_side> <prune_sample>",
      "<confound_flags_path> <output_side_panel> <output_bin_cells> <output_meta>"
    ),
    call. = FALSE
  )
}

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  out <- rep(NA_character_, length(x))
  if (!any(ok)) {
    return(out)
  }
  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) {
      return(NA_character_)
    }
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
  if (w == "pre_2021") return(df |> filter(year <= 2020))
  if (w == "pre_2023") return(df |> filter(year <= 2022))
  if (w == "pre_covid") return(df |> filter(year <= 2019))
  df
}

if (!unit_def %in% c("id", "loc_key", "unit_proxy")) {
  stop("unit_def must be one of: id, loc_key, unit_proxy", call. = FALSE)
}
if (!prune_sample %in% c("all", "pruned")) {
  stop("prune_sample must be all or pruned", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive integer", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side <= 0) {
  stop("bins_per_side must be a positive integer", call. = FALSE)
}

message(sprintf(
  "=== Build Listing Units Cache | bw=%d | window=%s | sample=%s | unit_def=%s | pctile=%d | bins=%d | prune=%s ===",
  bw_ft, window, sample_filter, unit_def, min_strictness_diff_pctile, bins_per_side, prune_sample
))

dat <- open_dataset(input, format = "parquet") |>
  select(
    file_date,
    ward_pair_id,
    segment_id,
    signed_dist,
    strictness_own,
    strictness_neighbor,
    latitude,
    longitude,
    id,
    beds,
    baths,
    sqft,
    building_type_clean
  ) |>
  collect() |>
  as_tibble() |>
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
  ) |>
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(segment_id), segment_id != "",
    !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
    !is.na(latitude), !is.na(longitude),
    abs(signed_dist) <= bw_ft
  ) |>
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat <- dat |> filter(building_type_clean == "multi_family")
}

if (prune_sample == "pruned") {
  if (identical(confound_flags_path, "NA") || !file.exists(confound_flags_path)) {
    stop(sprintf("Missing confound flags file for pruned cache: %s", confound_flags_path), call. = FALSE)
  }

  conf_flags <- read_csv(
    confound_flags_path,
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "drop_confound")
  ) |>
    transmute(
      pair_dash = normalize_pair_dash(ward_pair_id_dash),
      era = as.character(era),
      keep_pair_era = !as.logical(drop_confound)
    ) |>
    distinct()

  if (anyNA(conf_flags$pair_dash) || anyNA(conf_flags$era)) {
    stop("Confound flags have invalid pair/era keys.", call. = FALSE)
  }
  if (anyDuplicated(conf_flags[, c("pair_dash", "era")]) > 0) {
    stop("Confound flags contain duplicate pair-era keys.", call. = FALSE)
  }

  dat <- dat |>
    mutate(
      pair_dash = normalize_pair_dash(ward_pair),
      era = era_from_year(year)
    ) |>
    left_join(conf_flags, by = c("pair_dash", "era"))

  n_missing <- sum(is.na(dat$keep_pair_era))
  if (n_missing > 0) {
    message(sprintf(
      "Pruned cache: %d observations have no pair-era pruning flag and will be dropped.",
      n_missing
    ))
    dat <- dat |> mutate(keep_pair_era = if_else(is.na(keep_pair_era), FALSE, keep_pair_era))
  }

  n_before_prune <- nrow(dat)
  dat <- dat |> filter(keep_pair_era)
  message(sprintf("Observations after pair-era pruning: %d -> %d", n_before_prune, nrow(dat)))
}

if (min_strictness_diff_pctile > 0) {
  segment_diffs <- dat |>
    group_by(segment_id) |>
    summarise(diff = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE), .groups = "drop")
  cutoff <- quantile(segment_diffs$diff, min_strictness_diff_pctile / 100, na.rm = TRUE)
  keep_segments <- segment_diffs |>
    filter(diff >= cutoff) |>
    pull(segment_id)
  dat <- dat |> filter(segment_id %in% keep_segments)
  message(sprintf(
    "After p%d filter (cutoff=%.3f): %d obs, %d segments",
    min_strictness_diff_pctile, cutoff, nrow(dat), n_distinct(dat$segment_id)
  ))
}

dat <- dat |>
  mutate(listing_key = case_when(
    unit_def == "id" ~ if_else(!is.na(listing_id) & listing_id != "", listing_id, unit_proxy_key),
    unit_def == "loc_key" ~ loc_key,
    unit_def == "unit_proxy" ~ unit_proxy_key
  )) |>
  filter(!is.na(listing_key), listing_key != "")

pair_month_map <- dat |>
  group_by(segment_id, ward_pair, year_month) |>
  summarise(
    strict_more = max(strict_more, na.rm = TRUE),
    strict_less = min(strict_less, na.rm = TRUE),
    .groups = "drop"
  )

side_template <- bind_rows(
  pair_month_map |> transmute(segment_id, ward_pair, year_month, right = 0L, strictness_own = strict_less),
  pair_month_map |> transmute(segment_id, ward_pair, year_month, right = 1L, strictness_own = strict_more)
)

side_counts <- dat |>
  distinct(segment_id, ward_pair, right, year_month, listing_key) |>
  count(segment_id, ward_pair, right, year_month, name = "n_units")

side_panel <- side_template |>
  left_join(side_counts, by = c("segment_id", "ward_pair", "right", "year_month")) |>
  mutate(n_units = as.integer(coalesce(n_units, 0L))) |>
  arrange(segment_id, ward_pair, year_month, right)

bin_w <- bw_ft / bins_per_side
bin_cells <- dat |>
  mutate(bin_center = (floor(signed_dist / bin_w) + 0.5) * bin_w) |>
  distinct(segment_id, ward_pair, bin_center, year_month, listing_key) |>
  group_by(segment_id, ward_pair, bin_center, year_month) |>
  summarise(n_units = n(), .groups = "drop") |>
  mutate(
    right = as.integer(bin_center >= 0),
    log_n = log(n_units)
  ) |>
  arrange(segment_id, ward_pair, year_month, bin_center)

side_pair_month <- side_panel |>
  group_by(segment_id, year_month) |>
  summarise(n_sides_obs = sum(n_units > 0), .groups = "drop")

meta <- tibble(
  input_rows = nrow(dat),
  side_panel_rows = nrow(side_panel),
  side_panel_pair_months = nrow(side_pair_month),
  side_panel_segments = n_distinct(side_panel$segment_id),
  share_single_sided_pair_month = mean(side_pair_month$n_sides_obs == 1),
  share_zero_side_cells = mean(side_panel$n_units == 0),
  bin_cells_rows = nrow(bin_cells),
  bin_cells_segments = n_distinct(bin_cells$segment_id),
  bandwidth_ft = bw_ft,
  window = window,
  sample_filter = sample_filter,
  unit_def = unit_def,
  min_strictness_diff_pctile = min_strictness_diff_pctile,
  bins_per_side = bins_per_side,
  prune_sample = prune_sample
)

write_parquet(side_panel, output_side_panel)
write_parquet(bin_cells, output_bin_cells)
write_csv(meta, output_meta)

message(sprintf("Saved: %s", output_side_panel))
message(sprintf("Saved: %s", output_bin_cells))
message(sprintf("Saved: %s", output_meta))
