# --- Interactive Test Block ---
# setwd("tasks/audits/rental_characteristics_at_borders/code")
# bw_ft <- 500
# window <- "pre_2023"
# sample_filter <- "all"
# unit_def <- "unit_proxy"
# min_strictness_diff_pctile <- 0
# bins_per_side <- 8

source("../../../setup_environment/code/packages.R", local = new.env(parent = globalenv()))

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bw_ft, window, sample_filter, unit_def, min_strictness_diff_pctile, bins_per_side)
}

if (length(cli_args) == 6) {
  bw_ft <- suppressWarnings(as.integer(cli_args[1]))
  window <- cli_args[2]
  sample_filter <- cli_args[3]
  unit_def <- cli_args[4]
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[5]))
  bins_per_side <- suppressWarnings(as.integer(cli_args[6]))
} else {
  stop(
    paste(
      "FATAL: Script requires 6 args:",
      "<bw_ft> <window> <sample_filter> <unit_def>",
      "<min_strictness_diff_pctile> <bins_per_side>"
    ),
    call. = FALSE
  )
}

if (!unit_def %in% c("id", "loc_key", "unit_proxy")) {
  stop("unit_def must be one of: id, loc_key, unit_proxy", call. = FALSE)
}
if (!window %in% c("full", "pre_2021", "pre_2023", "pre_covid")) {
  stop("window must be one of: full, pre_2021, pre_2023, pre_covid", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive integer", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side <= 0) {
  stop("bins_per_side must be a positive integer", call. = FALSE)
}

output_side_panel <- sprintf(
  "../temp/listing_units_side_panel_bw%d_%s_%s_pct%d_%s_all.parquet",
  bw_ft, window, sample_filter, min_strictness_diff_pctile, unit_def
)
output_bin_cells <- sprintf(
  "../temp/listing_units_bin_cells_bw%d_%s_%s_pct%d_%s_bins%d_all.parquet",
  bw_ft, window, sample_filter, min_strictness_diff_pctile, unit_def, bins_per_side
)

message(sprintf(
  "=== Build Listing Units Cache | bw=%d | window=%s | sample=%s | unit_def=%s | pctile=%d | bins=%d ===",
  bw_ft, window, sample_filter, unit_def, min_strictness_diff_pctile, bins_per_side
))

dat <- open_dataset("../input/rent_with_ward_distances.parquet", format = "parquet") |>
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
  )

if (window == "pre_2021") {
  dat <- dat |> filter(year <= 2020)
} else if (window == "pre_2023") {
  dat <- dat |> filter(year <= 2022)
} else if (window == "pre_covid") {
  dat <- dat |> filter(year <= 2019)
}

if (sample_filter == "multifamily_only") {
  dat <- dat |> filter(building_type_clean == "multi_family")
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

write_parquet(side_panel, output_side_panel)
write_parquet(bin_cells, output_bin_cells)

message(sprintf("Saved: %s", output_side_panel))
message(sprintf("Saved: %s", output_bin_cells))
