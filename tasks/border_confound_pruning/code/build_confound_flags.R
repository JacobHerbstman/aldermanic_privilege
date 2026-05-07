source("../../setup_environment/code/packages.R")

library(data.table)

segment_path <- "../input/segment_classification.csv"
out_flags <- "../output/confounded_pair_era_flags.csv"
out_segment_flags <- "../output/confounded_segment_flags.csv"
out_keep <- "../output/keep_pair_era.csv"
out_drop <- "../output/drop_pair_era.csv"
out_segment_audit <- "../output/confound_pruning_segment_audit.csv"
out_threshold_sensitivity <- "../output/confound_pruning_threshold_sensitivity.csv"
out_summary <- "../output/confound_pruning_summary.md"

target_segment_length_m <- as.numeric(Sys.getenv("TARGET_SEGMENT_LENGTH_M", "400"))
if (!is.finite(target_segment_length_m) || target_segment_length_m <= 0) {
  stop("TARGET_SEGMENT_LENGTH_M must be positive.", call. = FALSE)
}
segment_park_water_drop_share <- 0.50
segment_expressway_drop_share <- 0.40
segment_arterial_drop_share <- 0.75
pair_physical_barrier_drop_share <- 0.50
pair_expressway_drop_share <- 0.40
pair_arterial_drop_share <- 0.60
park_water_sensitivity_shares <- c(0.25, 0.50, 0.75)
expressway_sensitivity_shares <- c(0.25, 0.40, 0.50, 0.75)
arterial_sensitivity_shares <- c(0.40, 0.60, 0.75, 0.85)

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  x <- x[grepl("^[0-9]+-[0-9]+$", x)]
  if (length(x) == 0) {
    return(character())
  }
  parts <- strsplit(x, "-", fixed = TRUE)
  vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) {
      return(NA_character_)
    }
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
}

fmt_pct <- function(x) {
  ifelse(is.finite(x), sprintf("%.1f%%", 100 * x), "NA")
}

stopifnot(file.exists(segment_path))
segments <- fread(segment_path)

derive_meter_col <- function(dt, meter_col, foot_col) {
  if (!meter_col %in% names(dt) && foot_col %in% names(dt)) {
    dt[, (meter_col) := as.numeric(get(foot_col)) * 0.3048]
  }
}

derive_meter_col(segments, "segment_length_m", "segment_length_ft")
derive_meter_col(segments, "waterway_overlap_m", "waterway_overlap_ft")
derive_meter_col(segments, "expressway_overlap_m", "expressway_overlap_ft")
derive_meter_col(segments, "major_overlap_arterial_m", "major_overlap_arterial_ft")
derive_meter_col(segments, "target_length_m", "target_length_ft")

required_cols <- c(
  "segment_id",
  "ward_pair_id",
  "era",
  "segment_length_m",
  "segment_type",
  "water_area_share",
  "park_area_share",
  "cemetery_area_share",
  "waterway_overlap_m",
  "expressway_overlap_m",
  "major_overlap_arterial_m",
  "target_length_m"
)

missing_cols <- setdiff(required_cols, names(segments))
if (length(missing_cols) > 0) {
  stop(
    sprintf("segment_classification.csv missing required columns: %s", paste(missing_cols, collapse = ", ")),
    call. = FALSE
  )
}

segments[, ward_pair_id_us := as.character(ward_pair_id)]
segments[, ward_pair_id_dash := normalize_pair_dash(ward_pair_id_us)]
segments <- segments[!is.na(ward_pair_id_dash)]

segments[, segment_length_m := pmax(0, as.numeric(segment_length_m))]
segments[!is.finite(segment_length_m), segment_length_m := 0]
segments[, major_overlap_arterial_m := pmax(0, as.numeric(major_overlap_arterial_m))]
segments[!is.finite(major_overlap_arterial_m), major_overlap_arterial_m := 0]
segments[, expressway_overlap_m := pmax(0, as.numeric(expressway_overlap_m))]
segments[!is.finite(expressway_overlap_m), expressway_overlap_m := 0]
segments[, target_length_m := as.numeric(target_length_m)]
segments <- segments[is.finite(target_length_m)]

# Restrict to the intended round-meter segment layer to avoid mixing products.
segments <- segments[abs(target_length_m - target_segment_length_m) < 1e-8]

if (nrow(segments) == 0) {
  stop(
    sprintf("No rows remained after filtering to target_length_m == %.0f.", target_segment_length_m),
    call. = FALSE
  )
}

if (anyDuplicated(segments$segment_id) > 0) {
  stop("Duplicate segment_id rows remain after target_length_m filtering.", call. = FALSE)
}

segments[, water_area_share := as.numeric(water_area_share)]
segments[, park_area_share := as.numeric(park_area_share)]
segments[, cemetery_area_share := as.numeric(cemetery_area_share)]
segments[, waterway_overlap_m := as.numeric(waterway_overlap_m)]
segments[!is.finite(water_area_share), water_area_share := 0]
segments[!is.finite(park_area_share), park_area_share := 0]
segments[!is.finite(cemetery_area_share), cemetery_area_share := 0]
segments[!is.finite(waterway_overlap_m), waterway_overlap_m := 0]
segments[, waterway_overlap_m := pmax(0, waterway_overlap_m)]

for (share_col in c("water_area_share", "park_area_share", "cemetery_area_share")) {
  bad_n <- nrow(segments[get(share_col) < 0 | get(share_col) > 1])
  if (bad_n > 0) {
    stop(
      sprintf("%s has %s values outside [0, 1].", share_col, format(bad_n, big.mark = ",")),
      call. = FALSE
    )
  }
}

if (
  all(is.na(segments$segment_type) | as.character(segments$segment_type) %in% c("no_feature", "")) &&
    all(segments$water_area_share <= 0, na.rm = TRUE) &&
    all(segments$park_area_share <= 0, na.rm = TRUE) &&
    all(segments$cemetery_area_share <= 0, na.rm = TRUE) &&
    all(segments$waterway_overlap_m <= 0, na.rm = TRUE) &&
    all(segments$expressway_overlap_m <= 0, na.rm = TRUE) &&
    all(segments$major_overlap_arterial_m <= 0, na.rm = TRUE)
) {
  stop(
    paste(
      sprintf("segment_classification.csv has no park/water/cemetery/expressway/arterial feature metrics after the %.0fm filter.", target_segment_length_m),
      "Refusing to create no-op pruning flags."
    ),
    call. = FALSE
  )
}

segments[, segment_type_clean := as.character(segment_type)]
segments[is.na(segment_type_clean) | segment_type_clean == "", segment_type_clean := "no_feature"]
segments[, waterway_overlap_capped_m := pmin(waterway_overlap_m, segment_length_m)]
segments[, waterway_overlap_share := fifelse(segment_length_m > 0, waterway_overlap_capped_m / segment_length_m, 0)]
segments[, park_water_area_share_sum := pmin(1, water_area_share + park_area_share)]
segments[, park_water_continuous_length_m := pmin(
  segment_length_m,
  segment_length_m * park_water_area_share_sum + waterway_overlap_capped_m
)]
segments[, cemetery_continuous_length_m := segment_length_m * cemetery_area_share]
segments[, physical_barrier_continuous_length_m := pmin(
  segment_length_m,
  park_water_continuous_length_m + cemetery_continuous_length_m
)]
segments[, is_park_water := (
  segment_type_clean == "park_water" |
    water_area_share > 0 |
    park_area_share > 0 |
    waterway_overlap_m > 0
)]
segments[, is_cemetery := segment_type_clean == "cemetery" | cemetery_area_share > 0]
segments[, park_water_tiny_positive := (
  is_park_water &
    segment_type_clean != "park_water" &
    pmax(park_water_area_share_sum, waterway_overlap_share, na.rm = TRUE) > 0 &
    pmax(park_water_area_share_sum, waterway_overlap_share, na.rm = TRUE) < 0.05
)]
segments[, cemetery_tiny_positive := (
  is_cemetery &
    cemetery_area_share > 0 &
    cemetery_area_share < 0.05
)]

segments[, expressway_overlap_uncapped_m := expressway_overlap_m]
segments[, expressway_overlap_capped_m := pmin(expressway_overlap_m, segment_length_m)]
segments[, expressway_overlap_was_capped := expressway_overlap_uncapped_m > segment_length_m]
segments[, expressway_overlap_share := fifelse(segment_length_m > 0, expressway_overlap_capped_m / segment_length_m, 0)]
segments[, arterial_overlap_uncapped_m := major_overlap_arterial_m]
segments[, arterial_overlap_capped_m := pmin(major_overlap_arterial_m, segment_length_m)]
segments[, arterial_overlap_was_capped := arterial_overlap_uncapped_m > segment_length_m]
segments[, arterial_overlap_share := fifelse(segment_length_m > 0, arterial_overlap_capped_m / segment_length_m, 0)]
segments[, park_water_continuous_share := fifelse(segment_length_m > 0, park_water_continuous_length_m / segment_length_m, 0)]
segments[, cemetery_continuous_share := fifelse(segment_length_m > 0, cemetery_continuous_length_m / segment_length_m, 0)]
segments[, physical_barrier_continuous_share := fifelse(segment_length_m > 0, physical_barrier_continuous_length_m / segment_length_m, 0)]

segments[, segment_flag_park_water := is.finite(park_water_continuous_share) &
  park_water_continuous_share >= segment_park_water_drop_share]
segments[, segment_flag_waterway := waterway_overlap_capped_m > 0]
segments[, segment_flag_cemetery := is_cemetery]
segments[, segment_flag_expressway := is.finite(expressway_overlap_share) &
  expressway_overlap_share >= segment_expressway_drop_share]
segments[, segment_flag_arterial := is.finite(arterial_overlap_share) &
  arterial_overlap_share >= segment_arterial_drop_share]
segments[, segment_drop_confound := (
  segment_flag_park_water |
    segment_flag_waterway |
    segment_flag_cemetery |
    segment_flag_expressway |
    segment_flag_arterial
)]
segments[, segment_drop_reason := mapply(function(park, waterway, cemetery, expressway, arterial) {
  reason <- character()
  if (isTRUE(park)) reason <- c(reason, "park_water")
  if (isTRUE(waterway)) reason <- c(reason, "waterway")
  if (isTRUE(cemetery)) reason <- c(reason, "cemetery")
  if (isTRUE(expressway)) reason <- c(reason, "expressway")
  if (isTRUE(arterial)) reason <- c(reason, "arterial")
  if (length(reason) == 0) return("none")
  paste(reason, collapse = "|")
}, segment_flag_park_water, segment_flag_waterway, segment_flag_cemetery, segment_flag_expressway, segment_flag_arterial, USE.NAMES = FALSE)]

segment_audit <- segments[, .(
  segment_id,
  ward_pair_id_dash,
  ward_pair_id_us,
  era,
  target_length_m,
  segment_length_m,
  segment_type = segment_type_clean,
  water_area_share,
  park_area_share,
  cemetery_area_share,
  waterway_overlap_m,
  waterway_overlap_capped_m,
  waterway_overlap_share,
  park_water_area_share_sum,
  park_water_continuous_length_m,
  cemetery_continuous_length_m,
  physical_barrier_continuous_length_m,
  park_water_continuous_share,
  cemetery_continuous_share,
  physical_barrier_continuous_share,
  park_water_tiny_positive,
  cemetery_tiny_positive,
  expressway_overlap_m,
  expressway_overlap_capped_m,
  expressway_overlap_share,
  expressway_overlap_was_capped,
  major_overlap_arterial_m,
  arterial_overlap_capped_m,
  arterial_overlap_share,
  arterial_overlap_was_capped,
  is_park_water,
  is_cemetery,
  segment_flag_park_water,
  segment_flag_waterway,
  segment_flag_cemetery,
  segment_flag_expressway,
  segment_flag_arterial,
  segment_drop_confound,
  segment_drop_reason
)]
setorder(segment_audit, era, ward_pair_id_dash, segment_id)
fwrite(segment_audit, out_segment_audit)

segment_flags <- segments[, .(
  ward_pair_id_dash,
  ward_pair_id_us,
  era,
  segment_id,
  segment_length_m,
  segment_type = segment_type_clean,
  water_area_share,
  park_area_share,
  cemetery_area_share,
  waterway_overlap_m,
  waterway_overlap_share,
  park_water_continuous_share,
  cemetery_continuous_share,
  physical_barrier_continuous_share,
  expressway_overlap_m,
  expressway_overlap_share,
  major_overlap_arterial_m,
  arterial_overlap_share,
  flag_park_water = as.logical(segment_flag_park_water),
  flag_waterway = as.logical(segment_flag_waterway),
  flag_cemetery = as.logical(segment_flag_cemetery),
  flag_expressway = as.logical(segment_flag_expressway),
  flag_arterial = as.logical(segment_flag_arterial),
  drop_confound = as.logical(segment_drop_confound),
  drop_reason = segment_drop_reason
)]
setorder(segment_flags, era, ward_pair_id_dash, segment_id)
if (anyDuplicated(segment_flags[, .(ward_pair_id_dash, era, segment_id)]) > 0) {
  stop("Duplicate (ward_pair_id_dash, era, segment_id) rows in segment confound flags.", call. = FALSE)
}
if (sum(segment_flags$drop_confound, na.rm = TRUE) == 0) {
  stop("Segment pruning flags would drop zero segments; refusing to create no-op pruning outputs.", call. = FALSE)
}
fwrite(segment_flags, out_segment_flags)

flags <- segments[, .(
  n_segments = .N,
  n_park_water_segments = sum(is_park_water, na.rm = TRUE),
  n_park_water_tiny_positive_segments = sum(park_water_tiny_positive, na.rm = TRUE),
  n_cemetery_segments = sum(is_cemetery, na.rm = TRUE),
  n_cemetery_tiny_positive_segments = sum(cemetery_tiny_positive, na.rm = TRUE),
  n_expressway_segments = sum(expressway_overlap_m > 0, na.rm = TRUE),
  n_expressway_capped_segments = sum(expressway_overlap_was_capped, na.rm = TRUE),
  n_arterial_segments = sum(major_overlap_arterial_m > 0, na.rm = TRUE),
  n_arterial_capped_segments = sum(arterial_overlap_was_capped, na.rm = TRUE),
  total_length_m = sum(segment_length_m, na.rm = TRUE),
  park_water_length_m = sum(segment_length_m * as.numeric(is_park_water), na.rm = TRUE),
  park_water_continuous_length_m = sum(park_water_continuous_length_m, na.rm = TRUE),
  park_water_tiny_positive_length_m = sum(segment_length_m * as.numeric(park_water_tiny_positive), na.rm = TRUE),
  cemetery_length_m = sum(segment_length_m * as.numeric(is_cemetery), na.rm = TRUE),
  cemetery_continuous_length_m = sum(cemetery_continuous_length_m, na.rm = TRUE),
  cemetery_tiny_positive_length_m = sum(segment_length_m * as.numeric(cemetery_tiny_positive), na.rm = TRUE),
  physical_barrier_continuous_length_m = sum(physical_barrier_continuous_length_m, na.rm = TRUE),
  expressway_overlap_length_m = sum(expressway_overlap_capped_m, na.rm = TRUE),
  expressway_overlap_uncapped_length_m = sum(expressway_overlap_uncapped_m, na.rm = TRUE),
  arterial_overlap_length_m = sum(arterial_overlap_capped_m, na.rm = TRUE),
  arterial_overlap_uncapped_length_m = sum(arterial_overlap_uncapped_m, na.rm = TRUE)
), by = .(ward_pair_id_dash, ward_pair_id_us, era)]

flags[, share_park_water_length := fifelse(total_length_m > 0, park_water_length_m / total_length_m, NA_real_)]
flags[, share_park_water_continuous_length := fifelse(total_length_m > 0, park_water_continuous_length_m / total_length_m, NA_real_)]
flags[, share_park_water_tiny_positive_length := fifelse(total_length_m > 0, park_water_tiny_positive_length_m / total_length_m, NA_real_)]
flags[, share_cemetery_length := fifelse(total_length_m > 0, cemetery_length_m / total_length_m, NA_real_)]
flags[, share_cemetery_continuous_length := fifelse(total_length_m > 0, cemetery_continuous_length_m / total_length_m, NA_real_)]
flags[, share_cemetery_tiny_positive_length := fifelse(total_length_m > 0, cemetery_tiny_positive_length_m / total_length_m, NA_real_)]
flags[, share_physical_barrier_length := fifelse(total_length_m > 0, physical_barrier_continuous_length_m / total_length_m, NA_real_)]
flags[, expressway_overlap_share := fifelse(total_length_m > 0, expressway_overlap_length_m / total_length_m, NA_real_)]
flags[, expressway_overlap_uncapped_share := fifelse(total_length_m > 0, expressway_overlap_uncapped_length_m / total_length_m, NA_real_)]
flags[, arterial_overlap_share := fifelse(total_length_m > 0, arterial_overlap_length_m / total_length_m, NA_real_)]
flags[, arterial_overlap_uncapped_share := fifelse(total_length_m > 0, arterial_overlap_uncapped_length_m / total_length_m, NA_real_)]

bad_total_n <- nrow(flags[!is.finite(total_length_m) | total_length_m <= 0])
if (bad_total_n > 0) {
  stop(
    sprintf("%s pair-era rows have non-positive total segment length.", format(bad_total_n, big.mark = ",")),
    call. = FALSE
  )
}

flags[, flag_park_water := is.finite(share_park_water_continuous_length) & share_park_water_continuous_length >= pair_physical_barrier_drop_share]
flags[, flag_park_water_continuous := is.finite(share_park_water_continuous_length) & share_park_water_continuous_length >= pair_physical_barrier_drop_share]
flags[, flag_cemetery := is.finite(share_cemetery_continuous_length) & share_cemetery_continuous_length >= pair_physical_barrier_drop_share]
flags[, flag_physical_barrier := is.finite(share_physical_barrier_length) & share_physical_barrier_length >= pair_physical_barrier_drop_share]
flags[, flag_expressway := is.finite(expressway_overlap_share) & expressway_overlap_share >= pair_expressway_drop_share]
flags[, flag_arterial := is.finite(arterial_overlap_share) & arterial_overlap_share >= pair_arterial_drop_share]
flags[, drop_confound := flag_physical_barrier | flag_expressway | flag_arterial]

flags[, drop_reason := mapply(function(a, b, c, d, e) {
  reason <- character()
  if (isTRUE(a)) reason <- c(reason, "park_water")
  if (isTRUE(b)) reason <- c(reason, "arterial")
  if (isTRUE(c)) reason <- c(reason, "cemetery")
  if (isTRUE(d)) reason <- c(reason, "expressway")
  if (isTRUE(e) && !isTRUE(a) && !isTRUE(c)) reason <- c(reason, "physical_barrier")
  if (length(reason) == 0) return("none")
  paste(reason, collapse = "|")
}, flag_park_water, flag_arterial, flag_cemetery, flag_expressway, flag_physical_barrier, USE.NAMES = FALSE)]

flags <- flags[, .(
  ward_pair_id_dash,
  ward_pair_id_us,
  era,
  n_segments,
  n_park_water_segments,
  n_park_water_tiny_positive_segments,
  n_cemetery_segments,
  n_cemetery_tiny_positive_segments,
  n_expressway_segments,
  n_expressway_capped_segments,
  n_arterial_segments,
  n_arterial_capped_segments,
  total_length_m,
  park_water_length_m,
  park_water_continuous_length_m,
  park_water_tiny_positive_length_m,
  cemetery_length_m,
  cemetery_continuous_length_m,
  cemetery_tiny_positive_length_m,
  physical_barrier_continuous_length_m,
  expressway_overlap_length_m,
  expressway_overlap_uncapped_length_m,
  arterial_overlap_length_m,
  arterial_overlap_uncapped_length_m,
  share_park_water_length,
  share_park_water_continuous_length,
  share_park_water_tiny_positive_length,
  share_cemetery_length,
  share_cemetery_continuous_length,
  share_cemetery_tiny_positive_length,
  share_physical_barrier_length,
  expressway_overlap_share,
  expressway_overlap_uncapped_share,
  arterial_overlap_share,
  arterial_overlap_uncapped_share,
  flag_park_water = as.logical(flag_park_water),
  flag_park_water_continuous = as.logical(flag_park_water_continuous),
  flag_cemetery = as.logical(flag_cemetery),
  flag_physical_barrier = as.logical(flag_physical_barrier),
  flag_expressway = as.logical(flag_expressway),
  flag_arterial = as.logical(flag_arterial),
  drop_confound = as.logical(drop_confound),
  drop_reason
)]

setorder(flags, era, ward_pair_id_dash)

if (anyDuplicated(flags[, .(ward_pair_id_dash, era)]) > 0) {
  stop("Duplicate (ward_pair_id_dash, era) rows in confound flags.", call. = FALSE)
}

keep <- flags[drop_confound == FALSE]
drop <- flags[drop_confound == TRUE]

if (nrow(drop) == 0) {
  stop("Pruning flags would drop zero pair-era rows; refusing to create no-op pruning outputs.", call. = FALSE)
}

fwrite(flags, out_flags)
fwrite(keep, out_keep)
fwrite(drop, out_drop)

threshold_sensitivity <- CJ(
  park_water_threshold = park_water_sensitivity_shares,
  expressway_threshold = expressway_sensitivity_shares,
  arterial_threshold = arterial_sensitivity_shares
)
threshold_sensitivity[, c(
  "dropped_pair_era_n",
  "drop_share",
  "physical_barrier_only_n",
  "expressway_only_n",
  "arterial_only_n",
  "multiple_reason_n",
  "arterial_capping_avoided_n"
) := {
  barrier_flag <- is.finite(flags$share_physical_barrier_length) &
    flags$share_physical_barrier_length >= park_water_threshold
  expressway_flag <- is.finite(flags$expressway_overlap_share) &
    flags$expressway_overlap_share >= expressway_threshold
  art_flag <- is.finite(flags$arterial_overlap_share) &
    flags$arterial_overlap_share >= arterial_threshold
  art_uncapped_flag <- is.finite(flags$arterial_overlap_uncapped_share) &
    flags$arterial_overlap_uncapped_share >= arterial_threshold
  drop_alt <- barrier_flag | expressway_flag | art_flag
  reason_count <- rowSums(cbind(barrier_flag, expressway_flag, art_flag), na.rm = TRUE)

  list(
    sum(drop_alt, na.rm = TRUE),
    mean(drop_alt, na.rm = TRUE),
    sum(barrier_flag & reason_count == 1, na.rm = TRUE),
    sum(expressway_flag & reason_count == 1, na.rm = TRUE),
    sum(art_flag & reason_count == 1, na.rm = TRUE),
    sum(reason_count > 1, na.rm = TRUE),
    sum(art_uncapped_flag & !art_flag, na.rm = TRUE)
  )
}, by = .(park_water_threshold, expressway_threshold, arterial_threshold)]
threshold_sensitivity[, baseline := (
  abs(park_water_threshold - pair_physical_barrier_drop_share) < 1e-8 &
    abs(expressway_threshold - pair_expressway_drop_share) < 1e-8 &
    abs(arterial_threshold - pair_arterial_drop_share) < 1e-8
)]
fwrite(threshold_sensitivity, out_threshold_sensitivity)

era_summary <- flags[, .(
  n_pair_era = .N,
  n_drop = sum(drop_confound, na.rm = TRUE),
  drop_share = mean(drop_confound, na.rm = TRUE),
  mean_physical_barrier_share = mean(share_physical_barrier_length, na.rm = TRUE),
  mean_expressway_share = mean(expressway_overlap_share, na.rm = TRUE),
  mean_arterial_share = mean(arterial_overlap_share, na.rm = TRUE)
), by = era][order(era)]

reason_summary <- flags[drop_confound == TRUE, .N, by = drop_reason][order(-N, drop_reason)]
if (nrow(reason_summary) == 0) {
  reason_summary <- data.table(drop_reason = "none", N = 0L)
}
segment_reason_summary <- segment_flags[drop_confound == TRUE, .N, by = drop_reason][order(-N, drop_reason)]
if (nrow(segment_reason_summary) == 0) {
  segment_reason_summary <- data.table(drop_reason = "none", N = 0L)
}
park_water_binary_only_n <- nrow(flags[flag_park_water == TRUE & flag_park_water_continuous == FALSE])
expressway_capping_cross_n <- nrow(flags[
  expressway_overlap_uncapped_share >= pair_expressway_drop_share &
    expressway_overlap_share < pair_expressway_drop_share
])
arterial_capping_cross_n <- nrow(flags[
  arterial_overlap_uncapped_share >= pair_arterial_drop_share &
    arterial_overlap_share < pair_arterial_drop_share
])

lines <- c(
  "# Confound Pruning Summary",
  "",
  sprintf("- segment source: `%s`", segment_path),
  sprintf("- target segment length: %.0fm", target_segment_length_m),
  sprintf("- preferred segment park/water drop threshold: %s", fmt_pct(segment_park_water_drop_share)),
  sprintf("- preferred segment expressway drop threshold: %s", fmt_pct(segment_expressway_drop_share)),
  sprintf("- preferred segment arterial drop threshold: %s", fmt_pct(segment_arterial_drop_share)),
  "- preferred segment rule drops any waterway or cemetery overlap",
  sprintf("- pair-era physical-barrier backstop threshold: %s", fmt_pct(pair_physical_barrier_drop_share)),
  sprintf("- pair-era expressway backstop threshold: %s", fmt_pct(pair_expressway_drop_share)),
  sprintf("- pair-era arterial backstop threshold: %s", fmt_pct(pair_arterial_drop_share)),
  sprintf("- segment rows used after target_length_m filter: %s", format(nrow(segments), big.mark = ",")),
  sprintf("- preferred segment-level dropped rows: %s (%s)", format(sum(segment_flags$drop_confound, na.rm = TRUE), big.mark = ","), fmt_pct(mean(segment_flags$drop_confound, na.rm = TRUE))),
  sprintf("- segment rows with capped expressway overlap: %s", format(sum(segments$expressway_overlap_was_capped, na.rm = TRUE), big.mark = ",")),
  sprintf("- segment rows with capped arterial overlap: %s", format(sum(segments$arterial_overlap_was_capped, na.rm = TRUE), big.mark = ",")),
  sprintf("- pair-era rows: %s", format(nrow(flags), big.mark = ",")),
  sprintf("- dropped pair-era rows: %s (%s)", format(nrow(drop), big.mark = ","), fmt_pct(nrow(drop) / max(nrow(flags), 1))),
  sprintf("- kept pair-era rows: %s", format(nrow(keep), big.mark = ",")),
  sprintf("- park/water pair-era flags that rely on binary full-segment counting: %s", format(park_water_binary_only_n, big.mark = ",")),
  sprintf("- expressway pair-era flags avoided only because of capping: %s", format(expressway_capping_cross_n, big.mark = ",")),
  sprintf("- arterial pair-era flags avoided only because of capping: %s", format(arterial_capping_cross_n, big.mark = ",")),
  "",
  "## By Era",
  "",
  "| Era | Pair-Era Rows | Dropped | Drop Share | Mean Barrier Share | Mean Expressway Share | Mean Arterial Share |",
  "|---|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(era_summary))) {
  rr <- era_summary[i]
  lines <- c(
    lines,
    sprintf(
      "| %s | %s | %s | %s | %.3f | %.3f | %.3f |",
      rr$era,
      format(rr$n_pair_era, big.mark = ","),
      format(rr$n_drop, big.mark = ","),
      fmt_pct(rr$drop_share),
      rr$mean_physical_barrier_share,
      rr$mean_expressway_share,
      rr$mean_arterial_share
    )
  )
}

lines <- c(
  lines,
  "",
  "## Preferred Segment Drop Reasons",
  "",
  "| Reason | Count |",
  "|---|---:|"
)

for (i in seq_len(nrow(segment_reason_summary))) {
  rr <- segment_reason_summary[i]
  reason_label <- gsub("|", " + ", rr$drop_reason, fixed = TRUE)
  lines <- c(lines, sprintf("| %s | %s |", reason_label, format(rr$N, big.mark = ",")))
}

lines <- c(
  lines,
  "",
  "## Pair-Era Backstop Drop Reasons",
  "",
  "| Reason | Count |",
  "|---|---:|"
)

for (i in seq_len(nrow(reason_summary))) {
  rr <- reason_summary[i]
  reason_label <- gsub("|", " + ", rr$drop_reason, fixed = TRUE)
  lines <- c(lines, sprintf("| %s | %s |", reason_label, format(rr$N, big.mark = ",")))
}

lines <- c(
  lines,
  "",
  "## Threshold Sensitivity",
  "",
  "| Barrier Threshold | Expressway Threshold | Arterial Threshold | Dropped | Drop Share | Barrier Only | Expressway Only | Arterial Only | Multiple |",
  "|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(threshold_sensitivity))) {
  rr <- threshold_sensitivity[i]
  lines <- c(
    lines,
    sprintf(
      "| %.2f%s | %.2f%s | %.2f%s | %s | %s | %s | %s | %s | %s |",
      rr$park_water_threshold,
      ifelse(rr$baseline, " baseline", ""),
      rr$expressway_threshold,
      ifelse(rr$baseline, " baseline", ""),
      rr$arterial_threshold,
      ifelse(rr$baseline, " baseline", ""),
      format(rr$dropped_pair_era_n, big.mark = ","),
      fmt_pct(rr$drop_share),
      format(rr$physical_barrier_only_n, big.mark = ","),
      format(rr$expressway_only_n, big.mark = ","),
      format(rr$arterial_only_n, big.mark = ","),
      format(rr$multiple_reason_n, big.mark = ",")
    )
  )
}

writeLines(lines, out_summary)

message("Saved:")
message(sprintf("  - %s", out_flags))
message(sprintf("  - %s", out_segment_flags))
message(sprintf("  - %s", out_keep))
message(sprintf("  - %s", out_drop))
message(sprintf("  - %s", out_segment_audit))
message(sprintf("  - %s", out_threshold_sensitivity))
message(sprintf("  - %s", out_summary))
