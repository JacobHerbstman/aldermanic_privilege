# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_confound_pruning/code")

source("../../setup_environment/code/packages.R")

library(data.table)

target_segment_length_ft <- as.numeric(Sys.getenv("TARGET_SEGMENT_LENGTH_FT", "1320"))
if (!is.finite(target_segment_length_ft) || target_segment_length_ft <= 0) {
  stop("TARGET_SEGMENT_LENGTH_FT must be positive.", call. = FALSE)
}
segment_park_water_drop_share <- 0.50
segment_expressway_drop_share <- 0.40
segment_arterial_drop_share <- 0.75
pair_physical_barrier_drop_share <- 0.50
pair_expressway_drop_share <- 0.40
pair_arterial_drop_share <- 0.60

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

segments <- fread("../input/segment_classification.csv")

derive_meter_col <- function(dt, meter_col, foot_col) {
  if (!meter_col %in% names(dt) && foot_col %in% names(dt)) {
    dt[, (meter_col) := as.numeric(get(foot_col)) * 0.3048]
  }
}

derive_foot_col <- function(dt, foot_col, meter_col) {
  if (!foot_col %in% names(dt) && meter_col %in% names(dt)) {
    dt[, (foot_col) := as.numeric(get(meter_col)) / 0.3048]
  }
}

derive_meter_col(segments, "segment_length_m", "segment_length_ft")
derive_meter_col(segments, "waterway_overlap_m", "waterway_overlap_ft")
derive_meter_col(segments, "expressway_overlap_m", "expressway_overlap_ft")
derive_meter_col(segments, "major_overlap_arterial_m", "major_overlap_arterial_ft")
derive_meter_col(segments, "target_length_m", "target_length_ft")
derive_foot_col(segments, "target_length_ft", "target_length_m")

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
  "target_length_ft",
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
segments[, target_length_ft := as.numeric(target_length_ft)]
segments <- segments[is.finite(target_length_ft)]
segments[, target_length_m := as.numeric(target_length_m)]

# Restrict to the intended foot-denominated segment layer to avoid mixing products.
segments <- segments[abs(target_length_ft - target_segment_length_ft) < 1e-8]

if (nrow(segments) == 0) {
  stop(
    sprintf("No rows remained after filtering to target_length_ft == %.0f.", target_segment_length_ft),
    call. = FALSE
  )
}

if (anyDuplicated(segments$segment_id) > 0) {
  stop("Duplicate segment_id rows remain after target_length_ft filtering.", call. = FALSE)
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
      sprintf("segment_classification.csv has no park/water/cemetery/expressway/arterial feature metrics after the %.0fft filter.", target_segment_length_ft),
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
fwrite(segment_flags, "../output/confounded_segment_flags.csv")

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

drop <- flags[drop_confound == TRUE]

if (nrow(drop) == 0) {
  stop("Pruning flags would drop zero pair-era rows; refusing to create no-op pruning outputs.", call. = FALSE)
}

fwrite(flags, "../output/confounded_pair_era_flags.csv")
