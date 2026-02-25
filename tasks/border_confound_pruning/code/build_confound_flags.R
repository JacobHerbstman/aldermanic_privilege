source("../../setup_environment/code/packages.R")

library(data.table)

segment_path <- "../input/segment_classification.csv"
sales_invalid_path <- "../input/border_verification_invalid_pairs_sales.csv"
rent_invalid_path <- "../input/border_verification_invalid_pairs_rent.csv"

out_flags <- "../output/confounded_pair_era_flags.csv"
out_keep <- "../output/keep_pair_era.csv"
out_drop <- "../output/drop_pair_era.csv"
out_summary <- "../output/confound_pruning_summary.md"

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

read_invalid_pairs <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }

  dt <- fread(path)
  cols <- intersect(c("ward_pair_id", "corrected_pair"), names(dt))
  if (length(cols) == 0) {
    return(character())
  }

  vals <- unique(unlist(dt[, ..cols], use.names = FALSE))
  vals <- vals[!is.na(vals)]
  vals <- normalize_pair_dash(vals)
  unique(vals[!is.na(vals)])
}

fmt_pct <- function(x) {
  ifelse(is.finite(x), sprintf("%.1f%%", 100 * x), "NA")
}

stopifnot(file.exists(segment_path))
segments <- fread(segment_path)

required_cols <- c(
  "segment_id",
  "ward_pair_id",
  "era",
  "segment_length_ft",
  "segment_type",
  "water_area_share",
  "park_area_share",
  "waterway_overlap_ft",
  "major_overlap_arterial_ft",
  "target_length_ft"
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

segments[, segment_length_ft := pmax(0, as.numeric(segment_length_ft))]
segments[!is.finite(segment_length_ft), segment_length_ft := 0]
segments[, major_overlap_arterial_ft := pmax(0, as.numeric(major_overlap_arterial_ft))]
segments[!is.finite(major_overlap_arterial_ft), major_overlap_arterial_ft := 0]
segments[, target_length_ft := as.numeric(target_length_ft)]
segments <- segments[is.finite(target_length_ft)]

# Restrict to the intended 1320ft segment layer to avoid mixing bandwidth products.
segments <- segments[abs(target_length_ft - 1320) < 1e-8]

if (nrow(segments) == 0) {
  stop("No rows remained after filtering to target_length_ft == 1320.", call. = FALSE)
}

if (anyDuplicated(segments$segment_id) > 0) {
  stop("Duplicate segment_id rows remain after target_length_ft filtering.", call. = FALSE)
}

segments[, water_area_share := as.numeric(water_area_share)]
segments[, park_area_share := as.numeric(park_area_share)]
segments[, waterway_overlap_ft := as.numeric(waterway_overlap_ft)]
segments[!is.finite(water_area_share), water_area_share := 0]
segments[!is.finite(park_area_share), park_area_share := 0]
segments[!is.finite(waterway_overlap_ft), waterway_overlap_ft := 0]

segments[, is_park_water := (
  as.character(segment_type) == "park_water" |
    water_area_share > 0 |
    park_area_share > 0 |
    waterway_overlap_ft > 0
)]

segments[, arterial_overlap_capped_ft := pmin(major_overlap_arterial_ft, segment_length_ft)]

flags <- segments[, .(
  n_segments = .N,
  total_length_ft = sum(segment_length_ft, na.rm = TRUE),
  park_water_length_ft = sum(segment_length_ft * as.numeric(is_park_water), na.rm = TRUE),
  arterial_overlap_length_ft = sum(arterial_overlap_capped_ft, na.rm = TRUE)
), by = .(ward_pair_id_dash, ward_pair_id_us, era)]

flags[, share_park_water_length := fifelse(total_length_ft > 0, park_water_length_ft / total_length_ft, NA_real_)]
flags[, arterial_overlap_share := fifelse(total_length_ft > 0, arterial_overlap_length_ft / total_length_ft, NA_real_)]

sales_invalid_pairs <- read_invalid_pairs(sales_invalid_path)
rent_invalid_pairs <- read_invalid_pairs(rent_invalid_path)
invalid_pairs <- unique(c(sales_invalid_pairs, rent_invalid_pairs))

flags[, invalid_pair_flag := era == "2003_2014" & ward_pair_id_dash %in% invalid_pairs]
flags[, flag_park_water := is.finite(share_park_water_length) & share_park_water_length >= 0.50]
flags[, flag_arterial := is.finite(arterial_overlap_share) & arterial_overlap_share >= 0.70]
flags[, drop_confound := flag_park_water | flag_arterial | invalid_pair_flag]

flags[, drop_reason := mapply(function(a, b, c) {
  reason <- character()
  if (isTRUE(a)) reason <- c(reason, "park_water")
  if (isTRUE(b)) reason <- c(reason, "arterial")
  if (isTRUE(c)) reason <- c(reason, "invalid_pair")
  if (length(reason) == 0) return("none")
  paste(reason, collapse = "|")
}, flag_park_water, flag_arterial, invalid_pair_flag, USE.NAMES = FALSE)]

flags <- flags[, .(
  ward_pair_id_dash,
  ward_pair_id_us,
  era,
  n_segments,
  total_length_ft,
  share_park_water_length,
  arterial_overlap_share,
  invalid_pair_flag = as.logical(invalid_pair_flag),
  drop_confound = as.logical(drop_confound),
  drop_reason
)]

setorder(flags, era, ward_pair_id_dash)

if (anyDuplicated(flags[, .(ward_pair_id_dash, era)]) > 0) {
  stop("Duplicate (ward_pair_id_dash, era) rows in confound flags.", call. = FALSE)
}

keep <- flags[drop_confound == FALSE]
drop <- flags[drop_confound == TRUE]

fwrite(flags, out_flags)
fwrite(keep, out_keep)
fwrite(drop, out_drop)

era_summary <- flags[, .(
  n_pair_era = .N,
  n_drop = sum(drop_confound, na.rm = TRUE),
  drop_share = mean(drop_confound, na.rm = TRUE),
  mean_park_water_share = mean(share_park_water_length, na.rm = TRUE),
  mean_arterial_share = mean(arterial_overlap_share, na.rm = TRUE)
), by = era][order(era)]

reason_summary <- flags[drop_confound == TRUE, .N, by = drop_reason][order(-N, drop_reason)]
if (nrow(reason_summary) == 0) {
  reason_summary <- data.table(drop_reason = "none", N = 0L)
}

lines <- c(
  "# Confound Pruning Summary",
  "",
  sprintf("- segment rows used after target_length_ft == 1320 filter: %s", format(nrow(segments), big.mark = ",")),
  sprintf("- pair-era rows: %s", format(nrow(flags), big.mark = ",")),
  sprintf("- dropped pair-era rows: %s (%s)", format(nrow(drop), big.mark = ","), fmt_pct(nrow(drop) / max(nrow(flags), 1))),
  sprintf("- kept pair-era rows: %s", format(nrow(keep), big.mark = ",")),
  sprintf("- invalid-pair candidates used: %s", format(length(invalid_pairs), big.mark = ",")),
  "",
  "## By Era",
  "",
  "| Era | Pair-Era Rows | Dropped | Drop Share | Mean Park/Water Share | Mean Arterial Share |",
  "|---|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(era_summary))) {
  rr <- era_summary[i]
  lines <- c(
    lines,
    sprintf(
      "| %s | %s | %s | %s | %.3f | %.3f |",
      rr$era,
      format(rr$n_pair_era, big.mark = ","),
      format(rr$n_drop, big.mark = ","),
      fmt_pct(rr$drop_share),
      rr$mean_park_water_share,
      rr$mean_arterial_share
    )
  )
}

lines <- c(
  lines,
  "",
  "## Drop Reasons",
  "",
  "| Reason | Count |",
  "|---|---:|"
)

for (i in seq_len(nrow(reason_summary))) {
  rr <- reason_summary[i]
  lines <- c(lines, sprintf("| %s | %s |", rr$drop_reason, format(rr$N, big.mark = ",")))
}

writeLines(lines, out_summary)

message("Saved:")
message(sprintf("  - %s", out_flags))
message(sprintf("  - %s", out_keep))
message(sprintf("  - %s", out_drop))
message(sprintf("  - %s", out_summary))
