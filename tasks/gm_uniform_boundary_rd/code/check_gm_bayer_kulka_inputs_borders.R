source("../../setup_environment/code/packages.R")

library(data.table)
library(readr)

parcels_path <- "../input/parcels_with_ward_distances.csv"
flags_path <- "../input/confounded_pair_era_flags.csv"
ref_plot_path <- "../output/bayer_kulka_construction/gm_bayer_kulka_construction_plot_detail_ft500_1000.csv"

out_dir <- "../output/bayer_kulka_construction/input_border_checks_ft500_1000"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

diag_out <- file.path(out_dir, "gm_bayer_kulka_input_border_diagnostics.csv")
unmatched_out <- file.path(out_dir, "gm_bayer_kulka_unmatched_pair_era_top.csv")
summary_out <- file.path(out_dir, "gm_bayer_kulka_input_border_checks_summary.md")

stopifnot(file.exists(parcels_path), file.exists(flags_path), file.exists(ref_plot_path))

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  out <- rep(NA_character_, length(x))
  ok <- grepl("^[0-9]+-[0-9]+$", x)
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

bandwidths <- data.table(
  bandwidth_label = c("500ft", "1000ft"),
  bandwidth_m = c(152.4, 304.8)
)
sample_tags <- c("all", "pruned")
outcomes <- c("density_far", "density_dupac")

message("Loading parcels...")
parcels <- as.data.table(read_csv(parcels_path, show_col_types = FALSE))

required_cols <- c(
  "ward_pair", "construction_year", "zone_code", "signed_distance",
  "arealotsf", "areabuilding", "unitscount", "density_far", "density_dupac"
)
missing_cols <- setdiff(required_cols, names(parcels))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns in parcels: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

parcels[, construction_year := as.integer(construction_year)]
parcels[, ward_pair := as.character(ward_pair)]
parcels[, signed_distance_m := as.numeric(signed_distance) * 0.3048]
parcels[, pair_dash := normalize_pair_dash(ward_pair)]
parcels[, era := era_from_year(construction_year)]

base <- parcels[
  arealotsf > 1 &
    areabuilding > 1 &
    construction_year >= 2006 &
    unitscount > 0 &
    is.finite(signed_distance_m) &
    !is.na(ward_pair) &
    !is.na(construction_year) &
    !is.na(pair_dash) &
    !is.na(era)
]

message("Loading confound flags...")
flags <- as.data.table(read_csv(
  flags_path,
  show_col_types = FALSE,
  col_select = c("ward_pair_id_dash", "era", "drop_confound", "drop_reason")
))
flags[, pair_dash := normalize_pair_dash(ward_pair_id_dash)]
flags[, era := as.character(era)]
flags[, drop_confound := as.logical(drop_confound)]

flags_valid <- flags[!is.na(pair_dash) & !is.na(era)]
flags_dup_n <- flags_valid[, .N, by = .(pair_dash, era)][N > 1, .N]
flags_valid <- unique(flags_valid[, .(pair_dash, era, drop_confound, drop_reason)])

base <- merge(
  base,
  flags_valid,
  by = c("pair_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)
base[, flag_matched := !is.na(drop_confound)]
base[is.na(drop_confound), `:=`(drop_confound = FALSE, drop_reason = "unmatched_default_keep")]

unmatched_pairs <- base[flag_matched == FALSE, .N, by = .(pair_dash, era)][order(-N)]
if (nrow(unmatched_pairs) == 0) {
  unmatched_pairs <- data.table(pair_dash = character(), era = character(), N = integer())
}
fwrite(head(unmatched_pairs, 50), unmatched_out)

message("Loading reference plot-detail for consistency checks...")
ref <- fread(ref_plot_path)
ref <- ref[
  sample_tag %in% sample_tags &
    transform == "log" &
    outcome %in% outcomes &
    bandwidth_label %in% bandwidths$bandwidth_label,
  .(sample_tag, fe_spec, outcome, bandwidth_label, n_obs_ref_model = as.integer(n_obs), p_value)
]

diag <- list()
add_diag <- function(name, value, note = "") {
  diag[[length(diag) + 1L]] <<- data.table(metric = name, value = as.character(value), note = note)
}

add_diag("parcels_rows_raw", nrow(parcels))
add_diag("parcels_rows_base_filtered", nrow(base))
add_diag("parcels_pair_dash_missing_raw", sum(is.na(parcels$pair_dash)))
add_diag("flags_rows_raw", nrow(flags))
add_diag("flags_rows_valid", nrow(flags_valid))
add_diag("flags_duplicate_pair_era_rows", flags_dup_n)

add_diag("base_flag_match_rate", sprintf("%.4f", mean(base$flag_matched)))
add_diag("base_rows_drop_confound_share", sprintf("%.4f", mean(base$drop_confound)))
add_diag("base_unique_pair_era", uniqueN(base[, .(pair_dash, era)]))
add_diag(
  "base_unique_pair_era_drop_share",
  sprintf("%.4f", base[, mean(any(drop_confound)), by = .(pair_dash, era)][, mean(V1)])
)
add_diag("base_unmatched_pair_era_n", nrow(unmatched_pairs))

for (bw_i in seq_len(nrow(bandwidths))) {
  bw_label <- bandwidths$bandwidth_label[bw_i]
  bw_m <- bandwidths$bandwidth_m[bw_i]
  for (sample_tag_val in sample_tags) {
    d_sample <- if (sample_tag_val == "pruned") base[drop_confound == FALSE] else base
    add_diag(
      sprintf("rows_%s_%s", sample_tag_val, bw_label),
      d_sample[abs(signed_distance_m) <= bw_m, .N]
    )
    for (outcome_val in outcomes) {
      add_diag(
        sprintf("rows_%s_%s_%s_positive", sample_tag_val, outcome_val, bw_label),
        d_sample[abs(signed_distance_m) <= bw_m & is.finite(get(outcome_val)) & get(outcome_val) > 0, .N]
      )
    }
  }
}

ref_check <- copy(ref)
bw_lookup <- setNames(bandwidths$bandwidth_m, bandwidths$bandwidth_label)
ref_check[, bandwidth_m := as.numeric(bw_lookup[bandwidth_label])]
ref_check[, n_obs_max_raw := integer(.N)]

for (i in seq_len(nrow(ref_check))) {
  rr <- ref_check[i]
  bw_m <- bandwidths[bandwidth_label == rr$bandwidth_label, bandwidth_m][1]
  d_sample <- if (rr$sample_tag == "pruned") base[drop_confound == FALSE] else base
  n_raw <- d_sample[
    abs(signed_distance_m) <= bw_m &
      is.finite(get(rr$outcome)) &
      get(rr$outcome) > 0,
    .N
  ]
  ref_check[i, n_obs_max_raw := as.integer(n_raw)]
}

ref_check[, feasible_ref_n_obs := n_obs_ref_model <= n_obs_max_raw]
n_infeasible <- ref_check[feasible_ref_n_obs == FALSE, .N]
add_diag("ref_rows_checked", nrow(ref_check))
add_diag("ref_infeasible_nobs_rows", n_infeasible)

fwrite(rbindlist(diag), diag_out)

summary_lines <- c(
  "# Input/Border Integrity Checks",
  "",
  sprintf("- Parcels raw rows: %d", nrow(parcels)),
  sprintf("- Base-filtered rows: %d", nrow(base)),
  sprintf("- Pair parsing failures in raw parcels: %d", sum(is.na(parcels$pair_dash))),
  sprintf("- Flags valid rows: %d", nrow(flags_valid)),
  sprintf("- Duplicate flag keys (pair_dash, era): %d", flags_dup_n),
  sprintf("- Flag match rate on base rows: %.4f", mean(base$flag_matched)),
  sprintf("- Drop-confound share on base rows: %.4f", mean(base$drop_confound)),
  sprintf("- Unique pair-era in base: %d", uniqueN(base[, .(pair_dash, era)])),
  sprintf("- Unique pair-era drop share: %.4f", base[, mean(any(drop_confound)), by = .(pair_dash, era)][, mean(V1)]),
  sprintf("- Unmatched pair-era count: %d", nrow(unmatched_pairs)),
  sprintf("- Reference rows checked (n_obs feasibility): %d", nrow(ref_check)),
  sprintf("- Reference rows with n_obs > max eligible raw rows: %d", n_infeasible),
  "",
  "## Bandwidth Support (Positive Outcomes)",
  ""
)

for (bw_i in seq_len(nrow(bandwidths))) {
  bw_label <- bandwidths$bandwidth_label[bw_i]
  bw_m <- bandwidths$bandwidth_m[bw_i]
  summary_lines <- c(summary_lines, sprintf("### %s (%.1fm)", bw_label, bw_m))
  for (sample_tag_val in sample_tags) {
    d_sample <- if (sample_tag_val == "pruned") base[drop_confound == FALSE] else base
    n_total <- d_sample[abs(signed_distance_m) <= bw_m, .N]
    n_far <- d_sample[abs(signed_distance_m) <= bw_m & density_far > 0 & is.finite(density_far), .N]
    n_dup <- d_sample[abs(signed_distance_m) <= bw_m & density_dupac > 0 & is.finite(density_dupac), .N]
    summary_lines <- c(
      summary_lines,
      sprintf("- %s: total=%d, FAR>0=%d, DUPAC>0=%d", sample_tag_val, n_total, n_far, n_dup)
    )
  }
  summary_lines <- c(summary_lines, "")
}

summary_lines <- c(
  summary_lines,
  "## Output Files",
  sprintf("- diagnostics: `%s`", diag_out),
  sprintf("- unmatched pair-era top: `%s`", unmatched_out),
  sprintf("- ref consistency table in memory checked against `%s`", ref_plot_path)
)

writeLines(summary_lines, summary_out)

message("Saved:")
message(sprintf("  - %s", diag_out))
message(sprintf("  - %s", unmatched_out))
message(sprintf("  - %s", summary_out))
