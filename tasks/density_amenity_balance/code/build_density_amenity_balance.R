# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_amenity_balance/code")
# bandwidth_m <- 152.4
# sample_filter <- "all"
# bandwidth_label <- "500ft"

source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")
source("../../_lib/border_pair_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    bandwidth_m,
    sample_filter,
    bandwidth_label
  )
}

if (length(cli_args) != 3) {
  stop(
    "FATAL: Script requires 3 args: <bandwidth_m> <sample_filter> <bandwidth_label>.",
    call. = FALSE
  )
}

bandwidth_m <- as.numeric(cli_args[1])
sample_filter <- cli_args[2]
bandwidth_label <- cli_args[3]

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be a positive number.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample_filter must be one of: all, multifamily.", call. = FALSE)
}
min_cluster_ward_pairs <- 20L

distance_display <- distance_display_config()
covariate_catalog <- tibble(
  covariate = c(
    "floor_area_ratio",
    "dist_cbd_m",
    "nearest_school_dist_m",
    "nearest_park_dist_m",
    "nearest_major_road_dist_m",
    "lake_michigan_dist_m"
  ),
  covariate_label = c(
    "Zoned FAR",
    sprintf("Distance to CBD (%s)", distance_display$unit),
    sprintf("Distance to School (%s)", distance_display$unit),
    sprintf("Distance to Park (%s)", distance_display$unit),
    sprintf("Distance to Major Road (%s)", distance_display$unit),
    sprintf("Distance to Lake Michigan (%s)", distance_display$unit)
  ),
  balance_digits = c(
    2,
    0,
    0,
    0,
    0,
    0
  )
)
paired_covariate_catalog <- bind_rows(
  tibble(
    covariate = "is_planned_development",
    covariate_label = "PD share",
    balance_digits = 3
  ),
  covariate_catalog
)

parcel_geometry <- st_read("../input/parcels_with_geometry.gpkg", quiet = TRUE) %>%
  mutate(pin = as.character(pin)) %>%
  st_transform(3435)

coords_tbl <- parcel_geometry %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(pin, x, y)
if (anyDuplicated(coords_tbl$pin) > 0) {
  stop("Parcel geometry input must provide exactly one coordinate row per PIN.", call. = FALSE)
}

coords_sf <- st_as_sf(coords_tbl, coords = c("x", "y"), crs = 3435, remove = FALSE)
schools <- read_amenity_layer("../input/schools_2015.gpkg")
parks <- read_amenity_layer("../input/parks.gpkg")
major_streets <- read_amenity_layer("../input/major_streets.gpkg")
lake <- lake_michigan_geom("../input/gis_osm_water_a_free_1.shp")
cbd <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>%
  st_transform(3435)

coords_tbl$nearest_school_dist_ft <- nearest_distance_ft(coords_sf, schools, label = "density parcels")
coords_tbl$nearest_park_dist_ft <- nearest_distance_ft(coords_sf, parks, label = "density parcels")
coords_tbl$nearest_major_road_dist_ft <- nearest_distance_ft(coords_sf, major_streets, label = "density parcels")
coords_tbl$lake_michigan_dist_ft <- nearest_distance_ft(coords_sf, lake, label = "density parcels")
coords_tbl$dist_cbd_ft <- as.numeric(st_distance(coords_sf, cbd))
coords_tbl <- coords_tbl %>%
  mutate(
    nearest_school_dist_m = nearest_school_dist_ft * 0.3048,
    nearest_park_dist_m = nearest_park_dist_ft * 0.3048,
    nearest_major_road_dist_m = nearest_major_road_dist_ft * 0.3048,
    lake_michigan_dist_m = lake_michigan_dist_ft * 0.3048,
    dist_cbd_m = dist_cbd_ft * 0.3048
  )
amenity_distance_cols <- c(
  "nearest_school_dist_m",
  "nearest_park_dist_m",
  "nearest_major_road_dist_m",
  "lake_michigan_dist_m",
  "dist_cbd_m"
)
if (any(!is.finite(as.matrix(coords_tbl[amenity_distance_cols])))) {
  stop("Amenity distance construction produced non-finite distances.", call. = FALSE)
}

analysis_sample <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    zone_group = zone_group_from_code(zone_code),
    is_planned_development = as.integer(zone_group == "Planned Development"),
    score_side = case_when(
      signed_distance_m < 0 ~ "lenient",
      signed_distance_m > 0 ~ "strict",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(coords_tbl, by = "pin", relationship = "many-to-one")

missing_geometry_n <- analysis_sample %>%
  filter(is.na(x) | is.na(y)) %>%
  summarise(n = n_distinct(pin), .groups = "drop") %>%
  pull(n)
if (missing_geometry_n > 0) {
  stop(sprintf("Scored parcel sample has %s PINs missing parcel geometry.", missing_geometry_n), call. = FALSE)
}

analysis_sample <- analysis_sample %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    if (sample_filter == "all") unitscount > 0 else unitscount > 1,
    dist_to_boundary_m <= bandwidth_m,
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    ward_pair != "",
    !is.na(zone_group),
    density_far > 0,
    density_dupac > 0,
    !is.na(strictness_own),
    !is.na(score_side)
  )

paired_balance_rows <- bind_rows(lapply(seq_len(nrow(paired_covariate_catalog)), function(i) {
  covariate_name <- paired_covariate_catalog$covariate[[i]]
  side_means <- analysis_sample %>%
    mutate(balance_value = as.numeric(.data[[covariate_name]])) %>%
    filter(is.finite(balance_value), !is.na(score_side)) %>%
    group_by(ward_pair, segment_id, score_side) %>%
    summarise(
      side_mean = mean(balance_value),
      side_n = n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = score_side,
      values_from = c(side_mean, side_n)
    )

  if (!all(c("side_mean_lenient", "side_mean_strict") %in% names(side_means))) {
    stop(sprintf("Could not build paired side means for %s.", covariate_name), call. = FALSE)
  }

  paired <- side_means %>%
    filter(is.finite(side_mean_lenient), is.finite(side_mean_strict)) %>%
    mutate(difference = side_mean_strict - side_mean_lenient)

  if (nrow(paired) == 0) {
    stop(sprintf("No paired segments remain for %s.", covariate_name), call. = FALSE)
  }

  pooled_sd <- sqrt((var(paired$side_mean_lenient) + var(paired$side_mean_strict)) / 2)
  paired_test_result <- paired_balance_test(paired, min_cluster_ward_pairs)

  tibble(
    covariate = covariate_name,
    covariate_label = paired_covariate_catalog$covariate_label[[i]],
    lenient_mean = mean(paired$side_mean_lenient),
    strict_mean = mean(paired$side_mean_strict),
    difference = mean(paired$difference),
    se = paired_test_result$se[[1]],
    p_value = paired_test_result$p_value[[1]],
    normalized_difference = ifelse(is.finite(pooled_sd) && pooled_sd > 0, mean(paired$difference) / pooled_sd, NA_real_),
    n_segments = nrow(paired),
    n_ward_pairs = n_distinct(paired$ward_pair),
    n_lenient_obs = sum(paired$side_n_lenient),
    n_strict_obs = sum(paired$side_n_strict),
    balance_digits = paired_covariate_catalog$balance_digits[[i]]
  )
}))

sample_label <- ifelse(sample_filter == "all", "all-construction", "multifamily")

paired_tex_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\small",
  "\\resizebox{\\linewidth}{!}{%",
  "\\begin{tabular}{lrrrrrrrr}",
  "\\toprule",
  "Covariate & Less stringent mean & More stringent mean & Diff. & SE & $p$-value & Norm. diff. & Segments & Ward pairs \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(paired_balance_rows))) {
  row_i <- paired_balance_rows[i, ]
  digits_i <- row_i$balance_digits[[1]]
  lenient_mean <- row_i$lenient_mean[[1]]
  strict_mean <- row_i$strict_mean[[1]]
  difference <- row_i$difference[[1]]
  se <- row_i$se[[1]]
  p_value <- row_i$p_value[[1]]
  normalized_difference <- row_i$normalized_difference[[1]]
  n_segments <- row_i$n_segments[[1]]
  n_ward_pairs <- row_i$n_ward_pairs[[1]]

  lenient_display <- if (is.finite(lenient_mean)) formatC(lenient_mean, digits = digits_i, format = "f", big.mark = ",") else ""
  strict_display <- if (is.finite(strict_mean)) formatC(strict_mean, digits = digits_i, format = "f", big.mark = ",") else ""
  difference_display <- if (is.finite(difference)) formatC(difference, digits = digits_i, format = "f", big.mark = ",") else ""
  se_display <- if (is.finite(se)) formatC(se, digits = digits_i, format = "f", big.mark = ",") else ""
  p_value_display <- if (is.finite(p_value)) formatC(p_value, digits = 3, format = "f", big.mark = ",") else ""
  normalized_display <- if (is.finite(normalized_difference)) formatC(normalized_difference, digits = 3, format = "f", big.mark = ",") else ""
  n_segments_display <- if (is.finite(n_segments)) formatC(round(n_segments), digits = 0, format = "d", big.mark = ",") else ""
  n_ward_pairs_display <- if (is.finite(n_ward_pairs)) formatC(round(n_ward_pairs), digits = 0, format = "d", big.mark = ",") else ""

  paired_tex_lines <- c(
    paired_tex_lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
      row_i$covariate_label[[1]],
      lenient_display,
      strict_display,
      difference_display,
      se_display,
      p_value_display,
      normalized_display,
      n_segments_display,
      n_ward_pairs_display
    )
  )
}

paired_tex_lines <- c(
  paired_tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "}%",
  paste0(
    "\\par\\vspace{0.5em}\\parbox{0.94\\linewidth}{\\footnotesize Notes: ",
    bandwidth_label,
    " ",
    sample_label,
    " density sample, restricted to 2006--2022 new construction. Rows first collapse parcels to segment-by-side means, then compare more-stringent-side and less-stringent-side means within the same boundary segment. Difference is more stringent minus less stringent. Standard errors are clustered by ward pair across segment-level paired differences when at least ",
    min_cluster_ward_pairs,
    " ward pairs are available. Normalized differences divide the paired mean difference by the pooled standard deviation of the segment-side means. PD share is the segment-side share of parcels whose zone code is classified as Planned Development. Zoned FAR has fewer paired segments because planned developments do not map to a single zoning FAR value in the scored parcel file.}"
  ),
  "\\endgroup"
)

writeLines(
  paired_tex_lines,
  sprintf("../output/density_paired_balance_%s_%s.tex", bandwidth_label, sample_filter)
)
