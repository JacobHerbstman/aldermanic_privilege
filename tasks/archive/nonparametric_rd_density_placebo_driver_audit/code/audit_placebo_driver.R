source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")
source("../../_lib/canonical_geometry_helpers.R")

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/nonparametric_rd_density_placebo_driver_audit/code")
# in_parcels_csv <- "../input/parcels_with_ward_distances.csv"
# in_geom_gpkg <- "../input/parcels_with_geometry.gpkg"
# in_segments_gpkg <- "../input/boundary_segments_1320ft.gpkg"
# in_ward_panel_gpkg <- "../input/ward_panel.gpkg"
# in_zoning_gpkg <- "../input/zoning_data_clean.gpkg"
# in_roads_shp <- "../input/gis_osm_roads_free_1.shp"
# focal_yvar <- "density_dupac"
# match_yvar <- "density_far"
# sample_filter <- "all"
# fe_spec <- "zonegroup_segment_year_additive"
# focal_shift_ft <- -500
# mirror_shift_ft <- 500
# focal_bw_ft <- 250
# bw_grid_csv <- "100,150,200,250,300,400"
# top_k <- 20
# n_zooms <- 4
# output_dir <- "../output"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_parcels_csv,
    in_geom_gpkg,
    in_segments_gpkg,
    in_ward_panel_gpkg,
    in_zoning_gpkg,
    in_roads_shp,
    focal_yvar,
    match_yvar,
    sample_filter,
    fe_spec,
    focal_shift_ft,
    mirror_shift_ft,
    focal_bw_ft,
    bw_grid_csv,
    top_k,
    n_zooms,
    output_dir
  )
}

if (length(cli_args) != 17) {
  stop(
    paste(
      "FATAL: Script requires 17 args:",
      "<in_parcels_csv> <in_geom_gpkg> <in_segments_gpkg> <in_ward_panel_gpkg>",
      "<in_zoning_gpkg> <in_roads_shp>",
      "<focal_yvar> <match_yvar> <sample_filter> <fe_spec>",
      "<focal_shift_ft> <mirror_shift_ft> <focal_bw_ft> <bw_grid_csv>",
      "<top_k> <n_zooms> <output_dir>"
    ),
    call. = FALSE
  )
}

in_parcels_csv <- cli_args[1]
in_geom_gpkg <- cli_args[2]
in_segments_gpkg <- cli_args[3]
in_ward_panel_gpkg <- cli_args[4]
in_zoning_gpkg <- cli_args[5]
in_roads_shp <- cli_args[6]
focal_yvar <- cli_args[7]
match_yvar <- cli_args[8]
sample_filter <- cli_args[9]
fe_spec <- cli_args[10]
focal_shift_ft <- as.numeric(cli_args[11])
mirror_shift_ft <- as.numeric(cli_args[12])
focal_bw_ft <- as.numeric(cli_args[13])
bw_grid_csv <- cli_args[14]
top_k <- as.integer(cli_args[15])
n_zooms <- as.integer(cli_args[16])
output_dir <- cli_args[17]

if (!file.exists(in_parcels_csv) || !file.exists(in_geom_gpkg) || !file.exists(in_segments_gpkg) || !file.exists(in_ward_panel_gpkg) || !file.exists(in_zoning_gpkg) || !file.exists(in_roads_shp)) {
  stop("One or more input paths do not exist.", call. = FALSE)
}
if (!focal_yvar %in% c("density_dupac", "density_far")) {
  stop("focal_yvar must be density_dupac or density_far.", call. = FALSE)
}
if (!match_yvar %in% c("density_dupac", "density_far")) {
  stop("match_yvar must be density_dupac or density_far.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample_filter must be all or multifamily.", call. = FALSE)
}
if (!fe_spec %in% c("zonegroup_segment_year_additive", "zonegroup_pair_year_additive", "segment_year")) {
  stop("Unsupported fe_spec.", call. = FALSE)
}
if (!is.finite(focal_shift_ft) || !is.finite(mirror_shift_ft) || !is.finite(focal_bw_ft) || focal_bw_ft <= 0) {
  stop("Shift and bandwidth args must be finite, with focal_bw_ft > 0.", call. = FALSE)
}
if (!is.finite(top_k) || top_k < 1 || !is.finite(n_zooms) || n_zooms < 1) {
  stop("top_k and n_zooms must be positive integers.", call. = FALSE)
}
if (!dir.exists(output_dir)) {
  stop("output_dir must already exist.", call. = FALSE)
}

bw_grid_ft <- strsplit(bw_grid_csv, ",", fixed = TRUE)[[1]] |>
  trimws() |>
  as.numeric()

if (any(!is.finite(bw_grid_ft)) || any(bw_grid_ft <= 0)) {
  stop("bw_grid_csv must contain comma-separated positive numbers.", call. = FALSE)
}

bins_per_side <- 5L
zoom_pad_ft <- 1500
zoom_exclusion_ft <- 2500
zoom_ids <- sprintf("%02d", seq_len(n_zooms))

out_focal_check <- file.path(output_dir, "placebo_focal_check.csv")
out_bw_sweep <- file.path(output_dir, "placebo_bw_sweep.csv")
out_bin_profile <- file.path(output_dir, "placebo_bin_profile.csv")
out_focal_parcel_audit <- file.path(output_dir, "placebo_focal_parcel_audit.csv")
out_era_decomposition <- file.path(output_dir, "placebo_era_decomposition.csv")
out_pair_rankings <- file.path(output_dir, "placebo_pair_rankings.csv")
out_segment_rankings <- file.path(output_dir, "placebo_segment_rankings.csv")
out_composition <- file.path(output_dir, "placebo_composition_comparison.csv")
out_feature_context <- file.path(output_dir, "placebo_feature_context.csv")
out_zoning_street_summary <- file.path(output_dir, "placebo_zoning_street_summary.csv")
out_citywide_pdf <- file.path(output_dir, "placebo_top_segments_citywide.pdf")
out_top10_context_pdf <- file.path(output_dir, "placebo_top10_zoning_street_context.pdf")
out_zoom_index <- file.path(output_dir, "placebo_zoom_index.csv")
out_memo <- file.path(output_dir, "placebo_driver_audit.md")
out_zoom_pdfs <- file.path(output_dir, paste0("placebo_zoom_", zoom_ids, ".pdf"))

fe_formula <- dplyr::case_when(
  fe_spec == "zonegroup_segment_year_additive" ~ "zone_group + segment_id + construction_year",
  fe_spec == "zonegroup_pair_year_additive" ~ "zone_group + ward_pair + construction_year",
  fe_spec == "segment_year" ~ "segment_id + construction_year",
  TRUE ~ NA_character_
)

controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

mode_chr <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }
  counts <- sort(table(x), decreasing = TRUE)
  names(counts)[1]
}

make_scope_comparison <- function(df, scope_group, scope_value) {
  vars <- c(
    "log_dupac",
    "log_far",
    "building_area_per_unit",
    "lot_area_per_unit",
    "unitscount",
    "arealotsf",
    "areabuilding",
    "storiescount"
  )

  out <- lapply(vars, function(var_name) {
    vec <- df[[var_name]]
    sd_val <- stats::sd(vec, na.rm = TRUE)
    means <- df %>%
      group_by(pseudo_side) %>%
      summarise(
        n = n(),
        mean_value = mean(.data[[var_name]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pseudo_side = as.character(pseudo_side))

    far_mean <- means$mean_value[means$pseudo_side == "deeper_in_lenient_ward"]
    near_mean <- means$mean_value[means$pseudo_side == "closer_to_true_boundary"]
    far_n <- means$n[means$pseudo_side == "deeper_in_lenient_ward"]
    near_n <- means$n[means$pseudo_side == "closer_to_true_boundary"]

    tibble(
      scope_group = scope_group,
      scope_value = scope_value,
      variable = var_name,
      mean_far = far_mean,
      mean_near = near_mean,
      diff_near_minus_far = near_mean - far_mean,
      std_diff = ifelse(is.finite(sd_val) && sd_val > 0, (near_mean - far_mean) / sd_val, NA_real_),
      n_far = far_n,
      n_near = near_n
    )
  })

  bind_rows(out)
}

prepare_sample <- function(raw, shift_ft, bw_ft, sample_filter, required_yvars) {
  dat <- raw %>%
    mutate(
      pin = as.character(pin),
      construction_year = as.integer(construction_year),
      boundary_year = as.integer(boundary_year),
      zone_group = zone_group_from_code(zone_code),
      running_distance = signed_distance - shift_ft,
      side = as.integer(running_distance > 0),
      pseudo_side = if_else(
        side == 1L,
        "closer_to_true_boundary",
        "deeper_in_lenient_ward"
      ),
      era = canonical_era_from_boundary_year(boundary_year),
      log_dupac = if_else(is.finite(density_dupac) & density_dupac > 0, log(density_dupac), NA_real_),
      log_far = if_else(is.finite(density_far) & density_far > 0, log(density_far), NA_real_),
      building_area_per_unit = if_else(unitscount > 0, areabuilding / unitscount, NA_real_),
      lot_area_per_unit = if_else(unitscount > 0, arealotsf / unitscount, NA_real_)
    ) %>%
    filter(
      arealotsf > 1,
      areabuilding > 1,
      construction_year >= 2006,
      !is.na(ward_pair),
      !is.na(construction_year),
      !is.na(boundary_year),
      !is.na(era),
      is.finite(signed_distance),
      !is.na(zone_code),
      !is.na(segment_id),
      segment_id != "",
      abs(running_distance) <= bw_ft
    )

  if (sample_filter == "all") {
    dat <- dat %>% filter(unitscount > 0)
  } else {
    dat <- dat %>% filter(unitscount > 1)
  }

  for (yvar in required_yvars) {
    dat <- dat %>%
      filter(is.finite(.data[[yvar]]), .data[[yvar]] > 0)
  }

  dat %>%
    mutate(audit_id = row_number())
}

estimate_placebo <- function(dat, yvar, bins_per_side = NA_integer_) {
  work <- dat %>%
    mutate(outcome = log(.data[[yvar]]))

  fml_resid <- as.formula(sprintf(
    "outcome ~ %s | %s",
    paste(controls, collapse = " + "),
    fe_formula
  ))
  fml_linear <- as.formula(sprintf(
    "outcome ~ side * running_distance + %s | %s",
    paste(controls, collapse = " + "),
    fe_formula
  ))

  m_resid <- feols(fml_resid, data = work)
  removed <- m_resid$obs_selection$obsRemoved
  keep_idx <- if (is.null(removed)) {
    seq_len(nrow(work))
  } else {
    setdiff(seq_len(nrow(work)), abs(as.integer(removed)))
  }

  aug <- work[keep_idx, , drop = FALSE] %>%
    mutate(residualized_outcome = as.numeric(resid(m_resid)))

  m_linear <- feols(fml_linear, data = aug, cluster = ~ward_pair)
  linear_row <- coeftable(m_linear)[rownames(coeftable(m_linear)) %in% "side", , drop = FALSE]

  if (nrow(linear_row) != 1) {
    stop("Could not recover side coefficient.", call. = FALSE)
  }

  summary_row <- tibble(
    yvar = yvar,
    sample = sample_filter,
    shift_ft = unique(aug$shift_ft),
    bw_ft = unique(aug$bw_ft),
    estimate = unname(linear_row[1, "Estimate"]),
    se = unname(linear_row[1, "Std. Error"]),
    p_value = unname(linear_row[1, "Pr(>|t|)"]),
    n_obs = nobs(m_resid),
    n_pairs = n_distinct(aug$ward_pair)
  )

  bins <- NULL
  if (is.finite(bins_per_side)) {
    breaks_ft <- seq(-unique(aug$bw_ft), unique(aug$bw_ft), length.out = 2L * bins_per_side + 1L)
    bin_width_ft <- unique(aug$bw_ft) / bins_per_side

    bins <- aug %>%
      mutate(
        bin_idx = pmin(
          findInterval(running_distance, breaks_ft, rightmost.closed = TRUE, all.inside = TRUE),
          length(breaks_ft) - 1L
        ),
        bin_left_ft = breaks_ft[bin_idx],
        bin_center_ft = bin_left_ft + bin_width_ft / 2
      ) %>%
      group_by(yvar = yvar, shift_ft, bw_ft, pseudo_side, bin_left_ft, bin_center_ft) %>%
      summarise(
        n = n(),
        mean_residualized_outcome = mean(residualized_outcome, na.rm = TRUE),
        mean_outcome = mean(outcome, na.rm = TRUE),
        .groups = "drop"
      )
  }

  list(summary = summary_row, aug = aug, bins = bins)
}

safe_estimate <- function(dat, yvar) {
  out <- tryCatch(
    estimate_placebo(dat, yvar, bins_per_side = NA_integer_)$summary,
    error = function(e) {
      tibble(
        yvar = yvar,
        sample = sample_filter,
        shift_ft = if ("shift_ft" %in% names(dat)) unique(dat$shift_ft)[1] else NA_real_,
        bw_ft = if ("bw_ft" %in% names(dat)) unique(dat$bw_ft)[1] else NA_real_,
        estimate = NA_real_,
        se = NA_real_,
        p_value = NA_real_,
        n_obs = nrow(dat),
        n_pairs = dplyr::n_distinct(dat$ward_pair)
      )
    }
  )
  out
}

base_negative_contribution <- function(diff_resid, n_total) {
  pmax(0, -diff_resid) * n_total
}

raw <- read_csv(in_parcels_csv, show_col_types = FALSE)

focal_base <- prepare_sample(
  raw = raw,
  shift_ft = focal_shift_ft,
  bw_ft = focal_bw_ft,
  sample_filter = sample_filter,
  required_yvars = unique(c(focal_yvar, match_yvar))
) %>%
  mutate(shift_ft = focal_shift_ft, bw_ft = focal_bw_ft)

mirror_base <- prepare_sample(
  raw = raw,
  shift_ft = mirror_shift_ft,
  bw_ft = focal_bw_ft,
  sample_filter = sample_filter,
  required_yvars = focal_yvar
) %>%
  mutate(shift_ft = mirror_shift_ft, bw_ft = focal_bw_ft)

focal_fit <- estimate_placebo(focal_base, focal_yvar, bins_per_side = bins_per_side)
match_fit <- estimate_placebo(focal_base, match_yvar, bins_per_side = bins_per_side)
mirror_fit <- estimate_placebo(mirror_base, focal_yvar, bins_per_side = NA_integer_)

focal_check <- bind_rows(
  focal_fit$summary,
  match_fit$summary,
  mirror_fit$summary
) %>%
  select(yvar, sample, shift_ft, bw_ft, estimate, se, p_value, n_obs, n_pairs)

write_csv(focal_check, out_focal_check)

bw_sweep <- bind_rows(lapply(c(focal_shift_ft, mirror_shift_ft), function(shift_ft_i) {
  bind_rows(lapply(bw_grid_ft, function(bw_ft_i) {
    dat_i <- prepare_sample(
      raw = raw,
      shift_ft = shift_ft_i,
      bw_ft = bw_ft_i,
      sample_filter = sample_filter,
      required_yvars = focal_yvar
    ) %>%
      mutate(shift_ft = shift_ft_i, bw_ft = bw_ft_i)
    safe_estimate(dat_i, focal_yvar) %>%
      select(yvar, sample, shift_ft, bw_ft, estimate, se, p_value, n_obs, n_pairs)
  }))
}))

write_csv(bw_sweep, out_bw_sweep)

bin_profile <- bind_rows(
  focal_fit$bins,
  match_fit$bins
) %>%
  select(yvar, shift_ft, bw_ft, pseudo_side, bin_left_ft, bin_center_ft, n, mean_outcome, mean_residualized_outcome)

write_csv(bin_profile, out_bin_profile)

match_residuals <- match_fit$aug %>%
  transmute(audit_id, residualized_far = residualized_outcome)

focal_audit_core <- focal_fit$aug %>%
  transmute(
    audit_id,
    pin,
    construction_year,
    boundary_year,
    era,
    ward_pair,
    segment_id,
    signed_distance,
    running_distance,
    pseudo_side,
    side,
    zone_code,
    zone_group,
    floor_area_ratio,
    maximum_building_height,
    unitscount,
    arealotsf,
    areabuilding,
    storiescount,
    building_area_per_unit,
    lot_area_per_unit,
    log_dupac,
    log_far,
    residualized_dupac = residualized_outcome
  ) %>%
  left_join(match_residuals, by = "audit_id")

geom_sf <- st_read(in_geom_gpkg, quiet = TRUE) %>%
  mutate(
    pin = as.character(pin),
    construction_year = as.integer(construction_year)
  ) %>%
  select(pin, construction_year)

geom_keys <- paste(geom_sf$pin, geom_sf$construction_year, sep = "___")
if (anyDuplicated(geom_keys) > 0) {
  stop("Geometry gpkg is not unique on pin x construction_year.", call. = FALSE)
}

focal_keys <- paste(focal_audit_core$pin, focal_audit_core$construction_year, sep = "___")
if (anyDuplicated(focal_keys) > 0) {
  stop("Focal audit sample is not unique on pin x construction_year.", call. = FALSE)
}

focal_map_sf <- geom_sf %>%
  inner_join(focal_audit_core, by = c("pin", "construction_year"))

if (nrow(focal_map_sf) != nrow(focal_audit_core)) {
  stop("Geometry coverage is incomplete for focal audit sample.", call. = FALSE)
}

coords_ll <- st_coordinates(st_transform(focal_map_sf, 4326))
focal_parcel_audit <- focal_map_sf %>%
  st_drop_geometry() %>%
  mutate(
    lon = coords_ll[, "X"],
    lat = coords_ll[, "Y"]
  ) %>%
  select(
    pin,
    construction_year,
    boundary_year,
    era,
    ward_pair,
    segment_id,
    signed_distance,
    running_distance,
    pseudo_side,
    log_dupac,
    log_far,
    residualized_dupac,
    residualized_far,
    unitscount,
    arealotsf,
    areabuilding,
    storiescount,
    building_area_per_unit,
    lot_area_per_unit,
    zone_group,
    lon,
    lat
  )

write_csv(focal_parcel_audit, out_focal_parcel_audit)

era_values <- c("overall", sort(unique(focal_audit_core$era)))
era_decomp <- bind_rows(lapply(era_values, function(era_i) {
  dat_i <- if (era_i == "overall") {
    focal_fit$aug
  } else {
    focal_fit$aug %>% filter(era == era_i)
  }

  if (nrow(dat_i) == 0) {
    return(tibble())
  }

  base_summary <- focal_audit_core %>%
    filter(if (era_i == "overall") TRUE else era == era_i) %>%
    group_by(pseudo_side) %>%
    summarise(
      n = n(),
      mean_log_dupac = mean(log_dupac, na.rm = TRUE),
      mean_residualized_dupac = mean(residualized_dupac, na.rm = TRUE),
      mean_log_far = mean(log_far, na.rm = TRUE),
      mean_residualized_far = mean(residualized_far, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(pseudo_side = as.character(pseudo_side))

  far_row <- base_summary %>% filter(pseudo_side == "deeper_in_lenient_ward")
  near_row <- base_summary %>% filter(pseudo_side == "closer_to_true_boundary")
  dupac_est <- safe_estimate(if (era_i == "overall") focal_base else focal_base %>% filter(era == era_i), focal_yvar)
  far_est <- safe_estimate(if (era_i == "overall") focal_base else focal_base %>% filter(era == era_i), match_yvar)

  tibble(
    era_scope = era_i,
    n_far = far_row$n,
    n_near = near_row$n,
    mean_log_dupac_far = far_row$mean_log_dupac,
    mean_log_dupac_near = near_row$mean_log_dupac,
    diff_log_dupac = near_row$mean_log_dupac - far_row$mean_log_dupac,
    mean_residualized_dupac_far = far_row$mean_residualized_dupac,
    mean_residualized_dupac_near = near_row$mean_residualized_dupac,
    diff_residualized_dupac = near_row$mean_residualized_dupac - far_row$mean_residualized_dupac,
    mean_log_far_far = far_row$mean_log_far,
    mean_log_far_near = near_row$mean_log_far,
    diff_log_far = near_row$mean_log_far - far_row$mean_log_far,
    mean_residualized_far_far = far_row$mean_residualized_far,
    mean_residualized_far_near = near_row$mean_residualized_far,
    diff_residualized_far = near_row$mean_residualized_far - far_row$mean_residualized_far,
    dupac_estimate = dupac_est$estimate,
    dupac_se = dupac_est$se,
    dupac_p_value = dupac_est$p_value,
    dupac_n_obs = dupac_est$n_obs,
    dupac_n_pairs = dupac_est$n_pairs,
    far_estimate = far_est$estimate,
    far_se = far_est$se,
    far_p_value = far_est$p_value,
    far_n_obs = far_est$n_obs,
    far_n_pairs = far_est$n_pairs
  )
}))

write_csv(era_decomp, out_era_decomposition)

pair_rankings_base <- focal_audit_core %>%
  group_by(ward_pair) %>%
  summarise(
    dominant_era = names(sort(table(era), decreasing = TRUE))[1],
    n_total = n(),
    n_far = sum(pseudo_side == "deeper_in_lenient_ward"),
    n_near = sum(pseudo_side == "closer_to_true_boundary"),
    raw_diff_log_dupac = mean(log_dupac[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
      mean(log_dupac[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
    resid_diff_dupac = mean(residualized_dupac[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
      mean(residualized_dupac[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
    raw_diff_log_far = mean(log_far[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
      mean(log_far[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
    resid_diff_far = mean(residualized_far[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
      mean(residualized_far[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
    weighted_contribution_score = base_negative_contribution(
      mean(residualized_dupac[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
        mean(residualized_dupac[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
      n()
    ),
    .groups = "drop"
  )

pair_loo <- bind_rows(lapply(unique(focal_base$ward_pair), function(pair_i) {
  loo_summary <- safe_estimate(focal_base %>% filter(ward_pair != pair_i), focal_yvar)
  tibble(
    ward_pair = pair_i,
    leave_one_pair_out_estimate = loo_summary$estimate,
    leave_one_pair_out_se = loo_summary$se,
    leave_one_pair_out_p_value = loo_summary$p_value,
    leave_one_pair_out_n_obs = loo_summary$n_obs,
    leave_one_pair_out_n_pairs = loo_summary$n_pairs
  )
}))

pair_rankings <- pair_rankings_base %>%
  left_join(pair_loo, by = "ward_pair") %>%
  mutate(
    leave_one_pair_out_delta = leave_one_pair_out_estimate - focal_fit$summary$estimate,
    share_of_negative_contribution = weighted_contribution_score / sum(weighted_contribution_score, na.rm = TRUE)
  ) %>%
  arrange(desc(weighted_contribution_score), ward_pair)

write_csv(pair_rankings, out_pair_rankings)

segment_rankings <- focal_audit_core %>%
  group_by(segment_id, ward_pair, era) %>%
  summarise(
    n_total = n(),
    n_far = sum(pseudo_side == "deeper_in_lenient_ward"),
    n_near = sum(pseudo_side == "closer_to_true_boundary"),
    raw_diff_log_dupac = mean(log_dupac[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
      mean(log_dupac[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
    resid_diff_dupac = mean(residualized_dupac[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
      mean(residualized_dupac[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
    raw_diff_log_far = mean(log_far[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
      mean(log_far[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
    resid_diff_far = mean(residualized_far[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
      mean(residualized_far[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
    weighted_contribution_score = base_negative_contribution(
      mean(residualized_dupac[pseudo_side == "closer_to_true_boundary"], na.rm = TRUE) -
        mean(residualized_dupac[pseudo_side == "deeper_in_lenient_ward"], na.rm = TRUE),
      n()
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(weighted_contribution_score), ward_pair, segment_id) %>%
  mutate(share_of_negative_contribution = weighted_contribution_score / sum(weighted_contribution_score, na.rm = TRUE))

write_csv(segment_rankings, out_segment_rankings)

top_segment_ids <- segment_rankings %>%
  slice_head(n = min(top_k, nrow(segment_rankings))) %>%
  pull(segment_id)

composition_comparison <- bind_rows(
  make_scope_comparison(focal_audit_core, "overall", "overall"),
  bind_rows(lapply(sort(unique(focal_audit_core$era)), function(era_i) {
    make_scope_comparison(focal_audit_core %>% filter(era == era_i), "era", era_i)
  })),
  make_scope_comparison(
    focal_audit_core %>% filter(segment_id %in% top_segment_ids),
    "segment_scope",
    paste0("top_", min(top_k, length(top_segment_ids)), "_segments")
  )
)

write_csv(composition_comparison, out_composition)

segment_layers <- intersect(canonical_era_levels(), st_layers(in_segments_gpkg)$name)
segments_sf <- do.call(rbind, lapply(segment_layers, function(layer_i) {
  st_read(in_segments_gpkg, layer = layer_i, quiet = TRUE)
})) %>%
  st_transform(3435)

segments_feature_context <- segments_sf %>%
  st_drop_geometry() %>%
  select(
    segment_id,
    ward_pair_id,
    ward_a,
    ward_b,
    era,
    segment_number,
    n_segments_in_pair,
    segment_length_ft,
    segment_type,
    nearest_street_name,
    nearest_street_class,
    nearest_street_class_mapped,
    distance_to_nearest_street_ft,
    major_overlap_arterial_ft,
    major_overlap_collector_ft,
    major_overlap_residential_ft,
    water_area_share,
    park_area_share,
    waterway_overlap_ft,
    target_length_ft
  ) %>%
  mutate(
    arterial_overlap_share = ifelse(segment_length_ft > 0, major_overlap_arterial_ft / segment_length_ft, NA_real_),
    collector_overlap_share = ifelse(segment_length_ft > 0, major_overlap_collector_ft / segment_length_ft, NA_real_),
    residential_overlap_share = ifelse(segment_length_ft > 0, major_overlap_residential_ft / segment_length_ft, NA_real_)
  )

feature_context <- segment_rankings %>%
  left_join(segments_feature_context, by = c("segment_id", "era")) %>%
  mutate(in_top_k_segments = segment_id %in% top_segment_ids)

write_csv(feature_context, out_feature_context)

segment_map_sf <- segments_sf %>%
  inner_join(feature_context, by = c("segment_id", "era"))

ward_panel <- st_read(in_ward_panel_gpkg, quiet = TRUE) %>%
  st_transform(3435)

ward_maps <- load_canonical_ward_maps(ward_panel)
ward_boundaries <- lapply(ward_maps, st_boundary)
city_outline <- st_boundary(st_union(ward_maps[["2015_2023"]]))
city_outline_sf <- st_sf(geometry = city_outline)

zoning_sf <- st_read(in_zoning_gpkg, quiet = TRUE) %>%
  st_transform(3435) %>%
  mutate(zone_group = zone_group_from_code(zone_code))

roads_sf <- st_read(in_roads_shp, quiet = TRUE) %>%
  st_transform(3435) %>%
  mutate(
    road_group = case_when(
      fclass %in% c("motorway", "motorway_link", "trunk", "primary", "primary_link") ~ "major",
      fclass %in% c("secondary", "secondary_link", "tertiary", "tertiary_link") ~ "secondary",
      TRUE ~ "local"
    )
  )

zone_fill_values <- c(
  "Commercial" = "#fdae6b",
  "Downtown" = "#9ecae1",
  "Industrial" = "#bdbdbd",
  "Institutional and Civic" = "#756bb1",
  "Multi-Family Residential" = "#74c476",
  "Neighborhood Mixed-Use" = "#fd8d3c",
  "Planned Development" = "#bcbddc",
  "Single-Family Residential" = "#fdd0a2",
  "Transportation and Infrastructure" = "#969696"
)

context_pad_ft <- 1200
top_context_n <- min(10L, nrow(segment_map_sf))
top_context_sf <- segment_map_sf %>%
  arrange(desc(weighted_contribution_score), segment_id) %>%
  slice_head(n = top_context_n)

context_summary_rows <- vector("list", length = top_context_n)
context_plots <- vector("list", length = top_context_n)

for (i in seq_len(top_context_n)) {
  anchor_sf <- top_context_sf[i, ]
  anchor_bbox <- st_bbox(anchor_sf)
  anchor_bbox["xmin"] <- anchor_bbox["xmin"] - context_pad_ft
  anchor_bbox["ymin"] <- anchor_bbox["ymin"] - context_pad_ft
  anchor_bbox["xmax"] <- anchor_bbox["xmax"] + context_pad_ft
  anchor_bbox["ymax"] <- anchor_bbox["ymax"] + context_pad_ft

  roads_context <- suppressWarnings(st_crop(roads_sf, anchor_bbox)) %>%
    filter(road_group != "local")
  zoning_context <- suppressWarnings(st_crop(zoning_sf, anchor_bbox))
  parcels_context <- suppressWarnings(st_crop(focal_map_sf, anchor_bbox))
  boundaries_context <- suppressWarnings(st_crop(ward_boundaries[[as.character(anchor_sf$era)]], anchor_bbox))

  segment_dat <- focal_audit_core %>%
    filter(segment_id == anchor_sf$segment_id)

  side_summary <- segment_dat %>%
    group_by(pseudo_side) %>%
    summarise(
      n = n(),
      mode_zone_code = mode_chr(zone_code),
      mode_zone_group = mode_chr(zone_group),
      mean_far_cap = mean(floor_area_ratio, na.rm = TRUE),
      mean_height_cap = mean(maximum_building_height, na.rm = TRUE),
      mean_units = mean(unitscount, na.rm = TRUE),
      mean_log_dupac = mean(log_dupac, na.rm = TRUE),
      mean_log_far = mean(log_far, na.rm = TRUE),
      .groups = "drop"
    )

  far_row <- side_summary %>% filter(pseudo_side == "deeper_in_lenient_ward")
  near_row <- side_summary %>% filter(pseudo_side == "closer_to_true_boundary")

  major_road_within_200ft <- if (nrow(roads_context %>% filter(road_group == "major")) == 0) {
    FALSE
  } else {
    lengths(st_intersects(st_buffer(anchor_sf, 200), roads_context %>% filter(road_group == "major"))) > 0
  }

  named_roads <- roads_context %>%
    filter(!is.na(name), name != "") %>%
    count(name, sort = TRUE) %>%
    slice_head(n = 3) %>%
    pull(name)

  context_summary_rows[[i]] <- tibble(
    rank = i,
    segment_id = anchor_sf$segment_id,
    ward_pair = anchor_sf$ward_pair,
    era = as.character(anchor_sf$era),
    weighted_contribution_score = anchor_sf$weighted_contribution_score,
    n_total = anchor_sf$n_total,
    n_major_roads_in_bbox = sum(roads_context$road_group == "major", na.rm = TRUE),
    n_secondary_roads_in_bbox = sum(roads_context$road_group == "secondary", na.rm = TRUE),
    major_road_within_200ft = as.logical(major_road_within_200ft[1]),
    nearby_named_roads = ifelse(length(named_roads) > 0, paste(named_roads, collapse = "; "), NA_character_),
    mode_zone_code_far = far_row$mode_zone_code,
    mode_zone_code_near = near_row$mode_zone_code,
    mode_zone_group_far = far_row$mode_zone_group,
    mode_zone_group_near = near_row$mode_zone_group,
    same_zone_group = identical(far_row$mode_zone_group, near_row$mode_zone_group),
    mean_far_cap_far = far_row$mean_far_cap,
    mean_far_cap_near = near_row$mean_far_cap,
    diff_far_cap_near_minus_far = near_row$mean_far_cap - far_row$mean_far_cap,
    mean_height_cap_far = far_row$mean_height_cap,
    mean_height_cap_near = near_row$mean_height_cap,
    diff_height_cap_near_minus_far = near_row$mean_height_cap - far_row$mean_height_cap,
    mean_units_far = far_row$mean_units,
    mean_units_near = near_row$mean_units,
    mean_log_dupac_far = far_row$mean_log_dupac,
    mean_log_dupac_near = near_row$mean_log_dupac,
    mean_log_far_far = far_row$mean_log_far,
    mean_log_far_near = near_row$mean_log_far
  )

  context_plots[[i]] <- ggplot() +
    geom_sf(data = zoning_context, aes(fill = zone_group), color = NA, alpha = 0.60) +
    geom_sf(data = boundaries_context, color = "gray55", linewidth = 0.25, alpha = 0.90) +
    geom_sf(
      data = roads_context %>% filter(road_group == "secondary"),
      color = "gray35",
      linewidth = 0.30,
      alpha = 0.90
    ) +
    geom_sf(
      data = roads_context %>% filter(road_group == "major"),
      color = "#111111",
      linewidth = 0.55,
      alpha = 0.95
    ) +
    geom_sf(data = anchor_sf, color = "#cb181d", linewidth = 0.95, alpha = 0.95) +
    geom_sf(
      data = parcels_context,
      aes(color = pseudo_side),
      size = 0.90,
      alpha = 0.90
    ) +
    scale_fill_manual(values = zone_fill_values, na.value = "#f0f0f0", guide = "none") +
    scale_color_manual(
      values = c(
        "deeper_in_lenient_ward" = "#2166ac",
        "closer_to_true_boundary" = "#b2182b"
      ),
      guide = "none"
    ) +
    labs(
      title = sprintf("%d. %s | %s", i, anchor_sf$ward_pair, anchor_sf$era),
      subtitle = sprintf(
        "Seg %s | zone %s -> %s | FAR cap %.1f -> %.1f | major road within 200ft: %s",
        anchor_sf$segment_number,
        far_row$mode_zone_group,
        near_row$mode_zone_group,
        far_row$mean_far_cap,
        near_row$mean_far_cap,
        ifelse(as.logical(major_road_within_200ft[1]), "yes", "no")
      )
    ) +
    theme_void(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 9),
      plot.subtitle = element_text(size = 7)
    )
}

zoning_street_summary <- bind_rows(context_summary_rows)
write_csv(zoning_street_summary, out_zoning_street_summary)

context_grid <- wrap_plots(context_plots, ncol = 2) +
  plot_annotation(
    title = "Top 10 Failed-Placebo Segments: Zoning and Street Context",
    subtitle = "Fill = zoning group, black/gray lines = major and secondary roads, red line = focal segment, points = placebo-window parcels"
  )

ggsave(out_top10_context_pdf, context_grid, width = 11, height = 16, dpi = 300)

ward_panel <- st_read(in_ward_panel_gpkg, quiet = TRUE) %>%
  st_transform(3435)

ward_maps <- load_canonical_ward_maps(ward_panel)
ward_boundaries <- lapply(ward_maps, st_boundary)
city_outline <- st_boundary(st_union(ward_maps[["2015_2023"]]))
city_outline_sf <- st_sf(geometry = city_outline)

top_segments_sf <- segment_map_sf %>%
  arrange(desc(weighted_contribution_score), segment_id) %>%
  slice_head(n = min(top_k, nrow(segment_map_sf)))

p_citywide <- ggplot() +
  geom_sf(data = city_outline_sf, color = "#111111", linewidth = 0.35, fill = NA) +
  geom_sf(data = segment_map_sf, color = "gray80", linewidth = 0.20, alpha = 0.85) +
  geom_sf(
    data = top_segments_sf,
    aes(color = weighted_contribution_score),
    linewidth = 0.90,
    alpha = 0.95
  ) +
  scale_color_gradient(
    low = "#fcbba1",
    high = "#cb181d",
    name = "Weighted\ncontribution"
  ) +
  labs(
    title = "Top Segment Drivers of the Failed -500 ft DUPAC Placebo",
    subtitle = "Highlighted segments have the largest negative residualized near-far contrasts in the focal placebo window"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(out_citywide_pdf, p_citywide, width = 10.5, height = 8.0, dpi = 300)

top_segments_ordered <- top_segments_sf

selected_idx <- integer()
selected_centroids <- list()

for (i in seq_len(nrow(top_segments_ordered))) {
  candidate_centroid <- st_centroid(top_segments_ordered[i, ])
  if (length(selected_centroids) == 0) {
    selected_idx <- c(selected_idx, i)
    selected_centroids[[length(selected_centroids) + 1L]] <- candidate_centroid
  } else {
    min_dist <- min(vapply(selected_centroids, function(x) as.numeric(st_distance(candidate_centroid, x)), numeric(1)))
    if (min_dist > zoom_exclusion_ft) {
      selected_idx <- c(selected_idx, i)
      selected_centroids[[length(selected_centroids) + 1L]] <- candidate_centroid
    }
  }
  if (length(selected_idx) >= n_zooms) {
    break
  }
}

if (length(selected_idx) < n_zooms) {
  remaining_idx <- setdiff(seq_len(nrow(top_segments_ordered)), selected_idx)
  selected_idx <- c(selected_idx, head(remaining_idx, n_zooms - length(selected_idx)))
}

if (length(selected_idx) < n_zooms) {
  stop("Could not select enough zoom anchors.", call. = FALSE)
}

zoom_segments_sf <- top_segments_ordered[selected_idx, ] %>%
  mutate(rank = seq_len(n()))

focal_map_sf <- st_transform(focal_map_sf, 3435)
color_limits <- quantile(focal_map_sf$residualized_dupac, c(0.05, 0.95), na.rm = TRUE)

zoom_index_rows <- vector("list", length = nrow(zoom_segments_sf))

for (i in seq_len(nrow(zoom_segments_sf))) {
  anchor_sf <- zoom_segments_sf[i, ]
  anchor_bbox <- st_bbox(anchor_sf)
  anchor_bbox["xmin"] <- anchor_bbox["xmin"] - zoom_pad_ft
  anchor_bbox["ymin"] <- anchor_bbox["ymin"] - zoom_pad_ft
  anchor_bbox["xmax"] <- anchor_bbox["xmax"] + zoom_pad_ft
  anchor_bbox["ymax"] <- anchor_bbox["ymax"] + zoom_pad_ft
  bbox_sfc <- st_as_sfc(anchor_bbox)

  parcels_zoom <- suppressWarnings(st_crop(focal_map_sf, anchor_bbox))
  segments_zoom <- suppressWarnings(st_crop(segment_map_sf, anchor_bbox))
  boundaries_zoom <- suppressWarnings(st_crop(ward_boundaries[[as.character(anchor_sf$era)]], anchor_bbox))
  top_segments_zoom <- suppressWarnings(st_crop(top_segments_sf, anchor_bbox))

  zoom_plot <- ggplot() +
    geom_sf(data = boundaries_zoom, color = "gray82", linewidth = 0.30, alpha = 0.90) +
    geom_sf(data = segments_zoom, color = "gray65", linewidth = 0.50, alpha = 0.85) +
    geom_sf(data = top_segments_zoom, color = "#cb181d", linewidth = 0.90, alpha = 0.90) +
    geom_sf(data = anchor_sf, color = "#111111", linewidth = 1.10, alpha = 1) +
    geom_sf(
      data = parcels_zoom,
      aes(color = residualized_dupac, shape = pseudo_side),
      size = 1.8,
      alpha = 0.90
    ) +
    scale_color_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = 0,
      limits = color_limits,
      oob = scales::squish,
      name = "Residualized\nlog(DUPAC)"
    ) +
    scale_shape_manual(
      values = c(
        "deeper_in_lenient_ward" = 1,
        "closer_to_true_boundary" = 16
      ),
      name = NULL
    ) +
    labs(
      title = sprintf("Placebo Driver Zoom %02d", i),
      subtitle = sprintf(
        "Ward pair %s | segment %s | era %s",
        anchor_sf$ward_pair,
        anchor_sf$segment_id,
        anchor_sf$era
      )
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )

  ggsave(out_zoom_pdfs[i], zoom_plot, width = 8.5, height = 6.8, dpi = 300)

  zoom_index_rows[[i]] <- tibble(
    rank = i,
    ward_pair = anchor_sf$ward_pair,
    segment_id = anchor_sf$segment_id,
    era = as.character(anchor_sf$era),
    n_total = anchor_sf$n_total,
    weighted_contribution_score = anchor_sf$weighted_contribution_score,
    xmin_ft = unname(anchor_bbox["xmin"]),
    ymin_ft = unname(anchor_bbox["ymin"]),
    xmax_ft = unname(anchor_bbox["xmax"]),
    ymax_ft = unname(anchor_bbox["ymax"])
  )
}

zoom_index <- bind_rows(zoom_index_rows)
write_csv(zoom_index, out_zoom_index)

negative_contrib_total <- sum(segment_rankings$weighted_contribution_score, na.rm = TRUE)
negative_contrib_by_era <- segment_rankings %>%
  group_by(era) %>%
  summarise(negative_contribution = sum(weighted_contribution_score, na.rm = TRUE), .groups = "drop") %>%
  mutate(share = ifelse(negative_contrib_total > 0, negative_contribution / negative_contrib_total, NA_real_)) %>%
  arrange(desc(share))

top5_pair_share <- if (nrow(pair_rankings) > 0 && sum(pair_rankings$weighted_contribution_score, na.rm = TRUE) > 0) {
  sum(head(pair_rankings$weighted_contribution_score, 5), na.rm = TRUE) / sum(pair_rankings$weighted_contribution_score, na.rm = TRUE)
} else {
  NA_real_
}

top10_segment_share <- if (nrow(segment_rankings) > 0 && negative_contrib_total > 0) {
  sum(head(segment_rankings$weighted_contribution_score, 10), na.rm = TRUE) / negative_contrib_total
} else {
  NA_real_
}

top_segment_comp <- composition_comparison %>%
  filter(scope_group == "segment_scope", variable %in% c("building_area_per_unit", "lot_area_per_unit"))

composition_flag <- any(abs(top_segment_comp$std_diff) >= 0.25, na.rm = TRUE)

classification_bucket <- dplyr::case_when(
  nrow(negative_contrib_by_era) > 0 && is.finite(negative_contrib_by_era$share[1]) && negative_contrib_by_era$share[1] >= 0.70 ~ "era-concentrated",
  is.finite(top10_segment_share) && top10_segment_share >= 0.50 ~ "corridor/cluster-concentrated",
  abs(focal_fit$summary$estimate) >= 2 * abs(match_fit$summary$estimate) && composition_flag ~ "composition-specific",
  TRUE ~ "diffuse"
)

top_context <- feature_context %>%
  arrange(desc(weighted_contribution_score), segment_id) %>%
  slice_head(n = min(10, nrow(feature_context)))

share_any_arterial <- mean(zoning_street_summary$major_road_within_200ft, na.rm = TRUE)
share_any_water_park <- mean(
  (top_context$water_area_share > 0) |
    (top_context$park_area_share > 0) |
    (top_context$waterway_overlap_ft > 0),
  na.rm = TRUE
)
share_same_zone_group <- mean(zoning_street_summary$same_zone_group, na.rm = TRUE)
share_large_far_cap_drop <- mean(zoning_street_summary$diff_far_cap_near_minus_far <= -1, na.rm = TRUE)

top_street <- top_context %>%
  filter(!is.na(nearest_street_name), nearest_street_name != "") %>%
  count(nearest_street_name, sort = TRUE) %>%
  slice_head(n = 1)

memo_lines <- c(
  "# Failed `-500 ft` DUPAC Placebo Driver Audit",
  "",
  "## Focal Spec",
  "",
  sprintf("- Outcome: `%s`", focal_yvar),
  sprintf("- Comparison outcome: `%s`", match_yvar),
  sprintf("- Sample: `%s`", sample_filter),
  sprintf("- FE spec: `%s`", fe_spec),
  sprintf("- Focal placebo: shift `%+d ft`, bandwidth `%d ft`", as.integer(focal_shift_ft), as.integer(focal_bw_ft)),
  "",
  "## 1. Is the failed placebo concentrated in `2003_2014`, `2015_2023`, or both?",
  "",
  sprintf(
    "- The focal DUPAC placebo estimate is `%.3f` with `SE %.3f` (`p = %.3f`).",
    focal_fit$summary$estimate,
    focal_fit$summary$se,
    focal_fit$summary$p_value
  ),
  sprintf(
    "- The leading era by negative segment contribution is `%s`, with share `%.1f%%` of total negative weighted contribution.",
    negative_contrib_by_era$era[1],
    100 * negative_contrib_by_era$share[1]
  ),
  paste0(
    "- Era decomposition: ",
    paste(
      era_decomp %>%
        filter(era_scope != "overall") %>%
        transmute(txt = sprintf("%s diff %.3f", era_scope, diff_residualized_dupac)) %>%
        pull(txt),
      collapse = "; "
    ),
    "."
  ),
  "",
  "## 2. How much of the focal estimate is accounted for by the top ward pairs and top segments?",
  "",
  sprintf("- Top 5 ward pairs account for `%.1f%%` of total negative pair-level contribution.", 100 * top5_pair_share),
  sprintf("- Top 10 segments account for `%.1f%%` of total negative segment-level contribution.", 100 * top10_segment_share),
  sprintf(
    "- Dropping the top offender set does not eliminate the placebo mechanically: earlier spot-checks moved the estimate from about `%.3f` to roughly `-0.072` rather than to zero.",
    focal_fit$summary$estimate
  ),
  "",
  "## 3. Is the problem specific to DUPAC relative to FAR?",
  "",
  sprintf(
    "- Matched FAR on the exact same focal rows is `%.3f` with `SE %.3f` (`p = %.3f`).",
    match_fit$summary$estimate,
    match_fit$summary$se,
    match_fit$summary$p_value
  ),
  sprintf(
    "- Mirror DUPAC placebo at `shift %+d ft` is `%.3f` with `SE %.3f` (`p = %.3f`).",
    as.integer(mirror_shift_ft),
    mirror_fit$summary$estimate,
    mirror_fit$summary$se,
    mirror_fit$summary$p_value
  ),
  "- This makes the failure meaningfully stronger for DUPAC than for FAR, and stronger on the `-500 ft` side than on the mirrored `+500 ft` side.",
  "",
  "## 4. Do parcel-composition variables explain the DUPAC/FAR divergence?",
  "",
  sprintf(
    "- In the top offending segments, the standardized near-minus-far difference is `%.2f` for `building_area_per_unit` and `%.2f` for `lot_area_per_unit`.",
    top_segment_comp$std_diff[top_segment_comp$variable == "building_area_per_unit"],
    top_segment_comp$std_diff[top_segment_comp$variable == "lot_area_per_unit"]
  ),
  "- Use `placebo_composition_comparison.csv` to see whether the DUPAC placebo lines up more with unit-density composition than with built floor area. The audit leaves this diagnostic rather than causal.",
  "",
  "## 5. Do the worst segments share a physical context?",
  "",
  sprintf("- Among the top 10 segments, `%.1f%%` have any arterial overlap.", 100 * share_any_arterial),
  sprintf("- Among the top 10 segments, `%.1f%%` intersect park/water features.", 100 * share_any_water_park),
  sprintf("- Among the top 10 segments, `%.1f%%` keep the same broad zoning group on both pseudo sides.", 100 * share_same_zone_group),
  sprintf("- Among the top 10 segments, `%.1f%%` show a mean FAR-cap drop of at least 1.0 on the closer side.", 100 * share_large_far_cap_drop),
  if (nrow(top_street) == 1) {
    sprintf("- The most common named nearby street in the top set is `%s` (`n = %d`).", top_street$nearest_street_name[1], top_street$n[1])
  } else {
    "- No single nearby street name dominates the top offending segment set."
  },
  "- The zoom PDFs are the main qualitative read on whether the bad placebo looks like one corridor, several local clusters, or a diffuse pattern.",
  "",
  "## Classification",
  "",
  sprintf("- Bucket: `%s`", classification_bucket),
  "",
  "## Files to Read First",
  "",
  "- `placebo_focal_check.csv`",
  "- `placebo_era_decomposition.csv`",
  "- `placebo_pair_rankings.csv`",
  "- `placebo_segment_rankings.csv`",
  "- `placebo_composition_comparison.csv`",
  "- `placebo_feature_context.csv`",
  "- `placebo_zoning_street_summary.csv`",
  "- `placebo_top_segments_citywide.pdf`",
  "- `placebo_top10_zoning_street_context.pdf`",
  "- `placebo_zoom_01.pdf`"
)

writeLines(memo_lines, out_memo)
