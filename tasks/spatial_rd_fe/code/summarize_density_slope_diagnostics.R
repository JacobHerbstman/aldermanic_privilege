source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

bw_ft <- 500
fe_spec <- "zonegroup_segment_year_additive"
fe_spec_exact <- "zone_segment_year_additive"

parcels_path <- "../input/parcels_with_ward_distances.csv"
parcels_geometry_path <- "../input/parcels_with_geometry.gpkg"
segments_gpkg <- "../input/boundary_segments_1320ft.gpkg"
major_streets_gpkg <- "../input/major_streets.gpkg"

style_panel_pdf <- sprintf(
  "../output/rd_fe_plot_style_comparison_bw%d_multifamily_%s_clust_ward_pair.pdf",
  bw_ft,
  fe_spec
)
distance_summary_csv <- sprintf("../output/rd_fe_distance_bin_summary_bw%d.csv", bw_ft)
distance_summary_tex <- sprintf("../output/rd_fe_distance_bin_summary_bw%d.tex", bw_ft)
comparison_csv <- sprintf("../output/rd_fe_slope_diagnostic_comparison_bw%d_multifamily_clust_ward_pair.csv", bw_ft)
comparison_tex <- sprintf("../output/rd_fe_slope_diagnostic_comparison_bw%d_multifamily_clust_ward_pair.tex", bw_ft)
pair_slope_csv <- sprintf("../output/rd_fe_pair_slope_summary_bw%d_multifamily.csv", bw_ft)
case_pairs_csv <- sprintf("../output/rd_fe_case_study_pairs_bw%d_multifamily.csv", bw_ft)
case_maps_pdf <- sprintf("../output/rd_fe_case_study_maps_bw%d_multifamily.pdf", bw_ft)

base_plot_paths <- tibble(
  yvar = c("density_far", "density_far", "density_dupac", "density_dupac"),
  plot_style = c("slope", "level", "slope", "level"),
  pdf_path = c(
    sprintf("../output/rd_fe_diag_plot_log_density_far_bw%d_multifamily_%s_clust_ward_pair_slope.pdf", bw_ft, fe_spec),
    sprintf("../output/rd_fe_diag_plot_log_density_far_bw%d_multifamily_%s_clust_ward_pair_level.pdf", bw_ft, fe_spec),
    sprintf("../output/rd_fe_diag_plot_log_density_dupac_bw%d_multifamily_%s_clust_ward_pair_slope.pdf", bw_ft, fe_spec),
    sprintf("../output/rd_fe_diag_plot_log_density_dupac_bw%d_multifamily_%s_clust_ward_pair_level.pdf", bw_ft, fe_spec)
  )
)

comparison_plot_paths <- tibble(
  yvar = rep(c("density_far", "density_dupac"), each = 4),
  specification = rep(
    c(
      "True boundary: zone-group FE",
      "True boundary: exact zoning FE",
      "Placebo: +500 ft",
      "Placebo: -500 ft"
    ),
    times = 2
  ),
  pdf_path = c(
    sprintf("../output/rd_fe_diag_plot_log_density_far_bw%d_multifamily_%s_clust_ward_pair_slope.pdf", bw_ft, fe_spec),
    sprintf("../output/rd_fe_diag_plot_log_density_far_bw%d_multifamily_%s_clust_ward_pair_slope.pdf", bw_ft, fe_spec_exact),
    sprintf("../output/placebo_rd_fe_diag_plot_log_density_far_bw%d_multifamily_%s_clust_ward_pair_shift500.pdf", bw_ft, fe_spec),
    sprintf("../output/placebo_rd_fe_diag_plot_log_density_far_bw%d_multifamily_%s_clust_ward_pair_shift-500.pdf", bw_ft, fe_spec),
    sprintf("../output/rd_fe_diag_plot_log_density_dupac_bw%d_multifamily_%s_clust_ward_pair_slope.pdf", bw_ft, fe_spec),
    sprintf("../output/rd_fe_diag_plot_log_density_dupac_bw%d_multifamily_%s_clust_ward_pair_slope.pdf", bw_ft, fe_spec_exact),
    sprintf("../output/placebo_rd_fe_diag_plot_log_density_dupac_bw%d_multifamily_%s_clust_ward_pair_shift500.pdf", bw_ft, fe_spec),
    sprintf("../output/placebo_rd_fe_diag_plot_log_density_dupac_bw%d_multifamily_%s_clust_ward_pair_shift-500.pdf", bw_ft, fe_spec)
  )
)

stopifnot(
  file.exists(parcels_path),
  file.exists(parcels_geometry_path),
  file.exists(segments_gpkg),
  file.exists(major_streets_gpkg),
  all(file.exists(base_plot_paths$pdf_path)),
  all(file.exists(comparison_plot_paths$pdf_path))
)

stars <- function(p) {
  out <- rep("", length(p))
  out[is.finite(p) & p <= 0.10] <- "*"
  out[is.finite(p) & p <= 0.05] <- "**"
  out[is.finite(p) & p <= 0.01] <- "***"
  out
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.finite(x), formatC(x, format = "f", digits = digits), "")
}

fmt_int <- function(x) {
  ifelse(is.finite(x), formatC(x, format = "f", digits = 0, big.mark = ","), "")
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.finite(x), sprintf("%.*f\\%%", digits, 100 * x), "")
}

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

mode_value <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }
  names(sort(table(x), decreasing = TRUE))[1]
}

era_label <- function(x) {
  dplyr::case_when(
    x == "1998_2002" ~ "1998-2002",
    x == "2003_2014" ~ "2003-2014",
    x == "2015_2023" ~ "2015-2023",
    x == "post_2023" ~ "2023-present",
    TRUE ~ x
  )
}

outcome_label <- function(x) {
  dplyr::case_when(
    x == "density_far" ~ "Log(FAR)",
    x == "density_dupac" ~ "Log(DUPAC)",
    TRUE ~ x
  )
}

read_plot_bundle <- function(pdf_path) {
  meta_path <- sub("\\.pdf$", "_meta.csv", pdf_path)
  bins_path <- sub("\\.pdf$", "_bins.csv", pdf_path)
  fit_path <- sub("\\.pdf$", "_fit.csv", pdf_path)

  stopifnot(file.exists(meta_path), file.exists(bins_path), file.exists(fit_path))

  list(
    meta = read_csv(meta_path, show_col_types = FALSE),
    bins = read_csv(bins_path, show_col_types = FALSE),
    fit = read_csv(fit_path, show_col_types = FALSE)
  )
}

build_rd_plot <- function(bundle) {
  meta <- bundle$meta %>% slice(1)
  bins <- bundle$bins
  fit <- bundle$fit

  subtitle_text <- if (meta$plot_style == "slope") {
    sprintf(
      "Slope plot | Jump = %s%s (SE %s)",
      fmt_num(meta$rd_jump_estimate, 3),
      stars(meta$rd_jump_p),
      fmt_num(meta$rd_jump_se, 3)
    )
  } else {
    sprintf(
      "Level-only plot | Gap = %s%s (SE %s)",
      fmt_num(meta$jump_estimate, 3),
      stars(meta$jump_p),
      fmt_num(meta$jump_se, 3)
    )
  }

  ggplot() +
    geom_point(
      data = bins,
      aes(x = bin_center, y = mean_y, color = factor(side)),
      size = 1.6,
      alpha = 0.9
    ) +
    geom_line(
      data = fit,
      aes(x = running_distance, y = fit, color = factor(side)),
      linewidth = 1.05
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray55") +
    scale_color_manual(values = c("0" = "#1f77b4", "1" = "#d62728"), guide = "none") +
    labs(
      title = outcome_label(meta$yvar),
      subtitle = subtitle_text,
      x = "Running distance (ft) relative to boundary",
      y = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 9)
    )
}

write_simple_latex_table <- function(path, col_spec, header_line, body_lines, note_line, resize_to_width = FALSE) {
  opening_lines <- if (resize_to_width) {
    c("\\centering", "\\resizebox{\\linewidth}{!}{%")
  } else {
    character(0)
  }

  closing_lines <- if (resize_to_width) {
    "}"
  } else {
    character(0)
  }

  writeLines(
    c(
      opening_lines,
      sprintf("\\begin{tabular}{%s}", col_spec),
      "\\toprule",
      header_line,
      "\\midrule",
      body_lines,
      "\\bottomrule",
      "\\end{tabular}",
      closing_lines,
      note_line
    ),
    path
  )
}

diag_bundles <- lapply(base_plot_paths$pdf_path, read_plot_bundle)

style_panel <- (
  build_rd_plot(diag_bundles[[1]]) | build_rd_plot(diag_bundles[[2]])
) / (
  build_rd_plot(diag_bundles[[3]]) | build_rd_plot(diag_bundles[[4]])
) +
  plot_annotation(
    title = "Border-density RD diagnostics: slope plots vs level-only plots",
    subtitle = "Level-only plots residualize fixed effects and controls but do not add back running-distance slopes."
  )

ggsave(style_panel_pdf, style_panel, width = 11, height = 8.5, dpi = 300)

raw <- read_csv(parcels_path, show_col_types = FALSE) %>%
  mutate(
    pin = as.character(pin),
    dist_abs = abs(signed_distance),
    dist_bin = cut(
      dist_abs,
      breaks = c(0, 100, 200, 300, 400, 500),
      include.lowest = TRUE,
      right = TRUE,
      labels = c("0-100", "100-200", "200-300", "300-400", "400-500")
    )
  )

all_new <- raw %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    unitscount > 0,
    is.finite(signed_distance),
    dist_abs <= bw_ft
  ) %>%
  mutate(zone_code = replace_na(zone_code, ""))

multifamily <- all_new %>%
  filter(unitscount > 1) %>%
  mutate(
    era = era_from_year(construction_year),
    side = if_else(signed_distance > 0, "Strict side", "Lenient side")
  )

distance_summary <- all_new %>%
  group_by(dist_bin) %>%
  summarise(
    all_n = n(),
    multifamily_share = mean(unitscount > 1, na.rm = TRUE),
    share_b = mean(str_detect(zone_code, "^B-"), na.rm = TRUE),
    share_c = mean(str_detect(zone_code, "^C-"), na.rm = TRUE),
    share_rt = mean(str_detect(zone_code, "^RT-"), na.rm = TRUE),
    share_rm = mean(str_detect(zone_code, "^RM-"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    multifamily %>%
      group_by(dist_bin) %>%
      summarise(
        mf_n = n(),
        mean_log_far = mean(ifelse(density_far > 0, log(density_far), NA_real_), na.rm = TRUE),
        mean_log_dupac = mean(ifelse(density_dupac > 0, log(density_dupac), NA_real_), na.rm = TRUE),
        mean_units = mean(unitscount, na.rm = TRUE),
        mean_lot_area = mean(arealotsf, na.rm = TRUE),
        mean_building_area = mean(areabuilding, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "dist_bin"
  )

write_csv(distance_summary, distance_summary_csv)

distance_body <- distance_summary %>%
  mutate(
    row = sprintf(
      "%s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
      dist_bin,
      fmt_int(all_n),
      fmt_pct(multifamily_share),
      fmt_pct(share_b),
      fmt_pct(share_c),
      fmt_pct(share_rt),
      fmt_pct(share_rm),
      fmt_int(mf_n),
      fmt_num(mean_log_far, 3),
      fmt_num(mean_log_dupac, 3),
      fmt_num(mean_units, 1),
      fmt_int(mean_lot_area),
      fmt_int(mean_building_area)
    )
  ) %>%
  pull(row)

write_simple_latex_table(
  distance_summary_tex,
  "lrrrrrrrrrrrr",
  "Distance Bin & All N & MF Share & B Share & C Share & RT Share & RM Share & MF N & Mean Log(FAR) & Mean Log(DUPAC) & Mean Units & Mean Lot Area & Mean Building Area \\\\",
  distance_body,
  "\\par\\vspace{0.4em}\\parbox{0.97\\linewidth}{\\footnotesize Notes: Distance bins use absolute distance to the true ward boundary within the 500-foot sample. Multifamily share and zoning shares use all new construction with positive unit counts. Means for log(FAR), log(DUPAC), units, lot area, and building area use the multifamily subsample only.}",
  resize_to_width = TRUE
)

comparison_summary <- purrr::pmap_dfr(
  comparison_plot_paths,
  function(yvar, specification, pdf_path) {
    meta <- read_plot_bundle(pdf_path)$meta %>% slice(1)
    tibble(
      outcome = outcome_label(yvar),
      specification = specification,
      jump = meta$rd_jump_estimate,
      jump_se = meta$rd_jump_se,
      jump_p = meta$rd_jump_p,
      slope_left = meta$slope_left,
      slope_right = meta$slope_right,
      n_obs = meta$n_obs
    )
  }
)

write_csv(comparison_summary, comparison_csv)

comparison_body <- comparison_summary %>%
  mutate(
    row = sprintf(
      "%s & %s & %s%s & %s & %s & %s & %s \\\\",
      outcome,
      specification,
      fmt_num(jump, 3),
      stars(jump_p),
      fmt_num(jump_se, 3),
      fmt_num(slope_left, 4),
      fmt_num(slope_right, 4),
      fmt_int(n_obs)
    )
  ) %>%
  pull(row)

write_simple_latex_table(
  comparison_tex,
  "llrrrrr",
  "Outcome & Specification & Jump & SE & Left Slope & Right Slope & N \\\\",
  comparison_body,
  "\\par\\vspace{0.4em}\\parbox{0.97\\linewidth}{\\footnotesize Notes: All rows use the 500-foot multifamily sample and standard errors clustered at the ward-pair level. The exact-zoning row replaces zoning-group fixed effects with zoning-code fixed effects. Placebo rows shift the cutoff by 500 feet into each ward while keeping the same FE-adjusted RD specification.}",
  resize_to_width = TRUE
)

pair_slope_summary <- multifamily %>%
  pivot_longer(
    cols = c(density_far, density_dupac),
    names_to = "outcome",
    values_to = "outcome_level"
  ) %>%
  filter(is.finite(outcome_level), outcome_level > 0) %>%
  group_by(ward_pair, side, outcome) %>%
  group_modify(~{
    dominant_era <- mode_value(.x$era)
    if (nrow(.x) < 5 || n_distinct(.x$dist_abs) < 3) {
      return(tibble(
        n_obs = nrow(.x),
        dominant_era = dominant_era,
        slope = NA_real_,
        slope_se = NA_real_,
        mean_log_outcome = safe_mean(log(.x$outcome_level))
      ))
    }
    fit <- lm(log(outcome_level) ~ dist_abs, data = .x)
    fit_coef <- summary(fit)$coefficients
    tibble(
      n_obs = nrow(.x),
      dominant_era = dominant_era,
      slope = fit_coef["dist_abs", "Estimate"],
      slope_se = fit_coef["dist_abs", "Std. Error"],
      mean_log_outcome = mean(log(.x$outcome_level), na.rm = TRUE)
    )
  }) %>%
  ungroup()

write_csv(pair_slope_summary, pair_slope_csv)

pair_case_summary <- multifamily %>%
  group_by(ward_pair) %>%
  summarise(
    total_obs = n(),
    dominant_era = mode_value(era),
    bc_share_near100 = safe_mean(str_detect(zone_code, "^(B-|C-)")[dist_abs <= 100]),
    bc_share_far100 = safe_mean(str_detect(zone_code, "^(B-|C-)")[dist_abs > 100]),
    .groups = "drop"
  ) %>%
  mutate(bc_delta_near_minus_far = bc_share_near100 - bc_share_far100) %>%
  left_join(
    pair_slope_summary %>%
      mutate(
        outcome_key = if_else(outcome == "density_far", "far", "dupac"),
        side_key = if_else(side == "Lenient side", "lenient", "strict"),
        slope_name = paste0(outcome_key, "_", side_key, "_slope")
      ) %>%
      select(ward_pair, slope_name, slope) %>%
      pivot_wider(names_from = slope_name, values_from = slope),
    by = "ward_pair"
  ) %>%
  left_join(
    pair_slope_summary %>%
      group_by(ward_pair) %>%
      summarise(
        n_valid_slopes = sum(is.finite(slope)),
        mean_slope = ifelse(
          sum(is.finite(slope)) > 0,
          weighted.mean(slope[is.finite(slope)], n_obs[is.finite(slope)]),
          NA_real_
        ),
        .groups = "drop"
      ),
    by = "ward_pair"
  ) %>%
  arrange(mean_slope, ward_pair)

case_pairs <- pair_case_summary %>%
  filter(n_valid_slopes >= 2, is.finite(mean_slope), mean_slope < 0) %>%
  slice_head(n = 6) %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    ward_pair,
    dominant_era,
    total_obs,
    n_valid_slopes,
    mean_slope,
    far_lenient_slope,
    far_strict_slope,
    dupac_lenient_slope,
    dupac_strict_slope,
    bc_share_near100,
    bc_share_far100,
    bc_delta_near_minus_far
  )

write_csv(case_pairs, case_pairs_csv)

if (nrow(case_pairs) == 0) {
  stop("No negative-slope ward pairs available for case-study maps.", call. = FALSE)
}

segment_layers <- setNames(
  lapply(c("1998_2002", "2003_2014", "2015_2023", "post_2023"), function(layer_name) {
    st_read(segments_gpkg, layer = layer_name, quiet = TRUE) %>%
      mutate(pair_dash = normalize_pair_dash(ward_pair_id))
  }),
  c("1998_2002", "2003_2014", "2015_2023", "post_2023")
)

major_streets <- st_read(major_streets_gpkg, quiet = TRUE)

mf_points <- st_read(parcels_geometry_path, quiet = TRUE) %>%
  mutate(pin = as.character(pin)) %>%
  left_join(
    raw %>%
      transmute(pin = as.character(pin), signed_distance),
    by = "pin"
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    unitscount > 1,
    is.finite(signed_distance),
    abs(signed_distance) <= bw_ft,
    density_dupac > 0
  ) %>%
  mutate(
    side_label = if_else(signed_distance > 0, "Strict side", "Lenient side"),
    era = era_from_year(construction_year)
  )

build_case_map <- function(case_row) {
  pair_id <- case_row$ward_pair
  pair_dash_i <- normalize_pair_dash(pair_id)
  era_i <- case_row$dominant_era
  pair_points <- mf_points %>% filter(ward_pair == pair_id)
  pair_segments <- segment_layers[[era_i]] %>% filter(pair_dash == !!pair_dash_i)

  focus_geom <- if (nrow(pair_segments) > 0) {
    st_union(st_geometry(pair_segments))
  } else {
    st_union(st_geometry(pair_points))
  }

  bbox_i <- st_bbox(st_buffer(focus_geom, 2000))
  streets_i <- suppressWarnings(st_crop(major_streets, bbox_i))
  points_i <- suppressWarnings(st_crop(pair_points, bbox_i))
  segments_i <- if (nrow(pair_segments) > 0) suppressWarnings(st_crop(pair_segments, bbox_i)) else pair_segments

  ggplot() +
    geom_sf(data = streets_i, color = "grey82", linewidth = 0.25) +
    geom_sf(data = segments_i, color = "#111111", linewidth = 1.0) +
    geom_sf(
      data = points_i,
      aes(color = side_label, size = log(density_dupac)),
      alpha = 0.85
    ) +
    scale_color_manual(
      name = NULL,
      values = c("Lenient side" = "#1f77b4", "Strict side" = "#d62728"),
      guide = "none"
    ) +
    scale_size_continuous(range = c(1.2, 4.8), guide = "none") +
    labs(
      title = paste0("Ward pair ", pair_id),
      subtitle = sprintf(
        "Mean slope %s | %s geometry",
        fmt_num(case_row$mean_slope, 4),
        era_label(era_i)
      )
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 8.5),
      legend.position = "bottom"
    )
}

case_maps <- purrr::map(
  seq_len(nrow(case_pairs)),
  ~build_case_map(case_pairs[.x, ])
)

case_study_panel <- wrap_plots(case_maps, ncol = 2, guides = "collect") +
  plot_annotation(
    title = "Steepest negative-slope ward pairs in the 500-foot multifamily sample",
    subtitle = "Boundary in black; major streets in gray; multifamily projects within 500 feet are scaled by log(DUPAC) and colored by side."
  ) &
  theme(legend.position = "none")

ggsave(case_maps_pdf, case_study_panel, width = 12, height = 10, dpi = 300)

cat("Saved:\n")
cat(" -", style_panel_pdf, "\n")
cat(" -", distance_summary_csv, "\n")
cat(" -", distance_summary_tex, "\n")
cat(" -", comparison_csv, "\n")
cat(" -", comparison_tex, "\n")
cat(" -", pair_slope_csv, "\n")
cat(" -", case_pairs_csv, "\n")
cat(" -", case_maps_pdf, "\n")
