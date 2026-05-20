source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")
source("../../_lib/border_pair_helpers.R")
library(fixest)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_amenity_balance/code")
# parcel_geometry_input <- "../input/parcels_with_geometry.gpkg"
# parcel_scores_input <- "../input/parcels_with_ward_distances.csv"
# schools_input <- "../input/schools_2015.gpkg"
# parks_input <- "../input/parks.gpkg"
# major_streets_input <- "../input/major_streets.gpkg"
# water_input <- "../input/gis_osm_water_a_free_1.shp"
# output_csv <- "../output/density_amenity_balance_100m_all.csv"
# output_tex <- "../output/density_amenity_balance_100m_all.tex"
# bandwidth_m <- 100
# sample_filter <- "all"
# bandwidth_label <- "100m"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    parcel_geometry_input,
    parcel_scores_input,
    schools_input,
    parks_input,
    major_streets_input,
    water_input,
    output_csv,
    output_tex,
    bandwidth_m,
    sample_filter,
    bandwidth_label
  )
}

if (length(args) != 11) {
  stop(
    paste(
      "FATAL: Script requires 11 args:",
      "<parcel_geometry_input> <parcel_scores_input> <schools_input> <parks_input>",
      "<major_streets_input> <water_input> <output_csv> <output_tex>",
      "<bandwidth_m> <sample_filter> <bandwidth_label>"
    ),
    call. = FALSE
  )
}

parcel_geometry_input <- args[1]
parcel_scores_input <- args[2]
schools_input <- args[3]
parks_input <- args[4]
major_streets_input <- args[5]
water_input <- args[6]
output_csv <- args[7]
output_tex <- args[8]
bandwidth_m <- as.numeric(args[9])
sample_filter <- args[10]
bandwidth_label <- args[11]

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be a positive number.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample_filter must be one of: all, multifamily.", call. = FALSE)
}

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
  )
)

fmt_num <- function(x, digits = 3) {
  ifelse(is.finite(x), formatC(x, digits = digits, format = "f"), "")
}

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

message("Loading parcel geometries...")
parcel_geometry <- st_read(parcel_geometry_input, quiet = TRUE) %>%
  mutate(pin = as.character(pin)) %>%
  st_transform(3435)

message("Computing exact amenity distances...")
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
schools <- read_amenity_layer(schools_input)
parks <- read_amenity_layer(parks_input)
major_streets <- read_amenity_layer(major_streets_input)
lake <- lake_michigan_geom(water_input)
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

message("Loading scored parcel sample...")
analysis_sample <- read_csv(
  parcel_scores_input,
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    zone_group = zone_group_from_code(zone_code),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
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
    !is.na(lenient_dist),
    !is.na(strict_dist)
  )

results <- bind_rows(lapply(covariate_catalog$covariate, function(covariate_name) {
  model_df <- analysis_sample %>%
    filter(is.finite(.data[[covariate_name]])) %>%
    mutate(
      outcome_std = (.data[[covariate_name]] - mean(.data[[covariate_name]], na.rm = TRUE)) /
        sd(.data[[covariate_name]], na.rm = TRUE)
    )

  model <- feols(
    outcome_std ~ strictness_own + lenient_dist + strict_dist | zone_group + segment_id + construction_year,
    data = model_df,
    cluster = ~ward_pair,
    warn = FALSE
  )

  coef_table <- coeftable(model)
  tibble(
    covariate = covariate_name,
    covariate_label = covariate_catalog$covariate_label[covariate_catalog$covariate == covariate_name],
    estimate = unname(coef_table["strictness_own", "Estimate"]),
    se = unname(coef_table["strictness_own", "Std. Error"]),
    p_value = unname(coef_table["strictness_own", "Pr(>|t|)"]),
    n_obs = nobs(model),
    n_ward_pairs = n_distinct(model_df$ward_pair)
  )
}))

write_csv(results, output_csv)

amenity_rows <- results %>% filter(covariate != "floor_area_ratio")
amenity_n <- amenity_rows %>% pull(n_obs) %>% unique()
amenity_pairs <- amenity_rows %>% pull(n_ward_pairs) %>% unique()
zoned_far_n <- results %>%
  filter(covariate == "floor_area_ratio") %>%
  pull(n_obs)
sample_label <- ifelse(sample_filter == "all", "all-construction", "multifamily")

if (length(amenity_n) != 1 || length(amenity_pairs) != 1) {
  stop("Amenity balance rows do not share a common sample size.", call. = FALSE)
}

tex_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Covariate & Coef. on stringency & SE & $p$-value \\\\",
  "\\midrule"
)

for (covariate_name in covariate_catalog$covariate) {
  row_i <- results %>% filter(covariate == covariate_name)
  tex_lines <- c(
    tex_lines,
    sprintf(
      "%s & %s & %s & %s \\\\",
      row_i$covariate_label[[1]],
      paste0(fmt_num(row_i$estimate[[1]]), stars(row_i$p_value[[1]])),
      paste0("(", fmt_num(row_i$se[[1]]), ")"),
      fmt_num(row_i$p_value[[1]])
    )
  )
}

tex_lines <- c(
  tex_lines,
  "\\midrule",
  sprintf("N & %s &  &  \\\\", format(amenity_n[[1]], big.mark = ",")),
  sprintf("Ward pairs & %s &  &  \\\\", format(amenity_pairs[[1]], big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: ",
    bandwidth_label,
    " ",
    sample_label,
    " density sample, restricted to 2006--2022 new construction.",
    " Each row standardizes the exact parcel covariate within the estimation sample and regresses it on the standardized",
    " alderman stringency score, side-specific distance slopes, and the same zone-group, segment, and construction-year fixed effects",
    " used in the main border-pair density specification. The exact distance rows use the full ",
    format(amenity_n[[1]], big.mark = ","),
    "-parcel, ",
    format(amenity_pairs[[1]], big.mark = ","),
    "-ward-pair sample.",
    " Zoned FAR is available for ",
    format(zoned_far_n[[1]], big.mark = ","),
    " parcels because planned developments (PDs) do not map to a single zoning FAR value in the scored parcel file.}"
  ),
  "\\endgroup"
)

writeLines(tex_lines, output_tex)
