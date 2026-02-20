source("../../setup_environment/code/packages.R")
library(optparse)
library(fixest)

# =======================================================================================
# Interactive diagnostics block (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_rezoning_strictness_scores/code")
# opt <- list(
#   input_rezoning = "../input/rezoning_dataset_usable_19990101_20260212.csv",
#   input_bg_controls = "../input/block_group_controls_2000_2023.csv",
#   input_alderman_panel = "../input/chicago_alderman_panel.csv",
#   input_cta = "../input/cta_stations.geojson",
#   input_city = "../input/city_boundary.geojson",
#   input_water = "../input/gis_osm_water_a_free_1.shp",
#   output_csv = "../output/alderman_rezoning_strictness_scores_debug.csv",
#   output_plot = "../output/rezoning_strictness_index_debug.pdf"
# )

# Quick run recipe:
# 1) Pick a spec preset below (or set toggles directly).
# 2) Source this file once: source("create_rezoning_strictness_score.R")
# 3) Inspect Conway/Reilly in debug output CSV:
#    dbg <- readr::read_csv("../output/alderman_rezoning_strictness_scores_debug.csv", show_col_types = FALSE)
#    dbg |>
#      dplyr::filter(alderman %in% c("Bill Conway", "Brendan Reilly")) |>
#      dplyr::select(alderman, n_rezonings, downzone_share, alderman_fe_raw, alderman_fe_shrunk, strictness_index)
#
# Optional spec presets (uncomment one):
# debug_spec <- "full"               # baseline full spec
# debug_spec <- "no_from_code_fe"    # remove from_code FE
# debug_spec <- "no_year_month_fe"   # remove year_month FE
# debug_spec <- "distances_only"     # keep only distance controls
# debug_spec <- "no_controls"        # FE-only (for stress-test)
# debug_spec <- "no_shrinkage"       # use centered raw FE instead of EB-shrunk FE
#
# if (exists("debug_spec")) {
#   if (debug_spec == "no_from_code_fe") debug_use_from_code_fe <- FALSE
#   if (debug_spec == "no_year_month_fe") debug_use_year_month_fe <- FALSE
#   if (debug_spec == "distances_only") {
#     debug_include_controls <- c("dist_cbd_km", "dist_nearest_cta", "dist_lakefront")
#   }
#   if (debug_spec == "no_controls") {
#     debug_include_controls <- c("dist_cbd_km")
#     debug_drop_controls <- "dist_cbd_km"
#   }
#   if (debug_spec == "no_shrinkage") debug_disable_eb <- TRUE
# }
#
# Specification toggles for debugging rank behavior:
# debug_use_passed_month <- FALSE         # TRUE: use matter_passed_date month FE key
# debug_min_rezonings <- 1L               # drop aldermen below this count in regression sample
# debug_ref_alderman <- NULL              # e.g., "Brendan Reilly"; NULL = modal alderman
# debug_cluster <- "ward"                 # "ward", "alderman", "none"
# debug_use_from_code_fe <- TRUE          # FALSE to remove from_code FE
# debug_use_year_month_fe <- TRUE         # FALSE to remove year_month FE
# debug_include_controls <- c(            # explicit controls to include
#   "dist_cbd_km", "dist_nearest_cta", "dist_lakefront",
#   "median_hh_income", "pct_owner_occupied", "pct_white",
#   "pct_black", "pct_hispanic", "pop_total", "median_home_value"
# )
# debug_drop_controls <- character(0)     # e.g., c("dist_lakefront", "median_home_value")
# debug_winsor_far <- NULL                # e.g., c(0.01, 0.99) to winsorize far_change in regression sample
# debug_disable_eb <- FALSE               # TRUE: strictness_index = -centered raw FE (no shrinkage)
# debug_use_all_rezonings <- FALSE        # TRUE: include unapproved rezonings in score sample
# =======================================================================================

option_list <- list(
  make_option(
    "--input_rezoning",
    type = "character",
    default = "../input/rezoning_dataset_usable_19990101_20260212.csv"
  ),
  make_option(
    "--input_bg_controls",
    type = "character",
    default = "../input/block_group_controls_2000_2023.csv"
  ),
  make_option(
    "--input_alderman_panel",
    type = "character",
    default = "../input/chicago_alderman_panel.csv"
  ),
  make_option(
    "--input_cta",
    type = "character",
    default = "../input/cta_stations.geojson"
  ),
  make_option(
    "--input_city",
    type = "character",
    default = "../input/city_boundary.geojson"
  ),
  make_option(
    "--input_water",
    type = "character",
    default = "../input/gis_osm_water_a_free_1.shp"
  ),
  make_option(
    "--output_csv",
    type = "character",
    default = "../output/alderman_rezoning_strictness_scores.csv"
  ),
  make_option(
    "--output_plot",
    type = "character",
    default = "../output/rezoning_strictness_index.pdf"
  ),
  make_option(
    "--use_all_rezonings",
    type = "character",
    default = "FALSE"
  )
)
if (!exists("opt")) {
  opt <- parse_args(OptionParser(option_list = option_list))
}

parse_mixed_date_mdy_first <- function(x) {
  chars <- as.character(x)
  chars <- trimws(chars)
  chars[chars %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      chars,
      orders = c("mdy", "ymd", "dmy"),
      quiet = TRUE
    )
  )
  as.Date(parsed)
}

parse_mixed_date_ymd_first <- function(x) {
  chars <- as.character(x)
  chars <- trimws(chars)
  chars[chars %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  out <- suppressWarnings(as.Date(lubridate::ymd(chars, quiet = TRUE)))
  idx <- is.na(out) & !is.na(chars)
  if (any(idx)) {
    out[idx] <- suppressWarnings(as.Date(lubridate::mdy(chars[idx], quiet = TRUE)))
  }
  idx <- is.na(out) & !is.na(chars)
  if (any(idx)) {
    out[idx] <- suppressWarnings(as.Date(lubridate::dmy(chars[idx], quiet = TRUE)))
  }
  out
}

parse_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(default)
  }
  x_chr <- tolower(trimws(as.character(x[[1]])))
  if (x_chr %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (x_chr %in% c("false", "f", "0", "no", "n")) return(FALSE)
  default
}

safe_div <- function(num, den) {
  out <- rep(NA_real_, length(num))
  keep <- !is.na(num) & !is.na(den) & den != 0
  out[keep] <- num[keep] / den[keep]
  out
}

first_non_na <- function(x) {
  idx <- which(!is.na(x))
  if (length(idx) == 0) {
    NA_real_
  } else {
    x[idx[1]]
  }
}

message("=== Build rezoning strictness scores ===")

rezoning_raw <- read_csv(opt$input_rezoning, show_col_types = FALSE)

required_rezoning_cols <- c(
  "assigned_alderman", "from_code", "far_change", "matter_intro_date",
  "matter_passed_date", "latitude", "longitude", "ward"
)
missing_rez_cols <- setdiff(required_rezoning_cols, names(rezoning_raw))
if (length(missing_rez_cols) > 0) {
  stop("Missing required rezoning columns: ", paste(missing_rez_cols, collapse = ", "))
}

rezoning_all <- rezoning_raw %>%
  mutate(
    assigned_alderman = str_squish(as.character(assigned_alderman)),
    from_code = str_squish(as.character(from_code)),
    far_change = suppressWarnings(as.numeric(far_change)),
    intro_date = parse_mixed_date_mdy_first(matter_intro_date),
    passed_date = parse_mixed_date_mdy_first(matter_passed_date),
    passed_date_summary = parse_mixed_date_ymd_first(matter_passed_date),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    ward = suppressWarnings(as.integer(ward))
  )

alderman_panel <- read_csv(opt$input_alderman_panel, show_col_types = FALSE) %>%
  mutate(
    ward = suppressWarnings(as.integer(ward)),
    month = str_squish(as.character(month)),
    panel_month = format(zoo::as.yearmon(month, format = "%b %Y"), "%Y-%m"),
    alderman = str_squish(as.character(alderman))
  ) %>%
  filter(!is.na(ward), !is.na(panel_month), !is.na(alderman), alderman != "")

all_aldermen <- alderman_panel %>%
  distinct(alderman) %>%
  arrange(alderman) %>%
  pull(alderman)

name_aliases <- c(
  "Felix Cardona Jr" = "Felix Cardona Jr.",
  "Michael Scott Jr" = "Michael Scott Jr.",
  "Walter Burnett, Jr" = "Walter Burnett, Jr."
)

rezoning_with_alderman <- rezoning_all %>%
  mutate(
    alderman = recode(assigned_alderman, !!!name_aliases),
    alderman = if_else(alderman %in% all_aldermen, alderman, NA_character_)
  )

use_all_rezonings <- parse_flag(opt$use_all_rezonings, default = FALSE)
if (exists("debug_use_all_rezonings")) {
  use_all_rezonings <- isTRUE(debug_use_all_rezonings)
}

sample_scope <- if (use_all_rezonings) "all_rezonings" else "approved_only"
in_scope <- if (use_all_rezonings) {
  rep(TRUE, nrow(rezoning_with_alderman))
} else {
  !is.na(rezoning_with_alderman$passed_date_summary)
}

message("Score sample scope: ", sample_scope)

unmatched_assigned <- rezoning_with_alderman %>%
  filter(!is.na(assigned_alderman), assigned_alderman != "", is.na(alderman)) %>%
  count(assigned_alderman, sort = TRUE)

if (nrow(unmatched_assigned) > 0) {
  stop(
    "Assigned alderman names missing from panel after alias normalization: ",
    paste(unmatched_assigned$assigned_alderman, collapse = ", ")
  )
}

approved_stats_baseline <- rezoning_with_alderman %>%
  filter(in_scope, !is.na(assigned_alderman), assigned_alderman != "") %>%
  mutate(alderman_baseline = recode(assigned_alderman, !!!name_aliases)) %>%
  group_by(alderman_baseline) %>%
  summarise(
    total_rezonings = n(),
    downzone_count = sum(far_change < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(alderman = alderman_baseline)

approved_stats_joined <- rezoning_with_alderman %>%
  filter(in_scope, !is.na(alderman)) %>%
  group_by(alderman) %>%
  summarise(
    total_rezonings = n(),
    downzone_count = sum(far_change < 0, na.rm = TRUE),
    .groups = "drop"
  )

integrity_check <- approved_stats_baseline %>%
  full_join(
    approved_stats_joined,
    by = "alderman",
    suffix = c("_baseline", "_joined")
  ) %>%
  mutate(
    total_rezonings_baseline = replace_na(total_rezonings_baseline, 0L),
    total_rezonings_joined = replace_na(total_rezonings_joined, 0L),
    downzone_count_baseline = replace_na(downzone_count_baseline, 0),
    downzone_count_joined = replace_na(downzone_count_joined, 0),
    total_diff = total_rezonings_joined - total_rezonings_baseline,
    downzone_diff = downzone_count_joined - downzone_count_baseline
  )

if (any(integrity_check$total_diff != 0 | integrity_check$downzone_diff != 0)) {
  bad <- integrity_check %>%
    filter(total_diff != 0 | downzone_diff != 0)
  stop(
    "Alderman integrity check failed. Non-zero diffs found for: ",
    paste(bad$alderman, collapse = ", ")
  )
}

message("Alderman integrity check passed: canonicalized counts match assigned_alderman counts within scope.")

all_approved_stats <- approved_stats_joined %>%
  left_join(
    rezoning_with_alderman %>%
      filter(in_scope, !is.na(alderman)) %>%
      group_by(alderman) %>%
      summarise(
        mean_far_change_raw_all_approved = mean(far_change, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "alderman"
  ) %>%
  transmute(
    alderman,
    n_rezonings_all_approved = total_rezonings,
    downzone_share_all_approved = if_else(total_rezonings > 0, downzone_count / total_rezonings, NA_real_),
    mean_far_change_raw_all_approved
  )

rezoning <- rezoning_with_alderman %>%
  filter(
    !is.na(alderman),
    in_scope,
    !is.na(far_change),
    !is.na(intro_date),
    !is.na(ward),
    !is.na(latitude),
    !is.na(longitude),
    !is.na(from_code),
    from_code != ""
  ) %>%
  mutate(
    intro_year = lubridate::year(intro_date),
    year_month = if (exists("debug_use_passed_month") && isTRUE(debug_use_passed_month)) {
      format(zoo::as.yearmon(passed_date), "%Y-%m")
    } else {
      format(zoo::as.yearmon(intro_date), "%Y-%m")
    }
  )

message("In-scope rezonings (canonicalized, pre-regression): ", sum(in_scope & !is.na(rezoning_with_alderman$alderman)))
message("In-scope rezonings with required fields for regression: ", nrow(rezoning))

if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("CENSUS_API_KEY not found in environment.")
}
census_api_key(Sys.getenv("CENSUS_API_KEY"))
options(tigris_use_cache = TRUE)

rezoning_sf <- st_as_sf(
  rezoning %>% mutate(row_id = row_number()),
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)

message("Loading block group geometry...")
block_groups <- tryCatch(
  {
    get_acs(
      geography = "block group",
      variables = "B01003_001",
      state = "IL",
      county = "Cook",
      year = 2019,
      survey = "acs5",
      geometry = TRUE
    ) %>%
      st_transform(st_crs(rezoning_sf)) %>%
      select(GEOID, geometry)
  },
  error = function(e) {
    cache_candidates <- c(
      "/Users/jacobherbstman/Library/Caches/tigris/cb_2019_17_bg_500k.shp",
      file.path(path.expand("~"), "Library/Caches/tigris/cb_2019_17_bg_500k.shp"),
      file.path(path.expand("~"), ".cache/tigris/cb_2019_17_bg_500k.shp")
    )
    cache_path <- cache_candidates[file.exists(cache_candidates)][1]
    if (is.na(cache_path) || !nzchar(cache_path)) {
      stop("get_acs failed and no cached Census BG shapefile found. Original error: ", conditionMessage(e))
    }
    message("get_acs failed; using cached Census BG shapefile: ", cache_path)
    st_read(cache_path, quiet = TRUE) %>%
      mutate(
        GEOID = as.character(GEOID),
        COUNTYFP = as.character(COUNTYFP)
      ) %>%
      filter(COUNTYFP == "031") %>%
      st_transform(st_crs(rezoning_sf)) %>%
      select(GEOID, geometry)
  }
)

rezoning_bg <- st_join(rezoning_sf, block_groups, join = st_within, left = TRUE)

missing_geoid <- which(is.na(rezoning_bg$GEOID))
if (length(missing_geoid) > 0) {
  nearest_idx <- st_nearest_feature(rezoning_bg[missing_geoid, ], block_groups)
  rezoning_bg$GEOID[missing_geoid] <- block_groups$GEOID[nearest_idx]
}
message("Rezonings assigned to GEOID by nearest fallback: ", length(missing_geoid))

cta_stations <- st_read(opt$input_cta, quiet = TRUE) %>%
  st_transform(st_crs(rezoning_bg))
city_boundary <- st_read(opt$input_city, quiet = TRUE) %>%
  st_transform(st_crs(rezoning_bg))
water_osm <- st_read(opt$input_water, quiet = TRUE) %>%
  st_transform(st_crs(rezoning_bg))

cbd_point <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>%
  st_transform(st_crs(rezoning_bg))

dist_cbd_km <- as.numeric(units::set_units(st_distance(rezoning_bg, cbd_point), "km"))
cta_idx <- st_nearest_feature(rezoning_bg, cta_stations)
dist_nearest_cta <- as.numeric(
  units::set_units(st_distance(rezoning_bg, cta_stations[cta_idx, ], by_element = TRUE), "km")
)

lake_poly <- water_osm %>%
  filter(!is.na(name), tolower(name) == "lake michigan")

dist_lakefront <- rep(NA_real_, nrow(rezoning_bg))
if (nrow(lake_poly) > 0) {
  lake_geom <- st_union(st_make_valid(lake_poly))
  dist_lakefront <- as.numeric(units::set_units(st_distance(rezoning_bg, lake_geom), "km"))
}

rezoning_df <- rezoning_bg %>%
  st_drop_geometry() %>%
  mutate(
    dist_cbd_km = dist_cbd_km,
    dist_nearest_cta = dist_nearest_cta,
    dist_lakefront = dist_lakefront,
    control_year_target = pmin(pmax(intro_year, 2000L), 2023L)
  )

bg_controls <- read_csv(opt$input_bg_controls, show_col_types = FALSE) %>%
  mutate(
    GEOID = as.character(GEOID),
    year = suppressWarnings(as.integer(year)),
    tot_pop = suppressWarnings(as.numeric(tot_pop)),
    tot_units = suppressWarnings(as.numeric(tot_units)),
    owner_occ = suppressWarnings(as.numeric(owner_occ)),
    pop_white = suppressWarnings(as.numeric(pop_white)),
    pop_black = suppressWarnings(as.numeric(pop_black)),
    pop_hisp = suppressWarnings(as.numeric(pop_hisp)),
    median_income = suppressWarnings(as.numeric(median_income)),
    agg_value = suppressWarnings(as.numeric(agg_value))
  ) %>%
  filter(!is.na(GEOID), !is.na(year))

bg_controls <- bg_controls %>%
  group_by(GEOID, year) %>%
  summarise(
    tot_pop = first_non_na(tot_pop),
    tot_units = first_non_na(tot_units),
    owner_occ = first_non_na(owner_occ),
    pop_white = first_non_na(pop_white),
    pop_black = first_non_na(pop_black),
    pop_hisp = first_non_na(pop_hisp),
    median_income = first_non_na(median_income),
    agg_value = first_non_na(agg_value),
    .groups = "drop"
  )

rezoning_with_controls <- rezoning_df %>%
  left_join(
    bg_controls,
    by = c("GEOID", "control_year_target" = "year")
  ) %>%
  rename(
    tot_pop_direct = tot_pop,
    tot_units_direct = tot_units,
    owner_occ_direct = owner_occ,
    pop_white_direct = pop_white,
    pop_black_direct = pop_black,
    pop_hisp_direct = pop_hisp,
    median_income_direct = median_income,
    agg_value_direct = agg_value
  )

missing_controls <- rezoning_with_controls %>%
  filter(is.na(tot_pop_direct)) %>%
  select(row_id, GEOID, control_year_target)

fallback_matches <- tibble(
  row_id = integer(),
  fallback_year = integer(),
  tot_pop_fallback = double(),
  tot_units_fallback = double(),
  owner_occ_fallback = double(),
  pop_white_fallback = double(),
  pop_black_fallback = double(),
  pop_hisp_fallback = double(),
  median_income_fallback = double(),
  agg_value_fallback = double()
)

if (nrow(missing_controls) > 0) {
  fallback_matches <- missing_controls %>%
    inner_join(bg_controls, by = "GEOID", relationship = "many-to-many") %>%
    mutate(abs_year_diff = abs(year - control_year_target)) %>%
    arrange(row_id, abs_year_diff, year) %>%
    group_by(row_id) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      row_id,
      fallback_year = year,
      tot_pop_fallback = tot_pop,
      tot_units_fallback = tot_units,
      owner_occ_fallback = owner_occ,
      pop_white_fallback = pop_white,
      pop_black_fallback = pop_black,
      pop_hisp_fallback = pop_hisp,
      median_income_fallback = median_income,
      agg_value_fallback = agg_value
    )
}

rezoning_with_controls <- rezoning_with_controls %>%
  left_join(fallback_matches, by = "row_id") %>%
  mutate(
    matched_bg_year = if_else(
      !is.na(tot_pop_direct),
      control_year_target,
      fallback_year
    ),
    tot_pop = coalesce(tot_pop_direct, tot_pop_fallback),
    tot_units = coalesce(tot_units_direct, tot_units_fallback),
    owner_occ = coalesce(owner_occ_direct, owner_occ_fallback),
    pop_white = coalesce(pop_white_direct, pop_white_fallback),
    pop_black = coalesce(pop_black_direct, pop_black_fallback),
    pop_hisp = coalesce(pop_hisp_direct, pop_hisp_fallback),
    median_income = coalesce(median_income_direct, median_income_fallback),
    agg_value = coalesce(agg_value_direct, agg_value_fallback)
  ) %>%
  mutate(
    pct_owner_occupied = safe_div(owner_occ, tot_units),
    pct_white = safe_div(pop_white, tot_pop),
    pct_black = safe_div(pop_black, tot_pop),
    pct_hispanic = safe_div(pop_hisp, tot_pop),
    median_hh_income = median_income,
    pop_total = tot_pop,
    median_home_value = safe_div(agg_value, owner_occ)
  )

message("Direct BG-year matches: ", sum(!is.na(rezoning_with_controls$tot_pop_direct)))
message("Fallback BG-year matches: ", sum(is.na(rezoning_with_controls$tot_pop_direct) & !is.na(rezoning_with_controls$tot_pop_fallback)))

control_vars <- c(
  "dist_cbd_km", "dist_nearest_cta", "dist_lakefront",
  "median_hh_income", "pct_owner_occupied", "pct_white",
  "pct_black", "pct_hispanic", "pop_total", "median_home_value"
)
if (exists("debug_include_controls")) {
  control_vars <- as.character(debug_include_controls)
}
if (exists("debug_drop_controls")) {
  control_vars <- setdiff(control_vars, as.character(debug_drop_controls))
}
if (length(control_vars) == 0) {
  stop("Control set is empty. Keep at least one control or use FE-only manually.")
}

regression_data <- rezoning_with_controls %>%
  filter(
    !is.na(alderman),
    !is.na(ward),
    !is.na(from_code),
    !is.na(year_month),
    if_all(all_of(control_vars), ~ !is.na(.x))
  ) %>%
  mutate(
    ward = as.integer(ward),
    alderman = as.character(alderman),
    from_code = as.character(from_code),
    year_month = as.character(year_month)
  )

if (exists("debug_winsor_far") && length(debug_winsor_far) == 2) {
  q_low <- as.numeric(debug_winsor_far[1])
  q_high <- as.numeric(debug_winsor_far[2])
  if (!is.na(q_low) && !is.na(q_high) && q_low < q_high && q_low >= 0 && q_high <= 1) {
    qs <- quantile(regression_data$far_change, probs = c(q_low, q_high), na.rm = TRUE, names = FALSE)
    regression_data <- regression_data %>%
      mutate(far_change = pmax(pmin(far_change, qs[2]), qs[1]))
    message("Applied far_change winsorization at quantiles: [", q_low, ", ", q_high, "]")
  }
}

message("Regression sample rows: ", nrow(regression_data))
message("Regression sample aldermen: ", n_distinct(regression_data$alderman))

if (nrow(regression_data) == 0) {
  stop("Regression sample is empty after applying required controls.")
}

counts_pre_filter <- regression_data %>%
  count(alderman, name = "n_rezonings")

min_rezonings <- if (exists("debug_min_rezonings")) as.integer(debug_min_rezonings) else 1L
if (is.na(min_rezonings) || min_rezonings < 1) {
  min_rezonings <- 1L
}
if (min_rezonings > 1L) {
  keep_aldermen <- counts_pre_filter %>%
    filter(n_rezonings >= min_rezonings) %>%
    pull(alderman)
  regression_data <- regression_data %>%
    filter(alderman %in% keep_aldermen)
  message("Applied min_rezonings filter: >= ", min_rezonings)
  message("Rows after min_rezonings filter: ", nrow(regression_data))
  message("Aldermen after min_rezonings filter: ", n_distinct(regression_data$alderman))
}
if (nrow(regression_data) == 0) {
  stop("Regression sample became empty after min_rezonings filter.")
}

if (exists("debug_ref_alderman") && !is.null(debug_ref_alderman) &&
  debug_ref_alderman %in% regression_data$alderman) {
  ref_alderman <- as.character(debug_ref_alderman)
} else {
  ref_alderman <- regression_data %>%
    count(alderman, sort = TRUE) %>%
    slice(1) %>%
    pull(alderman)
}

ref_quoted <- gsub("\"", "\\\\\"", ref_alderman)
rhs_terms <- c(control_vars, paste0("i(alderman, ref = \"", ref_quoted, "\")"))
rhs_controls <- paste(rhs_terms, collapse = " + ")

use_from_code_fe <- if (exists("debug_use_from_code_fe")) isTRUE(debug_use_from_code_fe) else TRUE
use_year_month_fe <- if (exists("debug_use_year_month_fe")) isTRUE(debug_use_year_month_fe) else TRUE
fe_terms <- character(0)
if (use_from_code_fe) fe_terms <- c(fe_terms, "from_code")
if (use_year_month_fe) fe_terms <- c(fe_terms, "year_month")

model_formula <- if (length(fe_terms) > 0) {
  as.formula(
    paste0("far_change ~ ", rhs_controls, " | ", paste(fe_terms, collapse = " + "))
  )
} else {
  as.formula(paste0("far_change ~ ", rhs_controls))
}

cluster_choice <- if (exists("debug_cluster")) tolower(as.character(debug_cluster)) else "ward"
vcov_arg <- if (cluster_choice %in% c("none", "iid", "")) {
  "iid"
} else if (cluster_choice == "alderman") {
  ~alderman
} else {
  ~ward
}

model <- feols(model_formula, data = regression_data, vcov = vcov_arg, warn = FALSE)

message("Reference alderman: ", ref_alderman)
message("Controls in model: ", paste(control_vars, collapse = ", "))
message("FE terms in model: ", if (length(fe_terms) == 0) "none" else paste(fe_terms, collapse = ", "))
message("Cluster choice: ", cluster_choice)
message("Model observations used: ", model$nobs)

coef_tbl <- tibble(
  term = names(coef(model)),
  alderman_fe_raw = as.numeric(coef(model))
)
se_tbl <- tibble(
  term = names(se(model)),
  alderman_se = as.numeric(se(model))
)

effects <- coef_tbl %>%
  filter(str_detect(term, "^alderman::")) %>%
  mutate(alderman = str_remove(term, "^alderman::")) %>%
  left_join(
    se_tbl %>%
      filter(str_detect(term, "^alderman::")) %>%
      mutate(alderman = str_remove(term, "^alderman::")) %>%
      select(alderman, alderman_se),
    by = "alderman"
  ) %>%
  select(alderman, alderman_fe_raw, alderman_se)

if (!ref_alderman %in% effects$alderman) {
  effects <- bind_rows(
    effects,
    tibble(alderman = ref_alderman, alderman_fe_raw = 0, alderman_se = 0)
  )
}

regression_stats <- regression_data %>%
  group_by(alderman) %>%
  summarise(
    n_rezonings = n(),
    downzone_share = mean(far_change < 0, na.rm = TRUE),
    mean_far_change_raw = mean(far_change, na.rm = TRUE),
    .groups = "drop"
  )

effects <- effects %>%
  left_join(regression_stats %>% select(alderman, n_rezonings), by = "alderman")

weighted_mean <- sum(
  effects$alderman_fe_raw * effects$n_rezonings,
  na.rm = TRUE
) / sum(effects$n_rezonings, na.rm = TRUE)

effects <- effects %>%
  mutate(alderman_fe_centered = alderman_fe_raw - weighted_mean)

signal_var <- var(effects$alderman_fe_centered, na.rm = TRUE) -
  mean(effects$alderman_se^2, na.rm = TRUE)
if (!is.finite(signal_var) || signal_var <= 0) {
  signal_var <- 1e-8
}

if (exists("debug_disable_eb") && isTRUE(debug_disable_eb)) {
  effects <- effects %>%
    mutate(
      shrinkage_B = 1,
      alderman_fe_shrunk = alderman_fe_centered,
      strictness_index = -1 * alderman_fe_shrunk
    )
  message("EB shrinkage disabled: strictness index uses centered raw FE.")
} else {
  effects <- effects %>%
    mutate(
      shrinkage_B = signal_var / (signal_var + alderman_se^2),
      alderman_fe_shrunk = shrinkage_B * alderman_fe_centered,
      strictness_index = -1 * alderman_fe_shrunk
    )
}

output <- tibble(alderman = all_aldermen) %>%
  left_join(regression_stats, by = "alderman") %>%
  left_join(all_approved_stats, by = "alderman") %>%
  left_join(
    effects %>%
      select(
        alderman,
        alderman_fe_raw,
        alderman_se,
        shrinkage_B,
        alderman_fe_shrunk,
        strictness_index
      ),
    by = "alderman"
  ) %>%
  mutate(
    sample_scope = sample_scope,
    n_rezonings = replace_na(n_rezonings, 0L),
    n_rezonings_all_approved = replace_na(n_rezonings_all_approved, 0L)
  ) %>%
  select(
    alderman,
    sample_scope,
    n_rezonings,
    downzone_share,
    mean_far_change_raw,
    alderman_fe_raw,
    alderman_se,
    shrinkage_B,
    alderman_fe_shrunk,
    strictness_index,
    n_rezonings_all_approved,
    downzone_share_all_approved,
    mean_far_change_raw_all_approved
  ) %>%
  arrange(desc(strictness_index), alderman)

write_csv(output, opt$output_csv)

plot_df <- output %>%
  filter(!is.na(strictness_index)) %>%
  arrange(strictness_index) %>%
  mutate(alderman = factor(alderman, levels = alderman))

score_plot <- ggplot(plot_df, aes(x = strictness_index, y = alderman, fill = strictness_index)) +
  geom_col() +
  scale_fill_distiller(
    palette = "RdYlBu",
    direction = -1,
    name = "Strictness"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7) +
  labs(
    title = "Alderman Strictness Index (Rezoning FAR-Based)",
    subtitle = "Higher values indicate stricter aldermanic rezoning behavior",
    x = "Strictness Index (= -EB-shrunk alderman FAR effect)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )

ggsave(opt$output_plot, plot = score_plot, width = 9, height = 13, device = "pdf", bg = "white")

message("Signal variance for EB shrinkage: ", round(signal_var, 6))
message("Scores written: ", opt$output_csv)
message("Plot written: ", opt$output_plot)
message("=== Rezoning strictness score complete ===")
