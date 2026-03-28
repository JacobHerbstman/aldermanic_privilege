source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_pair_fe/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_arg <- "all"
# window <- "pre_covid"
# sample_filter <- "all"
# output_tex <- "../output/fe_table_rental_bwall_pre_covid_all.tex"
# output_csv <- "../output/fe_table_rental_bwall_pre_covid_all.csv"
# output_year_diag <- "../output/year_diagnostics_bwall_pre_covid_all.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_arg, window, sample_filter, output_tex, output_csv, output_year_diag)
}

if (length(cli_args) >= 9) {
  input <- cli_args[1]
  bw_arg <- cli_args[2]
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  output_tex <- cli_args[5]
  output_csv <- cli_args[6]
  output_year_diag <- cli_args[7]
  fe_geo <- tolower(cli_args[8])
  cluster_level <- tolower(cli_args[9])
} else if (length(cli_args) >= 7) {
  input <- cli_args[1]
  bw_arg <- cli_args[2]
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  output_tex <- cli_args[5]
  output_csv <- cli_args[6]
  output_year_diag <- cli_args[7]
  fe_geo <- tolower(Sys.getenv("FE_GEO", "segment"))
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") ||
      !exists("output_tex") || !exists("output_csv") || !exists("output_year_diag") ||
      !exists("fe_geo") || !exists("cluster_level")) {
    stop(
      "FATAL: Script requires args: <input> <bw_ft> <window> <sample_filter> <output_tex> <output_csv> <output_year_diag> [<fe_geo> <cluster_level>]",
      call. = FALSE
    )
  }
}

parse_bw_ft <- function(x) {
  if (length(x) != 1) {
    stop("bw_ft must be a single value.", call. = FALSE)
  }
  if (is.character(x) && tolower(x) %in% c("all", "full", "none", "inf", "infinity")) {
    return(Inf)
  }
  out <- suppressWarnings(as.numeric(x))
  if (!is.finite(out) || out <= 0) {
    stop("--bw_ft must be a positive number or one of: all, full, none, inf", call. = FALSE)
  }
  out
}

bw_ft <- parse_bw_ft(bw_arg)
bw_label <- if (is.finite(bw_ft)) as.character(as.integer(round(bw_ft))) else "all"

if (!window %in% c("full", "pre_covid", "pre_2021", "pre_2023", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, pre_2023, drop_mid", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}
if (!fe_geo %in% c("segment", "ward_pair")) {
  stop("--fe_geo must be one of: segment, ward_pair", call. = FALSE)
}
if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("--cluster_level must be one of: segment, ward_pair", call. = FALSE)
}

prune_sample_raw <- tolower(Sys.getenv("PRUNE_SAMPLE", "all"))
if (prune_sample_raw %in% c("all", "false", "f", "0", "no", "off")) {
  prune_sample <- "all"
} else if (prune_sample_raw %in% c("pruned", "true", "t", "1", "yes", "on")) {
  prune_sample <- "pruned"
} else {
  stop("PRUNE_SAMPLE must map to one of: all/false/0 or pruned/true/1", call. = FALSE)
}
confound_flags_path <- Sys.getenv("CONFOUND_FLAGS_PATH", "../input/confounded_pair_era_flags.csv")
use_zone_group_fe_raw <- tolower(Sys.getenv("USE_ZONE_GROUP_FE", "false"))
use_zone_group_fe <- use_zone_group_fe_raw %in% c("true", "t", "1", "yes", "on")
zoning_gpkg <- Sys.getenv("ZONING_GPKG", "../input/zoning_data_clean.gpkg")

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  out <- rep(NA_character_, length(x))
  if (!any(ok)) return(out)
  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) return(NA_character_)
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

zone_group_from_code <- function(z) {
  z <- str_to_upper(as.character(z))
  case_when(
    str_starts(z, "RS-") ~ "Single-Family Residential",
    str_starts(z, "RT-") | str_starts(z, "RM-") ~ "Multi-Family Residential",
    str_starts(z, "B-") ~ "Neighborhood Mixed-Use",
    str_starts(z, "C-") ~ "Commercial",
    str_starts(z, "M-") ~ "Industrial",
    str_starts(z, "DX-") | str_starts(z, "DR-") | str_starts(z, "DS-") | str_starts(z, "DC-") ~ "Downtown",
    str_starts(z, "PD") ~ "Planned Development",
    TRUE ~ "Other"
  )
}

attach_zone_group <- function(df, lon_col, lat_col, zoning_path, chunk_n = 100000L) {
  if ("zone_group" %in% names(df)) return(df)

  if ("zone_code" %in% names(df)) {
    return(df %>% mutate(zone_group = zone_group_from_code(zone_code)))
  }

  if (!(lon_col %in% names(df) && lat_col %in% names(df))) {
    stop("USE_ZONE_GROUP_FE=TRUE requires either zone_code or longitude/latitude columns.", call. = FALSE)
  }
  if (!file.exists(zoning_path)) {
    stop(sprintf("Zoning file not found: %s", zoning_path), call. = FALSE)
  }

  layers <- st_layers(zoning_path)$name
  zoning <- st_read(zoning_path, layer = layers[1], quiet = TRUE)
  if (!"zone_code" %in% names(zoning)) {
    stop("Zoning layer missing zone_code column.", call. = FALSE)
  }
  zoning <- zoning %>%
    st_make_valid() %>%
    st_transform(3435) %>%
    select(zone_code)

  coords <- df %>%
    filter(is.finite(.data[[lon_col]]), is.finite(.data[[lat_col]])) %>%
    distinct(
      longitude = .data[[lon_col]],
      latitude = .data[[lat_col]]
    )

  if (nrow(coords) == 0) {
    stop("No finite coordinates available for zoning join.", call. = FALSE)
  }

  coords <- as.data.table(coords)
  coords[, zone_code := NA_character_]
  starts <- seq(1L, nrow(coords), by = chunk_n)

  for (s in starts) {
    e <- min(s + chunk_n - 1L, nrow(coords))
    idx <- s:e
    pts <- st_as_sf(coords[idx], coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
      st_transform(st_crs(zoning))
    hits <- st_intersects(pts, zoning)
    hit_idx <- vapply(hits, function(v) if (length(v) == 0) NA_integer_ else v[1], integer(1))
    coords[idx, zone_code := zoning$zone_code[hit_idx]]
  }

  coords <- as_tibble(coords) %>%
    mutate(zone_group = zone_group_from_code(zone_code))

  df %>%
    left_join(coords, by = setNames(c("longitude", "latitude"), c(lon_col, lat_col)))
}

window_rule <- function(df, window_name) {
  if (window_name == "full") {
    return(df)
  }
  if (window_name == "pre_covid") {
    return(df %>% filter(year <= 2019))
  }
  if (window_name == "pre_2021") {
    return(df %>% filter(year <= 2020))
  }
  if (window_name == "pre_2023") {
    return(df %>% filter(year <= 2022))
  }
  if (window_name == "drop_mid") {
    return(df %>% filter(year <= 2020 | year >= 2024))
  }
  df
}

window_label <- c(
  full = "All years (2014-2025)",
  pre_covid = "Pre-COVID (2014-2019)",
  pre_2021 = "Through 2020 (2014-2020)",
  pre_2023 = "Through 2022 (2014-2022)",
  drop_mid = "Skip 2021-2023"
)

message("=== Rental Border Pair FE ===")
message(sprintf("Input: %s", input))
message(sprintf("Bandwidth: %s", if (is.finite(bw_ft)) sprintf("%.0f ft", bw_ft) else "all distances"))
message(sprintf("Window: %s", window_label[[window]]))
message(sprintf("Sample filter: %s", sample_filter))
message(sprintf("Geo FE: %s", fe_geo))
message(sprintf("Cluster level: %s", cluster_level))
message(sprintf("Pruning spec: %s", prune_sample))
message(sprintf("Use zone-group FE: %s", ifelse(use_zone_group_fe, "TRUE", "FALSE")))

rent_raw <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id)
  ) %>%
  filter(
    !is.na(file_date),
    !is.na(rent_price),
    rent_price > 0,
    !is.na(ward_pair),
    !is.na(signed_dist),
    !is.na(strictness_own)
  )

if (is.finite(bw_ft)) {
  rent_raw <- rent_raw %>% filter(abs(signed_dist) <= bw_ft)
}

rent <- window_rule(rent_raw, window)
if (sample_filter == "multifamily_only") {
  rent <- rent %>% filter(building_type_clean == "multi_family")
}

if (prune_sample == "pruned") {
  if (!file.exists(confound_flags_path)) {
    stop(sprintf("Missing confound flags file for pruned run: %s", confound_flags_path), call. = FALSE)
  }

  conf_flags <- read_csv(
    confound_flags_path,
    show_col_types = FALSE,
    col_select = c("ward_pair_id_dash", "era", "drop_confound")
  ) %>%
    transmute(
      pair_dash = normalize_pair_dash(ward_pair_id_dash),
      era = as.character(era),
      keep_pair_era = !as.logical(drop_confound)
    ) %>%
    distinct()

  if (anyNA(conf_flags$pair_dash) || anyNA(conf_flags$era)) {
    stop("Confound flags have invalid pair/era keys.", call. = FALSE)
  }
  if (anyDuplicated(conf_flags[, c("pair_dash", "era")]) > 0) {
    stop("Confound flags contain duplicate pair-era keys.", call. = FALSE)
  }

  rent <- rent %>%
    mutate(
      pair_dash = normalize_pair_dash(ward_pair),
      era = era_from_year(year)
    ) %>%
    left_join(conf_flags, by = c("pair_dash", "era"))

  n_missing <- sum(is.na(rent$keep_pair_era))
  if (n_missing > 0) {
    message(sprintf(
      "Pruned run: %d observations have no pair-era pruning flag and will be dropped.",
      n_missing
    ))
    rent <- rent %>% mutate(keep_pair_era = if_else(is.na(keep_pair_era), FALSE, keep_pair_era))
  }

  n_before_prune <- nrow(rent)
  rent <- rent %>% filter(keep_pair_era)
  message(sprintf("Observations after pair-era pruning: %d -> %d", n_before_prune, nrow(rent)))
}

need_segment <- fe_geo == "segment" || cluster_level == "segment"
if (need_segment) {
  rent <- rent %>% filter(!is.na(segment_id), segment_id != "")
}

if (nrow(rent) == 0) {
  stop("No observations after filtering.", call. = FALSE)
}

strictness_sd <- sd(rent$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero/invalid SD in this sample.", call. = FALSE)
}

rent <- rent %>%
  mutate(
    strictness_std = strictness_own / strictness_sd,
    log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other"))
  )

rent_hedonics <- rent %>%
  filter(!is.na(log_sqft), !is.na(log_beds), !is.na(log_baths), !is.na(building_type_factor))

if (nrow(rent_hedonics) == 0) {
  stop("No complete-case rows for hedonic model.", call. = FALSE)
}

if (use_zone_group_fe) {
  rent <- attach_zone_group(rent, "longitude", "latitude", zoning_gpkg)
  rent_hedonics <- attach_zone_group(rent_hedonics, "longitude", "latitude", zoning_gpkg)
  rent <- rent %>% filter(!is.na(zone_group), zone_group != "")
  rent_hedonics <- rent_hedonics %>% filter(!is.na(zone_group), zone_group != "")
  if (nrow(rent) == 0 || nrow(rent_hedonics) == 0) {
    stop("No observations remain after requiring zone_group.", call. = FALSE)
  }
}

message(sprintf(
  "Sample sizes: no-hedonics = %d, with-hedonics = %d (%.1f%% hedonic coverage)",
  nrow(rent), nrow(rent_hedonics), 100 * nrow(rent_hedonics) / nrow(rent)
))

# Year diagnostics computed on the hedonic sample (complete-case)
year_diag <- rent_hedonics %>%
  group_by(year) %>%
  summarise(
    n = n(),
    median_rent = median(rent_price, na.rm = TRUE),
    mean_rent = mean(rent_price, na.rm = TRUE),
    share_multifamily = mean(building_type_clean == "multi_family", na.rm = TRUE),
    coverage_sqft = mean(!is.na(sqft) & sqft > 0),
    coverage_beds = mean(!is.na(beds) & beds > 0),
    coverage_baths = mean(!is.na(baths) & baths > 0),
    coverage_available_date = mean(!is.na(available_date)),
    .groups = "drop"
  )

write_csv(year_diag, output_year_diag)

mean_y_level <- function(x) {
  dat <- x$custom_data
  sprintf("%.0f", mean(dat$rent_price, na.rm = TRUE))
}
fitstat_register("myo", mean_y_level, alias = "Dep. Var. Mean")

n_ward_pairs <- function(x) {
  dat <- x$custom_data
  length(unique(stats::na.omit(dat$ward_pair)))
}
fitstat_register("nwp", n_ward_pairs, alias = "Ward Pairs")

m_no_hed <- feols(
  as.formula(paste0(
    "log(rent_price) ~ strictness_std | ",
    ifelse(fe_geo == "segment", "segment_id^year_month", "ward_pair^year_month"),
    ifelse(use_zone_group_fe, " + zone_group", "")
  )),
  data = rent,
  cluster = if (cluster_level == "segment") ~segment_id else ~ward_pair
)
m_no_hed$custom_data <- rent

n_type_levels <- n_distinct(rent_hedonics$building_type_factor)
hedonic_rhs <- "strictness_std + log_sqft + log_beds + log_baths"
if (n_type_levels >= 2) {
  hedonic_rhs <- paste0(hedonic_rhs, " + building_type_factor")
}

m_hed <- feols(
  as.formula(paste0(
    "log(rent_price) ~ ", hedonic_rhs, " | ",
    ifelse(fe_geo == "segment", "segment_id^year_month", "ward_pair^year_month"),
    ifelse(use_zone_group_fe, " + zone_group", "")
  )),
  data = rent_hedonics,
  cluster = if (cluster_level == "segment") ~segment_id else ~ward_pair
)
m_hed$custom_data <- rent_hedonics

setFixest_dict(c(
  strictness_std = "Stringency Index",
  ward_pair = "Ward Pair",
  year_month = "Year-Month",
  log_sqft = "Log Sqft",
  log_beds = "Log Beds",
  log_baths = "Log Baths",
  zone_group = "Zoning Group FE"
))

etable(
  list(m_no_hed, m_hed),
  keep = "Stringency Index",
  fitstat = ~ n + myo + nwp,
  style.tex = style.tex("aer",
    model.format = "",
    fixef.title = "",
    fixef.suffix = "",
    yesNo = c("$\\checkmark$", "")
  ),
  depvar = FALSE,
  drop.section = "fixef",
  digits = 3,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  extralines = {
    out <- list(
      "_Hedonic Controls" = c("", "$\\checkmark$"),
      "_FE Structure" = c(
        ifelse(fe_geo == "segment", "Segment $\\times$ Year-Month FE", "Ward-Pair $\\times$ Year-Month FE"),
        ifelse(fe_geo == "segment", "Segment $\\times$ Year-Month FE", "Ward-Pair $\\times$ Year-Month FE")
      ),
      "_Cluster Level" = c(
        ifelse(cluster_level == "segment", "Segment", "Ward Pair"),
        ifelse(cluster_level == "segment", "Segment", "Ward Pair")
      )
    )
    if (use_zone_group_fe) {
      out <- c(out, list("_Zoning Group FE" = c("$\\checkmark$", "$\\checkmark$")))
    }
    out
  },
  file = output_tex,
  replace = TRUE
)

coef_tbl <- tibble(
  specification = c("no_hedonics", "with_hedonics"),
  estimate = c(coef(m_no_hed)[["strictness_std"]], coef(m_hed)[["strictness_std"]]),
  std_error = c(se(m_no_hed)[["strictness_std"]], se(m_hed)[["strictness_std"]]),
  p_value = c(pvalue(m_no_hed)[["strictness_std"]], pvalue(m_hed)[["strictness_std"]]),
  n_obs = c(m_no_hed$nobs, m_hed$nobs),
  dep_var_mean = c(mean(rent$rent_price, na.rm = TRUE), mean(rent_hedonics$rent_price, na.rm = TRUE)),
  ward_pairs = c(length(unique(rent$ward_pair)), length(unique(rent_hedonics$ward_pair))),
  bandwidth_ft = bw_ft,
  bandwidth_label = bw_label,
  window = window,
  sample_filter = sample_filter,
  fe_geo = fe_geo,
  cluster_level = cluster_level,
  use_zone_group_fe = use_zone_group_fe
)

write_csv(coef_tbl, output_csv)

message(sprintf("Saved table: %s", output_tex))
message(sprintf("Saved coefficients: %s", output_csv))
message(sprintf("Saved year diagnostics: %s", output_year_diag))
