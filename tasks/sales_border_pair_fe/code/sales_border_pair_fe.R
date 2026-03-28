source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")
# input <- "../input/sales_with_hedonics.parquet"
# bw_ft <- 1000
# fe_time <- "year_quarter"
# output_tex <- "../output/fe_table_sales_bw1000.tex"
# output_csv <- "../output/fe_table_sales_bw1000.csv"
# output_year_diag <- "../output/year_diagnostics_sales_bw1000.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, bw_ft, fe_time, output_tex, output_csv, output_year_diag)
}

if (length(cli_args) >= 9) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  fe_time <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  output_year_diag <- cli_args[6]
  fe_geo <- tolower(cli_args[7])
  cluster_level <- tolower(cli_args[8])
  table_mode <- tolower(cli_args[9])
} else if (length(cli_args) >= 8) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  fe_time <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  output_year_diag <- cli_args[6]
  fe_geo <- tolower(cli_args[7])
  cluster_level <- tolower(cli_args[8])
  table_mode <- tolower(Sys.getenv("TABLE_MODE", "baseline"))
} else if (length(cli_args) >= 6) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  fe_time <- cli_args[3]
  output_tex <- cli_args[4]
  output_csv <- cli_args[5]
  output_year_diag <- cli_args[6]
  fe_geo <- tolower(Sys.getenv("FE_GEO", "segment"))
  cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
  table_mode <- tolower(Sys.getenv("TABLE_MODE", "baseline"))
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("fe_time") ||
      !exists("output_tex") || !exists("output_csv") || !exists("output_year_diag") ||
      !exists("fe_geo") || !exists("cluster_level") || !exists("table_mode")) {
    stop(
      "FATAL: Script requires args: <input> <bw_ft> <fe_time> <output_tex> <output_csv> <output_year_diag> [<fe_geo> <cluster_level> <table_mode>]",
      call. = FALSE
    )
  }
}

stopifnot(
  is.finite(bw_ft), bw_ft > 0,
  fe_time %in% c("year", "year_quarter", "year_month")
)
if (!fe_geo %in% c("segment", "ward_pair")) {
  stop("--fe_geo must be one of: segment, ward_pair", call. = FALSE)
}
if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("--cluster_level must be one of: segment, ward_pair", call. = FALSE)
}
if (!table_mode %in% c("baseline", "amenity")) {
  stop("--table_mode must be one of: baseline, amenity", call. = FALSE)
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

attach_zone_group <- function(df, lon_col, lat_col, zoning_path, chunk_n = 50000L) {
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

fe_time_label <- c(year = "Year", year_quarter = "Year-Quarter", year_month = "Year-Month")

message(sprintf("=== Sales Border Pair FE | bw=%d | fe=%s | geo=%s | cluster=%s | mode=%s ===", bw_ft, fe_time, fe_geo, cluster_level, table_mode))
message(sprintf("Pruning spec: %s", prune_sample))
message(sprintf("Use zone-group FE: %s", ifelse(use_zone_group_fe, "TRUE", "FALSE")))

# ── Load and filter ──
sales <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    year_factor = as.character(year)
  ) %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    !is.na(ward_pair), !is.na(signed_dist),
    abs(signed_dist) <= bw_ft,
    !is.na(strictness_own)
  )

need_segment <- fe_geo == "segment" || cluster_level == "segment"
if (need_segment) {
  sales <- sales %>% filter(!is.na(segment_id), segment_id != "")
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

  sales <- sales %>%
    mutate(
      pair_dash = normalize_pair_dash(ward_pair),
      era = era_from_year(year)
    ) %>%
    left_join(conf_flags, by = c("pair_dash", "era"))

  n_missing <- sum(is.na(sales$keep_pair_era))
  if (n_missing > 0) {
    message(sprintf(
      "Pruned run: %d observations have no pair-era pruning flag and will be dropped.",
      n_missing
    ))
    sales <- sales %>% mutate(keep_pair_era = if_else(is.na(keep_pair_era), FALSE, keep_pair_era))
  }

  n_before_prune <- nrow(sales)
  sales <- sales %>% filter(keep_pair_era)
  message(sprintf("Observations after pair-era pruning: %d -> %d", n_before_prune, nrow(sales)))
}

# Year diagnostics
year_diag <- sales %>%
  group_by(year) %>%
  summarise(
    n = n(),
    median_price = median(sale_price, na.rm = TRUE),
    mean_price = mean(sale_price, na.rm = TRUE),
    coverage_sqft = mean(!is.na(log_sqft)),
    coverage_bedrooms = mean(!is.na(log_bedrooms)),
    coverage_baths = mean(!is.na(log_baths)),
    .groups = "drop"
  )
write_csv(year_diag, output_year_diag)

# Standardize strictness
strictness_sd <- sd(sales$strictness_own, na.rm = TRUE)
stopifnot(is.finite(strictness_sd), strictness_sd > 0)
sales <- sales %>% mutate(strictness_std = strictness_own / strictness_sd)

if (use_zone_group_fe) {
  sales <- attach_zone_group(sales, "longitude", "latitude", zoning_gpkg)
  sales <- sales %>% filter(!is.na(zone_group), zone_group != "")
}

# Hedonic sample
sales_hed <- sales %>%
  filter(!is.na(log_sqft), !is.na(log_land_sqft), !is.na(log_building_age),
         !is.na(log_bedrooms), !is.na(log_baths), !is.na(has_garage))

if (use_zone_group_fe) {
  sales_hed <- sales_hed %>% filter(!is.na(zone_group), zone_group != "")
}

if (table_mode == "amenity") {
  amenity_cols <- c(
    "nearest_school_dist_ft",
    "nearest_park_dist_ft",
    "nearest_major_road_dist_ft",
    "lake_michigan_dist_ft"
  )
  missing_amenity_cols <- setdiff(amenity_cols, names(sales_hed))
  if (length(missing_amenity_cols) > 0) {
    stop(sprintf("Amenity mode requires columns: %s", paste(missing_amenity_cols, collapse = ", ")), call. = FALSE)
  }
  sales_amenity <- sales_hed %>%
    filter(
      !is.na(nearest_school_dist_ft),
      !is.na(nearest_park_dist_ft),
      !is.na(nearest_major_road_dist_ft),
      !is.na(lake_michigan_dist_ft)
    )
} else {
  sales_amenity <- NULL
}

message(sprintf("Full: %s obs, %d pairs | Hedonic: %s obs, %d pairs",
                format(nrow(sales), big.mark = ","), n_distinct(sales$ward_pair),
                format(nrow(sales_hed), big.mark = ","), n_distinct(sales_hed$ward_pair)))
if (table_mode == "amenity") {
  message(sprintf("Amenity complete-case: %s obs, %d pairs",
                  format(nrow(sales_amenity), big.mark = ","), n_distinct(sales_amenity$ward_pair)))
}

# ── Custom fit stats ──
fitstat_register("myo", function(x) sprintf("%.0f", mean(x$custom_data$sale_price, na.rm = TRUE)),
                 alias = "Dep. Var. Mean")
fitstat_register("nwp", function(x) length(unique(stats::na.omit(x$custom_data$ward_pair))),
                 alias = "Ward Pairs")

# ── Regressions ──
fe_var <- switch(fe_time, year = "year_factor", year_quarter = "year_quarter", year_month = "year_month")
fe_term <- paste0(
  ifelse(fe_geo == "segment", paste0("segment_id^", fe_var), paste0("ward_pair^", fe_var)),
  ifelse(use_zone_group_fe, " + zone_group", "")
)
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair

m1 <- feols(as.formula(paste0("log(sale_price) ~ strictness_std | ", fe_term)),
            data = sales, cluster = cluster_formula)
m1$custom_data <- sales

hedonic_rhs <- "strictness_std + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage"
m2 <- feols(as.formula(paste0("log(sale_price) ~ ", hedonic_rhs, " | ", fe_term)),
            data = sales_hed, cluster = cluster_formula)
m2$custom_data <- sales_hed

if (table_mode == "amenity") {
  amenity_rhs <- paste0(
    hedonic_rhs,
    " + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + lake_michigan_dist_ft"
  )
  m3 <- feols(as.formula(paste0("log(sale_price) ~ ", amenity_rhs, " | ", fe_term)),
              data = sales_amenity, cluster = cluster_formula)
  m3$custom_data <- sales_amenity
}

# ── Output table ──
setFixest_dict(c(
  strictness_std = "Stringency Index", ward_pair = "Ward Pair",
  year_factor = "Year", year_quarter = "Year-Quarter", year_month = "Year-Month",
  zone_group = "Zoning Group FE"
))

fe_label <- ifelse(
  fe_geo == "segment",
  paste0("Segment $\\times$ ", fe_time_label[[fe_time]], " FE"),
  paste0("Ward-Pair $\\times$ ", fe_time_label[[fe_time]], " FE")
)

etable(
  if (table_mode == "amenity") list(m1, m2, m3) else list(m1, m2),
  keep = "Stringency Index",
  fitstat = ~ n + myo + nwp,
  style.tex = style.tex("aer", model.format = "", fixef.title = "", fixef.suffix = "",
                         yesNo = c("$\\checkmark$", "")),
  depvar = FALSE, drop.section = "fixef", digits = 3,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  extralines = {
    n_cols <- if (table_mode == "amenity") 3 else 2
    cluster_entries <- rep(ifelse(cluster_level == "segment", "Segment", "Ward Pair"), n_cols)
    fe_entries <- rep("$\\checkmark$", n_cols)
    hedonic_entries <- if (table_mode == "amenity") c("", "$\\checkmark$", "$\\checkmark$") else c("", "$\\checkmark$")
    out <- c(
      list("_Hedonic Controls" = hedonic_entries)
    )
    if (table_mode == "amenity") {
      out <- c(out, list("_Amenity Controls" = c("", "", "$\\checkmark$")))
    }
    out <- c(
      out,
      setNames(list(fe_entries), paste0("_", fe_label)),
      list("_Cluster Level" = cluster_entries)
    )
    if (use_zone_group_fe) {
      out <- c(out, list("_Zoning Group FE" = rep("$\\checkmark$", n_cols)))
    }
    out
  },
  file = output_tex, replace = TRUE
)

# ── Coefficient CSV ──
coef_tbl <- bind_rows(
  tibble(
    specification = "no_hedonics",
    estimate = coef(m1)[["strictness_std"]],
    std_error = se(m1)[["strictness_std"]],
    p_value = pvalue(m1)[["strictness_std"]],
    n_obs = m1$nobs,
    dep_var_mean = mean(sales$sale_price, na.rm = TRUE),
    ward_pairs = n_distinct(sales$ward_pair),
    bandwidth_ft = bw_ft,
    fe_time = fe_time,
    fe_geo = fe_geo,
    cluster_level = cluster_level,
    use_zone_group_fe = use_zone_group_fe,
    use_amenity_controls = FALSE
  ),
  tibble(
    specification = "with_hedonics",
    estimate = coef(m2)[["strictness_std"]],
    std_error = se(m2)[["strictness_std"]],
    p_value = pvalue(m2)[["strictness_std"]],
    n_obs = m2$nobs,
    dep_var_mean = mean(sales_hed$sale_price, na.rm = TRUE),
    ward_pairs = n_distinct(sales_hed$ward_pair),
    bandwidth_ft = bw_ft,
    fe_time = fe_time,
    fe_geo = fe_geo,
    cluster_level = cluster_level,
    use_zone_group_fe = use_zone_group_fe,
    use_amenity_controls = FALSE
  )
)

if (table_mode == "amenity") {
  coef_tbl <- bind_rows(
    coef_tbl,
    tibble(
      specification = "with_hedonics_and_amenities",
      estimate = coef(m3)[["strictness_std"]],
      std_error = se(m3)[["strictness_std"]],
      p_value = pvalue(m3)[["strictness_std"]],
      n_obs = m3$nobs,
      dep_var_mean = mean(sales_amenity$sale_price, na.rm = TRUE),
      ward_pairs = n_distinct(sales_amenity$ward_pair),
      bandwidth_ft = bw_ft,
      fe_time = fe_time,
      fe_geo = fe_geo,
      cluster_level = cluster_level,
      use_zone_group_fe = use_zone_group_fe,
      use_amenity_controls = TRUE
    )
  )
}

write_csv(coef_tbl, output_csv)

message(sprintf("Saved: %s", output_tex))
