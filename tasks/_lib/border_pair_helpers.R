FT_TO_M <- 0.3048
M_TO_FT <- 1 / FT_TO_M

parse_bw_m <- function(x) {
  if (length(x) != 1) {
    stop("bandwidth_m must be a single value.", call. = FALSE)
  }
  if (is.character(x) && tolower(x) %in% c("all", "full", "none", "inf", "infinity")) {
    return(Inf)
  }
  out <- suppressWarnings(as.numeric(x))
  if (!is.finite(out) || out <= 0) {
    stop("bandwidth_m must be a positive number or one of: all, full, none, inf.", call. = FALSE)
  }
  out
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
    stop("bw_ft must be a positive number or one of: all, full, none, inf.", call. = FALSE)
  }
  out
}

ensure_meter_distance_columns <- function(df) {
  if (!"dist_to_boundary_m" %in% names(df) && "dist_to_boundary" %in% names(df)) {
    df <- df %>% mutate(dist_to_boundary_m = as.numeric(dist_to_boundary) * FT_TO_M)
  }
  if (!"signed_distance_m" %in% names(df) && "signed_distance" %in% names(df)) {
    df <- df %>% mutate(signed_distance_m = as.numeric(signed_distance) * FT_TO_M)
  }
  if (!"nearest_other_pair_dist_m" %in% names(df) && "nearest_other_pair_dist_ft" %in% names(df)) {
    df <- df %>% mutate(nearest_other_pair_dist_m = as.numeric(nearest_other_pair_dist_ft) * FT_TO_M)
  }
  df
}

distance_display_config <- function(default_unit = "m") {
  unit <- tolower(Sys.getenv("DISTANCE_DISPLAY_UNIT", default_unit))
  if (unit %in% c("ft", "feet", "foot")) {
    return(list(unit = "ft", scale = M_TO_FT))
  }
  if (unit %in% c("m", "meter", "meters")) {
    return(list(unit = "m", scale = 1))
  }
  stop("DISTANCE_DISPLAY_UNIT must be one of: m, meter, meters, ft, feet, foot.", call. = FALSE)
}

format_distance_label <- function(distance_m, display_config = distance_display_config()) {
  if (!is.finite(distance_m)) {
    return("")
  }
  display_value <- distance_m * display_config$scale
  if (abs(display_value - round(display_value)) < 1e-6) {
    return(sprintf("%d%s", as.integer(round(display_value)), display_config$unit))
  }
  sprintf("%.1f%s", display_value, display_config$unit)
}

paired_balance_test <- function(paired_df, min_cluster_ward_pairs) {
  if (nrow(paired_df) < 2) {
    return(tibble(se = NA_real_, p_value = NA_real_))
  }
  difference_sd <- sd(paired_df$difference, na.rm = TRUE)
  if (!is.finite(difference_sd) || difference_sd == 0) {
    return(tibble(
      se = 0,
      p_value = ifelse(mean(paired_df$difference, na.rm = TRUE) == 0, 1, 0)
    ))
  }
  if (n_distinct(paired_df$ward_pair) >= min_cluster_ward_pairs) {
    model <- feols(difference ~ 1, data = paired_df, cluster = ~ward_pair, warn = FALSE)
    return(tibble(
      se = unname(se(model)[["(Intercept)"]]),
      p_value = unname(pvalue(model)[["(Intercept)"]])
    ))
  }
  se_i <- difference_sd / sqrt(nrow(paired_df))
  tibble(
    se = se_i,
    p_value = 2 * pt(abs(mean(paired_df$difference, na.rm = TRUE) / se_i), df = nrow(paired_df) - 1, lower.tail = FALSE)
  )
}

format_signed_distance_label <- function(distance_m, display_config = distance_display_config()) {
  if (!is.finite(distance_m)) {
    return("")
  }
  display_value <- distance_m * display_config$scale
  if (abs(display_value - round(display_value)) < 1e-6) {
    return(sprintf("%+d%s", as.integer(round(display_value)), display_config$unit))
  }
  sprintf("%+.1f%s", display_value, display_config$unit)
}

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  out <- rep(NA_character_, length(x))
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

zone_group_from_code <- function(z) {
  z <- str_to_upper(as.character(z))
  case_when(
    str_starts(z, "RS-") ~ "Single-Family Residential",
    str_starts(z, "RT-") | str_starts(z, "RM-") ~ "Multi-Family Residential",
    str_detect(z, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    str_detect(z, "^C-?[1-7]-") ~ "Commercial",
    str_detect(z, "^M-?[1-7]-") ~ "Industrial",
    str_starts(z, "DX-") | str_starts(z, "DR-") | str_starts(z, "DS-") | str_starts(z, "DC-") ~ "Downtown",
    str_starts(z, "PD") ~ "Planned Development",
    str_starts(z, "PMD") ~ "Planned Manufacturing",
    str_starts(z, "POS") ~ "Open Space",
    TRUE ~ "Other"
  )
}

attach_zone_group <- function(df, lon_col, lat_col, zoning_path, chunk_n = 50000L) {
  if ("zone_group" %in% names(df)) {
    return(df)
  }

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

  for (s in seq(1L, nrow(coords), by = chunk_n)) {
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
