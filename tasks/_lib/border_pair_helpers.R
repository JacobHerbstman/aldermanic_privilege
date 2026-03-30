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
    str_starts(z, "B-") ~ "Neighborhood Mixed-Use",
    str_starts(z, "C-") ~ "Commercial",
    str_starts(z, "M-") ~ "Industrial",
    str_starts(z, "DX-") | str_starts(z, "DR-") | str_starts(z, "DS-") | str_starts(z, "DC-") ~ "Downtown",
    str_starts(z, "PD") ~ "Planned Development",
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
