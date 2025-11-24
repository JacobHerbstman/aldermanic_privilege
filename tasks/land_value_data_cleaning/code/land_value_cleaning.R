# --- Land/Building Values: clean, collapse to pin10, geocode, and save GeoParquet ---

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")
options(dplyr.summarise.inform = FALSE)

# ---------- helpers ----------
log_stage <- function(df, label, pin = "pin10", yr = "tax_year", n_show_years = 20) {
  rows <- nrow(df)
  pins <- if (pin %in% names(df)) dplyr::n_distinct(df[[pin]]) else NA_integer_
  years <- if (yr %in% names(df)) dplyr::n_distinct(df[[yr]]) else NA_integer_
  pin_year <- if (all(c(pin, yr) %in% names(df))) nrow(dplyr::distinct(df, dplyr::across(dplyr::all_of(c(pin, yr))))) else NA_integer_
  dup_pin_year <- if (all(c(pin, yr) %in% names(df))) rows - pin_year else NA_integer_
  
  message(sprintf("[%s] rows=%s, pins=%s, years=%s, pin×year=%s, dup pin×year=%s",
                  label, rows, pins, years, pin_year, dup_pin_year))
  if (yr %in% names(df)) {
    yy <- df %>%
      dplyr::count(.data[[yr]], name = "n") %>%
      dplyr::arrange(.data[[yr]])
    n_to_show <- min(n_show_years, nrow(yy))
    if (n_to_show > 0) {
      message(sprintf("[%s] rows by %s (showing %s/%s):", label, yr, n_to_show, nrow(yy)))
      print(utils::head(yy, n_to_show), row.names = FALSE)
    }
  }
}

assert_unique_key <- function(df, keys, label = "assert_unique_key") {
  dups <- anyDuplicated(df[, keys])
  if (dups) {
    message(sprintf("[ERROR] %s: non-unique on {%s}. Top offenders:", label, paste(keys, collapse = ", ")))
    print(df %>% dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>% dplyr::summarise(n = dplyr::n(), .groups = "drop") %>% dplyr::arrange(dplyr::desc(n)) %>% utils::head(20))
    stop(sprintf("%s failed: duplicates detected on {%s}", label, paste(keys, collapse = ", ")))
  }
}

# ---- Load ----
message("==> Loading raw inputs...")
data    <- read_csv("../input/land_value_raw.csv", show_col_types = FALSE)
parcels <- read_csv("../input/parcels.csv",      show_col_types = FALSE)
message(sprintf("    land_value_raw rows: %s", nrow(data)))
message(sprintf("    parcels rows: %s", nrow(parcels)))

# ---- Sanity check (non-invasive): ratio-of-sums == value-weighted mean of unit land_share
message("==> Running land_share sanity check...")
data_sanity <- data %>%
  dplyr::distinct(pin, tax_year, .keep_all = TRUE) %>%
  dplyr::mutate(
    pin10     = substr(pin, 1, 10),
    tot_val   = certified_land + certified_bldg,
    land_share = dplyr::if_else(tot_val > 0, certified_land / tot_val, NA_real_)
  ) %>%
  dplyr::group_by(pin10, tax_year) %>%
  dplyr::summarise(
    ls_ratio = sum(certified_land, na.rm = TRUE) / sum(tot_val, na.rm = TRUE),
    ls_vw    = stats::weighted.mean(land_share, w = tot_val, na.rm = TRUE),
    .groups  = "drop"
  )

max_diff <- max(abs(data_sanity$ls_ratio - data_sanity$ls_vw), na.rm = TRUE)
message(sprintf("    Sanity check max |ratio - weighted| diff: %.3e", max_diff))

# ---- Clean unit-level data; build unit land_share ----
message("==> Cleaning unit-level assessor data...")
data_clean0_n <- nrow(data)
data <- data %>%
  dplyr::select(pin, tax_year, class,
                certified_land, certified_bldg, certified_tot,
                board_land, board_bldg, board_tot) %>%
  dplyr::mutate(pin10 = substr(pin, 1, 10)) %>%
  dplyr::filter(!is.na(pin),
                !is.na(certified_land), !is.na(certified_bldg),
                (certified_land + certified_bldg) > 0) %>%
  dplyr::mutate(land_share = certified_land / (certified_land + certified_bldg))
message(sprintf("    Kept %s / %s rows (%.1f%%).",
                nrow(data), data_clean0_n, 100 * nrow(data) / max(1, data_clean0_n)))
log_stage(data, "unit-level cleaned")

# ---- Collapse to lot level: pin10 x tax_year (ratio-of-sums) ----
message("==> Collapsing to lot level (pin10 × tax_year)...")
data_pin10 <- data %>%
  dplyr::mutate(pin10 = substr(pin, 1, 10)) %>%
  dplyr::group_by(pin10, tax_year) %>%
  dplyr::summarise(
    class    = dplyr::first(class),
    land_sum = sum(certified_land, na.rm = TRUE),
    bldg_sum = sum(certified_bldg, na.rm = TRUE),
    n_units  = dplyr::n_distinct(pin),     # approx units for condos
    .groups  = "drop"
  ) %>%
  dplyr::mutate(land_share_pin10 = land_sum / (land_sum + bldg_sum)) %>%
  dplyr::filter((land_sum + bldg_sum) > 0)

assert_unique_key(data_pin10, c("pin10","tax_year"), "data_pin10 uniqueness")
log_stage(data_pin10, "pin10×year collapsed")

# ---- Prep parcels; reduce to one row per 14-digit PIN, then to one per pin10 ----
message("==> Preparing parcel coordinates...")
parcels0 <- parcels
parcels <- parcels %>%
  dplyr::mutate(pin = as.character(pin),
                pin10 = substr(pin, 1, 10)) %>%
  dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
  dplyr::distinct(pin, .keep_all = TRUE) %>%
  dplyr::select(pin, pin10, latitude, longitude)
message(sprintf("    Parcels with non-missing coords: %s / %s (%.1f%%).",
                nrow(parcels), nrow(parcels0), 100 * nrow(parcels) / max(1, nrow(parcels0))))

parcels_by_pin10 <- parcels %>%
  dplyr::group_by(pin10) %>%
  dplyr::summarise(
    latitude_pin10  = dplyr::first(stats::na.omit(latitude)),
    longitude_pin10 = dplyr::first(stats::na.omit(longitude)),
    .groups = "drop"
  ) %>%
  dplyr::filter(!is.na(latitude_pin10), !is.na(longitude_pin10))

stopifnot(!anyDuplicated(parcels_by_pin10$pin10))
message(sprintf("    parcels_by_pin10 rows: %s (unique pin10 with coords)", nrow(parcels_by_pin10)))

# ---- Merge lot-level values with lot-level coords ----
message("==> Joining values to coordinates (many-to-one on pin10)...")
n_before_join <- nrow(data_pin10)
merged <- data_pin10 %>%
  dplyr::left_join(parcels_by_pin10, by = "pin10", relationship = "many-to-one")
stopifnot(nrow(merged) == n_before_join)  # left_join should preserve row count

# Drop rows lacking coords (report loss)
n_before_filter <- nrow(merged)
merged <- merged %>%
  dplyr::filter(!is.na(latitude_pin10), !is.na(longitude_pin10)) %>% 
  dplyr::rename(
    latitude  = latitude_pin10,
    longitude = longitude_pin10
  )
message(sprintf("    Dropped %s rows without coords after join.",
                n_before_filter - nrow(merged)))
assert_unique_key(merged, c("pin10","tax_year"), "merged uniqueness")
log_stage(merged, "merged (pre-sf)")

# ---- Build sf geometry (WGS84 -> EPSG:3435) ----
message("==> Building sf geometry (WGS84 -> EPSG:3435)...")
merged_sf <- sf::st_as_sf(
  merged,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
) %>%
  sf::st_transform(3435)
log_stage(merged_sf, "merged_sf")

# ---- Save as GeoParquet (preserves geometry) ----
message("==> Writing GeoParquet outputs...")
sfarrow::st_write_parquet(
  merged_sf,
  "../output/land_values_geo.parquet",
  compression = "zstd"
)
message(sprintf("    WROTE ../output/land_values_geo.parquet with %s rows and %s unique pin10×tax_year.",
                nrow(merged_sf),
                names(merged_sf),
                nrow(dplyr::distinct(merged_sf, pin10, tax_year))))


# 5% sample by year (comment fixed)
merged_sf_sample <- dplyr::slice_sample(merged_sf, prop = 0.05, by = tax_year)
sfarrow::st_write_parquet(
  merged_sf_sample,
  "../output/land_values_geo_sample5.parquet",
  compression = "zstd"
)
message(sprintf("    WROTE ../output/land_values_geo_sample5.parquet with %s rows (5%% by year).",
                nrow(merged_sf_sample)))

message("==> Done.")
