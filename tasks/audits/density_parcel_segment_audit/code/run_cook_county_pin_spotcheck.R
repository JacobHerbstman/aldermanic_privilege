# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_parcel_segment_audit/code")

source("../../../setup_environment/code/packages.R")

library(httr2)
library(readr)
library(sf)

sf_use_s2(FALSE)

manual_sample <- read_csv(
  "../output/manual_spotcheck_sample.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)

query_cook_pin <- function(pin) {
  req <- request("https://datacatalog.cookcountyil.gov/resource/c49d-89sn.json") %>%
    req_url_query(pin = pin, `$limit` = 1)
  resp <- req_perform(req)
  body <- resp_body_json(resp, simplifyVector = TRUE)
  if (length(body) == 0 || nrow(body) == 0) {
    return(tibble(pin = pin, api_found = FALSE))
  }
  as_tibble(body) %>%
    slice_head(n = 1) %>%
    mutate(pin = as.character(pin), api_found = TRUE)
}

cook_rows <- bind_rows(lapply(unique(manual_sample$pin), query_cook_pin))

spotcheck <- manual_sample %>%
  left_join(cook_rows, by = "pin", suffix = c("_local", "_cook")) %>%
  mutate(
    cook_longitude = suppressWarnings(as.numeric(longitude_cook)),
    cook_latitude = suppressWarnings(as.numeric(latitude_cook)),
    local_longitude = suppressWarnings(as.numeric(longitude_local)),
    local_latitude = suppressWarnings(as.numeric(latitude_local))
  )

valid_coords <- is.finite(spotcheck$local_longitude) &
  is.finite(spotcheck$local_latitude) &
  is.finite(spotcheck$cook_longitude) &
  is.finite(spotcheck$cook_latitude)

spotcheck$coord_diff_m <- NA_real_
if (any(valid_coords)) {
  local_points <- st_as_sf(
    spotcheck[valid_coords, ],
    coords = c("local_longitude", "local_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(3435)
  cook_points <- st_as_sf(
    spotcheck[valid_coords, ],
    coords = c("cook_longitude", "cook_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(3435)
  spotcheck$coord_diff_m[valid_coords] <- as.numeric(st_distance(local_points, cook_points, by_element = TRUE)) * 0.3048
}

spotcheck_out <- spotcheck %>%
  transmute(
    spotcheck_stratum,
    pin,
    pin_formatted,
    property_address,
    property_city,
    property_zip,
    local_longitude,
    local_latitude,
    cook_longitude,
    cook_latitude,
    coord_diff_m,
    cook_current_ward = ward_cook,
    stored_historical_ward = ward_local,
    stored_historical_other_ward = other_ward,
    stored_historical_ward_pair = ward_pair,
    boundary_year,
    construction_year,
    stored_segment_id,
    api_found,
    note = if_else(
      boundary_year >= 2024,
      "Cook County current ward is comparable to the stored current ward.",
      "Cook County current ward validates the parcel location but is not comparable to the stored historical ward."
    )
  )

write_csv(spotcheck_out, "../output/cook_county_pin_spotcheck.csv")

cat("Cook County PIN spotcheck rows:", nrow(spotcheck_out), "\n")
cat("API matches:", sum(spotcheck_out$api_found, na.rm = TRUE), "\n")
cat("Max coordinate difference (m):", max(spotcheck_out$coord_diff_m, na.rm = TRUE), "\n")
cat("Saved: ../output/cook_county_pin_spotcheck.csv\n")
