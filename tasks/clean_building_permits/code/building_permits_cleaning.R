# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/clean_building_permits/code")

source("../../setup_environment/code/packages.R")

crs_projected <- 3435

building_permits <- read_csv("../input/building_permits.csv", show_col_types = FALSE)

building_permits_clean <- building_permits %>% 
  janitor::clean_names() %>% 
  mutate(across(
    .cols = matches("cost|fee|paid|waived|subtotal"),
    .fns = ~ as.numeric(gsub("[^0-9.-]", "", .x))
  ))

chicago_lat_min <- 41
chicago_lat_max <- 43
chicago_lon_min <- -89
chicago_lon_max <- -87
dominant_hotspot_latitude <- 42.00853640087
dominant_hotspot_longitude <- -87.91442843927
dominant_hotspot_tolerance <- 1e-6

needs_conversion_mask <- (
  is.na(building_permits_clean$latitude) | is.na(building_permits_clean$longitude)
) &
  is.finite(building_permits_clean$xcoordinate) &
  is.finite(building_permits_clean$ycoordinate)
permits_to_convert <- building_permits_clean[needs_conversion_mask, ]

converted_sf <- st_as_sf(
    permits_to_convert,
    coords = c("xcoordinate", "ycoordinate"),
    crs = crs_projected, # Source CRS: NAD83 Illinois State Plane East
    remove = FALSE
  ) %>%
  st_transform(crs = 4326)
  
new_coords <- st_coordinates(converted_sf)
building_permits_clean$longitude[needs_conversion_mask] <- new_coords[, "X"]
building_permits_clean$latitude[needs_conversion_mask] <- new_coords[, "Y"]

building_permits_clean <- building_permits_clean %>% 
  dplyr::filter(!is.na(latitude)) %>% 
  dplyr::filter(!is.na(longitude)) %>% 
  dplyr::filter(latitude >= chicago_lat_min) %>%
  dplyr::filter(latitude <= chicago_lat_max) %>%
  dplyr::filter(longitude >= chicago_lon_min) %>%
  dplyr::filter(longitude <= chicago_lon_max) %>%
  dplyr::filter(
    !(
      abs(latitude - dominant_hotspot_latitude) < dominant_hotspot_tolerance &
        abs(longitude - dominant_hotspot_longitude) < dominant_hotspot_tolerance
    )
  )

building_permits_clean <- building_permits_clean %>% 
  dplyr::mutate(issue_date = as.Date(issue_date, format = "%m/%d/%Y")) %>% 
  dplyr::mutate(application_start_date = as.Date(application_start_date, format = "%m/%d/%Y")) %>% 
  dplyr::filter(application_start_date >= as.Date("2006-01-01")) %>% 
  dplyr::mutate(issue_date_ym = zoo::as.yearmon(issue_date)) %>% 
  dplyr::mutate(application_start_date_ym = zoo::as.yearmon(application_start_date)) %>% 
  arrange(application_start_date) %>% 
  rename(pin = pin_list) %>% 
  dplyr::select(id, pin, ward, application_start_date_ym, issue_date_ym, everything())


high_discretion_permits <- c(
  "PERMIT - NEW CONSTRUCTION",
  "PERMIT - RENOVATION/ALTERATION",
  "PERMIT - WRECKING/DEMOLITION",
  "PERMIT - PORCH CONSTRUCTION",
  "PERMIT - REINSTATE REVOKED PMT"
)

minor_permits <- c(
  "PERMIT - EASY PERMIT PROCESS",
  "PERMIT – EXPRESS PERMIT PROGRAM",
  "PERMIT - SIGNS",
  "PERMIT - SCAFFOLDING"
)

building_permits_clean2 <- building_permits_clean %>%
  mutate(high_discretion = ifelse(permit_type %in% high_discretion_permits, 1, 0)) %>% 
  mutate(minor_permit = ifelse(permit_type %in% minor_permits, 1, 0))


building_permits_clean2 <- building_permits_clean2 %>% 
  dplyr::filter(processing_time >= 0) 

building_permits_final <- building_permits_clean2 %>%
  mutate(
    permit_issued = case_when(
      permit_status %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING") ~ 1,
      permit_status %in% c("EXPIRED", "CANCELLED", "REVOKED", "SUSPENDED") ~ 0,
      TRUE ~ NA_integer_
    ),
    corporate_applicant = as.integer(
      if_any(
        starts_with("contact_") & ends_with("_name"),
        ~ str_detect(
          str_to_upper(coalesce(.x, "")), 
          "IN|SERVICE|CO|LLC|INC|CORP|LTD|LLP|PC|ASSOCIATE|GROUP|COMPANY|CONSTRUCTION|DEVELOPMENT|PROPERTY|PROPERTIES"
        )
      )
    )
  ) %>% 
  dplyr::select(
    id, pin, ward, application_start_date_ym, issue_date_ym,
    processing_time, reported_cost, total_fee,
    high_discretion, permit_issued, corporate_applicant,
    everything(), -contains("contact_")
  )


building_permits_sf <- st_as_sf(
  building_permits_final,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>% 
  st_transform(crs_projected) %>%
  mutate(across(c(application_start_date_ym, issue_date_ym), 
        function(x) as.Date(x)))
        
## write clean data
st_write(
  building_permits_sf,
  "../output/building_permits_clean.gpkg",
  delete_layer = TRUE,
  quiet = TRUE
)
