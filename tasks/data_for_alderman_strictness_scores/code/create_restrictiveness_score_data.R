## this code creates a panel to be used to estimate aldermen fixed effects

source("../../setup_environment/code/packages.R")

# --- Core inputs ---
ward_panel     <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

ward_controls  <- read_csv("../input/ward_controls.csv")
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv") %>%
  mutate(month = as.yearmon(month))

# Permits (geospatial)
permits <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  mutate(application_start_date = year(application_start_date_ym),
         issue_date            = year(issue_date_ym))

if (st_crs(permits) != st_crs(ward_panel)) {
  message("CRS mismatch detected. Transforming parcels CRS to match wards.")
  permits <- st_transform(permits, st_crs(ward_panel))
}

# --- Helper to read + transform to ward CRS ---
read_to_ward_crs <- function(path) {
  obj <- st_read(path, quiet = TRUE)
  if (st_crs(obj) != st_crs(ward_panel)) obj <- st_transform(obj, st_crs(ward_panel))
  obj
}

# --- NEW external layers (already symlinked) ---
cta_stations    <- read_to_ward_crs("../input/cta_stations.geojson")          # points
city_boundary   <- read_to_ward_crs("../input/city_boundary.geojson")         # polygon
community_areas <- read_to_ward_crs("../input/community_areas.geojson")       # polygons (77)
water_osm       <- read_to_ward_crs("../input/gis_osm_water_a_free_1.shp")    # polygons (OSM water)

################################################
###### filter to HIGH DISCRETION PERMITS ONLY
permits_high_discretion <- permits %>% 
  filter(high_discretion == 1)
##############################################

# 1. SPATIAL JOIN PERMITS TO WARDS BY MONTH (UPDATED FOR 3 MAPS) ===========

# Map 1: Pre-2015 (Use 2014 shape)
ward_geoms_map1 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>% summarise(.groups = "drop") %>%
  mutate(map_version = 1)

# Map 2: 2015-2023 (Use 2016 shape to represent this era)
ward_geoms_map2 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>% summarise(.groups = "drop") %>%
  mutate(map_version = 2)

# Map 3: 2023-Present (Use 2024 shape to represent this era)
ward_geoms_map3 <- ward_panel %>%
  filter(year == max(year)) %>% 
  select(ward) %>%
  group_by(ward) %>% summarise(.groups = "drop") %>%
  mutate(map_version = 3)

# Function to assign ward based on 3 time intervals
assign_ward_by_month <- function(permits_data) {
  
  # Period 1: Before May 2015
  permits_p1 <- permits_data %>%
    filter(as.yearmon(application_start_date_ym) < as.yearmon("2015-05"))
  
  # Period 2: May 2015 to April 2023
  permits_p2 <- permits_data %>%
    filter(as.yearmon(application_start_date_ym) >= as.yearmon("2015-05") &
             as.yearmon(application_start_date_ym) < as.yearmon("2023-05"))
  
  # Period 3: May 2023 onwards
  permits_p3 <- permits_data %>%
    filter(as.yearmon(application_start_date_ym) >= as.yearmon("2023-05"))
  
  # Helper for joining
  do_join <- function(pts, polys) {
    st_join(pts, polys, join = st_within) %>%
      select(-ward.y) %>% 
      rename(ward = ward.x) %>% 
      filter(!is.na(ward)) %>%
      st_drop_geometry()
  }
  
  bind_rows(
    do_join(permits_p1, ward_geoms_map1),
    do_join(permits_p2, ward_geoms_map2),
    do_join(permits_p3, ward_geoms_map3)
  )
}

permits_ward_data <- assign_ward_by_month(permits_high_discretion)
message("Permits spatially joined to wards: ", nrow(permits_ward_data), " observations")

# 2. ADD ALDERMAN INFORMATION ==============================================

permits_with_alderman <- permits_ward_data %>%
  mutate(application_start_date_ym = as.yearmon(application_start_date_ym)) %>% 
  left_join(alderman_panel, by = c("ward", "application_start_date_ym" = "month")) %>%
  filter(!is.na(alderman))

message("Permits with alderman info: ", nrow(permits_with_alderman), " observations")

# 3. ADD WARD CONTROLS (WITH FAST FILL-FORWARD) ============================

# Detect years
max_permit_year  <- max(permits$application_start_date, na.rm = TRUE)
max_control_year <- max(ward_controls$year, na.rm = TRUE)

# Sanity check: Ensure we don't create 100 years of data due to a typo
if (max_permit_year > 2030) {
  warning("Found permit year > 2030. Capping fill-forward at 2030 to prevent freeze.")
  max_permit_year <- 2030
}

if (max_permit_year > max_control_year) {
  message(paste0("Extending ward controls from ", max_control_year, " to ", max_permit_year))
  
  # Fast vectorized fill using crossing
  filled_data <- ward_controls %>% 
    filter(year == max_control_year) %>% 
    select(-year) %>% 
    tidyr::crossing(year = (max_control_year + 1):max_permit_year)
  
  ward_controls <- bind_rows(ward_controls, filled_data)
}

ward_controls_monthly <- ward_controls %>%
  tidyr::crossing(month_num = 1:12) %>%
  mutate(month_date = as.yearmon(paste(year, month_num, sep = "-"))) %>%
  select(-month_num, -year) %>%
  rename(month = month_date)

permits_panel <- permits_with_alderman %>%
  left_join(ward_controls_monthly, by = c("ward", "application_start_date_ym" = "month")) %>%
  filter(!is.na(homeownership_rate))

# 4. CREATE ANALYSIS VARIABLES ==============================================

permits_analysis <- permits_panel %>%
  mutate(
    month = application_start_date_ym,
    month_index = as.numeric(month - as.yearmon("2010-01")),
    year = year(as.Date(month)),
    log_processing_time = log(processing_time), 
    log_reported_cost   = log(reported_cost),
    log_total_fee       = log(total_fee),
    permit_denied       = if_else(!is.na(permit_issued), 1L - permit_issued, NA_integer_),
    corporate_applicant = as.numeric(corporate_applicant)
  ) %>%
  filter(
    !is.na(alderman),
    !is.na(log_processing_time),
    !is.na(permit_issued),
    !is.na(permit_type),
    !is.na(ward),
    !is.na(month)
  ) %>% 
  st_drop_geometry()

message("Final permit panel: ", nrow(permits_analysis), " observations")
write_csv(permits_analysis, "../output/alderman_restrictiveness_scores_data.csv")

# 5. AGGREGATE TO WARD-MONTH LEVEL ==========================================

ward_monthly_panel <- permits_analysis %>%
  group_by(ward, month, alderman) %>%
  summarise(
    n_permits_applied     = n(),
    n_permits_issued      = sum(permit_issued, na.rm = TRUE),
    n_permits_denied      = sum(permit_denied, na.rm = TRUE),
    permit_approval_rate  = mean(permit_issued, na.rm = TRUE),
    permit_denial_rate    = mean(permit_denied, na.rm = TRUE),
    mean_processing_time  = mean(processing_time, na.rm = TRUE),
    median_processing_time= median(processing_time, na.rm = TRUE),
    mean_total_fee        = mean(total_fee, na.rm = TRUE),
    mean_reported_cost    = mean(reported_cost, na.rm = TRUE),
    pct_corporate_applicant = mean(corporate_applicant, na.rm = TRUE),
    homeownership_rate    = first(homeownership_rate),
    population_density    = first(population_density),
    median_income         = first(median_income),
    percent_black         = first(percent_black),
    percent_hispanic      = first(percent_hispanic),
    year                  = first(year),
    month_index           = first(month_index),
    map_version           = first(map_version), 
    sum_total_fee         = sum(total_fee, na.rm = TRUE),
    n_total_fee_nonmiss   = sum(!is.na(total_fee)),
    .groups = "drop"
  ) %>%
  left_join(
    permits_analysis %>%
      group_by(ward, month) %>%
      count(permit_type) %>%
      mutate(pct_permit_type = n / sum(n)) %>%
      select(-n) %>%
      pivot_wider(names_from = permit_type, 
                  values_from = pct_permit_type, 
                  names_prefix = "pct_",
                  values_fill = 0),
    by = c("ward", "month")
  )

# 6. LEAN GEOGRAPHY FEATURES (3 MAPS) =======================================

# Validity check
ward_geoms_map1 <- st_make_valid(ward_geoms_map1)
ward_geoms_map2 <- st_make_valid(ward_geoms_map2)
ward_geoms_map3 <- st_make_valid(ward_geoms_map3)

# 6a) Distance to CBD
cbd <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>% st_transform(st_crs(ward_panel))

dist_feat <- function(ward_sf, ver) {
  cent <- st_centroid(ward_sf)
  tibble(
    ward = ward_sf$ward,
    map_version = ver,
    dist_cbd_km = as.numeric(units::set_units(st_distance(cent, cbd), "km"))
  )
}
dist_feats <- bind_rows(
  dist_feat(ward_geoms_map1, 1),
  dist_feat(ward_geoms_map2, 2),
  dist_feat(ward_geoms_map3, 3)
)

# 6b) CTA access
cta_feat <- function(ward_sf, ver) {
  idx <- st_is_within_distance(ward_sf, cta_stations, dist = 800)
  tibble(
    ward = ward_sf$ward,
    map_version = ver,
    n_rail_stations_800m = lengths(idx)
  )
}
cta_feats <- bind_rows(
  cta_feat(ward_geoms_map1, 1),
  cta_feat(ward_geoms_map2, 2),
  cta_feat(ward_geoms_map3, 3)
)

# 6c) Lakefront share
lake_poly <- water_osm %>% filter(!is.na(name) & tolower(name) == "lake michigan")
city_buf2km <- st_buffer(st_make_valid(city_boundary), 2000)
lake_near   <- suppressWarnings(st_intersection(st_make_valid(lake_poly), city_buf2km))
lake_1km    <- st_buffer(lake_near, 1000)

lake_feat <- function(ward_sf, ver) {
  ward_sf <- st_make_valid(ward_sf)
  inter   <- suppressWarnings(st_intersection(ward_sf, lake_1km))
  share   <- rep(0, nrow(ward_sf))
  if (nrow(inter) > 0) {
    tmp <- inter %>%
      mutate(a = as.numeric(st_area(inter))) %>%
      st_drop_geometry() %>%
      group_by(ward) %>%
      summarise(a_sum = sum(a), .groups = "drop")
    share[match(tmp$ward, ward_sf$ward)] <- tmp$a_sum
  }
  tibble(
    ward = ward_sf$ward,
    map_version = ver,
    lakefront_share_1km = pmin(1, pmax(0, share / as.numeric(st_area(ward_sf))))
  )
}
lake_feats <- bind_rows(
  lake_feat(ward_geoms_map1, 1),
  lake_feat(ward_geoms_map2, 2),
  lake_feat(ward_geoms_map3, 3)
)

# 6d) Community Area shares
community_areas <- community_areas %>%
  rename(ca_id = area_numbe, ca_name = community) %>%
  mutate(ca_id = as.integer(ca_id))

ca_shares_fun <- function(ward_sf, ver) {
  inter <- suppressWarnings(st_intersection(st_make_valid(ward_sf %>% select(ward)),
                                            st_make_valid(community_areas %>% select(ca_id))))
  if (nrow(inter) == 0) {
    return(tibble(ward = ward_sf$ward, map_version = ver))
  }
  inter2 <- inter %>%
    mutate(a = as.numeric(st_area(inter))) %>%
    st_drop_geometry() %>%
    group_by(ward, ca_id) %>%
    summarise(area_sum = sum(a), .groups = "drop")
  ward_area <- tibble(ward = ward_sf$ward, ward_area = as.numeric(st_area(ward_sf)))
  shares <- inter2 %>%
    left_join(ward_area, by = "ward") %>%
    mutate(share = pmin(1, pmax(0, area_sum / ward_area))) %>%
    select(ward, ca_id, share) %>%
    pivot_wider(names_from = ca_id, values_from = share,
                names_prefix = "ca_share_", values_fill = 0) %>%
    mutate(map_version = ver)
  shares
}
ca_feats <- bind_rows(
  ca_shares_fun(ward_geoms_map1, 1),
  ca_shares_fun(ward_geoms_map2, 2),
  ca_shares_fun(ward_geoms_map3, 3)
)

# 6e) Merge all geo features to the ward-month panel
ward_monthly_panel <- ward_monthly_panel %>%
  left_join(dist_feats, by = c("ward","map_version")) %>%
  left_join(cta_feats,  by = c("ward","map_version")) %>%
  left_join(lake_feats, by = c("ward","map_version")) %>%
  left_join(ca_feats,   by = c("ward","map_version"))

# --- Diagnostics ---
message("Ward-monthly panel created: ", nrow(ward_monthly_panel), " ward-month observations")
message("Date range: ", min(ward_monthly_panel$month), " to ", max(ward_monthly_panel$month))

# Save the ward-monthly panel
write_csv(ward_monthly_panel, "../output/ward_monthly_panel_for_alderman_fe.csv")