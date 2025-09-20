## this code creates a panel to be used to estimate aldermen fixed effects

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# --- Core inputs ---
ward_panel     <- st_read("../input/ward_panel.gpkg")
ward_controls  <- read_csv("../input/ward_controls.csv")
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv") %>%
  mutate(month = as.yearmon(month))

# Permits (geospatial)
permits <- st_read("../input/building_permits_clean.gpkg") %>%
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
cta_stations     <- read_to_ward_crs("../input/cta_stations.geojson")           # points
city_boundary    <- read_to_ward_crs("../input/city_boundary.geojson")          # polygon
community_areas  <- read_to_ward_crs("../input/community_areas.geojson")        # polygons (77)
water_osm        <- read_to_ward_crs("../input/gis_osm_water_a_free_1.shp")     # polygons (OSM water); will filter to Lake Michigan

################################################
###### filter to HIGH DISCRETION PERMITS ONLY
permits_high_discretion <- permits %>% 
  filter(high_discretion == 1)
##############################################

# 1. SPATIAL JOIN PERMITS TO WARDS BY MONTH ================================

# Create ward geometries by period (pre and post May 2015)
ward_geoms_pre <- ward_panel %>%
  filter(year <= 2014) %>%  # Use 2014 boundaries for pre-May 2015
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(period = "pre")

ward_geoms_post <- ward_panel %>%
  filter(year >= 2016) %>%  # Use 2016+ boundaries for post-May 2015
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(period = "post")

# Function to assign ward based on application month
assign_ward_by_month <- function(permits_data) {
  permits_pre <- permits_data %>%
    filter(as.yearmon(application_start_date_ym) < as.yearmon("2015-05"))
  
  permits_post <- permits_data %>%
    filter(as.yearmon(application_start_date_ym) >= as.yearmon("2015-05"))
  
  permits_pre_joined <- st_join(permits_pre, ward_geoms_pre, join = st_within) %>%
    select(-ward.y) %>% 
    rename(ward = ward.x) %>% 
    filter(!is.na(ward)) %>%
    st_drop_geometry()
  
  permits_post_joined <- st_join(permits_post, ward_geoms_post, join = st_within) %>%
    select(-ward.y) %>% 
    rename(ward = ward.x) %>% 
    filter(!is.na(ward)) %>%
    st_drop_geometry()
  
  bind_rows(permits_pre_joined, permits_post_joined)
}

permits_ward_data <- assign_ward_by_month(permits_high_discretion)
message("Permits spatially joined to wards: ", nrow(permits_ward_data), " observations")

# 2. ADD ALDERMAN INFORMATION (ALREADY MONTHLY) =============================

permits_with_alderman <- permits_ward_data %>%
  mutate(application_start_date_ym = as.yearmon(application_start_date_ym)) %>% 
  left_join(alderman_panel, by = c("ward", "application_start_date_ym" = "month")) %>%
  filter(!is.na(alderman))

message("Permits with alderman info: ", nrow(permits_with_alderman), " observations")

# 3. ADD WARD CONTROLS (INTERPOLATE FROM ANNUAL TO MONTHLY) =================

ward_controls_monthly <- ward_controls %>%
  crossing(month_num = 1:12) %>%
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
    corporate_applicant = as.numeric(corporate_applicant),
    post_boundary_change= as.numeric(month >= as.yearmon("2015-05"))
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

# 5. AGGREGATE TO WARD-MONTH LEVEL ==============================================

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
    post_boundary_change  = first(post_boundary_change),
    .groups = "drop"
  ) %>%
  # Add permit type composition
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

# 6. LEAN GEOGRAPHY FEATURES (pre/post) ========================================

# Prepare ward geoms again (validity)
ward_geoms_pre  <- st_make_valid(ward_geoms_pre)
ward_geoms_post <- st_make_valid(ward_geoms_post)

# 6a) Distance to CBD (City Hall ~ 121 N LaSalle: -87.6313, 41.8837)
cbd <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>% st_transform(st_crs(ward_panel))

dist_feat <- function(ward_sf, post_flag) {
  cent <- st_centroid(ward_sf)
  tibble(
    ward = ward_sf$ward,
    post_boundary_change = post_flag,
    dist_cbd_km = as.numeric(units::set_units(st_distance(cent, cbd), "km"))
  )
}
dist_pre  <- dist_feat(ward_geoms_pre, 0L)
dist_post <- dist_feat(ward_geoms_post, 1L)
dist_feats <- bind_rows(dist_pre, dist_post)

# 6b) CTA access: # stations within 800m (no buffering required)
cta_feat <- function(ward_sf, post_flag) {
  idx <- st_is_within_distance(ward_sf, cta_stations, dist = 800)
  tibble(
    ward = ward_sf$ward,
    post_boundary_change = post_flag,
    n_rail_stations_800m = lengths(idx)
  )
}
cta_pre  <- cta_feat(ward_geoms_pre, 0L)
cta_post <- cta_feat(ward_geoms_post, 1L)
cta_feats <- bind_rows(cta_pre, cta_post)

# 6c) Lakefront share within 1km of Lake Michigan
lake_poly <- water_osm %>%
  filter(!is.na(name) & tolower(name) == "lake michigan")

# limit to area around Chicago to speed up ops
city_buf2km <- st_buffer(st_make_valid(city_boundary), 2000)
lake_near   <- suppressWarnings(st_intersection(st_make_valid(lake_poly), city_buf2km))
lake_1km    <- st_buffer(lake_near, 1000)

lake_feat <- function(ward_sf, post_flag) {
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
    post_boundary_change = post_flag,
    lakefront_share_1km = pmin(1, pmax(0, share / as.numeric(st_area(ward_sf))))
  )
}
lake_pre  <- lake_feat(ward_geoms_pre, 0L)
lake_post <- lake_feat(ward_geoms_post, 1L)
lake_feats <- bind_rows(lake_pre, lake_post)

# 6d) Community Area area-weighted shares
community_areas <- community_areas %>%
  rename(ca_id = area_numbe, ca_name = community) %>%
  mutate(ca_id = as.integer(ca_id))

ca_shares_fun <- function(ward_sf, post_flag) {
  inter <- suppressWarnings(st_intersection(st_make_valid(ward_sf %>% select(ward)),
                                            st_make_valid(community_areas %>% select(ca_id))))
  if (nrow(inter) == 0) {
    return(tibble(ward = ward_sf$ward, post_boundary_change = post_flag))
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
    mutate(post_boundary_change = post_flag)
  shares
}
ca_pre  <- ca_shares_fun(ward_geoms_pre, 0L)
ca_post <- ca_shares_fun(ward_geoms_post, 1L)
ca_feats <- bind_rows(ca_pre, ca_post)

# 6e) Merge all geo features to the ward-month panel
ward_monthly_panel <- ward_monthly_panel %>%
  left_join(dist_feats, by = c("ward","post_boundary_change")) %>%
  left_join(cta_feats,  by = c("ward","post_boundary_change")) %>%
  left_join(lake_feats, by = c("ward","post_boundary_change")) %>%
  left_join(ca_feats,   by = c("ward","post_boundary_change"))

# --- Diagnostics ---
message("Ward-monthly panel created: ", nrow(ward_monthly_panel), " ward-month observations")
message("Date range: ", min(ward_monthly_panel$month), " to ", max(ward_monthly_panel$month))
message("Average permits per ward-month: ", round(mean(ward_monthly_panel$n_permits_applied), 2))

permit_count_summary <- ward_monthly_panel %>%
  summarise(
    min_permits = min(n_permits_applied),
    q25_permits = quantile(n_permits_applied, 0.25),
    median_permits = median(n_permits_applied),
    mean_permits = mean(n_permits_applied),
    q75_permits = quantile(n_permits_applied, 0.75),
    max_permits = max(n_permits_applied),
    months_with_1_permit  = sum(n_permits_applied == 1),
    months_with_2plus_permits = sum(n_permits_applied >= 2),
    months_with_5plus_permits = sum(n_permits_applied >= 5)
  )

# Save the ward-monthly panel
write_csv(ward_monthly_panel, "../output/ward_monthly_panel_for_alderman_fe.csv")




### QUALITY CONTROL CHECKS ####


# 
# qc <- ward_monthly_panel %>%
#   select(ward, post_boundary_change, dist_cbd_km, lakefront_share_1km, starts_with("ca_share_")) %>%
#   distinct()
# 
# # ---- 1) Community Area shares ----
# ca_cols <- grep("^ca_share_", names(qc), value = TRUE)
# 
# if (length(ca_cols) > 0) {
#   qc_ca <- qc %>%
#     mutate(ca_sum = rowSums(across(all_of(ca_cols)), na.rm = TRUE),
#            ca_max = pmax(!!!rlang::syms(ca_cols), na.rm = TRUE))
#   
#   # (a) Should sum to ~1 (small numeric tolerances OK)
#   cat("\n[QC] Community area shares — sums by ward/post (expect ~1.0):\n")
#   print(qc_ca %>%
#           group_by(post_boundary_change) %>%
#           summarise(min_sum = min(ca_sum),
#                     q10_sum = quantile(ca_sum, 0.10),
#                     median_sum = median(ca_sum),
#                     q90_sum = quantile(ca_sum, 0.90),
#                     max_sum = max(ca_sum),
#                     .groups = "drop"))
#   
#   prob_ca <- qc_ca %>%
#     filter(ca_sum < 0.98 | ca_sum > 1.02)
#   if (nrow(prob_ca) > 0) {
#     cat("\n[QC WARNING] Wards with CA shares not summing to ~1 (outside [0.98, 1.02]):\n")
#     print(prob_ca %>% arrange(ca_sum) %>% select(ward, post_boundary_change, ca_sum) %>% head(20))
#   } else {
#     cat("\n[QC] CA share sums look good (all within [0.98, 1.02]).\n")
#   }
#   
#   # (b) Dominance: top CA share should usually be sizable
#   cat("\n[QC] Top CA share per ward/post (bigger is cleaner membership):\n")
#   print(qc_ca %>%
#           group_by(post_boundary_change) %>%
#           summarise(min_top = min(ca_max),
#                     q25_top = quantile(ca_max, 0.25),
#                     median_top = median(ca_max),
#                     q75_top = quantile(ca_max, 0.75),
#                     max_top = max(ca_max),
#                     .groups = "drop"))
#   
#   low_dom <- qc_ca %>% filter(ca_max < 0.4)
#   if (nrow(low_dom) > 0) {
#     cat("\n[QC NOTE] Wards with diffuse CA membership (top share < 0.4):\n")
#     print(low_dom %>% select(ward, post_boundary_change, ca_max) %>% head(20))
#   }
# } else {
#   cat("\n[QC] No CA share columns found (ca_share_*). Skipping CA checks.\n")
# }
# 
# # ---- 2) Lakefront share (1km) ----
# if ("lakefront_share_1km" %in% names(qc)) {
#   cat("\n[QC] Lakefront share within 1km — summary:\n")
#   print(qc %>%
#           group_by(post_boundary_change) %>%
#           summarise(min = min(lakefront_share_1km, na.rm = TRUE),
#                     q25 = quantile(lakefront_share_1km, 0.25, na.rm = TRUE),
#                     median = median(lakefront_share_1km, na.rm = TRUE),
#                     q75 = quantile(lakefront_share_1km, 0.75, na.rm = TRUE),
#                     max = max(lakefront_share_1km, na.rm = TRUE),
#                     .groups = "drop"))
#   
#   out_range <- qc %>% filter(lakefront_share_1km < -1e-6 | lakefront_share_1km > 1 + 1e-6)
#   if (nrow(out_range) > 0) {
#     cat("\n[QC WARNING] lakefront_share_1km outside [0,1]:\n")
#     print(out_range %>% select(ward, post_boundary_change, lakefront_share_1km))
#   }
#   
#   lakey <- qc %>% filter(lakefront_share_1km >= 0.20) %>%
#     arrange(desc(lakefront_share_1km))
#   cat("\n[QC] Wards with substantial lakefront (>= 0.20 of area within 1km):\n")
#   print(lakey %>% select(ward, post_boundary_change, lakefront_share_1km) %>% head(20))
# } else {
#   cat("\n[QC] lakefront_share_1km not found. Skipping lakefront checks.\n")
# }
# 
# # ---- 3) Distance to CBD (km) ----
# if ("dist_cbd_km" %in% names(qc)) {
#   cat("\n[QC] Distance to CBD (km) — summary:\n")
#   print(qc %>%
#           group_by(post_boundary_change) %>%
#           summarise(min = min(dist_cbd_km, na.rm = TRUE),
#                     q25 = quantile(dist_cbd_km, 0.25, na.rm = TRUE),
#                     median = median(dist_cbd_km, na.rm = TRUE),
#                     q75 = quantile(dist_cbd_km, 0.75, na.rm = TRUE),
#                     max = max(dist_cbd_km, na.rm = TRUE),
#                     .groups = "drop"))
#   
#   cat("\n[QC] Nearest 10 wards to CBD:\n")
#   print(qc %>% arrange(dist_cbd_km) %>% select(ward, post_boundary_change, dist_cbd_km) %>% head(10))
#   
#   cat("\n[QC] Farthest 10 wards from CBD:\n")
#   print(qc %>% arrange(desc(dist_cbd_km)) %>% select(ward, post_boundary_change, dist_cbd_km) %>% head(10))
# } else {
#   cat("\n[QC] dist_cbd_km not found. Skipping CBD checks.\n")
# }
