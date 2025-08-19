## this code creates a panel to be used to estimate aldermen fixed effects

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

ward_panel <- st_read("../input/ward_panel.gpkg")
ward_controls <-  read_csv("../input/ward_controls.csv") 
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv")

alderman_panel <- alderman_panel %>%
  mutate(month = as.yearmon(month))



## bring in parcels built after 2003
permits <- st_read("../input/building_permits_clean.gpkg") %>% 
  mutate(application_start_date = year(application_start_date_ym)) %>% 
  mutate(issue_date = year(issue_date_ym))

if (st_crs(permits) != st_crs(ward_panel)) {
  message("CRS mismatch detected. Transforming parcels CRS to match wards.")
  permits <- st_transform(permits, st_crs(ward_panel))
}

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
  filter(year >= 2016) %>%  # Use 2015+ boundaries for post-May 2015
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(period = "post")

# Function to assign ward based on application month
assign_ward_by_month <- function(permits_data) {
  # Split permits by period
  permits_pre <- permits_data %>%
    filter(as.yearmon(application_start_date_ym) < as.yearmon("May 2015"))
  
  permits_post <- permits_data %>%
    filter(as.yearmon(application_start_date_ym) >= as.yearmon("May 2015"))
  
  # Spatial join for pre-period
  permits_pre_joined <- st_join(permits_pre, ward_geoms_pre, join = st_within) %>%
    select(-ward.y) %>% 
    rename(ward = ward.x) %>% 
    filter(!is.na(ward)) %>%
    st_drop_geometry()
  
  # Spatial join for post-period  
  permits_post_joined <- st_join(permits_post, ward_geoms_post, join = st_within) %>%
    select(-ward.y) %>% 
    rename(ward = ward.x) %>% 
    filter(!is.na(ward)) %>%
    st_drop_geometry()
  
  # Combine
  bind_rows(permits_pre_joined, permits_post_joined)
}

permits_ward_data <- assign_ward_by_month(permits_high_discretion)
message("Permits spatially joined to wards: ", nrow(permits_ward_data), " observations")

# 2. ADD ALDERMAN INFORMATION (ALREADY MONTHLY) =============================

# The alderman panel is already at month level, so we can join directly
permits_with_alderman <- permits_ward_data %>%
  mutate(application_start_date_ym = as.yearmon(application_start_date_ym)) %>% 
  left_join(alderman_panel, by = c("ward", "application_start_date_ym" = "month")) %>%
  filter(!is.na(alderman))  # Keep only permits with known alderman

message("Permits with alderman info: ", nrow(permits_with_alderman), " observations")

# 3. ADD WARD CONTROLS (INTERPOLATE FROM ANNUAL TO MONTHLY) =================

# Create monthly ward controls by interpolating annual data
ward_controls_monthly <- ward_controls %>%
  # Create all month-year combinations
  crossing(month_num = 1:12) %>%
  mutate(
    month_date = as.yearmon(paste(year, month_num, sep = "-"))
  ) %>%
  # Filter to our analysis period
  select(-month_num, -year) %>%
  rename(month = month_date)

# Join ward-level controls
permits_panel <- permits_with_alderman %>%
  left_join(ward_controls_monthly, by = c("ward", "application_start_date_ym" = "month")) %>%
  filter(!is.na(homeownership_rate))  # Keep only observations with controls

# 4. CREATE ANALYSIS VARIABLES ==============================================

permits_analysis <- permits_panel %>%
  mutate(
    # Month variables
    month = application_start_date_ym,
    month_index = as.numeric(month - as.yearmon("2010-01")),
    year = year(as.Date(month)),
    
    # Outcome variables (log transformations)
    log_processing_time = log(processing_time),
    log_reported_cost = log(reported_cost),
    log_total_fee = log(total_fee),
    
    # Binary outcomes
    permit_denied = if_else(!is.na(permit_issued), 1L - permit_issued, NA_integer_),
    
    # Control variables
    corporate_applicant = as.numeric(corporate_applicant),
    
    # Ward boundary period indicator
    post_boundary_change = as.numeric(month >= as.yearmon("2015-05"))
  ) %>%
  # Filter out missing key variables
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

# Create ward-monthly panel with permit outcomes
ward_monthly_panel <- permits_analysis %>%
  group_by(ward, month, alderman) %>%
  summarise(
    # Count variables
    n_permits_applied = n(),
    n_permits_issued = sum(permit_issued, na.rm = TRUE),
    n_permits_denied = sum(permit_denied, na.rm = TRUE),
    
    # Rate variables  
    permit_approval_rate = mean(permit_issued, na.rm = TRUE),
    permit_denial_rate = mean(permit_denied, na.rm = TRUE),
    
    # Processing time variables
    mean_processing_time = mean(processing_time, na.rm = TRUE),
    median_processing_time = median(processing_time, na.rm = TRUE),
    
    # Fee variables
    mean_total_fee = mean(total_fee, na.rm = TRUE),

    # Project characteristics (for controls)
    mean_reported_cost = mean(reported_cost, na.rm = TRUE),
    pct_corporate_applicant = mean(corporate_applicant, na.rm = TRUE),
    
    # Ward controls (should be constant within ward-month)
    homeownership_rate = first(homeownership_rate),
    population_density = first(population_density),
    median_income = first(median_income),
    percent_black = first(percent_black),
    percent_hispanic = first(percent_hispanic),
    
    # Time variables
    year = first(year),
    month_index = first(month_index),
    post_boundary_change = first(post_boundary_change),
    
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

message("Ward-monthly panel created: ", nrow(ward_monthly_panel), " ward-month observations")
message("Date range: ", min(ward_monthly_panel$month), " to ", max(ward_monthly_panel$month))
message("Average permits per ward-month: ", round(mean(ward_monthly_panel$n_permits_applied), 2))

# Check distribution of permit counts
permit_count_summary <- ward_monthly_panel %>%
  summarise(
    min_permits = min(n_permits_applied),
    q25_permits = quantile(n_permits_applied, 0.25),
    median_permits = median(n_permits_applied),
    mean_permits = mean(n_permits_applied),
    q75_permits = quantile(n_permits_applied, 0.75),
    max_permits = max(n_permits_applied),
    months_with_1_permit = sum(n_permits_applied == 1),
    months_with_2plus_permits = sum(n_permits_applied >= 2),
    months_with_5plus_permits = sum(n_permits_applied >= 5)
  )

 
# Save the ward-monthly panel
write_csv(ward_monthly_panel, "../output/ward_monthly_panel_for_alderman_fe.csv")

