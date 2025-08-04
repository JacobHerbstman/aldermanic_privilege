# This script calculates signed distances from parcels to ward boundaries
# and assigns aldermen based on construction year and redistricting events

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")


# Configuration parameters for redistricting handling
REDISTRICTING_RULE <- "construction_year"  # Options: "strict_2015", "gradual_2012_2015", "construction_year"
REDISTRICTING_CUTOFF <- as.Date("2015-05-01")  # For strict rule
REDISTRICTING_START <- as.Date("2012-01-01")   # For gradual rule
REDISTRICTING_END <- as.Date("2015-05-01")     # For gradual rule

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

cat("Loading parcel data...\n")
parcels <- st_read("../input/year_built_sample.gpkg") %>%
  # Ensure we have clean data
  filter(!is.na(sa_yr_blt), sa_yr_blt >= 2006 & sa_yr_blt <= 2023 ) %>%
  # Add year as Date for easier joining
  mutate(construction_date = as.Date(paste0(sa_yr_blt, "-06-15"))) # Mid-year construction

cat("Loading ward boundaries...\n")
ward_panel <- st_read("../input/ward_panel.gpkg")

cat("Loading alderman panel...\n")
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv")


if (st_crs(parcels) != st_crs(ward_panel)) {
  message("CRS mismatch detected. Transforming parcels CRS to match census_block groups.")
  parcels <- st_transform(parcels, st_crs(ward_panel))
}

# -----------------------------------------------------------------------------
# 2. REDISTRICTING LOGIC FUNCTIONS
# -----------------------------------------------------------------------------

# Function to determine which ward boundaries to use based on construction year
get_ward_boundaries_for_year <- function(construction_year, rule = REDISTRICTING_RULE) {
  
  if (rule == "strict_2015") {
    # Simple cutoff at May 2015 (when new aldermen took office)
    boundary_year <- ifelse(construction_year < 2015, 2014, 2015)
    
  } else if (rule == "gradual_2012_2015") {
    # Gradual transition - old boundaries through 2012, new from 2015, ambiguous 2013-2014
    boundary_year <- case_when(
      construction_year <= 2012 ~ 2014,  # Use old boundaries through 2012
      construction_year >= 2015 ~ 2015,  # Use new boundaries from 2015 onward
      TRUE ~ 2014  # For ambiguous period 2013-2014, use old boundaries
    )
    
  } else if (rule == "construction_year") {
    # Use boundaries that match the construction year exactly when possible
    boundary_year <- case_when(
      construction_year <= 2014 ~ 2014,  # Old ward system
      construction_year >= 2015 ~ 2015,  # New ward system 
      TRUE ~ 2015  # Default to new boundaries
    )
  }
  
  return(boundary_year)
}

# Function to get alderman for a parcel based on construction date and ward
get_alderman_for_parcel <- function(ward_id, construction_date, alderman_data) {
  
  # Convert construction date to yearmon for matching
  construction_yearmon <- as.yearmon(construction_date)
  
  # Find alderman for this ward and time
  ward_aldermen <- alderman_data %>%
    filter(ward == ward_id)
  
  # Get alderman for the construction month
  alderman_match <- ward_aldermen %>%
    filter(month == construction_yearmon) %>%
    slice(1)  # Take first match if multiple
  
  if (nrow(alderman_match) == 0) {
    return(list(alderman = NA_character_, 
                finance_chair = 0, 
                zoning_chair = 0, 
                budget_chair = 0))
  }
  
  return(list(
    alderman = alderman_match$alderman,
    finance_chair = alderman_match$finance_chair,
    zoning_chair = alderman_match$zoning_chair,
    budget_chair = alderman_match$budget_chair
  ))
}



# -----------------------------------------------------------------------------
# 3. WARD ASSIGNMENT AND BOUNDARY DISTANCE CALCULATION
# -----------------------------------------------------------------------------

cat("Assigning wards to parcels based on construction year...\n")

# Function to assign ward and calculate distances for a batch of parcels
assign_ward_and_distances <- function(parcel_batch) {
  
  results <- parcel_batch %>%
    rowwise() %>%
    mutate(
      # Determine which boundary year to use
      boundary_year = get_ward_boundaries_for_year(sa_yr_blt, REDISTRICTING_RULE)
    ) %>%
    ungroup()
  
  # Spatial join to assign wards - using the correct geometry column names
  results_with_wards <- results %>%
    st_join(
      ward_panel %>% 
        filter(year %in% unique(results$boundary_year)) %>%
        select(year, ward, geom),  # Use 'geom' not 'geometry'
      suffix = c("", "_ward")
    ) %>%
    # Filter to correct year matches
    filter(boundary_year == year) %>%
    select(-year) %>%
    rename(assigned_ward = ward)
  
  return(results_with_wards)
}

# Process parcels in batches to manage memory
batch_size <- 5000
n_batches <- ceiling(nrow(parcels) / batch_size)

cat(sprintf("Processing %d parcels in %d batches...\n", nrow(parcels), n_batches))

parcels_with_wards <- map_dfr(1:n_batches, function(i) {
  
  cat(sprintf("Processing batch %d of %d...\n", i, n_batches))
  
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(parcels))
  
  batch <- parcels[start_idx:end_idx, ]
  
  assign_ward_and_distances(batch)
})

# -----------------------------------------------------------------------------
# 4. CALCULATE ADJACENT WARD PAIRS FIRST
# -----------------------------------------------------------------------------

cat("Identifying adjacent ward pairs...\n")

# Function to identify adjacent ward pairs
get_adjacent_ward_pairs <- function(ward_boundaries) {
  
  # Get all unique wards
  wards <- unique(ward_boundaries$ward)
  
  # Find touching ward pairs
  adjacent_pairs <- tibble()
  
  for (i in 1:(length(wards)-1)) {
    for (j in (i+1):length(wards)) {
      
      ward_a <- wards[i]
      ward_b <- wards[j]
      
      geom_a <- ward_boundaries %>% filter(ward == ward_a) %>% st_geometry()
      geom_b <- ward_boundaries %>% filter(ward == ward_b) %>% st_geometry()
      
      # Check if they touch
      if (st_touches(geom_a, geom_b, sparse = FALSE)[1,1]) {
        adjacent_pairs <- bind_rows(adjacent_pairs, 
                                    tibble(ward_a = ward_a, ward_b = ward_b,
                                           ward_pair = paste(min(ward_a, ward_b), 
                                                             max(ward_a, ward_b), sep = "_")))
      }
    }
  }
  
  return(adjacent_pairs)
}

# Get adjacent pairs for each boundary year
adjacent_pairs_2014 <- get_adjacent_ward_pairs(ward_panel %>% filter(year == 2014))
adjacent_pairs_2015 <- get_adjacent_ward_pairs(ward_panel %>% filter(year == 2015))

cat(sprintf("Found %d adjacent ward pairs for 2014\n", nrow(adjacent_pairs_2014)))
cat(sprintf("Found %d adjacent ward pairs for 2015\n", nrow(adjacent_pairs_2015)))


# -----------------------------------------------------------------------------
# 5. CALCULATE DISTANCES TO RELEVANT WARD BOUNDARIES
# -----------------------------------------------------------------------------

cat("Calculating distances to relevant ward boundaries...\n")

# Function to calculate distance to relevant boundaries only
calculate_relevant_boundary_distances <- function(parcels_df) {
  
  boundary_years <- unique(parcels_df$boundary_year)
  
  results_list <- map(boundary_years, function(by) {
    
    cat(sprintf("Processing boundary year %d...\n", by))
    
    year_parcels <- parcels_df %>% filter(boundary_year == by)
    if (nrow(year_parcels) == 0) return(NULL)
    
    year_boundaries <- ward_panel %>% filter(year == by)
    
    # Get adjacent pairs for this year
    if (by == 2014) {
      adjacent_pairs <- adjacent_pairs_2014
    } else {
      adjacent_pairs <- adjacent_pairs_2015
    }
    
    # Create boundary lines between adjacent ward pairs
    cat("Creating boundary lines between adjacent wards...\n")
    boundary_segments <- map_dfr(1:nrow(adjacent_pairs), function(i) {
      
      ward_a <- adjacent_pairs$ward_a[i]
      ward_b <- adjacent_pairs$ward_b[i]
      ward_pair <- adjacent_pairs$ward_pair[i]
      
      geom_a <- year_boundaries %>% filter(ward == ward_a) %>% st_geometry()
      geom_b <- year_boundaries %>% filter(ward == ward_b) %>% st_geometry()
      
      # Get shared boundary line
      boundary_line <- st_intersection(st_boundary(geom_a), st_boundary(geom_b))
      
      if(length(boundary_line) > 0 && !st_is_empty(boundary_line)) {
        tibble(
          ward_pair = ward_pair,
          ward_a = ward_a,
          ward_b = ward_b,
          boundary_year = by,
          geometry = boundary_line
        )
      } else {
        NULL
      }
    })
    
    if (nrow(boundary_segments) == 0) {
      cat("Warning: No boundary segments found for year", by, "\n")
      return(year_parcels %>% mutate(dist_to_boundary = NA_real_))
    }
    
    # Convert to sf object
    boundary_segments_sf <- boundary_segments %>% 
      st_as_sf() %>%
      st_cast("LINESTRING", warn = FALSE)
    
    cat(sprintf("Created %d boundary segments\n", nrow(boundary_segments_sf)))
    
    # For each parcel, calculate distance to all relevant boundaries
    cat("Calculating parcel distances to boundaries...\n")
    
    year_parcels_with_distances <- year_parcels %>%
      rowwise() %>%
      mutate(
        # Find boundary segments relevant to this parcel's ward
        relevant_boundaries = list({
          boundary_segments_sf %>%
            filter(ward_a == assigned_ward | ward_b == assigned_ward)
        }),
        
        # Calculate distances to all relevant boundaries
        dist_to_boundary = if(nrow(relevant_boundaries[[1]]) > 0) {
          
          # Calculate distances to all relevant boundary segments
          distances_to_segments <- st_distance(
            geometry, 
            relevant_boundaries[[1]]$geometry
          )
          
          # Return minimum distance
          as.numeric(min(distances_to_segments))
          
        } else {
          NA_real_  # No relevant boundaries (isolated ward)
        }
      ) %>%
      ungroup() %>%
      select(-relevant_boundaries)  # Remove helper column
    
    return(year_parcels_with_distances)
  })
  
  bind_rows(results_list)
}

# Calculate distances using the correct approach
parcels_with_distances <- calculate_relevant_boundary_distances(parcels_with_wards)

cat("Distance calculation completed!\n")

# -----------------------------------------------------------------------------
# 6. ADD WARD PAIR INFORMATION
# -----------------------------------------------------------------------------

cat("Adding ward pair information to parcels...\n")

# Create comprehensive ward pair lookup
create_ward_pair_lookup <- function(adjacent_pairs_df, boundary_year) {
  bind_rows(
    # Ward A perspective
    adjacent_pairs_df %>% 
      select(ward = ward_a, ward_pair) %>%
      mutate(boundary_year = boundary_year),
    # Ward B perspective  
    adjacent_pairs_df %>% 
      select(ward = ward_b, ward_pair) %>%
      mutate(boundary_year = boundary_year)
  ) %>%
    distinct(ward, ward_pair, boundary_year)
}

# Create lookup tables
ward_pair_lookup_2014 <- create_ward_pair_lookup(adjacent_pairs_2014, 2014)
ward_pair_lookup_2015 <- create_ward_pair_lookup(adjacent_pairs_2015, 2015)
all_ward_pair_lookup <- bind_rows(ward_pair_lookup_2014, ward_pair_lookup_2015)

# Add ward pair information (keeping first match for each parcel)
parcels_with_pairs <- parcels_with_distances %>%
  left_join(all_ward_pair_lookup, 
            by = c("boundary_year", "assigned_ward" = "ward"),
            relationship = "many-to-many") %>%
  # Keep first ward pair for each parcel (parcels near corners might touch multiple pairs)
  group_by(attom_id, assigned_ward, boundary_year) %>%
  slice(1) %>%
  ungroup()

cat("Ward pair assignment completed!\n")







# -----------------------------------------------------------------------------
# 7. ADD ALDERMAN INFORMATION
# -----------------------------------------------------------------------------

cat("Adding alderman information...\n")

# Add alderman information for each parcel
parcels_final <- parcels_with_pairs %>%
  rowwise() %>%
  mutate(
    alderman_info = list(get_alderman_for_parcel(assigned_ward, construction_date, alderman_panel)),
    alderman = alderman_info$alderman,
    finance_chair = alderman_info$finance_chair,
    zoning_chair = alderman_info$zoning_chair,
    budget_chair = alderman_info$budget_chair
  ) %>%
  select(-alderman_info) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 8. CREATE FINAL VARIABLES
# -----------------------------------------------------------------------------

cat("Creating final variables...\n")

final_dataset <- parcels_final %>%
  mutate(
    # Create signed distance (positive for one ward, negative for the other in each pair)
    # This will be refined in the RDD analysis step
    signed_dist_to_boundary = dist_to_boundary,
    
    # Clean up variable names
    ward = assigned_ward,
    construction_year = sa_yr_blt,
    
    # Add alderman tenure (months in office by construction date)
    construction_yearmon = as.yearmon(construction_date)
  ) %>%
  
  # Calculate alderman tenure
  left_join(
    alderman_panel %>%
      group_by(ward, alderman) %>%
      summarise(
        first_month = min(month),
        .groups = "drop"
      ),
    by = c("ward", "alderman")
  ) %>%
  mutate(
    alderman_tenure_months = as.numeric((construction_yearmon - first_month) * 12)
  ) %>%
  
  # Select final variables
  select(
    attom_id,
    geometry,
    construction_year,
    construction_date,
    boundary_year,
    ward,
    ward_pair,
    dist_to_boundary,
    signed_dist_to_boundary,
    alderman,
    alderman_tenure_months,
    finance_chair,
    zoning_chair,
    budget_chair,
    sa_lotsize,
    sa_sqft
  ) %>%
  
  # Remove parcels without ward assignments
  filter(!is.na(ward))

# -----------------------------------------------------------------------------
# 8. SAVE OUTPUT
# -----------------------------------------------------------------------------

cat("Saving output...\n")

# Save as spatial file
st_write(final_dataset, "../output/parcels_with_ward_distances.gpkg", delete_dsn = TRUE)

# Also save as parquet for faster loading
write_parquet(st_drop_geometry(final_dataset), "../output/parcels_with_ward_distances.parquet")

# Save summary statistics
summary_stats <- final_dataset %>%
  st_drop_geometry() %>%
  summarise(
    n_parcels = n(),
    n_wards = n_distinct(ward),
    n_ward_pairs = n_distinct(ward_pair, na.rm = TRUE),
    n_aldermen = n_distinct(alderman, na.rm = TRUE),
    mean_dist_to_boundary = mean(dist_to_boundary, na.rm = TRUE),
    median_dist_to_boundary = median(dist_to_boundary, na.rm = TRUE),
    .by = c(boundary_year, construction_year)
  )

write_csv(summary_stats, "../output/boundary_distance_summary.csv")

cat("Task completed successfully!\n")
cat(sprintf("Processed %d parcels across %d wards\n", 
            nrow(final_dataset), 
            n_distinct(final_dataset$ward)))
cat(sprintf("Found %d unique ward pairs\n", 
            n_distinct(final_dataset$ward_pair, na.rm = TRUE)))