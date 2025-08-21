# This script runs spatial RD analyses on the dataset from the calculate_ward_boundary_distances task

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

parcels <- st_read("../input/parcels_with_ward_distances.gpkg")

# Make sure 'parcels' is your dataframe/sf object
# For safety, drop geometry for non-spatial operations
parcels <- as_tibble(st_drop_geometry(parcels))

# --- 1. Create Density Outcome Variables ---
cat("Creating density outcome variables...\n")
parcels_analysis <- parcels %>%
  mutate(
    # Floor-Area-Ratio (FAR). Handle lotsize = 0 to avoid Inf.
    density_far = if_else(sa_lotsize > 0, sa_sqft / sa_lotsize, 0),
    # Lot area per unit (LAPU)
    density_lapu = if_else(sa_nbr_units > 0, sa_lotsize / sa_nbr_units, NA_real_)
  ) %>%
  # Filter out observations missing key data for RDD
  filter(!is.na(dist_to_boundary) & !is.na(ward_pair) & !is.na(strictness_index))




# --- 2. Create Signed Distance for Aggregate RDD ---
cat("Creating signed distance based on strictness index...\n")
# Create a lookup of strictness for each ward AND boundary_year.
# This is the key fix.
strictness_lookup <- parcels_analysis %>%
  group_by(ward, boundary_year) %>%
  # In case of minor floating point noise, take the mean. This ensures one value per ward-year.
  summarise(strictness_index = mean(strictness_index, na.rm = TRUE), .groups = "drop")

# Join the lookup to get the neighbor's strictness
parcels_signed <- parcels_analysis %>%
  # Derive other_ward from ward_pair (e.g., "1_10")
  mutate(
    wards_in_pair = str_split_fixed(ward_pair, "_", 2),
    ward_a = as.integer(wards_in_pair[, 1]),
    ward_b = as.integer(wards_in_pair[, 2]),
    other_ward = if_else(ward == ward_a, ward_b, ward_a)
  ) %>%
  # Join on BOTH other_ward and boundary_year
  left_join(
    strictness_lookup,
    by = c("other_ward" = "ward", "boundary_year" = "boundary_year")
  ) %>%
  # Rename columns for clarity
  rename(
    strictness_own = strictness_index.x,
    strictness_neighbor = strictness_index.y
  ) %>%
  # Create the sign: +1 if own ward is stricter, -1 if less strict
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_ # Exclude boundaries with identical strictness
    ),
    # Create the signed distance (the running variable)
    signed_distance = dist_to_boundary * sign
  ) %>%
  # Keep only parcels where the sign is computable
  filter(!is.na(signed_distance))

cat("Data preparation complete.\n")

#================================
# FAR
#================================

bw <- 250 # Bandwidth in meters
donut_hole <- 10 # The size of the hole to exclude (e.g., +/- 50 meters)


rd_robust_result <- rdrobust(
  y = parcels_signed$density_far,
  x = parcels_signed$signed_distance,
  c = 0,
  kernel = "epanechnikov", 
  p = 1, 
  q =2,
  h = bw,
  cluster = parcels_signed$ward_pair
)
summary(rd_robust_result)


# Generate the plot using rdplot()
rdplot(
  y = parcels_signed$density_far,
  x = parcels_signed$signed_distance,
  c = 0, # The cutoff point
  h = bw, # Use the same bandwidth as the main analysis for consistency
  p = 1,     # Use a 1st-order polynomial (local linear) for the regression lines, 
  kernel = "epanechnikov",
  ci = 95,
  
  # --- Customization for a publication-quality plot ---
  title = "Discontinuity in Development Density at Ward Boundaries",
  y.label = "Floor-Area Ratio (FAR)",
  x.label = "Distance to Stricter Ward Boundary (meters)",
  x.lim = c(-500,500), 
  y.lim = c(0, 3)
)



# 2. Define the subset of data to be used.
# This logical vector is TRUE for observations outside the hole.
donut_subset <- abs(parcels_signed$signed_distance) > donut_hole

# 3. Run rdrobust with the subset argument
rd_donut_result <- rdrobust(
  y = parcels_signed$density_far,
  x = parcels_signed$signed_distance,
  c = 0,
  kernel = "epanechnikov", 
  p = 1, 
  q = 2,
  h = bw,
  cluster = parcels_signed$ward_pair,
  subset = donut_subset # <-- Apply the donut subset here
)

cat("--- Donut RDD Results (Excluding +/- 50m from cutoff) ---\n")
summary(rd_donut_result)


# 4. Generate the plot for the donut RD
# It's crucial to use the SAME subset for the plot
rdplot(
  y = parcels_signed$density_far,
  x = parcels_signed$signed_distance,
  c = 0,
  h = bw,
  p = 1,
  kernel = "epanechnikov",
  ci = 95,
  subset = donut_subset, # <-- Apply the same donut subset to the plot
  
  # --- Customization for a publication-quality plot ---
  title = "Donut RD: Discontinuity in FAR (Excluding +/- 10m)",
  y.label = "Floor-Area Ratio (FAR)",
  x.label = "Distance to Stricter Ward Boundary (meters)",
  x.lim = c(-500, 500), 
  y.lim = c(0, 3)
)







#================================
# LAPU
#================================


rd_robust_result <- rdrobust(
  y = parcels_signed$density_lapu,
  x = parcels_signed$signed_distance,
  c = 0,
  kernel = "epanechnikov", 
  p = 1, 
  q =2,
  h = bw,
  cluster = parcels_signed$ward_pair
)
summary(rd_robust_result)


# Generate the plot using rdplot()
rdplot(
  y = parcels_signed$density_lapu,
  x = parcels_signed$signed_distance,
  c = 0, # The cutoff point
  h = bw, # Use the same bandwidth as the main analysis for consistency
  p = 1,     # Use a 1st-order polynomial (local linear) for the regression lines, 
  kernel = "epanechnikov",
  ci = 95, 
  
  # --- Customization for a publication-quality plot ---
  title = "Discontinuity in Development Density at Ward Boundaries",
  y.label = "Lot Area Per Unit (LAPU)",
  x.label = "Distance to Stricter Ward Boundary (meters)",
  x.lim = c(-500,500), 
  y.lim = c(2000, 5000)
)

summary(parcels_signed$density_lapu)


# Filter the data to only include parcels within the 500m bandwidth
scatter_data <- parcels_signed %>%
  filter(abs(signed_distance) <= 100)

# Create the scatterplot
ggplot(scatter_data, aes(x = signed_distance, y = density_far)) +
  # Add the points. Use a low alpha to handle overplotting.
  geom_point(alpha = 0.3, shape = 16) + 
  
  # Add a vertical line at the cutoff for reference
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  
  # Add labels and a title for clarity
  labs(
    title = "Parcel Density (FAR) vs. Distance to Ward Boundary",
    subtitle = "Showing all parcels within 500 meters of the boundary cutoff",
    x = "Distance to Stricter Ward Boundary (meters)",
    y = "Floor-Area Ratio (FAR)"
  ) +
  
  # Use a clean theme
  theme_minimal()

