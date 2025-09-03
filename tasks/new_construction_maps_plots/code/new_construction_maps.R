# This script makes maps of new construction around the city with ward boundaries overlaid 

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# load analysis sample for mapping 
cat("Loading and preparing data...\n")
parcels <- st_read("../input/parcels_with_geometry.gpkg")
ward_panel <- st_read("../input/ward_panel.gpkg")

# Filter parcels for construction years between 2006 and 2014
parcels_2006_2014 <- parcels %>%
  filter(construction_year >= 2006 & construction_year <= 2014) %>% 
  filter(density_far > 0)

parcels_2015_2023 <- parcels %>%
  filter(construction_year >= 2015) %>% 
  filter(density_far > 0)

# Use the 2014 ward boundaries, which were in effect for the 2006-2014 period
wards_2014 <- ward_panel %>%
  filter(year == 2014)

wards_2015 <- ward_panel %>%
  filter(year == 2015)

# -----------------------------------------------------------------------------
# 2. CREATE AND SAVE THE MAP
# -----------------------------------------------------------------------------

cat("Generating map of new construction...\n")

# Create the plot with ggplot2
construction_map <- ggplot() +
  # Add the ward boundaries with a white fill and clear, dark gray outlines
  geom_sf(data = wards_2014, fill = "white", color = "grey20") +
  
  # Add the parcel locations, with color mapped to Floor Area Ratio (FAR)
  geom_sf(data = parcels_2006_2014, aes(color = density_far), size = 0.3, alpha = 0.5) +
  
  # Use a capped color scale to handle outliers and show more variation
  scale_color_viridis_c(
    option = "plasma", 
    name = "Floor Area Ratio (FAR)",
    limits = c(0, 2),  # Set the range of the color scale
    oob = scales::squish # "Squish" out-of-bounds values to the limits
  ) +
  
  # Add labels and a clean theme
  labs(
    title = "New Residential Construction in Chicago (2006-2014)",
    subtitle = "Construction locations colored by density (FAR)"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


construction_map

# Save the plot to the output directory
ggsave("../output/construction_map_2006_2014_w_density.pdf", plot = construction_map, width = 8, height = 10, dpi = 300)

cat("Map saved to ../output/construction_map_2006_2014_w_density.pdf\n")



# -----------------------------------------------------------------------------
# 2. CREATE AND SAVE THE MAP
# -----------------------------------------------------------------------------

cat("Generating map of new construction...\n")

# Create the plot with ggplot2
construction_map <- ggplot() +
  # Add the ward boundaries with a white fill and clear, dark gray outlines
  geom_sf(data = wards_2015, fill = "white", color = "gray20") +
  
  # Add the parcel locations, with color mapped to Floor Area Ratio (FAR)
  geom_sf(data = parcels_2015_2023, aes(color = density_far), size = 0.3, alpha = 0.5) +
  
  # Use a capped color scale to handle outliers and show more variation
  scale_color_viridis_c(
    option = "plasma", 
    name = "Floor Area Ratio (FAR)",
    limits = c(0, 2),  # Set the range of the color scale
    oob = scales::squish # "Squish" out-of-bounds values to the limits
  ) +
  
  # Add labels and a clean theme
  labs(
    title = "New Residential Construction in Chicago (2015-2023)",
    subtitle = "Construction locations colored by density (FAR)"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


construction_map

ggsave("../output/construction_map_2015_2023_w_density.pdf", plot = construction_map, width = 8, height = 10, dpi = 300)

cat("Map saved to ../output/construction_map_2015_2023_w_density.pdf\n")

