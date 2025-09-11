# This script makes maps of new construction around the city with ward boundaries overlaid 

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# load analysis sample for mapping 
cat("Loading and preparing data...\n")
zoning_data <- st_read("../input/zoning_data_clean.gpkg") 


# Ensure sf knows which column is geometry (your gpkg names it "geom")
if (!inherits(st_geometry(zoning_data), "sfc")) {
  if ("geom" %in% names(zoning_data)) {
    st_geometry(zoning_data) <- "geom"
  } else {
    stop("No geometry column found. Expected a column named 'geom'.")
  }
}

# Make geometries valid (avoids sliver/plot errors)
zoning_data <- st_make_valid(zoning_data)

# Project to Chicago / Illinois StatePlane East (ftUS) EPSG:3435
if (is.na(st_crs(zoning_data))) {
  stop("zoning_data has no CRS. Please set its CRS before transforming.")
}
zoning_3435 <- st_transform(zoning_data, 3435)

# Optional: a broader class for a second map (R/B/C/D/M/PD/PMD/T/POS)
zoning_3435 <- zoning_3435 %>%
  mutate(zone_class = str_extract(zone_code, "^[A-Z]+")) %>% 
  mutate(across(c(floor_area_ratio, maximum_building_height, lot_area_per_unit), 
                function(x) as.numeric(x)))

# A minimal, legible theme for maps
theme_map <- theme_void(base_size = 10) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 7),
    plot.title   = element_text(face = "bold", size = 13),
    plot.subtitle= element_text(size = 10),
    plot.caption = element_text(size = 8)
  )

# -------- Map 1: by full zone_code (fine-grained) --------
p1 <- ggplot() +
  geom_sf(data = zoning_3435,
          aes(fill = zone_code),
          color = "white", linewidth = 0.08, alpha = 0.95) +
  coord_sf(crs = st_crs(zoning_3435), datum = NA) +
  scale_fill_viridis_d(name = "Zoning district", option = "turbo", direction = 1, na.translate = FALSE) +
  labs(
    title = "Chicago Base Zoning Districts",
    caption = "Data from City of Chicago and Second City Zoning"
  ) +
  theme_map


p1

p3 <- ggplot() +
  geom_sf(data = zoning_3435,
          aes(fill = floor_area_ratio),
          color = "white", linewidth = 0.08, alpha = 0.95) +
  coord_sf(crs = st_crs(zoning_3435), datum = NA) +
  scale_fill_viridis_c(
    name = "FAR",
    na.value = "grey85",
    limits = c(0, 4),
    oob = scales::squish,  
    breaks = function(lims) c(0.5, 0.9, 1.2, 2.2, 3, 5, 7, 10)[c(0.5, 0.9, 1.2, 2.2, 3, 5, 7, 10) <= lims[2]]
  ) +
  labs(
    title = "Chicago Zoning — Floor Area Ratio (FAR)",
    caption = "Data from City of Chicago and Second City Zoning"
  ) +
  theme_map

p3



# -------- (Optional) Map 2: by broad class (cleaner legend) --------
# Uncomment if you want a simpler legend by R / B / C / D / M / PD / PMD / T / POS
p2 <- ggplot() +
  geom_sf(data = zoning_3435,
          aes(fill = zone_class),
          color = "white", linewidth = 0.08, alpha = 0.95) +
  coord_sf(crs = st_crs(zoning_3435), datum = NA) +
  scale_fill_viridis_d(name = "Zoning class", option = "viridis", na.translate = FALSE) +
  labs(
    title = "Chicago Zoning (by class)",
    caption = "Source: City of Chicago"
  ) +
  theme_map

p2
