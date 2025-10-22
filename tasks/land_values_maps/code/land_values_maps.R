# This script makes maps of new construction around the city with ward boundaries overlaid 

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# load analysis sample for mapping 
cat("Loading and preparing data...\n")
event_study_df <- read_csv("../input/land_event_study_data.csv")

ward_panel <- st_read("../input/ward_panel.gpkg")

census_blocks <- read_csv("../input/census_blocks_2010.csv") %>%
  rename(geometry = the_geom)

census_blocks <- st_as_sf(census_blocks, wkt = "geometry", crs = 4269) %>%
  st_transform(st_crs(ward_panel)) %>% # Use the new annual panel for CRS
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id))

event_study_df <- event_study_df %>%
  filter(tax_year == 2020)

ward_panel <- ward_panel %>%
  filter(year == 2020)



cat("Aggregating land_share_pin10 by ward...\n")

ward_stats <- event_study_df %>%
  dplyr::filter(!is.na(ward), !is.na(land_share_pin10)) %>%
  dplyr::mutate(ward = as.integer(ward)) %>%
  dplyr::group_by(ward) %>%
  dplyr::summarise(
    land_share_mean = mean(land_share_pin10, na.rm = TRUE),
    n_parcels = dplyr::n(),
    .groups = "drop"
  )

# --- Join to ward geometries ---
ward_map <- ward_panel %>%
  dplyr::mutate(ward = as.integer(ward)) %>%
  dplyr::left_join(ward_stats, by = "ward")

# --- Plot ---
cat("Drawing map...\n")
p <- ggplot(ward_map) +
  geom_sf(aes(fill = land_share_mean), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Mean land share",
    na.value = "grey90"
  ) +
  labs(
    title = "Land Share by Ward (2020)",
    subtitle = "Mean of land_share_pin10 across parcels in each ward",
    caption = paste0("Parcels mapped: ", sum(ward_stats$n_parcels, na.rm = TRUE))
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

print(p)
ggsave("../output/land_share_by_ward_2020.pdf", plot = p, width = 8, height = 6)






### by census block 

census_blocks <- census_blocks %>%
  sf::st_make_valid() %>%
  sf::st_transform(sf::st_crs(ward_panel))

event_sf <- sf::st_as_sf(
  event_study_df,
  coords = c("longitude", "latitude"),
  crs = 4326,      # lon/lat
  remove = FALSE
) %>%
  sf::st_transform(sf::st_crs(ward_panel))


event_with_blocks <- sf::st_join(
  event_sf %>% dplyr::select(-block_id),
  census_blocks %>% dplyr::select(block_id),
  left = TRUE,   # keep all events; some may fall outside blocks
  join = sf::st_within
)

block_stats <- event_with_blocks %>%
  st_drop_geometry() %>%
  dplyr::filter(!is.na(block_id), !is.na(land_share_pin10)) %>%
  dplyr::group_by(block_id) %>%
  dplyr::summarise(
    land_share_mean = mean(land_share_pin10, na.rm = TRUE),
    n_parcels = dplyr::n(),
    .groups = "drop"
  )


blocks_map <- census_blocks %>%
  dplyr::left_join(block_stats, by = "block_id")

city_outline <- st_union(st_geometry(census_blocks))

# Plot: blocks filled by mean land share; ward outlines on top
cat("Drawing block map...\n")
p_blocks <- ggplot() +
  geom_sf(data = blocks_map, aes(fill = land_share_mean), color = NA) +
  geom_sf(data = ward_panel, fill = NA, color = "white", linewidth = 0.25) +
  geom_sf(data = city_outline, fill = NA, color = "grey50", linewidth = 0.25)+ 
  scale_fill_viridis_c(
    name   = "Mean land share",
    labels = scales::percent_format(accuracy = 1),
    na.value = "white"
  ) +
  labs(
    title    = "Land Share by Census Block (2020)",
    subtitle = "Mean of land_share_pin10 across parcels within each block",
    caption  = paste0(
      "Blocks with parcels: ",
      sum(!is.na(blocks_map$land_share_mean)),
      " Total joined parcels: ",
      sum(block_stats$n_parcels, na.rm = TRUE)
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_blocks

cowplot::save_plot("../output/land_share_by_census_block_2020.pdf", plot = p_blocks, base_width = 8, base_height = 6)
