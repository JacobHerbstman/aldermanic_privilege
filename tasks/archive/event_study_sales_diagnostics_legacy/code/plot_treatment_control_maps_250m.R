source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/event_study_sales_diagnostics/code")
# bandwidth_m <- 250
# output_suffix <- "250m"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth_m, output_suffix)
}

if (!length(args) %in% c(1, 2)) {
  stop("FATAL: Script requires args: <bandwidth_m> [<output_suffix>]", call. = FALSE)
}

bandwidth_m <- as.numeric(args[1])
output_suffix <- if (length(args) >= 2) args[2] else sprintf("%dm", as.integer(round(bandwidth_m)))

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", output_suffix)) {
  stop("output_suffix may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}

categorize_treatment <- function(strictness_change) {
  case_when(
    strictness_change > 0 ~ "Moved to Stricter",
    strictness_change < 0 ~ "Moved to Lenient",
    TRUE ~ "Control (No Change)"
  )
}

message("Loading map inputs...")

census_blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id))

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
census_blocks <- st_transform(census_blocks, st_crs(ward_panel))

treatment_panel <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
  mutate(
    block_id = as.character(block_id),
    treatment_group = categorize_treatment(strictness_change)
  )

block_distances_2015 <- read_csv("../input/sales_stacked_panel.csv", show_col_types = FALSE) %>%
  mutate(block_id = as.character(block_id)) %>%
  filter(
    cohort == "2015",
    !is.na(ward_pair_id),
    mean_dist_to_boundary_m < bandwidth_m
  ) %>%
  group_by(block_id) %>%
  summarise(
    mean_dist_to_boundary_m = mean(mean_dist_to_boundary_m, na.rm = TRUE),
    ward_pair_id = first(ward_pair_id),
    .groups = "drop"
  )

block_treatment_2015 <- treatment_panel %>%
  filter(cohort == "2015") %>%
  inner_join(block_distances_2015, by = "block_id")

blocks_for_map <- census_blocks %>%
  inner_join(block_treatment_2015, by = "block_id")

wards_2015 <- ward_panel %>% filter(year == 2015)

p_city <- ggplot() +
  geom_sf(data = wards_2015, fill = NA, color = "gray50", linewidth = 0.3) +
  geom_sf(data = blocks_for_map, aes(fill = treatment_group), color = NA, alpha = 1) +
  scale_fill_manual(
    values = c(
      "Control (No Change)" = "#D9D9D9",
      "Moved to Lenient" = "#4DABF7",
      "Moved to Stricter" = "#E63946"
    ),
    name = NULL
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "2015 Redistricting") +
  theme_void(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.margin = margin(2, 2, 2, 2)
  )

ggsave(sprintf("../output/treatment_control_map_2015_%s.pdf", output_suffix), p_city, width = 8, height = 10, bg = "white")

wards_2014 <- ward_panel %>% filter(year == 2014)
all_pairs_to_map <- c("13_23")

for (wp in all_pairs_to_map) {
  wards_in_pair <- as.numeric(strsplit(wp, "[-_]")[[1]])
  if (length(wards_in_pair) != 2 || any(is.na(wards_in_pair))) {
    stop(sprintf("Could not parse ward pair id: %s", wp), call. = FALSE)
  }

  ward_a <- wards_in_pair[1]
  ward_b <- wards_in_pair[2]

  wp_blocks <- blocks_for_map %>%
    filter(
      (ward_origin == ward_a & ward_dest == ward_b) |
        (ward_origin == ward_b & ward_dest == ward_a) |
        (ward_origin %in% c(ward_a, ward_b) & ward_dest %in% c(ward_a, ward_b))
    )

  if (nrow(wp_blocks) == 0) {
    warning(sprintf("No blocks found for ward pair %s.", wp))
    next
  }

  bbox <- st_bbox(wp_blocks)
  x_range <- as.numeric(bbox["xmax"]) - as.numeric(bbox["xmin"])
  y_range <- as.numeric(bbox["ymax"]) - as.numeric(bbox["ymin"])
  buffer <- max(x_range, y_range) * 0.1

  xmin_exp <- as.numeric(bbox["xmin"]) - buffer
  ymin_exp <- as.numeric(bbox["ymin"]) - buffer
  xmax_exp <- as.numeric(bbox["xmax"]) + buffer
  ymax_exp <- as.numeric(bbox["ymax"]) + buffer

  wards_2014_crop <- st_crop(wards_2014, xmin = xmin_exp, ymin = ymin_exp, xmax = xmax_exp, ymax = ymax_exp)
  wards_2015_crop <- st_crop(wards_2015, xmin = xmin_exp, ymin = ymin_exp, xmax = xmax_exp, ymax = ymax_exp)

  ward_nums <- sort(unique(c(wp_blocks$ward_origin, wp_blocks$ward_dest)))
  ward_colors <- setNames(c("#D62728", "#1F77B4"), as.character(ward_nums))

  map_theme <- theme_void(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.margin = margin(4, 4, 4, 4)
    )

  p_before <- ggplot() +
    geom_sf(data = wards_2014_crop, fill = NA, color = "black", linewidth = 1.0) +
    geom_sf(data = wp_blocks, aes(fill = factor(ward_origin)), color = "gray30", linewidth = 0.2, alpha = 1) +
    scale_fill_manual(values = ward_colors, name = "Ward") +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(title = "Before Redistricting (2014 Ward Boundaries)") +
    map_theme

  p_after <- ggplot() +
    geom_sf(data = wards_2015_crop, fill = NA, color = "black", linewidth = 1.0) +
    geom_sf(data = wp_blocks, aes(fill = factor(ward_dest)), color = "gray30", linewidth = 0.2, alpha = 1) +
    scale_fill_manual(values = ward_colors, name = "Ward") +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(title = "After Redistricting (2015 Ward Boundaries)") +
    map_theme

  p_treatment <- ggplot() +
    geom_sf(data = wards_2015_crop, fill = NA, color = "black", linewidth = 1.0) +
    geom_sf(data = wp_blocks, aes(fill = treatment_group), color = "gray30", linewidth = 0.2, alpha = 1) +
    scale_fill_manual(
      values = c(
        "Moved to Stricter" = "#D62728",
        "Moved to Lenient" = "#1F77B4",
        "Control (No Change)" = "#999999"
      ),
      name = "Treatment"
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(title = "Treatment Status") +
    map_theme

  ggsave(
    sprintf("../output/ward_pair_vertical_%s_%s.pdf", gsub("_", "-", wp), output_suffix),
    p_before / p_after / p_treatment + plot_layout(heights = c(1, 1, 1)),
    width = 11.2,
    height = 8.8,
    bg = "white"
  )
}

message(sprintf("Built %s treatment/control maps.", output_suffix))
