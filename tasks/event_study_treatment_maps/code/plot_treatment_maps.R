# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/event_study_treatment_maps/code")
# bandwidth_m <- 304.8
# bandwidth_label <- "1000ft"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_m, bandwidth_label)
}

if (length(cli_args) != 2) {
  stop("Usage: Rscript plot_treatment_maps.R <bandwidth_m> <bandwidth_label>", call. = FALSE)
}

bandwidth_m <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]
if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}

permit_blocks_2015 <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  as_tibble() %>%
  filter(
    dist_m <= bandwidth_m,
    relative_year >= -5,
    relative_year <= 5,
    !is.na(block_id),
    block_id != "",
    !is.na(ward_pair_id),
    ward_pair_id != "",
    !is.na(strictness_change)
  ) %>%
  transmute(
    block_id = as.character(block_id),
    cohort = "2015",
    ward_pair_id = gsub("_", "-", as.character(ward_pair_id), fixed = TRUE)
  ) %>%
  distinct()

if (nrow(permit_blocks_2015 %>%
  count(block_id, cohort, name = "n_assignments") %>%
  filter(n_assignments > 1)) > 0) {
  stop("Permit event-study panel has multiple boundary assignments for the same cohort-block.", call. = FALSE)
}

block_treatment <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
  mutate(
    block_id = as.character(block_id),
    cohort = as.character(cohort),
    treat = as.integer(switched)
  ) %>%
  filter(valid, !is.na(strictness_change))

if (nrow(block_treatment %>%
  filter(cohort == "2015") %>%
  count(block_id, cohort, name = "n_assignments") %>%
  filter(n_assignments > 1)) > 0) {
  stop("Block treatment panel has duplicate cohort-block assignments.", call. = FALSE)
}

sample_2015 <- permit_blocks_2015 %>%
  left_join(block_treatment, by = c("block_id", "cohort"), relationship = "one-to-one") %>%
  filter(!is.na(strictness_change)) %>%
  mutate(treatment_group = case_when(
    treat == 0 ~ "Control",
    strictness_change > 0 ~ "Moved to Stricter",
    strictness_change < 0 ~ "Moved to Lenient",
    TRUE ~ "Redistricted, No Score Change"
  ))

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
blocks_2010 <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  st_transform(st_crs(ward_panel)) %>%
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id)) %>%
  distinct(block_id, .keep_all = TRUE)

blocks_2015 <- blocks_2010 %>%
  inner_join(sample_2015, by = "block_id", relationship = "one-to-one")

wards_2015_citywide <- ward_panel %>%
  filter(year == 2015)

p_2015 <- ggplot() +
  geom_sf(data = wards_2015_citywide, fill = NA, color = "gray45", linewidth = 0.25) +
  geom_sf(data = blocks_2015, aes(fill = treatment_group), color = NA) +
  scale_fill_manual(
    values = c(
      "Control" = "#D9D9D9",
      "Moved to Lenient" = "#4DABF7",
      "Moved to Stricter" = "#E63946",
      "Redistricted, No Score Change" = "#F1C40F"
    ),
    breaks = c("Control", "Moved to Lenient", "Moved to Stricter", "Redistricted, No Score Change"),
    name = NULL
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "2015 Redistricting") +
  theme_void(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.margin = margin(2, 2, 2, 2)
  )

ggsave(
  sprintf("../output/treatment_control_map_2015_%s.pdf", bandwidth_label),
  p_2015,
  width = 8,
  height = 10,
  bg = "white"
)

pair_to_map <- "13-23"
wp_blocks <- blocks_2015 %>%
  filter(ward_pair_id == pair_to_map)
if (nrow(wp_blocks) == 0) {
  stop(sprintf("No 2015 blocks found for ward pair %s.", pair_to_map), call. = FALSE)
}

wards_in_pair <- as.integer(strsplit(pair_to_map, "-", fixed = TRUE)[[1]])
ward_colors <- setNames(c("#D62728", "#1F77B4"), as.character(wards_in_pair))

bbox <- st_bbox(wp_blocks)
x_range <- as.numeric(bbox["xmax"] - bbox["xmin"])
y_range <- as.numeric(bbox["ymax"] - bbox["ymin"])
buffer <- max(x_range, y_range) * 0.1
crop_bbox <- c(
  xmin = as.numeric(bbox["xmin"]) - buffer,
  ymin = as.numeric(bbox["ymin"]) - buffer,
  xmax = as.numeric(bbox["xmax"]) + buffer,
  ymax = as.numeric(bbox["ymax"]) + buffer
)

wards_2014_crop <- ward_panel %>%
  filter(year == 2014) %>%
  st_crop(crop_bbox)
wards_2015_crop <- ward_panel %>%
  filter(year == 2015) %>%
  st_crop(crop_bbox)

map_theme <- theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.margin = margin(4, 4, 4, 4)
  )

p_before <- ggplot() +
  geom_sf(data = wards_2014_crop, fill = NA, color = "black", linewidth = 0.8) +
  geom_sf(data = wp_blocks, aes(fill = factor(ward_origin)), color = "gray30", linewidth = 0.15) +
  scale_fill_manual(values = ward_colors, name = "Ward") +
  labs(title = "Before Redistricting") +
  map_theme

p_after <- ggplot() +
  geom_sf(data = wards_2015_crop, fill = NA, color = "black", linewidth = 0.8) +
  geom_sf(data = wp_blocks, aes(fill = factor(ward_dest)), color = "gray30", linewidth = 0.15) +
  scale_fill_manual(values = ward_colors, name = "Ward") +
  labs(title = "After Redistricting") +
  map_theme

p_treatment <- ggplot() +
  geom_sf(data = wards_2015_crop, fill = NA, color = "black", linewidth = 0.8) +
  geom_sf(data = wp_blocks, aes(fill = treatment_group), color = "gray30", linewidth = 0.15) +
  scale_fill_manual(
    values = c(
      "Control" = "#999999",
      "Moved to Lenient" = "#4DABF7",
      "Moved to Stricter" = "#E63946",
      "Redistricted, No Score Change" = "#F1C40F"
    ),
    name = "Treatment"
  ) +
  labs(title = "Treatment Status") +
  map_theme

combined_vertical <- p_before / p_after / p_treatment +
  plot_layout(heights = c(1, 1, 1))
ggsave(
  sprintf("../output/ward_pair_vertical_%s_%s.pdf", pair_to_map, bandwidth_label),
  combined_vertical,
  width = 11.2,
  height = 8.8,
  bg = "white"
)
