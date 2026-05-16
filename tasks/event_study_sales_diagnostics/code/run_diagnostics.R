source("../../setup_environment/code/packages.R")
source("../../_lib/border_pair_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/event_study_sales_diagnostics/code")
# bandwidth_m <- 304.8

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth_m)
}

if (length(args) != 1) {
  stop("FATAL: Script requires args: <bandwidth_m>", call. = FALSE)
}

bandwidth_m <- as.numeric(args[1])
if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be positive.", call. = FALSE)
}

distance_display <- distance_display_config()
bandwidth_label <- Sys.getenv("BANDWIDTH_LABEL", format_distance_label(bandwidth_m, distance_display))

read_blocks <- function(path, block_col, target_crs) {
  read_csv(path, show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_make_valid() %>%
    st_transform(target_crs) %>%
    rename(block_id = all_of(block_col)) %>%
    mutate(block_id = as.character(block_id)) %>%
    distinct(block_id, .keep_all = TRUE)
}

normalize_pair_label <- function(x) {
  gsub("_", "-", as.character(x), fixed = TRUE)
}

categorize_treatment <- function(treat, strictness_change) {
  case_when(
    treat == 0 ~ "Control",
    strictness_change > 0 ~ "Moved to Stricter",
    strictness_change < 0 ~ "Moved to Lenient",
    TRUE ~ "Redistricted, No Score Change"
  )
}

build_sales_block_candidates <- function(path, cohort_label) {
  candidates <- read_parquet(path) %>%
    as_tibble() %>%
    filter(
      dist_m <= bandwidth_m,
      !is.na(block_id),
      block_id != "",
      !is.na(ward_pair_id),
      ward_pair_id != ""
    ) %>%
    mutate(
      block_id = as.character(block_id),
      cohort = cohort_label,
      display_ward_pair_id = normalize_pair_label(ward_pair_id)
    ) %>%
    arrange(block_id, dist_m, display_ward_pair_id) %>%
    group_by(block_id, cohort) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      block_id,
      cohort,
      ward_pair_id = display_ward_pair_id
    )

  candidates
}

make_citywide_map <- function(blocks_sf, ward_year, title_text) {
  wards <- ward_panel %>%
    filter(year == ward_year)

  ggplot() +
    geom_sf(data = wards, fill = NA, color = "gray45", linewidth = 0.25) +
    geom_sf(data = blocks_sf, aes(fill = treatment_group), color = NA) +
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
    labs(title = title_text) +
    theme_void(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
      plot.margin = margin(2, 2, 2, 2)
    )
}

message("Loading corrected sales event-study panels...")
sales_blocks_2015 <- build_sales_block_candidates("../input/sales_transaction_panel_2015.parquet", "2015")
sales_blocks_2023 <- build_sales_block_candidates("../input/sales_transaction_panel_2023.parquet", "2023")

message("Loading block treatment assignments...")
block_treatment <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
  mutate(
    block_id = as.character(block_id),
    cohort = as.character(cohort),
    treat = as.integer(switched)
  ) %>%
  filter(valid, !is.na(strictness_change))

duplicate_treatment <- block_treatment %>%
  count(block_id, cohort, name = "n_assignments") %>%
  filter(n_assignments > 1)
if (nrow(duplicate_treatment) > 0) {
  stop("Block treatment panel has duplicate cohort-block assignments.", call. = FALSE)
}

sample_2015 <- sales_blocks_2015 %>%
  left_join(block_treatment, by = c("block_id", "cohort"), relationship = "one-to-one") %>%
  filter(!is.na(strictness_change)) %>%
  mutate(treatment_group = categorize_treatment(treat, strictness_change))
sample_2023 <- sales_blocks_2023 %>%
  left_join(block_treatment, by = c("block_id", "cohort"), relationship = "one-to-one") %>%
  filter(!is.na(strictness_change)) %>%
  mutate(treatment_group = categorize_treatment(treat, strictness_change))

message("Loading ward panel and census blocks...")
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
blocks_2010 <- read_blocks("../input/census_blocks_2010.csv", "GEOID10", st_crs(ward_panel))
blocks_2020 <- read_blocks("../input/census_blocks_2020.csv", "GEOID20", st_crs(ward_panel))

blocks_2015 <- blocks_2010 %>%
  inner_join(sample_2015, by = "block_id", relationship = "one-to-one")
blocks_2023 <- blocks_2020 %>%
  inner_join(sample_2023, by = "block_id", relationship = "one-to-one")

message(sprintf("2015 map blocks within %s: %s", bandwidth_label, format(nrow(blocks_2015), big.mark = ",")))
message(sprintf("2023 map blocks within %s: %s", bandwidth_label, format(nrow(blocks_2023), big.mark = ",")))

write_csv(st_drop_geometry(blocks_2015), sprintf("../output/treatment_control_map_2015_%s_data.csv", bandwidth_label))
write_csv(st_drop_geometry(blocks_2023), sprintf("../output/treatment_control_map_2023_%s_data.csv", bandwidth_label))

p_2015 <- make_citywide_map(blocks_2015, 2015, "2015 Redistricting")
p_2023 <- make_citywide_map(blocks_2023, 2024, "2023 Redistricting")

ggsave(
  sprintf("../output/treatment_control_map_2015_%s.pdf", bandwidth_label),
  p_2015,
  width = 8,
  height = 10,
  bg = "white"
)
ggsave(
  sprintf("../output/treatment_control_map_2023_%s.pdf", bandwidth_label),
  p_2023,
  width = 8,
  height = 10,
  bg = "white"
)

combined_citywide <- (p_2015 + theme(legend.position = "none")) +
  (p_2023 + guides(fill = guide_legend(nrow = 1))) +
  plot_layout(widths = c(1, 1))
ggsave(
  sprintf("../output/treatment_control_maps_combined_%s.pdf", bandwidth_label),
  combined_citywide,
  width = 5,
  height = 4,
  bg = "white"
)

message("Creating 13-23 before/after map from corrected 2015 panel...")
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

wards_2014_crop <- ward_panel %>%
  filter(year == 2014) %>%
  st_crop(
    xmin = as.numeric(bbox["xmin"]) - buffer,
    ymin = as.numeric(bbox["ymin"]) - buffer,
    xmax = as.numeric(bbox["xmax"]) + buffer,
    ymax = as.numeric(bbox["ymax"]) + buffer
  )
wards_2015_crop <- ward_panel %>%
  filter(year == 2015) %>%
  st_crop(
    xmin = as.numeric(bbox["xmin"]) - buffer,
    ymin = as.numeric(bbox["ymin"]) - buffer,
    xmax = as.numeric(bbox["xmax"]) + buffer,
    ymax = as.numeric(bbox["ymax"]) + buffer
  )

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

combined_horizontal <- p_before + p_after + p_treatment +
  plot_layout(ncol = 3)
ggsave(
  sprintf("../output/ward_pair_before_after_%s_%s.pdf", pair_to_map, bandwidth_label),
  combined_horizontal,
  width = 15,
  height = 6,
  bg = "white"
)

message("Done.")
