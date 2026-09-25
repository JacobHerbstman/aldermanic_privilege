# setwd("tasks/presentation_figures/code")
# Slide map of one reassigned ward pair: blocks within 500 ft (152.4 m) of the boundary in the event-study sample,
# colored by 2014 ward, 2015 ward and reassignment direction.
bandwidth_m <- 152.4
bandwidth_label <- "500ft"
ward_pair <- "1-26"
start_event <- -5L
end_event <- 5L

source("../../setup_environment/code/packages.R")

panel <- arrow::read_parquet("../input/permit_block_year_panel_2015.parquet") |>
  filter(dist_m <= bandwidth_m, relative_year >= start_event, relative_year <= end_event,
    !is.na(strictness_change_frozen), !is.na(ward_pair_id), ward_pair_id != "", stable_both,
    gsub("_", "-", ward_pair_id, fixed = TRUE) == ward_pair) |>
  group_by(block_id) |>
  filter(sum(n_high_discretion_application[relative_year < 0], na.rm = TRUE) > 0) |>
  ungroup() |>
  distinct(block_id, ward_origin, ward_dest, strictness_change_frozen) |>
  mutate(block_id = as.character(block_id), treatment = case_when(
    strictness_change_frozen > 0 ~ "More stringent",
    strictness_change_frozen < 0 ~ "More lenient", TRUE ~ "Unchanged"))
stopifnot(nrow(panel) > 0, !anyDuplicated(panel$block_id))
blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE,
  col_types = cols(GEOID10 = col_character(), .default = col_guess())) |>
  rename(block_id = GEOID10) |>
  filter(block_id %in% panel$block_id)
stopifnot(!anyDuplicated(blocks$block_id), nrow(blocks) == nrow(panel))
blocks <- blocks |>
  inner_join(panel, by = "block_id", relationship = "one-to-one") |>
  st_as_sf(wkt = "the_geom", crs = 4269) |> st_transform(3435) |> st_make_valid()
wards <- st_read("../input/ward_panel.gpkg", quiet = TRUE) |> st_transform(3435)
box <- st_bbox(blocks)
padding <- max(box["xmax"] - box["xmin"], box["ymax"] - box["ymin"]) * 0.08
xlim <- as.numeric(box[c("xmin", "xmax")]) + c(-padding, padding)
ylim <- as.numeric(box[c("ymin", "ymax")]) + c(-padding, padding)
ward_colors <- setNames(c("#D62728", "#1F77B4"), strsplit(ward_pair, "-", fixed = TRUE)[[1]])
plots <- vector("list", 3)
for (i in 1:3) {
  map_year <- if (i == 1) 2014 else 2015
  blocks$group <- if (i == 1) as.character(blocks$ward_origin) else if (i == 2) as.character(blocks$ward_dest) else blocks$treatment
  colors <- if (i < 3) ward_colors else c("Unchanged" = "#999999", "More lenient" = "#2478B5", "More stringent" = "#D92D27")
  plots[[i]] <- ggplot() +
    geom_sf(data = blocks, aes(fill = group), color = "gray35", linewidth = 0.10) +
    geom_sf(data = filter(wards, year == map_year), fill = NA, color = "black", linewidth = 0.6) +
    scale_fill_manual(values = colors, name = if (i < 3) "Ward" else NULL) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = c("Before redistricting", "After redistricting", "Reassignment")[i]) +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom", legend.text = element_text(size = 9),
      legend.key.size = grid::unit(0.3, "cm"), plot.margin = margin(4, 4, 4, 4))
}
plot <- wrap_plots(plots, nrow = 1)
ggsave(sprintf("../output/ward_pair_%s_%s.pdf", ward_pair, bandwidth_label), plot,
  width = 12.6, height = 3.8, bg = "white")
