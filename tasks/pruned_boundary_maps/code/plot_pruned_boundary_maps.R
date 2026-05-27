# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/pruned_boundary_maps/code")
# segment_length_ft <- 1320

source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(segment_length_ft)
}
if (length(args) != 1) {
  stop("FATAL: Script requires args: <segment_length_ft>", call. = FALSE)
}

segment_length_ft <- as.numeric(args[1])
if (!is.finite(segment_length_ft) || segment_length_ft <= 0) {
  stop("segment_length_ft must be positive.", call. = FALSE)
}

map_eras <- c("2003_2014", "2015_2023")
map_era_labels <- c(
  "2003_2014" = "2003-2015",
  "2015_2023" = "2015-2022"
)

segments_2003_2014 <- st_read(
  sprintf("../input/boundary_segments_%dft.gpkg", as.integer(round(segment_length_ft))),
  layer = "2003_2014",
  quiet = TRUE
)
segments_2003_2014$era <- "2003_2014"

segments_2015_2023 <- st_read(
  sprintf("../input/boundary_segments_%dft.gpkg", as.integer(round(segment_length_ft))),
  layer = "2015_2023",
  quiet = TRUE
)
segments_2015_2023$era <- "2015_2023"

segments <- rbind(segments_2003_2014, segments_2015_2023)

segments <- st_transform(segments, 4326)
segments$segment_parity <- factor(
  ifelse(as.integer(segments$segment_number) %% 2L == 0L, "Even segment", "Odd segment"),
  levels = c("Odd segment", "Even segment")
)

segments <- segments[as.character(segments$era) %in% map_eras, ]
segments$era_facet <- factor(
  unname(map_era_labels[as.character(segments$era)]),
  levels = unname(map_era_labels[map_eras])
)

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_panel <- st_transform(ward_panel, st_crs(segments))
ward_year_ref <- max(as.integer(ward_panel$year), na.rm = TRUE)
city_outline_geom <- st_boundary(
  st_union(st_geometry(ward_panel[as.integer(ward_panel$year) == ward_year_ref, ]))
)
city_outline <- st_sf(
  era = map_eras,
  geometry = st_sfc(
    city_outline_geom[[1]],
    city_outline_geom[[1]],
    crs = st_crs(ward_panel)
  )
)
city_outline <- st_transform(city_outline, 4326)
city_outline$era_facet <- factor(
  unname(map_era_labels[as.character(city_outline$era)]),
  levels = unname(map_era_labels[map_eras])
)

p_all_segments <- ggplot() +
  geom_sf(
    data = city_outline,
    color = "#111111",
    linewidth = 0.40,
    inherit.aes = FALSE
  ) +
  geom_sf(data = segments, aes(color = segment_parity), linewidth = 0.25, alpha = 0.95) +
  facet_wrap(~era_facet, ncol = 3) +
  scale_color_manual(values = c("Odd segment" = "#1f77b4", "Even segment" = "#ff7f0e")) +
  labs(
    title = "Ward Boundaries Divided into Segments",
    subtitle = "Alternating segment parity highlights segment-level FE geography",
    color = NULL
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("../output/pruned_boundaries_all_segments_by_era.pdf", p_all_segments, width = 11, height = 8.5, dpi = 300)
