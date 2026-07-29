# setwd("tasks/audits/density_adjusted_figure/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    segment_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(within_500ft) |>
  dplyr::mutate(
    n_all = 1L,
    n_multifamily = as.integer(external_multifamily)
  ) |>
  dplyr::group_by(segment_id) |>
  dplyr::summarise(
    n_all = sum(n_all),
    n_multifamily = sum(n_multifamily),
    .groups = "drop"
  )

segments <- readr::read_csv(
  "../input/segment_classification.csv",
  show_col_types = FALSE,
  col_select = c(
    segment_id,
    ward_pair_id,
    era,
    nearest_street_name,
    centroid_lat,
    centroid_lon,
    segment_length_ft,
    major_overlap_expressway_ft,
    osm_overlap_expressway_ft,
    expressway_overlap_ft,
    major_overlap_arterial_ft,
    waterway_overlap_ft,
    water_area_share,
    park_area_share,
    cemetery_area_share
  ),
  col_types = readr::cols(
    segment_id = readr::col_character(),
    ward_pair_id = readr::col_character(),
    era = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(segments$segment_id) > 0) {
  stop("Segment classifications are not unique by segment ID.")
}

review <- segments |>
  dplyr::inner_join(
    projects,
    by = "segment_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    city_expressway_share = (
      major_overlap_expressway_ft / segment_length_ft
    ),
    osm_expressway_share = (
      osm_overlap_expressway_ft / segment_length_ft
    ),
    expressway_share = expressway_overlap_ft / segment_length_ft,
    arterial_share = major_overlap_arterial_ft / segment_length_ft,
    waterway_share = waterway_overlap_ft / segment_length_ft,
    water_share = pmax(waterway_share, water_area_share),
    park_water_share = pmin(
      1,
      water_area_share + park_area_share + waterway_share
    ),
    physical_barrier_share = pmin(
      1,
      park_water_share + cemetery_area_share
    ),
    barrier_share = pmax(expressway_share, water_share),
    any_barrier_contact = barrier_share > 0,
    barrier_25pct = barrier_share >= 0.25,
    barrier_50pct = barrier_share >= 0.50,
    barrier_75pct = barrier_share >= 0.75,
    expressway_sources_agree_25pct = (
      city_expressway_share >= 0.25 &
        osm_expressway_share >= 0.25
    ),
    possible_crossing = barrier_share > 0 & barrier_share < 0.25,
    existing_segment_drop = (
      physical_barrier_share >= 0.50 |
        expressway_share >= 0.40 |
        arterial_share >= 0.75 |
        waterway_share > 0 |
        cemetery_area_share > 0
    ),
    share_based_segment_drop = (
      physical_barrier_share >= 0.50 |
        expressway_share >= 0.40 |
        arterial_share >= 0.75
    )
  ) |>
  dplyr::arrange(
    dplyr::desc(n_multifamily),
    dplyr::desc(n_all),
    dplyr::desc(barrier_share)
  )

readr::write_csv(
  review,
  "../output/density_segment_classifier_review.csv"
)

threshold_summary <- dplyr::bind_rows(lapply(
  c(0, 0.10, 0.25, 0.50, 0.75),
  function(threshold) {
    review |>
      dplyr::summarise(
        threshold,
        n_segments = sum(barrier_share > threshold),
        n_projects = sum(n_all[barrier_share > threshold]),
        n_multifamily_projects = sum(
          n_multifamily[barrier_share > threshold]
        ),
        n_city_expressway_segments = sum(
          city_expressway_share > threshold
        ),
        n_osm_expressway_segments = sum(
          osm_expressway_share > threshold
        ),
        n_expressway_segments_in_both_sources = sum(
          city_expressway_share > threshold &
            osm_expressway_share > threshold
        )
      )
  }
))

readr::write_csv(
  threshold_summary,
  "../output/density_segment_classifier_thresholds.csv"
)

review_ids <- review |>
  dplyr::filter(barrier_share >= 0.25) |>
  dplyr::slice_head(n = 8) |>
  dplyr::pull(segment_id)

extra_drop_ids <- review |>
  dplyr::filter(
    existing_segment_drop,
    !share_based_segment_drop
  ) |>
  dplyr::slice_head(n = 8) |>
  dplyr::pull(segment_id)

segment_layers <- lapply(
  c("2003_2014", "2015_2023"),
  function(layer_name) {
    sf::st_read(
      "../input/boundary_segments_1320ft.gpkg",
      layer = layer_name,
      quiet = TRUE
    )
  }
)
segment_geometry <- do.call(rbind, segment_layers) |>
  dplyr::filter(segment_id %in% review$segment_id) |>
  dplyr::select(segment_id)

if (anyDuplicated(segment_geometry$segment_id) > 0) {
  stop("Boundary geometry is not unique by segment ID.")
}
if (nrow(segment_geometry) != nrow(review)) {
  stop("Some reviewed segments lack boundary geometry.")
}

analysis_window <- sf::st_sf(
  geometry = sf::st_buffer(
    sf::st_union(sf::st_geometry(segment_geometry)),
    1500
  )
)

read_nearby_layer <- function(path) {
  layer <- sf::st_read(path, quiet = TRUE) |>
    sf::st_zm(drop = TRUE, what = "ZM") |>
    sf::st_make_valid() |>
    sf::st_transform(3435)
  suppressWarnings(
    sf::st_filter(layer, analysis_window, .predicate = sf::st_intersects)
  )
}

major_streets <- read_nearby_layer("../input/major_streets.geojson")
city_roads <- major_streets |>
  dplyr::filter(CLASS %in% c(1L, 9L))

osm_roads_all <- read_nearby_layer("../input/gis_osm_roads_free_1.shp")
osm_roads <- osm_roads_all |>
  dplyr::filter(
    tolower(fclass) %in% c(
      "motorway",
      "motorway_link",
      "trunk",
      "trunk_link"
    )
  )

water_polygons <- read_nearby_layer(
  "../input/gis_osm_water_a_free_1.shp"
)

waterways <- read_nearby_layer(
  "../input/gis_osm_waterways_free_1.shp"
)

cemeteries <- read_nearby_layer(
  "../input/gis_osm_landuse_a_free_1.shp"
)
parks <- cemeteries |>
  dplyr::filter(
    tolower(fclass) %in% c(
      "park",
      "recreation_ground",
      "grass",
      "forest",
      "nature_reserve",
      "meadow",
      "village_green",
      "greenfield"
    )
  )
cemeteries <- cemeteries |>
  dplyr::filter(tolower(fclass) == "cemetery")

line_overlap_ft <- function(segment_sf, feature_sf) {
  if (nrow(feature_sf) == 0) {
    return(rep(0, nrow(segment_sf)))
  }

  segment_lines <- sf::st_sf(
    segment_row = seq_len(nrow(segment_sf)),
    geometry = sf::st_geometry(segment_sf)
  )
  feature_union <- sf::st_sf(
    geometry = sf::st_union(
      sf::st_buffer(sf::st_geometry(feature_sf), 30 / 0.3048)
    )
  )
  intersections <- suppressWarnings(
    sf::st_intersection(segment_lines, feature_union)
  )
  overlap <- rep(0, nrow(segment_sf))
  if (nrow(intersections) == 0) {
    return(overlap)
  }

  intersections$overlap_ft <- as.numeric(sf::st_length(intersections))
  totals <- tapply(
    intersections$overlap_ft,
    intersections$segment_row,
    sum,
    na.rm = TRUE
  )
  overlap[as.integer(names(totals))] <- as.numeric(totals)
  overlap
}

area_overlap_share <- function(segment_sf, polygon_sf) {
  if (nrow(polygon_sf) == 0) {
    return(rep(0, nrow(segment_sf)))
  }

  corridors <- sf::st_sf(
    segment_row = seq_len(nrow(segment_sf)),
    geometry = sf::st_buffer(
      sf::st_geometry(segment_sf),
      30 / 0.3048,
      endCapStyle = "FLAT"
    )
  )
  corridor_area <- as.numeric(sf::st_area(corridors))
  polygon_union <- sf::st_sf(
    geometry = sf::st_union(sf::st_geometry(polygon_sf))
  )
  intersections <- suppressWarnings(
    sf::st_intersection(corridors, polygon_union)
  )
  overlap <- rep(0, nrow(segment_sf))
  if (nrow(intersections) == 0) {
    return(overlap)
  }

  intersections$overlap_area <- as.numeric(sf::st_area(intersections))
  totals <- tapply(
    intersections$overlap_area,
    intersections$segment_row,
    sum,
    na.rm = TRUE
  )
  matched_rows <- as.integer(names(totals))
  overlap[matched_rows] <- pmin(
    1,
    as.numeric(totals) / corridor_area[matched_rows]
  )
  overlap
}

expressway_features <- rbind(
  major_streets |>
    dplyr::filter(CLASS %in% c(1L, 9L)) |>
    dplyr::select(geometry),
  osm_roads |>
    dplyr::select(geometry)
)

fresh_overlap <- sf::st_drop_geometry(segment_geometry) |>
  dplyr::mutate(
    major_overlap_expressway_ft = line_overlap_ft(
      segment_geometry,
      major_streets |>
        dplyr::filter(CLASS == 1L)
    ),
    osm_overlap_expressway_ft = line_overlap_ft(
      segment_geometry,
      osm_roads
    ),
    expressway_overlap_ft = line_overlap_ft(
      segment_geometry,
      expressway_features
    ),
    major_overlap_arterial_ft = line_overlap_ft(
      segment_geometry,
      major_streets |>
        dplyr::filter(CLASS == 2L)
    ),
    waterway_overlap_ft = line_overlap_ft(
      segment_geometry,
      waterways
    ),
    water_area_share = area_overlap_share(
      segment_geometry,
      water_polygons
    ),
    park_area_share = area_overlap_share(
      segment_geometry,
      parks
    ),
    cemetery_area_share = area_overlap_share(
      segment_geometry,
      cemeteries
    )
  )

overlap_fields <- c(
  "major_overlap_expressway_ft",
  "osm_overlap_expressway_ft",
  "expressway_overlap_ft",
  "major_overlap_arterial_ft",
  "waterway_overlap_ft",
  "water_area_share",
  "park_area_share",
  "cemetery_area_share"
)

stored_overlap <- segments |>
  dplyr::semi_join(review, by = "segment_id") |>
  dplyr::select(segment_id, dplyr::all_of(overlap_fields))

recalculation_comparison <- stored_overlap |>
  dplyr::inner_join(
    fresh_overlap,
    by = "segment_id",
    suffix = c("_stored", "_fresh"),
    relationship = "one-to-one"
  )

recalculation_summary <- dplyr::bind_rows(lapply(
  overlap_fields,
  function(field_name) {
    difference <- abs(
      recalculation_comparison[[paste0(field_name, "_stored")]] -
        recalculation_comparison[[paste0(field_name, "_fresh")]]
    )
    tibble::tibble(
      measure = field_name,
      n_segments = length(difference),
      maximum_absolute_difference = max(difference),
      mean_absolute_difference = mean(difference),
      segments_differing_above_1e_6 = sum(difference > 1e-6)
    )
  }
))

readr::write_csv(
  recalculation_summary,
  "../output/density_segment_classifier_recalculation.csv"
)

display_geometry <- segment_geometry |>
  dplyr::filter(segment_id %in% c(review_ids, extra_drop_ids))

plots <- vector("list", length(review_ids))

for (i in seq_along(review_ids)) {
  segment_id_i <- review_ids[i]
  segment_i <- display_geometry |>
    dplyr::filter(segment_id == segment_id_i)
  review_i <- review |>
    dplyr::filter(segment_id == segment_id_i)
  plot_window <- sf::st_buffer(segment_i, 350)

  city_i <- suppressWarnings(
    sf::st_filter(city_roads, plot_window, .predicate = sf::st_intersects)
  )
  osm_i <- suppressWarnings(
    sf::st_filter(osm_roads, plot_window, .predicate = sf::st_intersects)
  )
  water_i <- suppressWarnings(
    sf::st_filter(
      water_polygons,
      plot_window,
      .predicate = sf::st_intersects
    )
  )
  waterways_i <- suppressWarnings(
    sf::st_filter(waterways, plot_window, .predicate = sf::st_intersects)
  )
  bbox_i <- sf::st_bbox(plot_window)

  plots[[i]] <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = water_i,
      fill = "#A6CEE3",
      color = "#5AA2C9",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = waterways_i,
      color = "#2B8CBE",
      linewidth = 1
    ) +
    ggplot2::geom_sf(
      data = city_i,
      color = "#F28E2B",
      linewidth = 1.2
    ) +
    ggplot2::geom_sf(
      data = osm_i,
      color = "#D62728",
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    ggplot2::geom_sf(
      data = segment_i,
      color = "black",
      linewidth = 1.5
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox_i["xmin"], bbox_i["xmax"]),
      ylim = c(bbox_i["ymin"], bbox_i["ymax"]),
      expand = FALSE,
      datum = NA
    ) +
    ggplot2::labs(
      title = segment_id_i,
      subtitle = sprintf(
        "N=%d; MF=%d; expressway=%.2f; water=%.2f",
        review_i$n_all,
        review_i$n_multifamily,
        review_i$expressway_share,
        review_i$water_share
      )
    ) +
    ggplot2::theme_void(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 8),
      plot.subtitle = ggplot2::element_text(size = 7),
      aspect.ratio = 1
    )
}

classifier_map <- patchwork::wrap_plots(plots, ncol = 2) +
  patchwork::plot_annotation(
    title = "Highest-impact boundary segments with substantial barrier overlap",
    subtitle = paste(
      "Black: ward boundary; orange: City expressway/ramp;",
      "dashed red: OSM motorway/trunk; blue: water"
    )
  )

ggplot2::ggsave(
  "../output/density_segment_classifier_review.png",
  classifier_map,
  width = 10,
  height = 17,
  dpi = 220
)

extra_drop_plots <- vector("list", length(extra_drop_ids))

for (i in seq_along(extra_drop_ids)) {
  segment_id_i <- extra_drop_ids[i]
  segment_i <- display_geometry |>
    dplyr::filter(segment_id == segment_id_i)
  review_i <- review |>
    dplyr::filter(segment_id == segment_id_i)
  plot_window <- sf::st_buffer(segment_i, 350)

  cemetery_i <- suppressWarnings(
    sf::st_filter(cemeteries, plot_window, .predicate = sf::st_intersects)
  )
  water_i <- suppressWarnings(
    sf::st_filter(
      water_polygons,
      plot_window,
      .predicate = sf::st_intersects
    )
  )
  waterways_i <- suppressWarnings(
    sf::st_filter(waterways, plot_window, .predicate = sf::st_intersects)
  )
  bbox_i <- sf::st_bbox(plot_window)

  extra_drop_plots[[i]] <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = cemetery_i,
      fill = "#B39DDB",
      color = "#6A51A3",
      linewidth = 0.3
    ) +
    ggplot2::geom_sf(
      data = water_i,
      fill = "#A6CEE3",
      color = "#5AA2C9",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = waterways_i,
      color = "#2B8CBE",
      linewidth = 1
    ) +
    ggplot2::geom_sf(
      data = segment_i,
      color = "black",
      linewidth = 1.5
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox_i["xmin"], bbox_i["xmax"]),
      ylim = c(bbox_i["ymin"], bbox_i["ymax"]),
      expand = FALSE,
      datum = NA
    ) +
    ggplot2::labs(
      title = segment_id_i,
      subtitle = sprintf(
        "N=%d; MF=%d; water=%.2f; cemetery=%.2f",
        review_i$n_all,
        review_i$n_multifamily,
        review_i$waterway_share,
        review_i$cemetery_area_share
      )
    ) +
    ggplot2::theme_void(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 8),
      plot.subtitle = ggplot2::element_text(size = 7),
      aspect.ratio = 1
    )
}

extra_drop_map <- patchwork::wrap_plots(extra_drop_plots, ncol = 2) +
  patchwork::plot_annotation(
    title = "Segments excluded only by the old any-contact rule",
    subtitle = paste(
      "Black: ward boundary; purple: cemetery;",
      "blue: water or waterway"
    )
  )

ggplot2::ggsave(
  "../output/density_segment_classifier_any_contact_review.png",
  extra_drop_map,
  width = 10,
  height = 17,
  dpi = 220
)
