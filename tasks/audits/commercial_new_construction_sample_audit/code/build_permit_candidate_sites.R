# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

chains <- readr::read_csv(
  "../output/permit_residual_evidence_matrix.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(evidence_review_priority %in% c(
    "high_completion_candidate",
    "medium_completion_candidate"
  ))
links_2015 <- readr::read_csv(
  "../output/permit_residual_city_building_footprint_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    harris_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    strong_footprint_match,
    permit_chain_id %in% chains$permit_chain_id
  )
links_2008 <- readr::read_csv(
  "../output/permit_residual_2008_footprint_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_2008_id = readr::col_character(),
    harris_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    strong_footprint_match,
    permit_chain_id %in% chains$permit_chain_id
  )
footprints_2015 <- sf::st_read(
  "../output/permit_residual_city_building_footprints.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)
names(footprints_2015)[
  names(footprints_2015) == attr(footprints_2015, "sf_column")
] <- "geometry"
sf::st_geometry(footprints_2015) <- "geometry"
footprints_2008 <- sf::st_read(
  "../output/cook_building_footprints_2006_2008.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)

if (anyDuplicated(chains$permit_chain_id) ||
    anyDuplicated(footprints_2015$footprint_id) ||
    anyDuplicated(footprints_2008$footprint_2008_id)) {
  stop("Candidate chains and footprint sources must be unique.", call. = FALSE)
}
if (any(!links_2015$permit_chain_id %in% chains$permit_chain_id) ||
    any(!links_2008$permit_chain_id %in% chains$permit_chain_id)) {
  stop("A strong footprint link refers to an unknown permit chain.", call. = FALSE)
}

edges <- links_2015 |>
  dplyr::transmute(
    from = paste0("chain:", permit_chain_id),
    to = paste0("footprint2015:", footprint_id)
  ) |>
  dplyr::distinct()
graph <- igraph::graph_from_data_frame(edges, directed = FALSE)
graph <- igraph::add_vertices(
  graph,
  nv = length(setdiff(
    paste0("chain:", chains$permit_chain_id),
    igraph::V(graph)$name
  )),
  name = setdiff(
    paste0("chain:", chains$permit_chain_id),
    igraph::V(graph)$name
  )
)
membership <- igraph::components(graph)$membership

component_members <- tibble::tibble(
  node = names(membership),
  component_number = as.integer(membership)
) |>
  dplyr::group_by(component_number) |>
  dplyr::mutate(component_sort_key = min(node)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    candidate_site_id = paste0(
      "permit_candidate_site_",
      stringr::str_pad(
        dplyr::dense_rank(component_sort_key),
        width = 3,
        pad = "0"
      )
    ),
    node_type = stringr::str_extract(node, "^[^:]+"),
    source_id = stringr::str_remove(node, "^[^:]+:")
  ) |>
  dplyr::select(candidate_site_id, node_type, source_id)

chain_members <- component_members |>
  dplyr::filter(node_type == "chain") |>
  dplyr::transmute(candidate_site_id, permit_chain_id = source_id) |>
  dplyr::left_join(
    chains,
    by = "permit_chain_id",
    relationship = "one-to-one"
  )
footprint_members <- component_members |>
  dplyr::filter(node_type == "footprint2015") |>
  dplyr::transmute(candidate_site_id, footprint_id = source_id)

if (any(is.na(chain_members$representative_permit_number)) ||
    any(!footprint_members$footprint_id %in% footprints_2015$footprint_id)) {
  stop("Candidate-site graph membership does not reconcile.", call. = FALSE)
}

site_polygons <- footprints_2015 |>
  dplyr::inner_join(
    footprint_members,
    by = "footprint_id",
    relationship = "one-to-one"
  ) |>
  dplyr::group_by(candidate_site_id) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
sf::st_geometry(site_polygons) <- "geometry"
sf::st_geometry(site_polygons) <- sf::st_collection_extract(
  sf::st_make_valid(sf::st_geometry(site_polygons)),
  "POLYGON"
)

chain_points <- sf::st_as_sf(
  chain_members,
  coords = c("representative_x_3435", "representative_y_3435"),
  crs = 3435,
  remove = FALSE
)
site_reference_points <- purrr::map_dfr(
  seq_len(nrow(site_polygons)),
  function(i) {
    point <- suppressWarnings(sf::st_point_on_surface(site_polygons[i, ]))
    coordinates <- sf::st_coordinates(point)
    tibble::tibble(
      candidate_site_id = site_polygons$candidate_site_id[i],
      reference_x_3435 = coordinates[1, 1],
      reference_y_3435 = coordinates[1, 2]
    )
  }
) |>
  sf::st_as_sf(
    coords = c("reference_x_3435", "reference_y_3435"),
    crs = 3435,
    remove = FALSE
  ) |>
  dplyr::select(candidate_site_id)
site_reference_points <- rbind(
  site_reference_points,
  chain_points |>
    dplyr::anti_join(
      sf::st_drop_geometry(site_reference_points),
      by = "candidate_site_id"
    ) |>
    dplyr::group_by(candidate_site_id) |>
    dplyr::slice_min(
      representative_application_date,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(candidate_site_id)
) |>
  dplyr::arrange(candidate_site_id)

if (anyDuplicated(site_reference_points$candidate_site_id) ||
    !setequal(
      site_reference_points$candidate_site_id,
      chain_members$candidate_site_id
    )) {
  stop("Candidate-site reference points are not one row per site.", call. = FALSE)
}

footprint_2015_evidence <- links_2015 |>
  dplyr::inner_join(
    footprint_members,
    by = "footprint_id",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(candidate_site_id) |>
  dplyr::summarise(
    footprint_2015_count = dplyr::n_distinct(footprint_id),
    footprint_2015_ids = paste(sort(unique(footprint_id)), collapse = "/"),
    footprint_2015_years = paste(
      sort(unique(city_year_built[is.finite(city_year_built)])),
      collapse = "/"
    ),
    footprint_2015_units = paste(
      sort(unique(city_units[is.finite(city_units) & city_units > 0])),
      collapse = "/"
    ),
    represented_project_ids = paste(
      sort(unique(
        unlist(stringr::str_split(
          represented_project_ids[
            !is.na(represented_project_ids) &
              represented_project_ids != ""
          ],
          "/"
        ))
      )),
      collapse = "/"
    ),
    .groups = "drop"
  )

footprint_2008_evidence <- links_2008 |>
  dplyr::inner_join(
    chain_members |>
      dplyr::select(candidate_site_id, permit_chain_id),
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(candidate_site_id) |>
  dplyr::summarise(
    footprint_2008_count = dplyr::n_distinct(footprint_2008_id),
    footprint_2008_ids = paste(
      sort(unique(footprint_2008_id)),
      collapse = "/"
    ),
    footprint_2008_years = paste(
      sort(unique(
        footprint_year_built[is.finite(footprint_year_built)]
      )),
      collapse = "/"
    ),
    .groups = "drop"
  )

site_ledger <- chain_members |>
  dplyr::group_by(candidate_site_id) |>
  dplyr::summarise(
    permit_chain_count = dplyr::n_distinct(permit_chain_id),
    permit_chain_ids = paste(sort(unique(permit_chain_id)), collapse = "/"),
    permit_numbers = paste(
      sort(unique(representative_permit_number)),
      collapse = "/"
    ),
    earliest_application_date = min(representative_application_date),
    latest_issue_date = max(representative_issue_date),
    application_years = paste(
      sort(unique(lubridate::year(representative_application_date))),
      collapse = "/"
    ),
    permit_addresses = paste(
      sort(unique(representative_address)),
      collapse = " || "
    ),
    maximum_unit_mention = suppressWarnings(max(
      maximum_unit_mention,
      na.rm = TRUE
    )),
    application_ward_pairs = paste(
      sort(unique(application_ward_pair)),
      collapse = "/"
    ),
    minimum_boundary_distance_ft = min(application_boundary_distance_ft),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    maximum_unit_mention = dplyr::if_else(
      is.infinite(maximum_unit_mention),
      NA_real_,
      maximum_unit_mention
    )
  ) |>
  dplyr::left_join(
    footprint_2015_evidence,
    by = "candidate_site_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    footprint_2008_evidence,
    by = "candidate_site_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    footprint_2015_count = dplyr::coalesce(footprint_2015_count, 0L),
    footprint_2008_count = dplyr::coalesce(footprint_2008_count, 0L),
    represented_project_ids = dplyr::na_if(represented_project_ids, ""),
    preliminary_site_status = dplyr::case_when(
      !is.na(represented_project_ids) ~
        "represented_site_requires_episode_confirmation",
      footprint_2008_count > 0 ~
        "unrepresented_site_confirmed_in_2008_and_2015",
      footprint_2015_count > 0 ~
        "unrepresented_site_confirmed_in_2015",
      TRUE ~ "unrepresented_permit_without_footprint"
    )
  ) |>
  dplyr::arrange(candidate_site_id)

site_coordinates <- sf::st_coordinates(site_reference_points)
site_ledger <- site_ledger |>
  dplyr::left_join(
    sf::st_drop_geometry(site_reference_points) |>
      dplyr::mutate(
        reference_x_3435 = site_coordinates[, 1],
        reference_y_3435 = site_coordinates[, 2]
      ),
    by = "candidate_site_id",
    relationship = "one-to-one"
  )

summary <- dplyr::bind_rows(
  site_ledger |>
    dplyr::count(preliminary_site_status, name = "value") |>
    dplyr::transmute(
      section = "site_status",
      metric = preliminary_site_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "candidate_permit_chains",
      "candidate_physical_sites",
      "sites_with_multiple_permit_chains",
      "sites_with_multiple_2015_footprints",
      "sites_with_2008_and_2015_footprints",
      "sites_without_any_footprint",
      "sites_within_500ft_by_permit_point",
      "sites_within_1500ft_by_permit_point"
    ),
    value = c(
      nrow(chain_members),
      nrow(site_ledger),
      sum(site_ledger$permit_chain_count > 1),
      sum(site_ledger$footprint_2015_count > 1),
      sum(site_ledger$footprint_2008_count > 0),
      sum(site_ledger$footprint_2015_count == 0),
      sum(site_ledger$minimum_boundary_distance_ft <= 500),
      sum(site_ledger$minimum_boundary_distance_ft <= 1500)
    )
  )
)

readr::write_csv(
  chain_members |>
    sf::st_drop_geometry() |>
    dplyr::select(
      candidate_site_id,
      permit_chain_id,
      dplyr::everything()
    ),
  "../output/permit_candidate_site_chain_members.csv"
)
readr::write_csv(
  footprint_members,
  "../output/permit_candidate_site_footprint_members.csv"
)
readr::write_csv(
  site_ledger,
  "../output/permit_candidate_site_ledger.csv"
)
sf::st_write(
  site_reference_points,
  "../output/permit_candidate_site_reference_points.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  site_polygons,
  "../output/permit_candidate_site_polygons.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  summary,
  "../output/permit_candidate_site_summary.csv"
)
