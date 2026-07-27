# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/reviewed_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    audit_construction_year = readr::col_integer(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double(),
    .default = readr::col_character()
  )
) |>
  dplyr::filter(
    audit_decision == "retain_assessor_only_pending_review"
  )

if (anyDuplicated(projects$project_id)) {
  stop("Pending Assessor-only project IDs are not unique.")
}

project_pins <- projects |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::transmute(
    project_id,
    pin = stringr::str_pad(component_pins, 14, pad = "0")
  )

if (anyDuplicated(project_pins$pin)) {
  stop("A component PIN is assigned to more than one pending project.")
}

sales <- data.table::fread(
  "../input/parcel_sales_city.csv",
  select = c(
    "pin",
    "sale_date",
    "sale_price",
    "sale_document_num",
    "sale_type"
  ),
  colClasses = c(
    pin = "character",
    sale_date = "character",
    sale_document_num = "character",
    sale_type = "character"
  ),
  showProgress = FALSE
) |>
  tibble::as_tibble() |>
  dplyr::filter(pin %in% project_pins$pin) |>
  dplyr::mutate(
    sale_date = as.Date(substr(sale_date, 1, 10)),
    sale_type = stringr::str_to_upper(sale_type)
  ) |>
  dplyr::inner_join(
    project_pins,
    by = "pin",
    relationship = "many-to-one"
  ) |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id, audit_construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    construction_date = as.Date(paste0(audit_construction_year, "-06-15")),
    years_from_construction = as.numeric(
      difftime(sale_date, construction_date, units = "days")
    ) / 365.25,
    valid_price = !is.na(sale_price) & sale_price >= 10000,
    land_sale = valid_price & sale_type == "LAND",
    improved_sale = valid_price & sale_type == "LAND AND BUILDING"
  )

sale_evidence <- sales |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    sale_count = dplyr::n(),
    prior_land_sale = any(
      land_sale &
        dplyr::between(years_from_construction, -6, 0)
    ),
    completion_period_improved_sale = any(
      improved_sale &
        dplyr::between(years_from_construction, -1, 3)
    ),
    substantially_earlier_improved_sale = any(
      improved_sale & years_from_construction < -2
    ),
    prior_land_sale_dates = paste(
      sort(unique(sale_date[
        land_sale &
          dplyr::between(years_from_construction, -6, 0)
      ])),
      collapse = "/"
    ),
    completion_improved_sale_dates = paste(
      sort(unique(sale_date[
        improved_sale &
          dplyr::between(years_from_construction, -1, 3)
      ])),
      collapse = "/"
    ),
    earlier_improved_sale_dates = paste(
      sort(unique(sale_date[
        improved_sale & years_from_construction < -2
      ])),
      collapse = "/"
    ),
    evidence_document_numbers = paste(
      sort(unique(sale_document_num[
        (land_sale &
          dplyr::between(years_from_construction, -6, 0)) |
          (improved_sale &
            dplyr::between(years_from_construction, -1, 3)) |
          (improved_sale & years_from_construction < -2)
      ])),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    sale_evidence = dplyr::case_when(
      prior_land_sale & completion_period_improved_sale ~
        "land_to_building_transition",
      completion_period_improved_sale ~
        "completion_period_improved_sale",
      substantially_earlier_improved_sale ~
        "earlier_improved_sale_requires_review",
      sale_count > 0 ~ "sale_history_not_informative",
      TRUE ~ "no_sale_history"
    )
  )

project_points <- projects |>
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = FALSE
  )

nearby_projects <- sf::st_is_within_distance(
  project_points,
  project_points,
  dist = 100
)

cluster_edges <- do.call(
  rbind,
  lapply(seq_along(nearby_projects), function(i) {
    j <- nearby_projects[[i]]
    j <- j[
      j > i &
        abs(
          projects$audit_construction_year[j] -
            projects$audit_construction_year[i]
        ) <= 1
    ]
    if (length(j) == 0) {
      return(NULL)
    }
    cbind(i, j)
  })
)

project_graph <- igraph::make_empty_graph(
  n = nrow(projects),
  directed = FALSE
)
if (!is.null(cluster_edges)) {
  project_graph <- igraph::add_edges(
    project_graph,
    as.vector(t(cluster_edges))
  )
}

projects$validation_cluster <- igraph::components(
  project_graph
)$membership

validation <- projects |>
  dplyr::left_join(
    sale_evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    sale_count = dplyr::coalesce(sale_count, 0L),
    prior_land_sale = dplyr::coalesce(prior_land_sale, FALSE),
    completion_period_improved_sale = dplyr::coalesce(
      completion_period_improved_sale,
      FALSE
    ),
    substantially_earlier_improved_sale = dplyr::coalesce(
      substantially_earlier_improved_sale,
      FALSE
    ),
    sale_evidence = dplyr::coalesce(
      sale_evidence,
      "no_sale_history"
    )
  ) |>
  dplyr::group_by(validation_cluster) |>
  dplyr::mutate(
    cluster_project_count = dplyr::n(),
    cluster_sale_support = any(
      sale_evidence %in% c(
        "land_to_building_transition",
        "completion_period_improved_sale"
      )
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    dplyr::desc(cluster_project_count),
    validation_cluster,
    audit_construction_year,
    project_id
  )

if (nrow(validation) != nrow(projects)) {
  stop("Assessor-only validation changed the project row count.")
}

cluster_summary <- validation |>
  dplyr::group_by(validation_cluster) |>
  dplyr::summarise(
    project_count = dplyr::n(),
    construction_years = paste(
      sort(unique(audit_construction_year)),
      collapse = "/"
    ),
    ward_pairs = paste(sort(unique(ward_pair)), collapse = "/"),
    addresses = paste(review_address, collapse = " | "),
    sale_support = any(cluster_sale_support),
    earlier_improved_sale = any(
      substantially_earlier_improved_sale
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(project_count),
    validation_cluster
  )

validation_summary <- validation |>
  dplyr::count(
    sale_evidence,
    cluster_project_count > 1,
    name = "projects"
  ) |>
  dplyr::rename(
    multi_project_cluster = `cluster_project_count > 1`
  )

readr::write_csv(
  validation,
  "../output/assessor_only_validation.csv",
  na = ""
)
readr::write_csv(
  cluster_summary,
  "../output/assessor_only_cluster_summary.csv",
  na = ""
)
readr::write_csv(
  validation_summary,
  "../output/assessor_only_validation_summary.csv",
  na = ""
)
