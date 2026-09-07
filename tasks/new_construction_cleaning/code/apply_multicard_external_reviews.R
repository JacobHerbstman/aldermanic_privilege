# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

density <- readr::read_csv(
  "../output/multicard_adjudicated_density_model_input.csv",
  show_col_types = FALSE
)
reviews <- readr::read_csv(
  "../output/multicard_external_review_queue.csv",
  show_col_types = FALSE
)
if (
  anyDuplicated(density$project_id) ||
    anyDuplicated(reviews$project_id)
) {
  stop("The external-review inputs are not uniquely keyed.", call. = FALSE)
}
if (
  any(!reviews$review_status %in% c("complete", "lineage_pending", "not_reviewed")) ||
    any(
      reviews$review_status %in% c("complete", "not_reviewed") &
        !reviews$multifamily_disposition %in%
          c("include", "exclude", "suppress")
    ) ||
    any(
      reviews$review_status == "lineage_pending" &
        reviews$multifamily_disposition != "pending"
    )
) {
  stop("The external review contains an incomplete disposition.", call. = FALSE)
}

review_fields <- reviews |>
  dplyr::select(
    project_id,
    pin,
    review_priority,
    review_address,
    review_status,
    external_structure_class,
    multifamily_disposition,
    classification_source,
    external_building_count,
    external_unit_count,
    external_building_sqft,
    source_1_url,
    source_2_url,
    supports_building_type,
    supports_final_units,
    reviewer_notes,
    review_date
  )

current_input <- density |>
  dplyr::mutate(
    original_dwelling_units = dwelling_units,
    original_building_sqft = building_sqft,
    externally_reviewed = project_id %in% reviews$project_id[reviews$review_status == "complete"],
    external_multifamily = dwelling_units > 1,
    external_value_used = FALSE
  ) |>
  dplyr::left_join(
    review_fields,
    by = "project_id",
    relationship = "one-to-one"
  )

validity_screen <- current_input |>
  dplyr::filter(
    is.na(multifamily_disposition) |
      !multifamily_disposition %in% c("suppress", "pending")
  )

type_classification <- validity_screen |>
  dplyr::mutate(
    external_multifamily = dplyr::case_when(
      multifamily_disposition == "include" ~ TRUE,
      multifamily_disposition == "exclude" ~ FALSE,
      TRUE ~ dwelling_units > 1
    )
  )

external_input <- type_classification |>
  dplyr::mutate(
    dwelling_units = dplyr::if_else(
      multifamily_disposition %in% c("include", "exclude") &
        !is.na(external_unit_count),
      external_unit_count,
      dwelling_units
    ),
    building_sqft = dplyr::if_else(
      multifamily_disposition %in% c("include", "exclude") &
        !is.na(external_building_sqft),
      external_building_sqft,
      building_sqft
    ),
    external_value_used =
      dplyr::coalesce(
        dwelling_units != original_dwelling_units,
        FALSE
      ) |
        dplyr::coalesce(
          building_sqft != original_building_sqft,
          FALSE
        ),
    density_far = building_sqft / land_sqft,
    density_dupac = 43560 * dwelling_units / land_sqft
  )

if (
  anyDuplicated(external_input$project_id) ||
    any(
      external_input$multifamily_disposition == "include" &
        !external_input$external_multifamily,
      na.rm = TRUE
    ) ||
    any(
      external_input$multifamily_disposition == "exclude" &
        external_input$external_multifamily,
      na.rm = TRUE
    ) ||
    any(
      external_input$external_multifamily &
        external_input$dwelling_units <= 1,
      na.rm = TRUE
    )
) {
  stop("The reviewed model input violates its sample rules.", call. = FALSE)
}

readr::write_csv(
  external_input,
  "../output/multicard_external_reviewed_model_input.csv",
  na = ""
)
