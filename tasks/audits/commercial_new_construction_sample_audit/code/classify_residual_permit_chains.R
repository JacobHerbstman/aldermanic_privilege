# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

extract_unit_mentions <- function(text) {
  matches <- stringr::str_match_all(
    stringr::str_to_upper(dplyr::coalesce(text, "")),
    paste0(
      "\\b([0-9]{1,4})\\s*",
      "(?:D\\s*\\.?\\s*U\\.?|DWELLING\\s+UNITS?|",
      "RESIDENTIAL\\s+UNITS?|APARTMENTS?|CONDOS?|UNITS?)\\b"
    )
  )
  vapply(matches, function(values) {
    if (nrow(values) == 0) {
      return(NA_real_)
    }
    max(suppressWarnings(as.numeric(values[, 2])), na.rm = TRUE)
  }, numeric(1))
}

chains <- readr::read_csv(
  "../output/permit_first_unmatched_residential_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
permits <- readr::read_csv(
  "../output/permit_first_permit_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    permit_chain_id %in% chains$permit_chain_id,
    permit_issued,
    dplyr::between(lubridate::year(application_date), 2006L, 2022L)
  ) |>
  dplyr::mutate(
    description_upper = stringr::str_squish(stringr::str_to_upper(
      dplyr::coalesce(work_description, "")
    )),
    residential_object_signal = stringr::str_detect(
      description_upper,
      paste0(
        "\\b(SINGLE[- ]?FAMILY|SFR|RESIDENTIAL|DWELLING|",
        "APARTMENT|CONDO(?:MINIUM)?|MULTI[- ]?FAMILY|",
        "MIXED[- ]?USE|D\\s*\\.?\\s*U\\.?)\\b"
      )
    ),
    full_building_action_signal = stringr::str_detect(
      description_upper,
      paste0(
        "\\b(ERECT|CONSTRUCT(?:ION)?|BUILD)\\b.{0,100}",
        "\\b(BUILDING|BLDG|RESIDENCE|HOUSE|HOME|",
        "SINGLE[- ]?FAMILY|SFR|DWELLING|APARTMENT|",
        "CONDO(?:MINIUM)?|MIXED[- ]?USE)\\b|",
        "\\bNEW\\s+(?:[0-9]+[- ]?(?:STORY|STOREY)\\s+)?",
        "(?:RESIDENTIAL|MIXED[- ]?USE|APARTMENT|CONDO|",
        "SINGLE[- ]?FAMILY|SFR)\\b|",
        "\\bFULL\\s+BUILDING\\b"
      )
    ),
    accessory_object_signal = stringr::str_detect(
      description_upper,
      "\\b(GARAGE|CARPORT|DECK|PORCH|FENCE|CANOPY|SHED)\\b"
    ),
    addition_signal = stringr::str_detect(
      description_upper,
      "\\b(ADDITION|ADD\\s+(?:A|ONE|[0-9]+)\\s+(?:DWELLING|RESIDENTIAL))\\b"
    ),
    revision_signal = stringr::str_detect(
      description_upper,
      "\\b(REVISION|REVISIONS|REVISE|REVISION TO PERMIT)\\b"
    ),
    foundation_signal = stringr::str_detect(
      description_upper,
      "\\b(FOUNDATION|CAISSON|UNDERSIDE OF SECOND)\\b"
    ),
    phase_only_signal = stringr::str_detect(
      description_upper,
      "\\b(ONLY|PHASE|SHELL)\\b"
    ),
    demolition_only_signal = stringr::str_detect(
      description_upper,
      "\\bDEMOLI(?:SH|TION)\\b"
    ) & !full_building_action_signal,
    main_object_accessory = stringr::str_detect(
      description_upper,
      paste0(
        "^(?:NEW\\s+)?(?:ERECT|CONSTRUCT|BUILD)?\\s*",
        "(?:A\\s+|AN\\s+|NEW\\s+|[0-9]+[- ]?(?:CAR|STALL)\\s+)*",
        "(?:DETACHED\\s+|MASONRY\\s+|FRAME\\s+|STEEL\\s+)*",
        "(GARAGE|CARPORT|DECK|PORCH|FENCE|CANOPY|SHED)\\b"
      )
    ),
    main_object_addition = stringr::str_detect(
      description_upper,
      paste0(
        "^(?:NEW\\s+)?(?:ERECT|CONSTRUCT|BUILD)?\\s*",
        "(?:A\\s+|AN\\s+|NEW\\s+|[0-9'\"X ]+\\s+)*",
        "(?:MASONRY\\s+|FRAME\\s+|REAR\\s+|FRONT\\s+)*",
        "ADDITION\\b"
      )
    ),
    parsed_unit_mention = extract_unit_mentions(description_upper)
  )

if (anyDuplicated(permits$permit_id) ||
    any(!permits$permit_chain_id %in% chains$permit_chain_id)) {
  stop("Residual permit semantic input violates its key contract.", call. = FALSE)
}

chain_semantics <- permits |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    issued_permit_records = dplyr::n(),
    any_residential_object = any(residential_object_signal),
    any_full_building_action = any(full_building_action_signal),
    any_accessory_object = any(accessory_object_signal),
    all_main_objects_accessory = all(
      main_object_accessory |
        revision_signal |
        demolition_only_signal
    ),
    any_addition = any(addition_signal),
    all_main_objects_addition = all(
      main_object_addition |
        revision_signal |
        demolition_only_signal
    ),
    any_revision = any(revision_signal),
    all_revision_or_phase = all(
      revision_signal |
        (foundation_signal & phase_only_signal) |
        demolition_only_signal
    ),
    any_foundation_phase = any(foundation_signal & phase_only_signal),
    maximum_parsed_unit_mention = suppressWarnings(max(
      parsed_unit_mention,
      na.rm = TRUE
    )),
    semantic_permit_numbers = paste(
      sort(unique(permit_number)),
      collapse = "/"
    ),
    semantic_descriptions = paste(
      sort(unique(work_description)),
      collapse = " || "
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    maximum_parsed_unit_mention = dplyr::if_else(
      is.infinite(maximum_parsed_unit_mention),
      NA_real_,
      maximum_parsed_unit_mention
    ),
    semantic_building_class = dplyr::case_when(
      any_full_building_action &
        any_residential_object &
        !all_main_objects_accessory &
        !all_main_objects_addition &
        !all_revision_or_phase ~ "full_residential_building",
      all_main_objects_accessory ~ "accessory_structure_only",
      all_main_objects_addition ~ "addition_only",
      all_revision_or_phase & any_foundation_phase ~
        "foundation_or_phase_only",
      all_revision_or_phase ~ "revision_or_demolition_only",
      any_residential_object ~ "ambiguous_residential_new_construction",
      TRUE ~ "no_residential_building_object"
    )
  ) |>
  dplyr::right_join(
    chains |>
      dplyr::select(
        permit_chain_id,
        representative_permit_number,
        representative_application_date,
        representative_address,
        representative_description,
        application_ward_pair,
        application_boundary_distance_ft,
        review_priority
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::arrange(
    factor(
      semantic_building_class,
      levels = c(
        "full_residential_building",
        "ambiguous_residential_new_construction",
        "foundation_or_phase_only",
        "accessory_structure_only",
        "addition_only",
        "revision_or_demolition_only",
        "no_residential_building_object"
      )
    ),
    application_boundary_distance_ft,
    permit_chain_id
  )

evidence <- readr::read_csv(
  "../output/permit_residual_evidence_matrix.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    permit_chain_id,
    completion_evidence_class,
    evidence_review_priority
  )

chain_semantics <- chain_semantics |>
  dplyr::left_join(
    evidence,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    independent_completion_evidence =
      evidence_review_priority %in% c(
        "high_completion_candidate",
        "medium_completion_candidate",
        "conflicting_completion_evidence",
        "likely_existing_project_link"
      ),
    final_semantic_review_priority = dplyr::case_when(
      semantic_building_class == "full_residential_building" &
        independent_completion_evidence ~
        "full_building_with_completion_evidence",
      semantic_building_class == "full_residential_building" ~
        "full_building_without_completion_match",
      semantic_building_class == "ambiguous_residential_new_construction" &
        independent_completion_evidence ~
        "ambiguous_building_with_completion_evidence",
      semantic_building_class == "ambiguous_residential_new_construction" ~
        "ambiguous_building_without_completion_match",
      semantic_building_class %in% c(
        "foundation_or_phase_only",
        "revision_or_demolition_only"
      ) ~ "phase_or_revision_chain",
      TRUE ~ "non_building_residential_context"
    )
  )

summary <- dplyr::bind_rows(
  chain_semantics |>
    dplyr::count(semantic_building_class, name = "value") |>
    dplyr::transmute(
      section = "semantic_class",
      metric = semantic_building_class,
      value
    ),
  chain_semantics |>
    dplyr::count(final_semantic_review_priority, name = "value") |>
    dplyr::transmute(
      section = "review_priority",
      metric = final_semantic_review_priority,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "residual_issued_residential_chains",
      "chains_with_improved_unit_mention",
      "full_building_chains_within_500ft",
      "full_building_chains_within_1500ft"
    ),
    value = c(
      nrow(chain_semantics),
      sum(is.finite(chain_semantics$maximum_parsed_unit_mention)),
      sum(
        chain_semantics$semantic_building_class ==
          "full_residential_building" &
          chain_semantics$application_boundary_distance_ft <= 500
      ),
      sum(
        chain_semantics$semantic_building_class ==
          "full_residential_building" &
          chain_semantics$application_boundary_distance_ft <= 1500
      )
    )
  )
)

readr::write_csv(
  permits,
  "../output/residual_permit_record_semantics.csv"
)
readr::write_csv(
  chain_semantics,
  "../output/residual_permit_chain_semantics.csv"
)
readr::write_csv(
  summary,
  "../output/residual_permit_semantic_summary.csv"
)
