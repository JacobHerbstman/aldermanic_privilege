# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

collapse_values <- function(x) {
  values <- sort(unique(x[!is.na(x) & x != ""]))
  if (length(values) == 0) NA_character_ else paste(values, collapse = " | ")
}

projects <- readr::read_csv(
  "../output/project_evidence_inventory.csv",
  show_col_types = FALSE
)

aliases <- dplyr::bind_rows(
  projects |>
    dplyr::transmute(project_id, alias = project_id),
  projects |>
    dplyr::select(project_id, source_project_ids) |>
    tidyr::separate_longer_delim(source_project_ids, delim = "/") |>
    dplyr::transmute(project_id, alias = source_project_ids),
  projects |>
    dplyr::select(project_id, source_family, component_pins) |>
    tidyr::separate_longer_delim(component_pins, delim = "/") |>
    dplyr::transmute(
      project_id,
      alias = paste0(source_family, "_", component_pins)
    ),
  projects |>
    dplyr::filter(source_family == "residential") |>
    dplyr::select(project_id, component_pins) |>
    tidyr::separate_longer_delim(component_pins, delim = "/") |>
    dplyr::transmute(
      project_id,
      alias = paste0("residential_multicard_", component_pins)
    )
) |>
  dplyr::filter(!is.na(alias), alias != "") |>
  dplyr::distinct() |>
  dplyr::add_count(alias, name = "alias_project_count") |>
  dplyr::filter(alias_project_count == 1L) |>
  dplyr::select(-alias_project_count)

permit_links <- readr::read_csv(
  "../input/project_permit_chain_links.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
) |>
  dplyr::inner_join(
    aliases,
    by = c("project_id" = "alias"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    work_description = stringr::str_squish(
      stringr::str_to_upper(dplyr::coalesce(work_description, ""))
    ),
    explicit_new_building_raw = stringr::str_detect(
      work_description,
      paste0(
        "\\bNEW CONSTRUCTION\\b|",
        "\\bCONSTRUCTION OF (?:A |AN )?NEW\\b|",
        "\\bCONSTRUCT(?:ION)? (?:A |AN )?NEW\\b|",
        "\\bERECT (?:A |AN )?NEW\\b|",
        "\\bERECT (?:A |AN )?[0-9]+[ -]?STORY\\b|",
        "\\bERECT .*\\b(BUILDING|RESIDENCE|HOUSE|DWELLING|TOWNHOUSE)\\b|",
        "\\bFULL BUILDING PERMIT\\b"
      )
    ),
    addition_or_accessory_scope = stringr::str_detect(
      work_description,
      paste0(
        "\\b(ADDITION|ADDITIONS)\\b.*\\bEXISTING\\b|",
        "\\bEXISTING\\b.*\\b(ADDITION|ADDITIONS)\\b|",
        "\\bNEW (GARAGE|PORCH|DECK|FENCE|CANOPY|VESTIBULE|STAIR|STAIRS)\\b|",
        "\\b(GARAGE|PORCH|DECK|FENCE|CANOPY|VESTIBULE|STAIR|STAIRS)\\b",
        ".*\\bEXISTING\\b"
      )
    ),
    explicit_new_building =
      explicit_new_building_raw & !addition_or_accessory_scope,
    explicit_existing_building_work = stringr::str_detect(
      work_description,
      paste0(
        "\\b(DECONVERSION|CONVERSION|ALTERATION|ALTERATIONS|MODIFICATION|",
        "MODIFICATIONS|REMODEL|REMODELING|RENOVATION|RENOVATIONS|REHAB|",
        "ADDITION|ADDITIONS|BUILDOUT|BUILD-OUT)\\b.*\\bEXISTING\\b|",
        "\\bEXISTING\\b.*\\b(DECONVERSION|CONVERSION|ALTERATION|",
        "ALTERATIONS|MODIFICATION|MODIFICATIONS|REMODEL|REMODELING|",
        "RENOVATION|RENOVATIONS|REHAB|ADDITION|ADDITIONS|BUILDOUT|",
        "BUILD-OUT)\\b|",
        "\\bCONVERSION OF UNIT\\b"
      )
    ),
    expanded_new_building_scope = stringr::str_detect(
      work_description,
      paste0(
        "\\bNEW\\b.*\\b(BUILDING|BLDG|RESIDENCE|HOUSE|DWELLING|",
        "TOWNHOUSE|APARTMENT|FLAT|D\\.U\\.|DU)\\b|",
        "\\b(PROPOSED|FUTURE)\\b.*\\b(BUILDING|BLDG|TOWER|RESIDENCE|",
        "HOUSE|DWELLING|APARTMENT|BUIDLING)\\b|",
        "\\b(CONSTRUCT|CONSTRUCTION OF|ERECT|ERECTION OF|EREC)\\b",
        ".*\\b(BUILDING|BLDG|RESIDENCE|HOUSE|DWELLING|TOWNHOUSE|",
        "APARTMENT|BUIDLING)\\b|",
        "\\bERECT\\b.*\\b([0-9]+|TWO|THREE|FOUR|FIVE|SIX|SEVEN|",
        "EIGHT|NINE|TEN)[ -]?(UNIT|DWELLING|D\\.U\\.?|DU)\\b|",
        "\\bCONSTRUCTION OF\\b.*\\b[0-9]+[ -]?(RESIDENTIAL )?UNITS?\\b|",
        "\\b[0-9]+[ -]?STORY\\b.*\\bRESIDENTIAL ",
        "(BUILDING|BLDG|APARTMENT|APARTMENTS)\\b|",
        "\\b[0-9]+[ -]?STORY BUILDING\\b.*\\bRESIDENTIAL APARTMENTS\\b|",
        "\\b[0-9]+[ -]?STORY\\b.*\\b[0-9]+[ -]?",
        "(UNIT|DWELLING|D\\.U\\.?|DU)",
        "\\b.*\\b(BUILDING|BLDG|BUIDLING|RESIDENTIAL|APARTMENT|PROJECT)\\b|",
        "\\b[0-9]+[ -]?(UNIT|DWELLING|D\\.U\\.?|DU)\\b",
        ".*\\b(BUILDING|BLDG|BUIDLING|RESIDENTIAL|APARTMENT|PROJECT)\\b|",
        "\\b(TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN)[ -]?",
        "(UNIT|FLAT)\\b.*\\b(BUILDING|BLDG|BUIDLING|RESIDENCE|",
        "RESIDENTIAL)\\b|",
        "\\bFULL BUILDING\\b|",
        "\\bAFFORDABLE HOUSING PROJECT\\b|",
        "\\bMULTI-FAMILY RESIDENTIAL DWELLINGS\\b"
      )
    ) &
      !addition_or_accessory_scope &
      !explicit_existing_building_work,
    direct_positive_new_building =
      directly_matched & explicit_new_building,
    chain_positive_new_building = explicit_new_building,
    direct_expanded_new_building =
      directly_matched & expanded_new_building_scope,
    chain_expanded_new_building = expanded_new_building_scope,
    direct_existing_building_work =
      directly_matched & explicit_existing_building_work,
    possible_new_building_phrase = stringr::str_detect(
      work_description,
      paste0(
        "\\bNEW (BUILDING|RESIDENCE|HOUSE|DWELLING|TOWNHOUSE|",
        "SINGLE FAMILY|SINGLE-FAMILY|TWO-FLAT|[0-9]+ UNIT|",
        "[0-9]+-UNIT)\\b|",
        "\\bNEW (?:[0-9]+|ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|",
        "NINE|TEN)[ -]?(?:STORY|UNIT)\\b|",
        "\\bERECT\\b"
      )
    ) &
      !explicit_new_building &
      !addition_or_accessory_scope
  )

permit_summary <- permit_links |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    chain_permit_records = dplyr::n_distinct(permit_id),
    directly_matched_permit_records =
      dplyr::n_distinct(permit_id[directly_matched]),
    direct_positive_new_building = any(direct_positive_new_building),
    chain_positive_new_building = any(chain_positive_new_building),
    direct_expanded_new_building = any(direct_expanded_new_building),
    chain_expanded_new_building = any(chain_expanded_new_building),
    direct_existing_building_work = any(direct_existing_building_work),
    possible_new_building_phrase = any(possible_new_building_phrase),
    possible_new_building_permits = collapse_values(
      permit_number[possible_new_building_phrase]
    ),
    possible_new_building_descriptions = collapse_values(
      work_description[possible_new_building_phrase]
    ),
    direct_permit_numbers = collapse_values(permit_number[directly_matched]),
    direct_permit_descriptions = collapse_values(
      work_description[directly_matched]
    ),
    chain_permit_numbers = collapse_values(permit_number),
    chain_permit_descriptions = collapse_values(work_description),
    .groups = "drop"
  )

coverage <- projects |>
  dplyr::left_join(
    permit_summary,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        direct_positive_new_building,
        chain_positive_new_building,
        direct_expanded_new_building,
        chain_expanded_new_building,
        direct_existing_building_work,
        possible_new_building_phrase
      ),
      ~ dplyr::coalesce(.x, FALSE)
    ),
    chain_permit_records = dplyr::coalesce(chain_permit_records, 0L),
    directly_matched_permit_records = dplyr::coalesce(
      directly_matched_permit_records,
      0L
    ),
    permit_rule_status = dplyr::case_when(
      positive_new_building_permit ~ "current_rule_positive",
      direct_expanded_new_building ~ "expanded_direct_positive",
      chain_expanded_new_building ~ "expanded_chain_positive",
      direct_existing_building_work ~ "direct_existing_building_work",
      possible_new_building_phrase ~ "possible_positive_phrase_review",
      chain_permit_records > 0 ~ "linked_chain_without_positive_scope",
      TRUE ~ "no_linked_permit_chain"
    )
  )

summary <- coverage |>
  dplyr::filter(within_1500ft) |>
  dplyr::count(
    period = dplyr::if_else(construction_year < 2008, "2006-2007", "2008-2022"),
    sample = dplyr::if_else(current_multifamily, "multifamily", "all_other"),
    permit_rule_status,
    name = "projects"
  ) |>
  dplyr::arrange(period, sample, permit_rule_status)

readr::write_csv(
  coverage,
  "../output/permit_rule_coverage.csv",
  na = ""
)
readr::write_csv(
  coverage |>
    dplyr::filter(
      within_1500ft,
      !positive_new_building_permit,
      chain_permit_records > 0
    ) |>
    dplyr::arrange(
      dplyr::desc(within_500ft),
      dplyr::desc(current_multifamily),
      permit_rule_status,
      project_id
    ),
  "../output/permit_rule_gap_review.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_rule_coverage_summary.csv",
  na = ""
)
