classify_restricted_renovation_permits <- function(permits, building_permits_input) {
  permits <- permits %>%
    mutate(id = as.character(id))
  if (anyDuplicated(permits$id) > 0) {
    stop("Uncertainty permit input has duplicate permit id values.", call. = FALSE)
  }

  renovation_ids <- permits %>%
    filter(permit_type_clean == "renovation") %>%
    select(id)

  renovation_text <- st_read(
    building_permits_input,
    query = "SELECT id, work_description FROM building_permits_clean",
    quiet = TRUE
  ) %>%
    st_drop_geometry() %>%
    mutate(id = as.character(id)) %>%
    semi_join(renovation_ids, by = "id")
  if (anyDuplicated(renovation_text$id) > 0) {
    stop("Building permit text input has duplicate permit id values.", call. = FALSE)
  }

  allowed_buckets <- c(
    "addition_expansion",
    "unit_reconfiguration",
    "tenant_commercial_buildout",
    "rehab_interior_remodel",
    "facade_masonry_envelope",
    "systems_mep"
  )
  summary_order <- c(
    allowed_buckets,
    "telecom_infrastructure",
    "site_other_specialized",
    "porch_deck_stair_only",
    "generic_other",
    "missing_description"
  )

  renovation_classification <- renovation_ids %>%
    left_join(renovation_text, by = "id", relationship = "one-to-one") %>%
    mutate(
      work_description = na_if(work_description, ""),
      work_description_norm = work_description %>%
        replace_na("") %>%
        str_to_upper() %>%
        str_replace_all("[^A-Z0-9]+", " ") %>%
        str_squish(),
      has_description = work_description_norm != "",
      has_porch_deck_stair_keyword = has_description &
        str_detect(work_description_norm, "\\b(PORCH|PORCHES|DECK|DECKS|STAIR|STAIRS|STAIRWAY|STAIRWAYS)\\b"),
      is_addition_expansion = has_description & str_detect(
        work_description_norm,
        "\\b(ADDITION|ADDITIONS|ADDN|DORMER|ELEVATOR SHAFT|SHAFT|SECOND FLOOR ADDITION|2ND FLOOR ADDITION|SECOND FLR ADDITION|2ND FLR ADDITION|REAR ADDITION|STORY ADDITION|ROOM ADDITION|TWO STORY ADDITION|2 STORY ADDITION|ONE STORY REAR ADDITION|1 STORY REAR ADDITION|SECOND STORY ADDITION)\\b"
      ),
      is_unit_reconfiguration = has_description & str_detect(
        work_description_norm,
        "\\b(DECONVERT|DECONVERTING|DUPLEX|DUPLEXING|CONVERT|CONVERTING|BASEMENT APARTMENT|ATTIC|REMOVE KITCHEN|DWELLING UNIT|DWELLING UNITS|\\d+DU|DU)\\b"
      ),
      is_tenant_commercial_buildout = has_description & str_detect(
        work_description_norm,
        "\\b(TENANT|OFFICE|RETAIL|RESTAURANT|STOREFRONT|SUITE|COMMERCIAL|COFFEE BAR|SHOWROOM|FAST FOOD|\\bBAR\\b|ADMINISTRATIVE OFFICES)\\b"
      ),
      is_rehab_interior_remodel = has_description & str_detect(
        work_description_norm,
        "\\b(GUT REHAB|REHAB|REHABILITAT|RENOVAT|REMODEL|REMODELING|INTERIOR ALTERATION|INTERIOR ALTERATIONS|INTERIOR BUILD|INTERIOR BUILDOUT|INTERIOR REHAB|INTERIOR REMODEL|INTERIOR REMODELING|ALTERATION AND REPAIR|ALTERATION AND REPAIRS|ALTERATIONS AND REPAIR|ALTERATIONS AND REPAIRS)\\b"
      ),
      is_facade_masonry_envelope = has_description & str_detect(
        work_description_norm,
        "\\b(MASONRY|BRICK|LINTEL|FACADE|TUCKPOINT|ROOF|ROOFING|WINDOW|WINDOWS|DOOR|DOORS|BALCONY|SIDING)\\b"
      ),
      is_systems_mep = has_description & str_detect(
        work_description_norm,
        "\\b(ELECTRICAL|PLUMBING|MECHANICAL|HVAC|FURNACE|BOILER|DUCTWORK|SPRINKLER|TOILET ROOM|BATHROOM|SERVICE)\\b"
      ),
      is_telecom_infrastructure = has_description & str_detect(
        work_description_norm,
        "\\b(ANTENNA|WIRELESS|CLEARWIRE|SPRINT|CABLING|EQUIPMENT CABINETS|EQUIPMENT SHELTER|CELLULAR|CELL PHONE)\\b"
      ),
      is_site_other_specialized = has_description & str_detect(
        work_description_norm,
        "\\b(PLAYLOT|PLAY LOT|ADA ACCESSIBLE|GARAGE|FENCE|DRIVEWAY|PARKING LOT|PARKING SPOT)\\b"
      ),
      renovation_text_bucket = case_when(
        !has_description ~ "missing_description",
        is_addition_expansion ~ "addition_expansion",
        is_unit_reconfiguration ~ "unit_reconfiguration",
        is_tenant_commercial_buildout ~ "tenant_commercial_buildout",
        is_rehab_interior_remodel ~ "rehab_interior_remodel",
        is_facade_masonry_envelope ~ "facade_masonry_envelope",
        is_systems_mep ~ "systems_mep",
        is_telecom_infrastructure ~ "telecom_infrastructure",
        is_site_other_specialized ~ "site_other_specialized",
        has_porch_deck_stair_keyword ~ "porch_deck_stair_only",
        TRUE ~ "generic_other"
      ),
      keep_restricted_renovation = renovation_text_bucket %in% allowed_buckets,
      naive_regex_drop = has_porch_deck_stair_keyword,
      naive_regex_wrong_drop = naive_regex_drop & renovation_text_bucket != "porch_deck_stair_only"
    )

  filtered_permits <- permits %>%
    left_join(
      renovation_classification %>%
        select(id, renovation_text_bucket, keep_restricted_renovation, has_porch_deck_stair_keyword),
      by = "id",
      relationship = "one-to-one"
    ) %>%
    filter(
      permit_type_clean != "renovation" |
        (permit_type_clean == "renovation" & keep_restricted_renovation)
    )

  list(
    classification = renovation_classification,
    filtered_permits = filtered_permits,
    allowed_buckets = allowed_buckets,
    summary_order = summary_order
  )
}
