## Classify renovation permit text for the restricted-renovation score

source("../../setup_environment/code/packages.R")
source("../../_lib/alderman_uncertainty_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_variants/code")
# permits_input <- "../input/permits_for_uncertainty_index.csv"
# building_permits_input <- "../input/building_permits_clean.gpkg"
# classification_output <- "../output/renovation_text_classification_restricted_renovation.csv"
# summary_output <- "../output/renovation_text_classification_summary_restricted_renovation.csv"
# porch_summary_output <- "../output/renovation_text_classification_porch_keyword_summary_restricted_renovation.csv"
# mixed_summary_output <- "../output/renovation_text_classification_mixed_job_summary_restricted_renovation.csv"
# review_output <- "../output/renovation_text_classification_dropped_description_review_restricted_renovation.csv"
# filtered_permits_output <- "../output/permits_for_uncertainty_index_restricted_renovation.csv"
# max_permit_year <- 2022

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    permits_input,
    building_permits_input,
    classification_output,
    summary_output,
    porch_summary_output,
    mixed_summary_output,
    review_output,
    filtered_permits_output,
    max_permit_year
  )
}

if (length(args) >= 9) {
  permits_input <- args[1]
  building_permits_input <- args[2]
  classification_output <- args[3]
  summary_output <- args[4]
  porch_summary_output <- args[5]
  mixed_summary_output <- args[6]
  review_output <- args[7]
  filtered_permits_output <- args[8]
  max_permit_year <- as.integer(args[9])
} else {
  stop(
    "FATAL: Script requires 9 args: <permits_input> <building_permits_input> <classification_output> <summary_output> <porch_summary_output> <mixed_summary_output> <review_output> <filtered_permits_output> <max_permit_year>",
    call. = FALSE
  )
}

if (!is.finite(max_permit_year)) {
  stop("FATAL: max_permit_year must be a valid integer.", call. = FALSE)
}

permits <- load_uncertainty_permits(permits_input) %>%
  mutate(id = as.character(id)) %>%
  filter(year <= max_permit_year)
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

dropped_buckets <- c(
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

summary_order <- c(allowed_buckets, "telecom_infrastructure", "site_other_specialized", "porch_deck_stair_only", "generic_other", "missing_description")

classification_summary <- renovation_classification %>%
  count(renovation_text_bucket, keep_restricted_renovation, name = "n") %>%
  mutate(
    total_renovation = nrow(renovation_classification),
    total_nonmissing_description = sum(renovation_classification$has_description),
    share_of_all = n / total_renovation
  ) %>%
  mutate(renovation_text_bucket = factor(renovation_text_bucket, levels = summary_order)) %>%
  arrange(renovation_text_bucket) %>%
  mutate(renovation_text_bucket = as.character(renovation_text_bucket))

porch_keyword_summary <- renovation_classification %>%
  filter(has_porch_deck_stair_keyword) %>%
  count(renovation_text_bucket, keep_restricted_renovation, name = "n") %>%
  mutate(
    total_porch_keyword_rows = sum(n),
    share_of_porch_keyword_rows = n / total_porch_keyword_rows
  ) %>%
  mutate(renovation_text_bucket = factor(renovation_text_bucket, levels = summary_order)) %>%
  arrange(renovation_text_bucket) %>%
  mutate(renovation_text_bucket = as.character(renovation_text_bucket))

mixed_job_summary <- tibble(
  total_renovation_rows = nrow(renovation_classification),
  total_nonmissing_description_rows = sum(renovation_classification$has_description),
  missing_description_rows = sum(!renovation_classification$has_description),
  total_porch_keyword_rows = sum(renovation_classification$has_porch_deck_stair_keyword),
  porch_keyword_rows_porch_only = sum(renovation_classification$renovation_text_bucket == "porch_deck_stair_only"),
  porch_keyword_rows_mixed_substantive = sum(renovation_classification$naive_regex_wrong_drop),
  naive_regex_drop_rows = sum(renovation_classification$naive_regex_drop),
  naive_regex_wrong_drop_rows = sum(renovation_classification$naive_regex_wrong_drop),
  naive_regex_wrong_drop_share_of_porch_keyword_rows = mean(
    renovation_classification$naive_regex_wrong_drop[renovation_classification$has_porch_deck_stair_keyword]
  ),
  naive_regex_wrong_drop_share_of_naive_drop_rows = sum(renovation_classification$naive_regex_wrong_drop) /
    sum(renovation_classification$naive_regex_drop)
)

dropped_description_review <- renovation_classification %>%
  filter(renovation_text_bucket %in% dropped_buckets, has_description) %>%
  count(renovation_text_bucket, work_description, sort = TRUE, name = "n") %>%
  group_by(renovation_text_bucket) %>%
  mutate(
    bucket_total = sum(n),
    share_within_bucket = n / bucket_total,
    bucket_rank = row_number()
  ) %>%
  ungroup() %>%
  filter(bucket_rank <= 40)

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

write_csv(renovation_classification, classification_output)
write_csv(classification_summary, summary_output)
write_csv(porch_keyword_summary, porch_summary_output)
write_csv(mixed_job_summary, mixed_summary_output)
write_csv(dropped_description_review, review_output)
write_csv(filtered_permits, filtered_permits_output)

message("Saved restricted-renovation classification outputs:")
message("  Classification CSV: ", classification_output)
message("  Summary CSV: ", summary_output)
message("  Porch-keyword summary CSV: ", porch_summary_output)
message("  Mixed-job summary CSV: ", mixed_summary_output)
message("  Dropped-description review CSV: ", review_output)
message("  Filtered permits CSV: ", filtered_permits_output)
