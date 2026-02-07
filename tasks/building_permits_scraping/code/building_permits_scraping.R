## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

clean_names_base <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

to_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}

safe_min_date <- function(x) {
  if (all(is.na(x))) {
    return(as.Date(NA))
  }
  min(x, na.rm = TRUE)
}

safe_max_date <- function(x) {
  if (all(is.na(x))) {
    return(as.Date(NA))
  }
  max(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

safe_quantile <- function(x, p) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  as.numeric(quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
}

first_non_na <- function(x) {
  idx <- which(!is.na(x))
  if (length(idx) == 0) {
    return(NA_real_)
  }
  x[idx[1]]
}

last_non_na <- function(x) {
  idx <- which(!is.na(x))
  if (length(idx) == 0) {
    return(NA_real_)
  }
  x[idx[length(idx)]]
}

first_non_empty <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  idx <- which(!is.na(x) & x != "")
  if (length(idx) == 0) {
    return(NA_character_)
  }
  x[idx[1]]
}

collapse_unique_text <- function(x, max_items = 5L) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }
  vals <- unique(x)
  paste(vals[seq_len(min(length(vals), max_items))], collapse = " | ")
}

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- toupper(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("PERMIT\\s*#?\\s*[A-Z0-9-]+", " ", x, perl = TRUE)
  x <- gsub("#\\s*[0-9]{4,}", " ", x, perl = TRUE)
  x <- gsub("[^A-Z0-9/\\- ]+", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

input_file <- "../input/Building_Permits_20251121.csv"
output_permit_file <- "../output/building_permits_text_features.csv.gz"
output_project_file <- "../output/building_permit_project_features.csv.gz"
output_parcel_file <- "../output/building_permit_parcel_features.csv.gz"
output_sample_file <- "../output/building_permits_extraction_sample.csv"

permits <- data.table::fread(input_file, na.strings = c("", "NA"), encoding = "UTF-8")
setDT(permits)
setnames(permits, clean_names_base(names(permits)))

numeric_cols <- grep("cost|fee|paid|waived|subtotal|reported_cost|processing_time", names(permits), value = TRUE)
for (col in numeric_cols) {
  permits[, (col) := to_numeric(get(col))]
}

permits[, latitude := suppressWarnings(as.numeric(latitude))]
permits[, longitude := suppressWarnings(as.numeric(longitude))]
permits[, xcoordinate := suppressWarnings(as.numeric(xcoordinate))]
permits[, ycoordinate := suppressWarnings(as.numeric(ycoordinate))]

needs_conversion <- is.na(permits$latitude) & !is.na(permits$xcoordinate) & !is.na(permits$ycoordinate)
if (any(needs_conversion)) {
  converted_sf <- st_as_sf(
    permits[needs_conversion, .(xcoordinate, ycoordinate)],
    coords = c("xcoordinate", "ycoordinate"),
    crs = 3435,
    remove = FALSE
  )
  converted_sf <- st_transform(converted_sf, crs = 4326)
  converted_coords <- st_coordinates(converted_sf)
  permits[needs_conversion, `:=`(
    longitude = converted_coords[, "X"],
    latitude = converted_coords[, "Y"]
  )]
  rm(converted_sf, converted_coords)
}

permits[, application_start_date := as.Date(application_start_date, format = "%m/%d/%Y")]
permits[, issue_date := as.Date(issue_date, format = "%m/%d/%Y")]
permits[, event_date := issue_date]
permits[is.na(event_date), event_date := application_start_date]

permits[, permit_status_clean := toupper(trimws(as.character(permit_status)))]
permits[, permit_type_clean := toupper(trimws(as.character(permit_type)))]
permits[, review_type_clean := toupper(trimws(as.character(review_type)))]

issued_statuses <- c("COMPLETE", "ACTIVE", "PHASED PERMITTING", "ISSUED")
failed_statuses <- c("EXPIRED", "CANCELLED", "REVOKED", "SUSPENDED", "DENIED", "VOID")

permits[, permit_issued := fifelse(permit_status_clean %chin% issued_statuses, 1L,
                                   fifelse(permit_status_clean %chin% failed_statuses, 0L, NA_integer_))]
permits[, permit_failed_flag := fifelse(permit_status_clean %chin% failed_statuses, 1L, 0L)]

permits[, processing_time_from_dates := as.numeric(issue_date - application_start_date)]
permits[, processing_time_days := processing_time]
permits[is.na(processing_time_days), processing_time_days := processing_time_from_dates]
permits[, processing_time_gap := processing_time_days - processing_time_from_dates]
permits[, processing_time_nonnegative := fifelse(!is.na(processing_time_days) & processing_time_days >= 0, 1L, 0L)]

permits[, desc_upper_raw := toupper(ifelse(is.na(work_description), "", work_description))]
permits[, condition_upper_raw := toupper(ifelse(is.na(permit_condition), "", permit_condition))]
permits[, desc_clean := normalize_text(work_description)]
permits[, condition_clean := normalize_text(permit_condition)]
permits[, analysis_text := trimws(paste(desc_clean, condition_clean))]
permits[, desc_length := nchar(desc_clean)]

permits[, referenced_permit_n := 0L]
permits[, referenced_permit_last := NA_character_]
ref_candidate_idx <- which(str_detect(permits$desc_upper_raw, "PERMIT\\s*#?\\s*[A-Z0-9-]{6,}"))
if (length(ref_candidate_idx) > 0) {
  ref_matches <- str_match_all(permits$desc_upper_raw[ref_candidate_idx], "PERMIT\\s*#?\\s*([A-Z0-9-]{6,})")
  permits$referenced_permit_n[ref_candidate_idx] <- vapply(ref_matches, function(m) nrow(m), integer(1))
  permits$referenced_permit_last[ref_candidate_idx] <- vapply(
    ref_matches,
    function(m) if (nrow(m) == 0) NA_character_ else m[nrow(m), 2],
    character(1)
  )
  rm(ref_matches)
}

flag_patterns <- list(
  residential = "\\b(RESIDENTIAL|DWELLING|APARTMENT|APT\\b|MULTI\\s*FAMILY|SINGLE\\s*FAMILY|CONDO|D\\.?\\s*U\\.?)\\b",
  revision = "\\b(REVIS(?:E|ED|ION|IONS)|AMEND(?:ED|MENT)?|MODIF(?:Y|IED|ICATION)|CHANGE\\s+ORDER)\\b",
  revision_contractor = "\\b(REVIS(?:ION|ED)|CHANGE)\\b.*\\bCONTRACTOR\\b",
  conversion = "\\bCONVERT(?:ED|ING|ION)?\\b|\\bCNVERT\\b",
  deconversion = "\\bDE\\s*CONVERT(?:ED|ING|ION)?\\b|\\bDECONVERT(?:ED|ING|ION)?\\b",
  reduction = "\\b(REDUC(?:E|ED|TION)|DECREAS(?:E|ED)|ELIMINAT(?:E|ED)|REMOVE(?:D)?)\\b",
  increase = "\\b(INCREAS(?:E|ED)|ADD(?:ING|ED)?|EXPAND(?:ED|ING)?)\\b",
  adu = "\\bADU\\b|ACCESSORY\\s+DWELLING|COACH\\s+HOUSE|GARDEN\\s+UNIT|BASEMENT\\s+DWELLING",
  basement_unit = "BASEMENT\\s+(?:UNIT|DWELLING|APARTMENT)",
  coach_house = "COACH\\s+HOUSE",
  garden_unit = "GARDEN\\s+UNIT",
  single_family = "\\b(SINGLE\\s*FAMILY|SFR|1\\s*FAMILY|ONE\\s*FAMILY)\\b",
  multifamily = "\\b(MULTI\\s*FAMILY|APARTMENTS?|APTS?|DWELLING\\s+UNITS?)\\b",
  new_construction_phrase = "\\b(ERECT|NEW\\s+CONSTRUCTION|GROUND\\s*UP|NEW\\s+BUILDING)\\b",
  demolition_phrase = "\\b(DEMOL(?:ITION|ISH|ISHED)|WRECK(?:ING|ED)?)\\b",
  mixed_use = "\\bMIX(?:ED)?\\s*USE\\b|\\bRETAIL\\b|\\bCOMMERCIAL\\b",
  affordable = "\\bAFFORDABLE\\b|LOW\\s*INCOME|AHP\\b",
  zoning_relief = "\\b(TYPE\\s*1|VARIATION|VARIANCE|SPECIAL\\s+USE|PLANNED\\s+DEVELOPMENT|\\bPD\\b|ADMIN\\.?\\s*ADJ\\.?|ZONING)\\b",
  stop_or_denial = "\\b(DENI(?:ED|AL)|REVOK(?:E|ED)|CANCEL(?:LED|ED)|VOID|STOP\\s+WORK|WITHDRAWN?)\\b"
)

for (nm in names(flag_patterns)) {
  permits[, (paste0("flag_", nm)) := as.integer(str_detect(analysis_text, flag_patterns[[nm]]))]
}

unit_token <- "(?:D\\.?\\s*U\\.?|DWELLING(?:\\s+UNITS?)?|UNITS?|APTS?|APARTMENTS?|FLATS?|RESIDENCES?|CONDOS?|CONDOMINIUMS?)"
unit_mention_pattern <- paste0("(?<![0-9])(\\d{1,4}(?:,\\d{3})?)\\s*", unit_token, "\\b")
from_to_pattern_a <- paste0(
  "\\b(?:FROM|CONVERT(?:ED|ING)?\\s+FROM|DECONVERT(?:ED|ING)?\\s+FROM)\\s*(\\d{1,4})\\s*(?:",
  unit_token,
  ")?\\s*(?:TO|INTO)\\s*(\\d{1,4})\\s*(?:",
  unit_token,
  ")?\\b"
)
from_to_pattern_b <- paste0(
  "\\b(\\d{1,4})\\s*",
  unit_token,
  "\\s*(?:TO|INTO)\\s*(\\d{1,4})\\s*",
  unit_token,
  "\\b"
)

n <- nrow(permits)
unit_mentions_n <- integer(n)
units_first_mention <- rep(NA_real_, n)
units_last_mention <- rep(NA_real_, n)
units_min_mention <- rep(NA_real_, n)
units_max_mention <- rep(NA_real_, n)

unit_candidate_idx <- which(str_detect(permits$analysis_text, unit_token))
if (length(unit_candidate_idx) > 0) {
  unit_matches <- str_extract_all(permits$analysis_text[unit_candidate_idx], unit_mention_pattern)
  unit_numbers <- lapply(unit_matches, function(v) {
    if (length(v) == 0) {
      return(numeric())
    }
    suppressWarnings(as.numeric(gsub(",", "", str_extract(v, "\\d{1,4}(?:,\\d{3})?"))))
  })
  unit_mentions_n[unit_candidate_idx] <- lengths(unit_numbers)
  units_first_mention[unit_candidate_idx] <- vapply(unit_numbers, function(v) if (length(v) == 0) NA_real_ else v[1], numeric(1))
  units_last_mention[unit_candidate_idx] <- vapply(unit_numbers, function(v) if (length(v) == 0) NA_real_ else v[length(v)], numeric(1))
  units_min_mention[unit_candidate_idx] <- vapply(unit_numbers, function(v) if (length(v) == 0) NA_real_ else min(v, na.rm = TRUE), numeric(1))
  units_max_mention[unit_candidate_idx] <- vapply(unit_numbers, function(v) if (length(v) == 0) NA_real_ else max(v, na.rm = TRUE), numeric(1))
  rm(unit_matches, unit_numbers)
}

units_first_mention[units_first_mention > 1000] <- NA_real_
units_last_mention[units_last_mention > 1000] <- NA_real_
units_min_mention[units_min_mention > 1000] <- NA_real_
units_max_mention[units_max_mention > 1000] <- NA_real_

permits[, unit_mentions_n := unit_mentions_n]
permits[, units_first_mention := units_first_mention]
permits[, units_last_mention := units_last_mention]
permits[, units_min_mention := units_min_mention]
permits[, units_max_mention := units_max_mention]

rm(unit_mentions_n, units_first_mention, units_last_mention, units_min_mention, units_max_mention)

units_from_to_first <- rep(NA_real_, n)
units_to_first <- rep(NA_real_, n)
units_from_to_last <- rep(NA_real_, n)
units_to_last <- rep(NA_real_, n)
units_delta_last <- rep(NA_real_, n)
units_delta_max_abs <- rep(NA_real_, n)
from_to_pair_n <- integer(n)

from_to_candidate_idx <- which(
  str_detect(permits$analysis_text, "FROM\\s*\\d|CONVERT|DECONVERT|\\bTO\\b") &
    str_detect(permits$analysis_text, unit_token)
)

if (length(from_to_candidate_idx) > 0) {
  matches_a <- str_match_all(permits$analysis_text[from_to_candidate_idx], from_to_pattern_a)
  matches_b <- str_match_all(permits$analysis_text[from_to_candidate_idx], from_to_pattern_b)
  combined_pairs <- Map(function(a, b) {
    left <- if (nrow(a) == 0) matrix(numeric(0), ncol = 2) else suppressWarnings(matrix(as.numeric(a[, 2:3, drop = FALSE]), ncol = 2))
    right <- if (nrow(b) == 0) matrix(numeric(0), ncol = 2) else suppressWarnings(matrix(as.numeric(b[, 2:3, drop = FALSE]), ncol = 2))
    if (nrow(left) == 0 && nrow(right) == 0) {
      return(matrix(numeric(0), ncol = 2))
    }
    if (nrow(left) == 0) {
      return(right)
    }
    if (nrow(right) == 0) {
      return(left)
    }
    rbind(left, right)
  }, matches_a, matches_b)

  from_to_pair_n[from_to_candidate_idx] <- vapply(combined_pairs, nrow, integer(1))
  units_from_to_first[from_to_candidate_idx] <- vapply(combined_pairs, function(m) if (nrow(m) == 0) NA_real_ else m[1, 1], numeric(1))
  units_to_first[from_to_candidate_idx] <- vapply(combined_pairs, function(m) if (nrow(m) == 0) NA_real_ else m[1, 2], numeric(1))
  units_from_to_last[from_to_candidate_idx] <- vapply(combined_pairs, function(m) if (nrow(m) == 0) NA_real_ else m[nrow(m), 1], numeric(1))
  units_to_last[from_to_candidate_idx] <- vapply(combined_pairs, function(m) if (nrow(m) == 0) NA_real_ else m[nrow(m), 2], numeric(1))
  units_delta_max_abs[from_to_candidate_idx] <- vapply(
    combined_pairs,
    function(m) if (nrow(m) == 0) NA_real_ else max(abs(m[, 2] - m[, 1]), na.rm = TRUE),
    numeric(1)
  )

  rm(matches_a, matches_b, combined_pairs)
}

units_delta_last <- units_to_last - units_from_to_last
units_from_to_first[units_from_to_first > 1000] <- NA_real_
units_to_first[units_to_first > 1000] <- NA_real_
units_from_to_last[units_from_to_last > 1000] <- NA_real_
units_to_last[units_to_last > 1000] <- NA_real_
units_delta_last[abs(units_delta_last) > 1000] <- NA_real_
units_delta_max_abs[units_delta_max_abs > 1000] <- NA_real_

permits[, from_to_pair_n := from_to_pair_n]
permits[, units_from_to_first := units_from_to_first]
permits[, units_to_first := units_to_first]
permits[, units_from_to_last := units_from_to_last]
permits[, units_to_last := units_to_last]
permits[, units_delta_last := units_delta_last]
permits[, units_delta_max_abs := units_delta_max_abs]

rm(from_to_pair_n, units_from_to_first, units_to_first, units_from_to_last, units_to_last, units_delta_last, units_delta_max_abs)

story_pattern <- "\\b(\\d{1,2})\\s*(?:STORY|STORIES|STY|FLR|FLOOR)\\b"
sqft_pattern <- "\\b(\\d{2,7}(?:,\\d{3})*)\\s*(?:SQ\\.?\\s*FT\\.?|SQUARE\\s*FEET|\\bSF\\b)\\b"
parking_pattern <- "\\b(\\d{1,3})\\s*(?:PARKING\\s+SPACES?|PARKING|STALLS?|GARAGE\\s+SPACES?|CAR\\s+GARAGE)\\b"
bedroom_pattern <- "\\b(\\d{1,2})\\s*(?:BEDROOMS?|BR\\b)\\b"
bathroom_pattern <- "\\b(\\d{1,2})(?:\\.(\\d))?\\s*(?:BATHROOMS?|BATHS?|BA\\b)\\b"
commercial_space_pattern <- "\\b(\\d{1,3})\\s*(?:RETAIL|COMMERCIAL|BUSINESS)\\s*(?:SPACES?|UNITS?)\\b"
garage_car_pattern <- "\\b(\\d{1,3})\\s*(?:CAR|AUTO)\\s*GARAGE\\b"

stories_n <- integer(n)
stories_first <- rep(NA_real_, n)
stories_last <- rep(NA_real_, n)
stories_max <- rep(NA_real_, n)
stories_candidate_idx <- which(str_detect(permits$analysis_text, "STORY|STORIES|STY|FLR|FLOOR"))
if (length(stories_candidate_idx) > 0) {
  story_matches <- str_extract_all(permits$analysis_text[stories_candidate_idx], story_pattern)
  story_vals <- lapply(story_matches, function(v) {
    if (length(v) == 0) {
      return(numeric())
    }
    suppressWarnings(as.numeric(str_extract(v, "\\d{1,2}")))
  })
  stories_n[stories_candidate_idx] <- lengths(story_vals)
  stories_first[stories_candidate_idx] <- vapply(story_vals, function(v) if (length(v) == 0) NA_real_ else v[1], numeric(1))
  stories_last[stories_candidate_idx] <- vapply(story_vals, function(v) if (length(v) == 0) NA_real_ else v[length(v)], numeric(1))
  stories_max[stories_candidate_idx] <- vapply(story_vals, function(v) if (length(v) == 0) NA_real_ else max(v, na.rm = TRUE), numeric(1))
  rm(story_matches, story_vals)
}

sqft_n <- integer(n)
sqft_first <- rep(NA_real_, n)
sqft_last <- rep(NA_real_, n)
sqft_max <- rep(NA_real_, n)
sqft_candidate_idx <- which(str_detect(permits$analysis_text, "SQ\\.?\\s*FT|SQUARE\\s*FEET|\\bSF\\b"))
if (length(sqft_candidate_idx) > 0) {
  sqft_matches <- str_extract_all(permits$analysis_text[sqft_candidate_idx], sqft_pattern)
  sqft_vals <- lapply(sqft_matches, function(v) {
    if (length(v) == 0) {
      return(numeric())
    }
    suppressWarnings(as.numeric(gsub(",", "", str_extract(v, "\\d{2,7}(?:,\\d{3})?"))))
  })
  sqft_n[sqft_candidate_idx] <- lengths(sqft_vals)
  sqft_first[sqft_candidate_idx] <- vapply(sqft_vals, function(v) if (length(v) == 0) NA_real_ else v[1], numeric(1))
  sqft_last[sqft_candidate_idx] <- vapply(sqft_vals, function(v) if (length(v) == 0) NA_real_ else v[length(v)], numeric(1))
  sqft_max[sqft_candidate_idx] <- vapply(sqft_vals, function(v) if (length(v) == 0) NA_real_ else max(v, na.rm = TRUE), numeric(1))
  rm(sqft_matches, sqft_vals)
}

parking_n <- integer(n)
parking_first <- rep(NA_real_, n)
parking_last <- rep(NA_real_, n)
parking_max <- rep(NA_real_, n)
parking_candidate_idx <- which(str_detect(permits$analysis_text, "PARKING|STALL|GARAGE\\s+SPACE|CAR\\s+GARAGE"))
if (length(parking_candidate_idx) > 0) {
  parking_matches <- str_extract_all(permits$analysis_text[parking_candidate_idx], parking_pattern)
  parking_vals <- lapply(parking_matches, function(v) {
    if (length(v) == 0) {
      return(numeric())
    }
    suppressWarnings(as.numeric(str_extract(v, "\\d{1,3}")))
  })
  parking_n[parking_candidate_idx] <- lengths(parking_vals)
  parking_first[parking_candidate_idx] <- vapply(parking_vals, function(v) if (length(v) == 0) NA_real_ else v[1], numeric(1))
  parking_last[parking_candidate_idx] <- vapply(parking_vals, function(v) if (length(v) == 0) NA_real_ else v[length(v)], numeric(1))
  parking_max[parking_candidate_idx] <- vapply(parking_vals, function(v) if (length(v) == 0) NA_real_ else max(v, na.rm = TRUE), numeric(1))
  rm(parking_matches, parking_vals)
}

permits[, stories_mentions_n := stories_n]
permits[, stories_first_mention := stories_first]
permits[, stories_last_mention := stories_last]
permits[, stories_max_mention := stories_max]

permits[, sqft_mentions_n := sqft_n]
permits[, sqft_first_mention := sqft_first]
permits[, sqft_last_mention := sqft_last]
permits[, sqft_max_mention := sqft_max]

permits[, parking_mentions_n := parking_n]
permits[, parking_first_mention := parking_first]
permits[, parking_last_mention := parking_last]
permits[, parking_max_mention := parking_max]

rm(stories_n, stories_first, stories_last, stories_max, sqft_n, sqft_first, sqft_last, sqft_max, parking_n, parking_first, parking_last, parking_max)

permits[, bedrooms_text := suppressWarnings(as.numeric(str_match(analysis_text, bedroom_pattern)[, 2]))]
bathroom_matches <- str_match(permits$analysis_text, bathroom_pattern)
bathroom_whole <- suppressWarnings(as.numeric(bathroom_matches[, 2]))
bathroom_decimal <- suppressWarnings(as.numeric(bathroom_matches[, 3]))
bathroom_decimal[is.na(bathroom_decimal)] <- 0
bathroom_value <- bathroom_whole + bathroom_decimal / 10
bathroom_value[is.na(bathroom_whole)] <- NA_real_
permits[, bathrooms_text := bathroom_value]
rm(bathroom_matches, bathroom_whole, bathroom_decimal, bathroom_value)
permits[, commercial_spaces_text := suppressWarnings(as.numeric(str_match(analysis_text, commercial_space_pattern)[, 2]))]
permits[, garage_car_capacity_text := suppressWarnings(as.numeric(str_match(analysis_text, garage_car_pattern)[, 2]))]

permits[, stories_text := stories_last_mention]
permits[, sqft_text := sqft_last_mention]
permits[, parking_spaces_text := parking_last_mention]

permits[, unit_change_text := units_delta_last]
permits[is.na(unit_change_text) & flag_deconversion == 1L, unit_change_text := -1]
permits[is.na(unit_change_text) & flag_adu == 1L, unit_change_text := 1]
permits[is.na(unit_change_text) & flag_reduction == 1L & flag_residential == 1L, unit_change_text := -1]
permits[is.na(unit_change_text) & flag_increase == 1L & flag_residential == 1L, unit_change_text := 1]

permits[, unit_change_direction := fifelse(!is.na(unit_change_text) & unit_change_text < 0, "decrease",
                                           fifelse(!is.na(unit_change_text) & unit_change_text > 0, "increase",
                                                   fifelse(!is.na(unit_change_text) & unit_change_text == 0, "no_change", NA_character_)))]

permits[, unit_change_confidence := fifelse(!is.na(units_delta_last), "high",
                                            fifelse(from_to_pair_n > 0, "high",
                                                    fifelse(flag_deconversion == 1L | flag_adu == 1L, "medium",
                                                            fifelse(flag_reduction == 1L | flag_increase == 1L, "low", "none"))))]

permits[, unit_change_signal := as.integer(
  !is.na(unit_change_text) | from_to_pair_n > 0 | flag_deconversion == 1L | flag_adu == 1L |
    (flag_reduction == 1L & flag_residential == 1L) | (flag_increase == 1L & flag_residential == 1L)
)]

permits[, unit_reduction_signal := as.integer(unit_change_signal == 1L & !is.na(unit_change_text) & unit_change_text < 0)]
permits[, unit_increase_signal := as.integer(unit_change_signal == 1L & !is.na(unit_change_text) & unit_change_text > 0)]

permits[, permit_year := as.integer(format(issue_date, "%Y"))]
permits[is.na(permit_year), permit_year := as.integer(format(application_start_date, "%Y"))]
permits[, application_year := as.integer(format(application_start_date, "%Y"))]
permits[, issue_month := as.integer(format(issue_date, "%m"))]
permits[, application_month := as.integer(format(application_start_date, "%m"))]

permits[, pin_primary := str_extract(as.character(pin_list), "\\d{14}")]
permits[, pin_count := str_count(as.character(pin_list), "\\d{14}")]

permits[, address_key := str_squish(trimws(paste(
  as.character(street_number),
  as.character(street_direction),
  as.character(street_name)
)))]
permits[, address_key := toupper(address_key)]
permits[address_key == "", address_key := NA_character_]

permits[, project_group_key := fifelse(!is.na(pin_primary), paste0("PIN_", pin_primary),
                                       fifelse(!is.na(address_key), paste0("ADDR_", address_key), NA_character_))]
permits[is.na(project_group_key), project_group_key := paste0("ID_", id)]

setorder(permits, project_group_key, event_date, id)
permits[, days_since_prev := as.numeric(event_date - shift(event_date)), by = project_group_key]
permits[, new_cluster := fifelse(is.na(days_since_prev) | days_since_prev > 730, 1L, 0L)]
permits[, project_cluster := cumsum(new_cluster), by = project_group_key]
permits[, project_id := paste0(project_group_key, "_", sprintf("%03d", project_cluster))]
permits[, parcel_id := fifelse(!is.na(pin_primary), paste0("PIN_", pin_primary),
                               fifelse(!is.na(address_key), paste0("ADDR_", address_key), project_id))]

project_features <- permits[, .(
  pin_primary = first_non_empty(pin_primary),
  address_key = first_non_empty(address_key),
  ward = first_non_empty(ward),
  permit_n = .N,
  permit_type_n = uniqueN(permit_type_clean),
  review_type_n = uniqueN(review_type_clean),
  application_start_date_min = safe_min_date(application_start_date),
  issue_date_max = safe_max_date(issue_date),
  event_date_min = safe_min_date(event_date),
  event_date_max = safe_max_date(event_date),
  processing_time_days_mean = safe_mean(processing_time_days),
  processing_time_days_max = safe_max(processing_time_days),
  processing_time_days_p90 = safe_quantile(processing_time_days, p = 0.9),
  permit_issued_share = safe_mean(permit_issued),
  permit_failed_share = safe_mean(permit_failed_flag),
  any_residential = as.integer(any(flag_residential == 1L, na.rm = TRUE)),
  any_revision = as.integer(any(flag_revision == 1L, na.rm = TRUE)),
  any_revision_contractor = as.integer(any(flag_revision_contractor == 1L, na.rm = TRUE)),
  any_conversion = as.integer(any(flag_conversion == 1L, na.rm = TRUE)),
  any_deconversion = as.integer(any(flag_deconversion == 1L, na.rm = TRUE)),
  any_adu = as.integer(any(flag_adu == 1L, na.rm = TRUE)),
  any_reduction = as.integer(any(flag_reduction == 1L, na.rm = TRUE)),
  any_increase = as.integer(any(flag_increase == 1L, na.rm = TRUE)),
  any_new_construction_phrase = as.integer(any(flag_new_construction_phrase == 1L, na.rm = TRUE)),
  any_demolition_phrase = as.integer(any(flag_demolition_phrase == 1L, na.rm = TRUE)),
  any_zoning_relief = as.integer(any(flag_zoning_relief == 1L, na.rm = TRUE)),
  any_unit_change_signal = as.integer(any(unit_change_signal == 1L, na.rm = TRUE)),
  any_unit_reduction_signal = as.integer(any(unit_reduction_signal == 1L, na.rm = TRUE)),
  any_unit_increase_signal = as.integer(any(unit_increase_signal == 1L, na.rm = TRUE)),
  total_unit_mentions = sum(unit_mentions_n, na.rm = TRUE),
  total_from_to_pairs = sum(from_to_pair_n, na.rm = TRUE),
  units_signal_initial = first_non_na(ifelse(!is.na(units_from_to_first), units_from_to_first, units_first_mention)),
  units_signal_final = last_non_na(ifelse(!is.na(units_to_last), units_to_last, units_last_mention)),
  unit_change_text_initial_final = last_non_na(ifelse(!is.na(unit_change_text), unit_change_text, NA_real_)),
  unit_change_text_sum = sum(unit_change_text, na.rm = TRUE),
  stories_signal_initial = first_non_na(stories_first_mention),
  stories_signal_final = last_non_na(stories_last_mention),
  stories_text_max = safe_max(stories_max_mention),
  sqft_signal_initial = first_non_na(sqft_first_mention),
  sqft_signal_final = last_non_na(sqft_last_mention),
  sqft_text_max = safe_max(sqft_max_mention),
  parking_signal_initial = first_non_na(parking_first_mention),
  parking_signal_final = last_non_na(parking_last_mention),
  parking_spaces_text_max = safe_max(parking_max_mention),
  bedrooms_text_max = safe_max(bedrooms_text),
  bathrooms_text_max = safe_max(bathrooms_text),
  commercial_spaces_text_max = safe_max(commercial_spaces_text),
  garage_car_capacity_text_max = safe_max(garage_car_capacity_text),
  referenced_permit_n_total = sum(referenced_permit_n, na.rm = TRUE),
  example_descriptions = collapse_unique_text(work_description, max_items = 5L)
), by = project_id]

project_features[, units_signal_delta := units_signal_final - units_signal_initial]
project_features[, stories_signal_delta := stories_signal_final - stories_signal_initial]
project_features[, sqft_signal_delta := sqft_signal_final - sqft_signal_initial]
project_features[, parking_signal_delta := parking_signal_final - parking_signal_initial]
project_features[is.infinite(processing_time_days_p90), processing_time_days_p90 := NA_real_]
project_features[is.nan(processing_time_days_p90), processing_time_days_p90 := NA_real_]

setorder(permits, parcel_id, event_date, id)
parcel_features <- permits[, .(
  pin_primary = first_non_empty(pin_primary),
  address_key = first_non_empty(address_key),
  ward_mode = first_non_empty(ward),
  permit_n = .N,
  project_n = uniqueN(project_id),
  permit_type_n = uniqueN(permit_type_clean),
  review_type_n = uniqueN(review_type_clean),
  application_start_date_min = safe_min_date(application_start_date),
  issue_date_max = safe_max_date(issue_date),
  event_date_min = safe_min_date(event_date),
  event_date_max = safe_max_date(event_date),
  processing_time_days_mean = safe_mean(processing_time_days),
  processing_time_days_p90 = safe_quantile(processing_time_days, p = 0.9),
  processing_time_days_max = safe_max(processing_time_days),
  permit_issued_share = safe_mean(permit_issued),
  permit_failed_share = safe_mean(permit_failed_flag),
  total_reported_cost = sum(reported_cost, na.rm = TRUE),
  total_fee_paid = sum(total_fee, na.rm = TRUE),
  any_residential = as.integer(any(flag_residential == 1L, na.rm = TRUE)),
  any_new_construction_phrase = as.integer(any(flag_new_construction_phrase == 1L, na.rm = TRUE)),
  any_demolition_phrase = as.integer(any(flag_demolition_phrase == 1L, na.rm = TRUE)),
  any_revision = as.integer(any(flag_revision == 1L, na.rm = TRUE)),
  any_zoning_relief = as.integer(any(flag_zoning_relief == 1L, na.rm = TRUE)),
  any_unit_change_signal = as.integer(any(unit_change_signal == 1L, na.rm = TRUE)),
  any_unit_reduction_signal = as.integer(any(unit_reduction_signal == 1L, na.rm = TRUE)),
  any_unit_increase_signal = as.integer(any(unit_increase_signal == 1L, na.rm = TRUE)),
  unit_change_signal_count = sum(unit_change_signal, na.rm = TRUE),
  unit_reduction_signal_count = sum(unit_reduction_signal, na.rm = TRUE),
  unit_increase_signal_count = sum(unit_increase_signal, na.rm = TRUE),
  units_signal_initial = first_non_na(ifelse(!is.na(units_from_to_first), units_from_to_first, units_first_mention)),
  units_signal_final = last_non_na(ifelse(!is.na(units_to_last), units_to_last, units_last_mention)),
  unit_change_text_sum = sum(unit_change_text, na.rm = TRUE),
  total_unit_mentions = sum(unit_mentions_n, na.rm = TRUE),
  total_from_to_pairs = sum(from_to_pair_n, na.rm = TRUE),
  stories_signal_initial = first_non_na(stories_first_mention),
  stories_signal_final = last_non_na(stories_last_mention),
  stories_text_max = safe_max(stories_max_mention),
  sqft_signal_initial = first_non_na(sqft_first_mention),
  sqft_signal_final = last_non_na(sqft_last_mention),
  sqft_text_max = safe_max(sqft_max_mention),
  parking_signal_initial = first_non_na(parking_first_mention),
  parking_signal_final = last_non_na(parking_last_mention),
  parking_spaces_text_max = safe_max(parking_max_mention),
  bedrooms_text_max = safe_max(bedrooms_text),
  bathrooms_text_max = safe_max(bathrooms_text),
  commercial_spaces_text_max = safe_max(commercial_spaces_text),
  garage_car_capacity_text_max = safe_max(garage_car_capacity_text),
  referenced_permit_n_total = sum(referenced_permit_n, na.rm = TRUE),
  latitude = first_non_na(latitude),
  longitude = first_non_na(longitude),
  example_descriptions = collapse_unique_text(work_description, max_items = 8L)
), by = parcel_id]

parcel_features[, units_signal_delta := units_signal_final - units_signal_initial]
parcel_features[, stories_signal_delta := stories_signal_final - stories_signal_initial]
parcel_features[, sqft_signal_delta := sqft_signal_final - sqft_signal_initial]
parcel_features[, parking_signal_delta := parking_signal_final - parking_signal_initial]

contact_cols <- grep("^contact_[0-9]+_", names(permits), value = TRUE)
drop_cols <- c(contact_cols, "desc_upper_raw", "condition_upper_raw", "analysis_text")
drop_cols <- intersect(drop_cols, names(permits))
permit_export <- permits[, !drop_cols, with = FALSE]

sample_cols <- c(
  "id", "permit_type", "permit_status", "application_start_date", "issue_date", "processing_time_days",
  "unit_change_signal", "unit_change_direction", "unit_change_confidence",
  "units_from_to_last", "units_to_last", "units_delta_last",
  "units_first_mention", "units_last_mention", "unit_mentions_n",
  "stories_first_mention", "stories_last_mention", "stories_max_mention",
  "sqft_first_mention", "sqft_last_mention", "sqft_max_mention",
  "parking_first_mention", "parking_last_mention", "parking_max_mention",
  "bedrooms_text", "bathrooms_text", "commercial_spaces_text", "garage_car_capacity_text",
  "flag_revision", "flag_deconversion", "flag_conversion", "flag_adu",
  "flag_reduction", "flag_increase", "project_id", "work_description", "permit_condition"
)
sample_cols <- intersect(sample_cols, names(permit_export))

sample_rows <- permit_export[
  order(-unit_change_signal, -from_to_pair_n, -flag_revision, -unit_mentions_n, -processing_time_days)
][1:min(.N, 1000), ..sample_cols]

data.table::fwrite(permit_export, output_permit_file)
data.table::fwrite(project_features, output_project_file)
data.table::fwrite(parcel_features, output_parcel_file)
data.table::fwrite(sample_rows, output_sample_file)

cat("Rows in permit-level output:", nrow(permit_export), "\n")
cat("Rows in project-level output:", nrow(project_features), "\n")
cat("Rows in parcel-level output:", nrow(parcel_features), "\n")
cat("Permits with unit-change signal:", sum(permit_export$unit_change_signal, na.rm = TRUE), "\n")
cat("Permits with explicit from-to pairs:", sum(permit_export$from_to_pair_n > 0, na.rm = TRUE), "\n")
