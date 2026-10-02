# setwd("tasks/prepare_permit_construction/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/normalize_chicago_address.R")
source("../../shared/code/street_key.R")
source("../../shared/code/permit_unit_patterns.R")

first_issue_year <- 2006L
last_issue_year <- 2022L
repeat_permit_years <- 3L  # permits at one address with one dwelling count within these years are one building

permits <- read_csv("../input/building_permits_full.csv", col_types = cols(.default = col_character()),
  col_select = c(permit_id = id, permit_number = permit_, permit_type, permit_status, permit_milestone,
    issue_date, street_number, street_direction, street_name, work_description, pin_list, latitude, longitude)) |>
  filter(permit_type %in% c("PERMIT - NEW CONSTRUCTION", "PERMIT - RENOVATION/ALTERATION", "PERMIT - EASY PERMIT PROCESS",
    "PERMIT – EXPRESS PERMIT PROGRAM")) |>
  mutate(issue_date = as.Date(substr(issue_date, 1, 10)), issue_year = as.integer(format(issue_date, "%Y"))) |>
  filter(between(issue_year, first_issue_year, last_issue_year)) |>
  mutate(description = str_squish(str_to_upper(coalesce(work_description, ""))),
    # Notes describe later permits ("{ALSO SEE PERMIT #... TO: DECONVERSION ...}", "[SEE PERMIT #... TO CONVERT ...]",
    # "SEE REVISION #... TO ADD TWO FLOORS") or the review program ("***SELF CERT PROJECT***"), not this building.
    # Common misspellings and abbreviations are corrected.
    main_text = description |> str_remove_all("\\{[^}]*(?:\\}|$)|\\[[^\\]]*(?:\\]|$)|\\*{2,}[^*]*\\*{2,}") |>
      str_remove("\\bSEE (?:REVISION|PERMIT|#).*$") |>
      str_replace_all("\\bDWELING", "DWELLING") |> str_replace_all("\\bAPARMENT", "APARTMENT") |>
      str_replace_all("\\bTWELEVE\\b", "TWELVE") |>
      str_replace_all("\\bUNIT(S?)(BUILDING|BLDG)\\b", "UNIT\\1 \\2") |> str_replace_all("([0-9]) ?\\(DU\\)", "\\1 DU") |>
      str_replace_all("\\b(?:EXIST\\.|EXST'?G|EXSITING)", "EXISTING") |> str_replace_all("\\bSRF\\b", "SFR") |>
      str_replace_all("\\bSTRY\\b", "STORY") |> str_squish(),
    address = normalize_address(paste(str_remove(street_number, "^0+"), street_direction, street_name)),
    permit_pin10s = map_chr(str_extract_all(coalesce(pin_list, ""), "[0-9]{10}"),
      \(x) paste(unique(x), collapse = "/")),
    latitude = as.numeric(latitude), longitude = as.numeric(longitude))
stopifnot(!anyDuplicated(permits$permit_id), !anyNA(permits$issue_date))
# Some new buildings are filed as renovation, easy or express permits. Those count when the first sentence, after an
# optional label ("RESIDENTIAL PROJECT - ", "SELF-CERT: ") or wrecking clause ("REMOVE EXISTING BUILDING AND "),
# erects a building named directly as its object ("ERECT NEW 2 1/2 STORY SINGLE FAMILY FRAME RESIDENCE WITH REAR OPEN
# DECK AND DETACHED FRAME GARAGE"): the words before the dwelling state a height or a new building and name no
# accessory structure, trade work or place ("ERECT NEW PARTITIONS IN BASEMENT OF SFR"), and the sentence describes no
# work on an existing building or trade work for a new one ("NEW TWO STORY SINGLE FAMILY HOUSE WITH 200A SERVICE").
first_sentence <- str_split_i(permits$main_text, "\\. ", 1)
lead <- str_remove(first_sentence, paste0("^(?:(?!NEW CONSTRUCTION)[A-Z0-9 '.-]{0,30}?(?::| - ?) ?)?",
  "(?:(?:REMOVE|DEMOLISH|WRECK|RAZE)(?: AND REMOVE)? (?:THE |AN? )?EXISTING [A-Z0-9 -]{0,40}?(?:,? AND|&|THEN) )?"))
erect_verb <- "^(?:ERECT|CONSTRUCT|BUILD\\b(?!-? ?OUT)|NEW CONSTRUCTION(?: OF)?|NEW\\b)"
lead_object <- str_extract(lead, paste0(erect_verb, ".*?(?:RESIDEN[A-Z]*|\\bHOMES?\\b|\\bHOUSES?\\b|DWELLING[A-Z]*|",
  "\\bS\\.?F\\.?R\\b|\\bUNITS?\\b|TOWN ?HO[A-Z]*|ROW ?HO[A-Z]*|\\b(?:[0-9]|TWO|THREE|FOUR|SIX)[- ]?FLATS?\\b|BUILDING|BLDG)"))
permits <- permits |>
  filter(permit_type == "PERMIT - NEW CONSTRUCTION" | (!is.na(lead_object) &
    str_detect(lead_object, paste0("\\b(?:[0-9]+|ONE|TWO|THREE|FOUR|FIVE|SIX)(?: 1/2)?[- ]?(?:STORY|STORIES)\\b|",
      "^(?:ERECT|CONSTRUCT|BUILD) (?:A |AN )?NEW\\b|^NEW CONSTRUCTION")) &
    !str_detect(str_remove_all(str_remove(lead_object, erect_verb), "WITH (?:A |FULL )?BASE?MENT"),
      "\\b(?:OF|IN|ON|AT|FOR|TO|WITH|WITHIN|INTO|FROM|PROTECTING|SERVING|BEHIND)\\b|@") &
    !str_detect(lead_object, paste0("PORCH|DECK|GARAGE|STAIR|RAMP|CANOP|PATIO|PERGOLA|TRASH|ENCLOSURE|\\bBAY\\b|EXCAVAT|",
      "DRYWALL|FOUNDATION|BUILD-? ?OUT|WIRING|CIRCUIT|ELECTRIC|LOW VOLTAGE|\\bOVER\\b|COACH|ACCESSORY|SHED|FENCE|SIGN\\b|",
      "TENT|WALLS?\\b|PARTITION|ROOF|SERVICE|HVAC|FACADE|WINDOW|ELEVATOR|INSTALL")) &
    !str_detect(str_remove(lead, "ALL NEW ELECTRICAL,? MECHANICAL,? (?:AND )?PLUMBING"), paste0("\\bADDI|\\bADDT|\\bADITI|",
      "EXISTING|CONVER|REHAB|ALTERATION|RENOVAT|INTERIOR|REPLAC|REPAIR|DORMER|ELECTRIC|WIRING|LOW VOLTAGE|\\bSERVICE\\b|",
      "\\b[0-9]+ ?AMP?\\b|METER|FIRE ALARM|\\bFA SYSTEM|(?:SFR|RESIDENCE|HOME|HOUSE) (?:DETACHED |ATTACHED |REAR )?",
      "(?:CARPORT|GARAGE|DECK|PORCH)"))))

# House numbers the description gives on the permit's street, besides its own: "329, 335, 337, 339 EAST 25TH PLACE",
# "1626-46 SOUTH PRAIRIE", "1231/1233/1235 W GRENSHAW", with ranges on the permit's side of the street. Prototype
# references ("PER PROTOTYPE PERMIT #... AT 3156 S. STEWART") name another building.
listed_house_numbers <- function(text, street, own) {
  text <- str_remove_all(text, "PROTOTYPE[^.;]*")
  groups <- str_match_all(text, paste0("((?:\\b[0-9]{2,5}(?:\\s*-\\s*[0-9]{1,5})?(?:\\s*(?:,|/|&|AND|THRU|THROUGH|TO)\\s*)?)+)",
    "\\s+(?:[NSEW]\\.?\\s+|NORTH\\s+|SOUTH\\s+|EAST\\s+|WEST\\s+)?", street, "\\b"))[[1]][, 2]
  numbers <- unlist(map(groups, \(group) {
    parts <- str_match_all(group, "([0-9]{2,5})(?:\\s*(?:-|THRU|THROUGH|TO)\\s*([0-9]{1,5}))?")[[1]]
    unlist(map2(as.integer(parts[, 2]), parts[, 3], \(start, end) {
      if (is.na(end)) return(start)
      end <- as.integer(if (nchar(end) < nchar(start)) paste0(substr(start, 1, nchar(start) - nchar(end)), end) else end)
      if (end > start && end - start <= 200) seq(start, end, by = 2) else start
    }))
  }))
  numbers <- numbers[abs(numbers - own) <= 400 & numbers %% 2 == own %% 2 & numbers != own]
  paste(sort(unique(numbers)), collapse = "/")
}
permits <- bind_cols(permits, address_parts(permits$address) |> rename(house = number, street_word = street)) |>
  mutate(house_numbers = pmap_chr(list(main_text, street_word, house),
    \(text, street, own) if (is.na(own) | is.na(street)) "" else listed_house_numbers(text, street, own)))

# Dwelling counts stated in the description: "35 DWELLING UNITS", "3 D.U.", "(6) UNIT", "2-FLAT", "(7) 3-STORY ROWHOMES".
number_words <- c(ONE = "1", TWO = "2", THREE = "3", FOUR = "4", FIVE = "5", SIX = "6", SEVEN = "7",
  EIGHT = "8", NINE = "9", TEN = "10", ELEVEN = "11", TWELVE = "12")
count_text <- permits$main_text |>
  str_replace_all("\\b(ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN|ELEVEN|TWELVE)\\b(?:\\s*\\([0-9]+\\))?",
    \(x) unname(number_words[str_extract(x, "^[A-Z]+")]))
dwelling_count_pattern <- paste0("\\(?\\b([0-9]{1,4})\\)?[- ]*",
  "(?:(?:NEW|ATTACHED|RESIDENTIAL|CONDOMINIUM|CONDO|RENTAL|MASONRY|FRAME|BRICK|SINGLE[- ]FAMILY)[- ]+){0,2}",
  "(?:DWELLING UNITS?|DWELLINGS?|D\\s?\\.?\\s?U\\.?'?S?|RESIDENTIAL UNITS?|APARTMENT UNITS?|APARTMENTS?|APTS?|",
  "CONDO(?:MINIUM)? UNITS?|UNITS?|FLATS?|FAMILY|TOWN ?HOUSES?|TOWN ?HOMES?|ROW ?HOUSES?|ROW ?HOMES?)\\b")
counts_found <- map2(str_match_all(count_text, dwelling_count_pattern),
  str_match_all(count_text, attached_house_count_pattern),
  \(a, b) unique(as.integer(c(a[, 2], str_extract(b[, 1], "[0-9]+")))))

# Words naming a dwelling structure; generic "residential use" alone does not identify one.
dwelling_words <- paste0("\\bRESIDENCES?\\b|RESIDENTIAL (?:BUILDING|BLDG|STRUCTURE|DEVELOPMENT|HIGH[- ]?RISE|TOWER)|DWELLING|",
  "RESIDENTIAL UNITS?|FLOORS? (?:FOR )?RESIDENTIAL|(?:ELDERLY|SENIOR|AFFORDABLE|STUDENT|SUPPORTIVE) HOUSING|",
  "\\bS\\.?F\\.?[RHD]\\b|SINGLE[- ]?FA[A-Z]*LY|MULTI[- ]?FAMILY|TWO[- ]FAMILY|\\bDUPLEX\\b|APARTMENT|CONDO|",
  "MIXED[- ]USE|RESIDENTIAL ?/ ?COMMERCIAL|GROUND FLOOR (?:COMMERCIAL|RETAIL|BUSINESS|OFFICE)|",
  "TOWN ?HOUSE|TOWN ?HOME|ROW ?HOUSE|ROW ?HOME|\\bFLATS?\\b|",
  "(?<!NURSING |FUNERAL |MOBILE )\\bHOMES?\\b|(?<!PUMP |CLUB |FIELD |BOAT |GATE |GUARD |POWER |SCREEN )\\bHOUSES?\\b")
single_home_words <- paste0("\\bS\\.?F\\.?[RHD]\\b|SINGLE[- ]?FA[A-Z]*LY|ONE[- ]FAMILY|\\bRESIDENCE\\b|",
  "\\b(?:TOWN ?HOME|TOWN ?HOUSE|ROW ?HOME|ROW ?HOUSE|HOME|HOUSE)\\b")
permits <- permits |>
  mutate(stated_counts = map_chr(counts_found, \(x) paste(x, collapse = "/")),
    dwelling_text = str_detect(main_text, dwelling_words),
    permit_units = case_when(
      lengths(counts_found) > 0L ~ map_int(counts_found, \(x) x[1]),
      str_detect(main_text, "\\bDUPLEX\\b|TWO[- ]FAMILY") ~ 2L,
      str_detect(main_text, single_home_words) & !str_detect(main_text, "MULTI|\\bUNITS\\b") ~ 1L,
      TRUE ~ NA_integer_))

# Uses that make a building of unstated use non-residential, and work on an existing structure.
non_residential_words <- paste0("HOTEL|MOTEL|OFFICE|DAY ?CARE|SCHOOL|CHURCH|RESTAURANT|RETAIL|COMMER|MERCANTILE|DORM|ACCESSORY|STORE\\b|",
  "WAREHOUSE|STORAGE|INDUSTRIAL|FACTORY|PLANT\\b|BUILD-?OUT|TENANT|INTERIOR|CAR ?WASH|COMMUNITY|RECREATION|FIELD ?HOUSE|",
  "STATION|HOSPITAL|CLINIC|MEDICAL|LIBRARY|THEATER|BANK\\b|TOILET|SHELTER|CANOPY|PAVILION|STADIUM|POOL|EQUIPMENT|",
  "MECHANICAL|TRANSFORMER|GENERATOR|EXISTING")
# An accessory structure is the object of the first clause: "ERECT A 33X24 FRAME GARAGE PER PLANS, TO AN EXISTING ...",
# "NEW DETACHED GARAGE-PER 2019 CHICAGO BUILDING CODE". Additions include misspellings ("NEW FRAME ONE STORY ADDTION").
first_clause <- str_split_i(permits$main_text, ",|;|\\. |\\b(?:WITH|W/|AND|FOR|TO|AT|ON|OF|IN|BEHIND|SERVING)\\b", 1)
# Every new-construction permit keeps one scope reason. New residential buildings continue, and so do new buildings
# whose use the permit does not state ("ERECT NEW 3 STORY MASONRY BUILDING AS PER PLANS"): the Assessor decides those.
# Foundation and superstructure phases remain: they are grouped with the full permit or resolved by Assessor claims.
# A first clause erecting dwellings makes them new even when an addition follows ("ERECT EIGHT TOWNHOUSE, AN ADDITION TO
# A FOUR EXISTING ... TOWNHOUSES").
erects_dwellings <- str_detect(first_clause, "^(?:ERECT|CONSTRUCT|BUILD)\\b") & str_detect(first_clause, dwelling_words) &
  !str_detect(first_clause, "ADDI|CONVER|REHAB|EXISTING|FLOOR")
permits <- permits |> mutate(scope = case_when(
  str_detect(main_text, "\\bREVISION TO\\b|\\bREVISIONS? (?:OF|FOR|TO) (?:THE )?(?:DDS )?PERMIT\\b|\\bPERMIT REVISION\\b|\\bREINSTAT") ~ "revision",
  str_detect(main_text, "\\bERECTION STARTS\\b|\\bPERMIT EXPIRES ON\\b|\\bTENTS?\\b|\\bTEMPORARY (?:STRUCTURE|EXHIBIT|STAGE)") ~ "temporary_structure",
  str_detect(main_text, "(?<!IN )\\bAD+I?T+I?ONS?\\b|CONVER(?:T|SION)|\\bREHAB|\\bINTERIOR (?:ALTERATION|RENOVATION|REMODEL)") &
    !erects_dwellings ~ "addition_or_conversion",
  str_detect(first_clause, paste0("\\b(?:GARAGES?|CARPORTS?|DECKS?|PORCH(?:ES)?|STAIRS?|STAIRWAYS?|FENCES?|PERGOLAS?|GAZEBOS?|SHEDS?|",
    "BREEZEWAY|RAMPS?|LANDINGS?|SUNROOMS?|CANOP(?:Y|IES))\\b")) &
    !str_detect(first_clause, dwelling_words) &
    !str_detect(str_remove_all(first_clause, "BUILDING CODE"), "\\b(?:BUILDING|BLDG|UNITS?|D\\.?U)\\b") ~ "accessory_structure",
  str_detect(main_text, paste0("\\b(?:AT|FOR|TO|ON|SERVE|SERVES|SERVING|BEHIND) (?:AN |THE )?EXISTING (?:[0-9A-Z/.-]+ ){0,6}?",
    "(?:S\\.?F\\.?R|SINGLE[- ]?FAMILY|RESIDENCE|HOUSE|HOME|BUILDING|BLDG|DWELLING|UNIT)|\\bEXISTING ?:")) ~ "work_at_existing_building",
  stated_counts != "" | dwelling_text ~ "new_residential",
  str_detect(main_text, "\\b(?:ERECT|NEW|CONSTRUCT)") & str_detect(main_text, "\\b(?:BUILDINGS?|BLDGS?|STRUCTURES?|STOR(?:Y|IES))\\b") &
    !str_detect(main_text, non_residential_words) ~ "building_use_not_stated",
  TRUE ~ "not_residential"))

# The same address, dwelling count and a short interval identify a repeated permit for one building.
residential <- permits |> filter(scope == "new_residential") |> arrange(address, permit_units, issue_date, permit_id) |>
  group_by(address, permit_units) |>
  mutate(new_group = is.na(permit_units) | is.na(lag(issue_date)) | issue_year - lag(issue_year) > repeat_permit_years,
    building_group = cumsum(new_group)) |>
  group_by(address, permit_units, building_group) |>
  mutate(repeat_permit_numbers = if (n() > 1L) paste(permit_number[-1], collapse = "/") else "",
    any_permit_complete = any(permit_status == "COMPLETE")) |>
  ungroup() |> filter(new_group)
permits <- permits |> left_join(residential |> select(permit_id, repeat_permit_numbers, any_permit_complete),
    by = "permit_id", relationship = "one-to-one") |>
  mutate(scope = if_else(scope == "new_residential" & is.na(repeat_permit_numbers), "repeated_permit", scope)) |>
  filter(permit_type == "PERMIT - NEW CONSTRUCTION" | scope == "new_residential") |>
  select(permit_id, permit_number, permit_type, scope, issue_date, issue_year, address, house_numbers, permit_pin10s,
    permit_units, stated_counts,
    permit_status, permit_milestone, any_permit_complete, repeat_permit_numbers, latitude, longitude, description) |>
  arrange(issue_date, permit_id)
SaveData(permits, "permit_id", "../output/construction_permits.csv", na = "")
