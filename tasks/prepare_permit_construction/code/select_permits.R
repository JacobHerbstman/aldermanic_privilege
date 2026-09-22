# setwd("tasks/prepare_permit_construction/code")
# first_issue_year <- 2006
# last_issue_year <- 2022
# repeat_permit_years <- 3
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/normalize_chicago_address.R")
source("../../shared/code/permit_unit_patterns.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(first_issue_year, last_issue_year, repeat_permit_years)
stopifnot(length(args) == 3L)
first_issue_year <- as.integer(args[1])
last_issue_year <- as.integer(args[2])
repeat_permit_years <- as.integer(args[3])

permits <- read_csv("../input/building_permits_full.csv", col_types = cols(.default = col_character()),
  col_select = c(permit_id = id, permit_number = permit_, permit_type, permit_status, permit_milestone,
    issue_date, street_number, street_direction, street_name, work_description, pin_list, latitude, longitude)) |>
  filter(permit_type == "PERMIT - NEW CONSTRUCTION") |>
  mutate(issue_date = as.Date(substr(issue_date, 1, 10)), issue_year = as.integer(format(issue_date, "%Y"))) |>
  filter(between(issue_year, first_issue_year, last_issue_year)) |>
  mutate(description = str_squish(str_to_upper(coalesce(work_description, ""))),
    # Braced notes describe later permits ("{ALSO SEE PERMIT #... TO: DECONVERSION ...}"), not this building.
    main_text = str_squish(str_remove_all(description, "\\{[^}]*(?:\\}|$)")),
    address = normalize_address(paste(str_remove(street_number, "^0+"), street_direction, street_name)),
    permit_pin10s = map_chr(str_extract_all(coalesce(pin_list, ""), "[0-9]{10}"),
      \(x) paste(unique(x), collapse = "/")),
    latitude = as.numeric(latitude), longitude = as.numeric(longitude))
stopifnot(!anyDuplicated(permits$permit_id), !anyNA(permits$issue_date))

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
dwelling_words <- paste0("\\bRESIDENCES?\\b|RESIDENTIAL (?:BUILDING|BLDG|STRUCTURE|DEVELOPMENT|HIGH ?RISE|TOWER)|DWELLING|",
  "\\bSFR\\b|\\bSFH\\b|\\bSFD\\b|SINGLE[- ]?FA[A-Z]*LY|MULTI[- ]?FAMILY|TWO[- ]FAMILY|\\bDUPLEX\\b|APARTMENT|CONDO|",
  "TOWN ?HOUSE|TOWN ?HOME|ROW ?HOUSE|ROW ?HOME|\\bFLATS?\\b|",
  "(?<!NURSING |FUNERAL |MOBILE )\\bHOMES?\\b|(?<!PUMP |CLUB |FIELD |BOAT |GATE |GUARD |POWER |SCREEN )\\bHOUSES?\\b")
single_home_words <- paste0("\\bSFR\\b|\\bSFH\\b|\\bSFD\\b|SINGLE[- ]?FA[A-Z]*LY|ONE[- ]FAMILY|\\bRESIDENCE\\b|",
  "\\b(?:TOWN ?HOME|TOWN ?HOUSE|ROW ?HOME|ROW ?HOUSE|HOME|HOUSE)\\b")
permits <- permits |>
  mutate(stated_counts = map_chr(counts_found, \(x) paste(x, collapse = "/")),
    dwelling_text = str_detect(main_text, dwelling_words),
    permit_units = case_when(
      lengths(counts_found) > 0L ~ map_int(counts_found, \(x) x[1]),
      str_detect(main_text, "\\bDUPLEX\\b|TWO[- ]FAMILY") ~ 2L,
      str_detect(main_text, single_home_words) & !str_detect(main_text, "MULTI|\\bUNITS\\b") ~ 1L,
      TRUE ~ NA_integer_))

# Every new-construction permit keeps one scope reason; only new residential buildings continue.
# Foundation and superstructure phases remain: they are grouped with the full permit or resolved by Assessor claims.
permits <- permits |> mutate(scope = case_when(
  str_detect(main_text, "\\bREVISION TO\\b|\\bREVISIONS? (?:OF|FOR|TO) (?:THE )?(?:DDS )?PERMIT\\b|\\bREINSTAT") ~ "revision",
  str_detect(main_text, "\\bERECTION STARTS\\b|\\bPERMIT EXPIRES ON\\b|\\bTENTS?\\b|\\bTEMPORARY (?:STRUCTURE|EXHIBIT|STAGE)") ~ "temporary_structure",
  str_detect(main_text, "(?<!IN )\\bADDITIONS?\\b|CONVER(?:T|SION)|\\bINTERIOR (?:ALTERATION|RENOVATION|REMODEL)") ~ "addition_or_conversion",
  str_detect(main_text, "\\bGARAGE\\b") & stated_counts == "" & !dwelling_text ~ "garage",
  str_detect(main_text, "^(?:ERECT |CONSTRUCT |BUILD |NEW )?(?:A |AN )?(?:NEW )?(?:[0-9]+[- ]CAR )?(?:(?:DETACHED|ATTACHED|FRAME|MASONRY|1 STORY) )*GARAGE\\b") ~ "garage",
  stated_counts != "" | dwelling_text ~ "new_residential",
  TRUE ~ "not_residential"))

# The same address, dwelling count and a short interval identify a repeated permit for one building.
residential <- permits |> filter(scope == "new_residential") |> arrange(address, permit_units, issue_date, permit_id) |>
  group_by(address, permit_units) |>
  mutate(new_group = is.na(permit_units) | is.na(lag(issue_date)) | issue_year - lag(issue_year) > repeat_permit_years,
    building_group = cumsum(new_group)) |>
  group_by(address, permit_units, building_group) |>
  mutate(repeat_permit_numbers = if (n() > 1L) paste(permit_number[-1], collapse = "/") else "",
    any_permit_complete = any(permit_status == "COMPLETE")) |>
  ungroup() |> filter(new_group | is.na(permit_units))
permits <- permits |> left_join(residential |> select(permit_id, repeat_permit_numbers, any_permit_complete),
    by = "permit_id", relationship = "one-to-one") |>
  mutate(scope = if_else(scope == "new_residential" & is.na(repeat_permit_numbers), "repeated_permit", scope)) |>
  select(permit_id, permit_number, scope, issue_date, issue_year, address, permit_pin10s, permit_units, stated_counts,
    permit_status, permit_milestone, any_permit_complete, repeat_permit_numbers, latitude, longitude, description) |>
  arrange(issue_date, permit_id)
SaveData(permits, "permit_id", "../output/construction_permits.csv", na = "")
