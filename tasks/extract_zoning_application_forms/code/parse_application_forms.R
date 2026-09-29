# setwd("tasks/extract_zoning_application_forms/code")
# The size of the project described in each zoning map amendment's application, from the fresh OCR of its form and
# narrative pages (ocr_application_forms.R), one row per record:
#   lot_sqft: item 10, "Lot size in square feet (or dimensions)": a square footage ("16,678 sq. ft."), else the
#     product of dimensions ("90' X 125'"), else acres, else a bare number ("11,248"). A lot below 500 square feet,
#     from the form or a narrative, is an OCR misreading ("1_ 981 63 sq ft") and is left blank (a form's with source
#     form_unreadable);
#   dwelling_units, parking_spaces, commercial_sqft, height_ft, stories: item 13, "Describe the proposed use of the
#     property after the rezoning. Indicate the number of dwelling units; number of parking spaces; approximate square
#     footage of any commercial space; and height of the proposed building. (BE SPECIFIC)", which runs to the
#     affordable-housing question. Counts may be words ("thirty-four (34)", where the digits are read); where the
#     text gives several, the largest is kept, except that parking listed in parts in one sentence is added up ("one
#     (1) surface parking space and two (2) garage parking spaces") unless the sentence gives the total. "No
#     commercial space", "no dwelling units" and "no parking" are read as 0, a single-family home as one unit, a "2 car
#     garage" as two spaces. Heights are in whole feet as printed (44'-6" is 44). An existing building that remains
#     is the building after the rezoning;
#   the same fields from the lines of a Type 1 narrative ("Lot Area 4,960 Square Feet", "Building Height 27 Feet",
#     "Parking 0 Parking Spaces"), for Type 1 applications whose form does not give them;
#   type_1: whether any page names a Type 1 application.
# Each value keeps its source (form or narrative) and the text it was read from. Planned developments' bulk tables are
# not read: their labels and values come apart in OCR.
units_words <- c(zero = 0, no = 0, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8,
  nine = 9, ten = 10, eleven = 11, twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15, sixteen = 16,
  seventeen = 17, eighteen = 18, nineteen = 19)
tens_words <- c(twenty = 20, thirty = 30, forty = 40, fifty = 50, sixty = 60, seventy = 70, eighty = 80, ninety = 90)
square_feet_per_acre <- 43560
minimum_lot_sqft <- 500

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

area_unit <- "(?:s[aq][._]?\\s*f(?:ee)?t\\.?|square(?:\\s+f\\w{2,3})?|s\\.?\\s?-?f\\.?\\b|saft)"
feet_unit <- "(?:'|\u2019|ft\\.?|feet|foot)"
# OCR reads a thousands comma in an area as a period ("2.784 square feet", "57.799.35"), as a colon ("7:724.5 sq ft")
# or with a space before it ("49 ,460").
pages <- arrow::read_parquet("../output/application_form_pages.parquet") |>
  mutate(text = str_squish(ocr_text) |>
    str_replace_all(regex(paste0("(\\d{1,3})[.:](\\d{3})(?=[.,]\\d|\\s*", area_unit, ")"), ignore_case = TRUE),
      "\\1\\2") |>
    str_replace_all("(\\d) ,(\\d{3})\\b", "\\1,\\2"))

# Counts written as digits or words ("four", "thirty-four"), with digits in parentheses ("two (2)") read in preference.
number_word <- paste0("(?:(?:", paste(names(tens_words), collapse = "|"), ")(?:[- ](?:",
  paste(names(units_words)[3:11], collapse = "|"), "))?|", paste(names(units_words), collapse = "|"), ")\\b")
count_pattern <- paste0("(\\d[\\d,]*|", number_word, ")(?:[\\s-]*\\((\\d+)\\))?")
as_count <- function(word, digits) {
  word <- tolower(gsub(",", "", word))
  parts <- strsplit(word, "[- ]")
  from_words <- vapply(parts, function(p) sum(c(tens_words, units_words)[p]), numeric(1))
  coalesce(suppressWarnings(as.numeric(digits)), suppressWarnings(as.numeric(word)), from_words)
}
largest <- function(values) if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE)
# The counts in the text between the words before them and the words after them ("four-story": after = "stor").
counts <- function(text, before = "", after = "") {
  found <- str_match_all(text, regex(paste0(before, "\\b", count_pattern, after), ignore_case = TRUE))[[1]]
  as_count(found[, 2], found[, 3])
}
largest_count <- function(text, before = "", after = "") largest(counts(text, before, after))

# 1. The form's items 10 and 13, from each record's selected pages joined in order (items run across pages). Item
# labels are matched loosely ("(or dimensions'):", "(BE'SPECIFIC) |"); an item ends at the next item's label.
item_10 <- regex("lot\\W*size\\W+in\\W+square\\W+feet\\W+\\(?\\s*or\\W+dimensions\\W*(.{0,160})", ignore_case = TRUE)
item_11 <- regex("(?:\\d{1,2}\\s*[.,]\\s*)?(?:c\\w{1,4}ent\\W+use|reason\\W+for\\W+rezoning).*$", ignore_case = TRUE)
item_13 <- regex("proposed\\s*building\\W+\\S{0,3}\\W*be\\W*\\w{5,9}\\W*(.{0,2500})", ignore_case = TRUE)
item_14 <- regex("(?:\\d{1,2}\\s*[.,]\\s*)?(?:on may 14|the\\s+a\\w{1,3}ordable\\s+req|is this project subject).*$",
  ignore_case = TRUE)
forms <- pages |>
  filter(!data_page) |>
  arrange(record_number, file_name, page) |>
  summarise(text = paste(text, collapse = " "), .by = c(matter_id, record_number)) |>
  transmute(matter_id, record_number,
    lot_text = str_squish(str_remove(str_match(text, item_10)[, 2], item_11)),
    use_text = str_squish(str_remove(str_match(text, item_13)[, 2], item_14)))

# In item 10 the thousands separator is also read as a space or a mark ("12, 366.70 square feet", "3 044 sf",
# "23»501", "2+909", "97/970", "19-940.01"). A number after "x" is a dimension
# ("100 x 150 square feet"), not the area; an area whose digits are grouped wrongly ("22,202525") is not read. A
# dimension may be in feet and inches ("50-0 feet") or have a decimal comma ("30,0'").
lot_square_feet <- function(text) {
  text <- str_replace_all(tolower(coalesce(text, "")),
    paste0("(\\d{1,3})(?:,? |\\s?[\u00bb+/.-]\\s?)(\\d{3})(?=(?:\\.\\d+)?\\s*", area_unit, ")"), "\\1\\2")
  area <- str_match(text, paste0("(?<!x\\s{0,2})(?<![\\d.,])((?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d+)?)\\s*",
    area_unit))[, 2]
  dimension <- "(\\d+(?:[.,]\\d+)?)(?:\\s*'?\\s*-\\s*\\d{1,2}(?:\\.\\d+)?)?"
  dimensions <- str_match(text, paste0(dimension, "\\s*(?:", feet_unit, "|[^\\w\\s])?[\\s_]*(?:xk?|by)\\s*", dimension))
  dimensions[, 2:3] <- sub(",", ".", dimensions[, 2:3])
  acres <- str_match(text, "(\\d+(?:\\.\\d+)?)\\s*acres?")[, 2]
  bare <- str_match(text, "^\\W*(\\d{1,3}(?:,\\d{3})+|\\d{3,})(?:\\.\\d+)?\\b")[, 2]
  case_when(!is.na(area) ~ as.numeric(gsub(",", "", area)),
    !is.na(dimensions[, 1]) ~ as.numeric(dimensions[, 2]) * as.numeric(dimensions[, 3]),
    !is.na(acres) ~ as.numeric(acres) * square_feet_per_acre,
    !is.na(bare) ~ as.numeric(gsub(",", "", bare)))
}
use_fields <- function(text) {
  text <- coalesce(text, "")
  says <- function(pattern) grepl(pattern, text, ignore.case = TRUE, perl = TRUE)
  area <- "(\\d[\\d,]*(?:\\.\\d+)?)"
  units <- largest(c(
    largest_count(text, after = paste0("[\\s-]+(?:(?:new|residential|dwelling|affordable|rental|condominium|senior|",
      "apartment)\\s+){0,2}(?:units?|DUs?)\\b")),
    largest_count(text, after = "[\\s-]+(?:townhomes|townhouses)\\b")))
  # Commercial space before its area ("3,000 square feet of retail space") or after it ("commercial space (+/- 1,920
  # square feet)").
  commercial <- str_match_all(text, regex(paste0(area, "\\s*", area_unit, "\\s*(?:of\\s+)?(?:\\w+\\s+){0,3}",
    "(?:commercial|retail|office)|(?:commercial|retail|office)(?:/\\w+)?\\s+space\\s*\\(?\\s*",
    "(?:\\+/-\\s*|approx\\S*\\s*)?", area, "\\s*", area_unit), ignore_case = TRUE))[[1]]
  heights <- str_match_all(text, regex(paste0("height[^.;\\d]{0,40}?(\\d+(?:\\.\\d+)?)\\s*", feet_unit, "|",
    "(\\d+(?:\\.\\d+)?|", number_word, ")[\\s-]*", feet_unit, "(?:[\\s,-]*\\d+\\s*(?:\"|\u201d|inch\\w*|in\\.?))?",
    "[\\s,]*(?:\\(approx\\.?\\)\\s*)?(?:-\\s*)?(?:tall|high|in height)"), ignore_case = TRUE))[[1]]
  # Parking stated in parts within a sentence ("One (1) surface parking space and two (2) garage parking spaces") is
  # added up, unless the sentence also states the total ("for a total of twenty-eight (28) ... parking spaces").
  spaces <- "[\\s-]+(?:[a-z-]+[\\s-]+){0,2}parking\\s+spaces?"
  sentence_parking <- function(sentence) {
    total <- counts(sentence, before = "total of\\s+", after = spaces)
    if (length(total) > 0) return(max(total))
    found <- c(counts(sentence, after = spaces),
      counts(sentence, after = "[\\s-]+(?:car|vehicle)[\\s-]+garage"),
      counts(sentence, after = "[\\s-]+spaces?[\\s-]+parking"),
      counts(sentence, before = "parking(?:\\s+\\w+)?\\s+for\\s+(?:up to\\s+|at least\\s+|an additional\\s+)?",
        after = "\\s+(?:\\w+\\s+)?vehicles"))
    if (length(found) == 0) NA_real_ else sum(found)
  }
  tibble(
    dwelling_units = case_when(!is.na(units) ~ units,
      says("single[- ]family (?:home|residence|house)") ~ 1,
      says("\\bno (?:proposed )?(?:dwelling units|residential)") ~ 0),
    parking_spaces = largest(c(vapply(str_split(text, "\\.\\s+")[[1]], sentence_parking, numeric(1)),
      if (says("\\b(?:no|zero)\\s+(?:on-?site\\s+|off-?\\s?street\\s+)?parking\\b")) 0)),
    commercial_sqft = coalesce(largest(as.numeric(gsub(",", "", coalesce(commercial[, 2], commercial[, 3])))),
      if (says("\\bno commercial\\b")) 0 else NA_real_),
    height_ft = largest(floor(coalesce(as.numeric(heights[, 2]), as_count(heights[, 3], NA)))),
    stories = largest_count(text, after = "[\\s-]+stor(?:y|ies)\\b"))
}

# 2. The lines of a Type 1 narrative, from its data pages.
narrative_fields <- pages |>
  filter(data_page) |>
  summarise(text = paste(text, collapse = " "), .by = c(matter_id, record_number)) |>
  mutate(
    narrative_lot_sqft = as.numeric(gsub(",", "", str_match(text, regex(paste0("lot area\\s*:?\\s*(\\d[\\d,]*)\\s*",
      area_unit), ignore_case = TRUE))[, 2])),
    narrative_lot_sqft = if_else(narrative_lot_sqft < minimum_lot_sqft, NA, narrative_lot_sqft),
    narrative_height_ft = floor(as.numeric(str_match(text, regex(paste0("building height\\s*:?\\s*",
      "(\\d+(?:\\.\\d+)?)\\s*", feet_unit), ignore_case = TRUE))[, 2])),
    narrative_parking = map_dbl(text, largest_count, before = "parking\\s*:?\\s*",
      after = "\\s*(?:parking\\s+)?spaces?"),
    narrative_units = map_dbl(text, largest_count, before = "(?:number of )?(?:dwelling )?units\\s*:?\\s*")) |>
  select(-text)

# 3. One row per record: the form's value, else (for a Type 1 application) the narrative's, with its source.
type_1 <- pages |>
  summarise(type_1 = any(str_detect(text, regex("\\btype\\s*(?:1|i|one)\\b", ignore_case = TRUE))),
    .by = c(matter_id, record_number))
fields <- forms |>
  bind_cols(bind_rows(lapply(forms$use_text, use_fields))) |>
  mutate(lot_sqft = lot_square_feet(lot_text), lot_unreadable = coalesce(lot_sqft < minimum_lot_sqft, FALSE),
    lot_sqft = if_else(lot_unreadable, NA, lot_sqft)) |>
  full_join(narrative_fields, by = c("matter_id", "record_number"), relationship = "one-to-one") |>
  left_join(type_1, by = c("matter_id", "record_number"), relationship = "one-to-one") |>
  mutate(
    across(starts_with("narrative_"), \(x) if_else(type_1, x, NA_real_)),
    lot_source = case_when(!is.na(lot_sqft) ~ "form", !is.na(narrative_lot_sqft) ~ "narrative",
      lot_unreadable ~ "form_unreadable"),
    lot_sqft = coalesce(lot_sqft, narrative_lot_sqft),
    height_source = case_when(!is.na(height_ft) ~ "form", !is.na(narrative_height_ft) ~ "narrative"),
    height_ft = coalesce(height_ft, narrative_height_ft),
    parking_source = case_when(!is.na(parking_spaces) ~ "form", !is.na(narrative_parking) ~ "narrative"),
    parking_spaces = coalesce(parking_spaces, narrative_parking),
    units_source = case_when(!is.na(dwelling_units) ~ "form", !is.na(narrative_units) ~ "narrative"),
    dwelling_units = coalesce(dwelling_units, narrative_units)) |>
  select(matter_id, record_number, type_1, lot_sqft, lot_source, dwelling_units, units_source, parking_spaces,
    parking_source, commercial_sqft, height_ft, height_source, stories, lot_text, use_text)
stopifnot(!anyDuplicated(fields$record_number))

# 4. Hand checks: 60 records' forms read from the page images (adjudication/application_form_reads.csv). On the records
# whose reading is unambiguous, every value read from the form must be the hand read's (a lot size within half a
# percent, for dimensions given in feet and inches), except where the OCR text itself misreads the printed value.
reads <- read_csv("../adjudication/application_form_reads.csv", col_types = cols(.default = col_character())) |>
  filter(clear == "TRUE")
disagrees <- function(parsed, read, tolerance = 0) {
  read <- as.numeric(read)
  !is.na(parsed) & (is.na(read) | abs(parsed - read) > tolerance * coalesce(read, 0))
}
checked <- fields |>
  inner_join(reads, by = "record_number", suffix = c("", "_read"), relationship = "one-to-one") |>
  mutate(mismatch = disagrees(if_else(lot_source %in% "form" & !ocr_misread %in% "lot_sqft", lot_sqft, NA),
      lot_sqft_read, 0.005) |
    disagrees(if_else(units_source %in% "form", dwelling_units, NA), dwelling_units_read) |
    disagrees(if_else(parking_source %in% "form", parking_spaces, NA), parking_spaces_read) |
    disagrees(commercial_sqft, commercial_sqft_read) |
    disagrees(if_else(height_source %in% "form", height_ft, NA), height_ft_read) |
    disagrees(stories, stories_read))
stopifnot(nrow(checked) == nrow(reads))
if (any(checked$mismatch)) print(select(filter(checked, mismatch), record_number, lot_sqft:stories, ends_with("_read")))
stopifnot(!any(checked$mismatch))

SaveData(fields, "record_number", "../output/application_form_fields.csv")
