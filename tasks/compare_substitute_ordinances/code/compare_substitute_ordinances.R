# setwd("tasks/compare_substitute_ordinances/code")
# How a zoning map amendment changed between the ordinance introduced and the substitute that replaced it, one row per
# amendment and field. The fields are the district after (its highest allowed floor-area ratio, from the ordinance
# sentences read in tasks/clean_zoning_map_amendments) and the project's floor-area ratio, dwelling units and building
# height as each version's narrative or planned development's bulk table states them. The narrative and data-table
# pages come from tasks/extract_zoning_application_forms, re-read by tesseract: those of the ordinance files, where a
# substitute's file is named SO (or S0), and, from 2023, those of the application files eLMS attaches, where the
# version passed is named "Final" ("Final Narrative and Plans.pdf"). Each version's value is the first found in its
# files, in file-name order, by the first of the field's rules below that matches (a labelled table entry before a
# sentence). A pair is compared only if both versions state the field, by the same rule, from text read alike by the
# Clerk's text layer and tesseract where a page has both, and neither nine or more times the other (the mark of a
# dropped foot mark or decimal point, "674'" for 67'4"). Outside planned developments a stated floor-area ratio above
# far_above_district times the maximum of the amendment's district after is taken for a misreading and not compared;
# bonuses for transit-served and affordable housing take it up to about one and a half times. Values within
# same_within of each other are the same (restated or rounded, 4.07 and 4.08).
far_above_district <- 2
same_within <- c(district = 0, far = 0.05, units = 0, height = 1)

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

districts <- read_csv("../input/zoning-code-summary-district-types.csv", show_col_types = FALSE) |>
  transmute(district = district_type_code, floor_area_ratio = suppressWarnings(as.numeric(floor_area_ratio)))
amendments <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE)
max_far <- function(codes) {
  vapply(strsplit(coalesce(codes, ""), ";", fixed = TRUE), function(x) {
    far <- districts$floor_area_ratio[match(x, districts$district)]
    if (length(far) == 0 || all(is.na(far))) NA_real_ else max(far, na.rm = TRUE)
  }, numeric(1))
}

# Each record's pages belong to the amendment that kept it (an amendment may have several records), not to the
# record's own matter.
records <- amendments |>
  select(matter_id, record_numbers) |>
  tidyr::separate_longer_delim(record_numbers, ";") |>
  rename(record_number = record_numbers)
pages <- bind_rows(
  arrow::read_parquet("../input/application_form_pages.parquet") |>
    mutate(version = if_else(grepl("^S[O0]", file_name), "substitute", "introduced")),
  arrow::read_parquet("../input/attached_application_pages.parquet") |>
    mutate(version = if_else(grepl("final", file_name, ignore.case = TRUE), "substitute", "introduced"))) |>
  filter(data_page, !form_page) |>
  select(-matter_id) |>
  inner_join(records, by = "record_number", relationship = "many-to-one") |>
  arrange(matter_id, version, file_name, url, page)
# A file is identified by its address: a record may hold two different files of one name.
files <- pages |>
  summarise(ocr_text = str_squish(paste(ocr_text, collapse = " ")),
    embedded_text = str_squish(paste(coalesce(embedded_text, ""), collapse = " ")),
    .by = c(matter_id, version, file_name, url))

# Reading rules, in order. A number runs on into no further digit ("FAR 127 dwelling units" is not a ratio of 12;
# "2.4145" for 2.145 is not read) and is followed by nothing OCR confuses with a digit ("1]" for 11). Heights are in
# feet, with inches as twelfths ("45 FEET 6 INCHES", "102'-4"").
count <- "(\\d{1,3}(?:,\\d{3})*|\\d+)\\b(?![\\]|])"
ratio <- "(?<![\\d.])(\\d{1,2}(?:\\.\\d{1,3})?|\\.\\d{1,3})(?!\\d|\\.\\d|[\\]|]|\\s*(?:,\\d|square|sq|sf|%))"
feet <- paste0("(\\d{1,4}(?:\\.\\d+)?)\\s*(?:feet|foot|fect|ft\\b\\.?|['’])\\s*",
  "(?:[,-]?\\s*(\\d{1,2}(?:\\.\\d+)?)\\s*(?:inches|inch|in\\b\\.?|[\"”]))?")
rules <- list(
  far = c(
    label = paste0("(?:floor\\s+area\\s+ratio|\\bF\\.?\\s?A\\.?\\s?R\\b\\.?)",
      "\\s*(?:\\(\\W{0,2}FAR\\W{0,2}\\))?\\s*[:=]?\\s*", ratio),
    # The ratio printed before "FAR" only in parentheses or after an area in a table ("(1.41 FAR)", "137,969 square
    # feet 2.5 FAR"), not a bonus's ("a bonus of approximately 4.5 FAR").
    parenthesis = paste0("\\(\\s*", ratio, "\\s*FAR\\s*\\)"),
    after_area = paste0("(?:square\\s+feet|sq\\.?\\s*ft\\.?|acres\\))\\s+", ratio, "\\s*FAR\\b")),
  units = c(
    label = paste0("(?:maximum\\s+)?(?:(?:permitted|total|proposed)\\s+)?(?:number\\s+of\\s+)?(?:residential\\s+)?",
      "(?:dwelling\\s+)?units\\s*[:=]\\s*", count, "(?!\\s*(?:square|sq|sf))"),
    value_label = ":\\s*(\\d{1,3}(?:,\\d{3})*|\\d+)\\s+(?:residential\\s+)?dwelling\\s+units",
    total = paste0("total\\s+(?:of\\s+)?(?:[a-z-]+\\s+)?\\(?(\\d{1,3}(?:,\\d{3})*|\\d+)\\)?\\s+(?:new\\s+)?",
      "(?:residential\\s+)?dwelling\\s+units"),
    sentence = "\\b\\(?(\\d{1,3}(?:,\\d{3})*|\\d+)\\)?\\s+(?:new\\s+)?(?:residential\\s+)?dwelling\\s+units"),
  height = c(
    label = paste0("building\\s+height\\s*[:=]?\\s*(?:of\\s+)?(?:approximately\\s+)?", feet),
    short_label = paste0("\\bheight\\s*[:=]\\s*(?:approximately\\s+)?", feet),
    sentence = paste0("height\\s+(?:of|will\\s+be|shall\\s+be|is)\\s+(?:approximately\\s+)?", feet),
    sentence_after = paste0(feet, "\\s*(?:in\\s+height|high)\\b")))
read_field <- function(text, field) {
  for (rule in names(rules[[field]])) {
    m <- str_match(text, regex(rules[[field]][[rule]], ignore_case = TRUE))
    if (!is.na(m[1, 1])) {
      value <- as.numeric(gsub(",", "", m[1, 2]))
      if (field == "height") value <- value + coalesce(as.numeric(m[1, 3]), 0) / 12
      return(tibble(value = value, rule = rule, text = m[1, 1]))
    }
  }
  tibble(value = NA_real_, rule = NA_character_, text = NA_character_)
}
file_values <- bind_rows(lapply(seq_len(nrow(files)), function(i) {
  bind_rows(lapply(names(rules), function(field) {
    read <- read_field(files$ocr_text[i], field)
    # The Clerk's text layer, where the page has one, must give the same value.
    layer <- if (nchar(files$embedded_text[i]) > 100) read_field(files$embedded_text[i], field)$value else NA_real_
    mutate(read, matter_id = files$matter_id[i], version = files$version[i], file_name = files$file_name[i],
      url = files$url[i], field = field, layers_disagree = !is.na(value) & !is.na(layer) & value != layer)
  }))
})) |>
  filter(!is.na(value), field != "far" | (value > 0 & value <= 40))

# Each version's value: the first file stating the field.
version_values <- file_values |>
  arrange(matter_id, field, version, file_name, url) |>
  summarise(value = dplyr::first(value), rule = dplyr::first(rule), text = dplyr::first(text),
    file_name = dplyr::first(file_name), url = dplyr::first(url), layers_disagree = dplyr::first(layers_disagree),
    .by = c(matter_id, field, version)) |>
  tidyr::pivot_wider(names_from = version, values_from = c(value, rule, text, file_name, url, layers_disagree),
    names_glue = "{version}_{.value}")

# The district after as each version states it, for amendments with both versions among their ordinance files.
district_values <- amendments |>
  filter(!is.na(introduced_to_planned_development), !is.na(substitute_to_planned_development)) |>
  transmute(matter_id, field = "district",
    introduced_value = if_else(introduced_to_planned_development, NA_real_, max_far(introduced_to_districts)),
    substitute_value = if_else(substitute_to_planned_development, NA_real_, max_far(substitute_to_districts)),
    introduced_text = if_else(introduced_to_planned_development, "planned development", introduced_to_districts),
    substitute_text = if_else(substitute_to_planned_development, "planned development", substitute_to_districts))

substitute_changes <- bind_rows(district_values, version_values) |>
  inner_join(amendments |> select(matter_id, record_number, filed_by_alderman, outcome, introduction_date, direction,
    district_far = to_far), by = "matter_id", relationship = "many-to-one") |>
  mutate(
    far_above_district = field == "far" & direction != "to_planned_development" & !is.na(district_far) &
      pmax(introduced_value, substitute_value, na.rm = TRUE) > far_above_district * district_far,
    not_compared = case_when(
      is.na(introduced_text) ~ "introduced_not_stated",
      is.na(substitute_text) ~ "substitute_not_stated",
      field == "district" & introduced_text != substitute_text &
        (introduced_text == "planned development" | substitute_text == "planned development") ~
        "planned_development_in_one_version",
      field == "district" & (is.na(introduced_value) | is.na(substitute_value)) ~ "no_floor_area_ratio",
      field != "district" & introduced_rule != substitute_rule ~ "different_rules",
      field != "district" & (introduced_layers_disagree | substitute_layers_disagree) ~ "text_layers_disagree",
      far_above_district ~ "far_above_district",
      pmax(introduced_value, substitute_value) >= 9 * pmin(introduced_value, substitute_value) &
        pmin(introduced_value, substitute_value) > 0 ~ "tenfold"),
    change = case_when(
      !is.na(not_compared) ~ NA_character_,
      substitute_value < introduced_value - same_within[field] ~ "down",
      substitute_value > introduced_value + same_within[field] ~ "up",
      TRUE ~ "same")) |>
  select(matter_id, record_number, field, filed_by_alderman, outcome, introduction_date, direction, introduced_value,
    substitute_value, change, not_compared, introduced_text, substitute_text, introduced_rule, substitute_rule,
    introduced_file = introduced_file_name, substitute_file = substitute_file_name, introduced_url, substitute_url) |>
  arrange(introduction_date, record_number, field)
SaveData(substitute_changes, c("matter_id", "field"), "../output/substitute_changes.csv")
