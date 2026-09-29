# setwd("tasks/audits/zoning_record_validation/code")
# The application-form parser's fields (tasks/extract_zoning_application_forms) on the holdout applications
# (draw_form_holdout.R), against their forms as read by hand from the page images
# (adjudication/form_holdout_reads.csv; readers saw only the images and were given the parser's documented reading
# rules). One row per application and field: agrees (a lot size within half a percent, to allow for dimensions in
# feet and inches, other fields exactly), parser blank, differs, parser reads a value the form does not state, both
# blank, or not scored where the form gives a range ("17,000-18,000") rather than a number.
lot_tolerance <- 0.005

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

fields <- c("lot_sqft", "dwelling_units", "parking_spaces", "commercial_sqft", "height_ft", "stories")
reads <- read_csv("../adjudication/form_holdout_reads.csv", col_types = cols(.default = col_character()))
parsed <- read_csv("../input/application_form_fields.csv", col_types = cols(.default = col_character())) |>
  semi_join(reads, by = "record_number")
stopifnot(nrow(parsed) == nrow(reads), !anyDuplicated(reads$record_number))

sources <- parsed |>
  transmute(record_number, lot_sqft = lot_source, dwelling_units = units_source, parking_spaces = parking_source,
    commercial_sqft = if_else(is.na(commercial_sqft), NA, "form"), height_ft = height_source,
    stories = if_else(is.na(stories), NA, "form")) |>
  pivot_longer(-record_number, names_to = "field", values_to = "source")
scores <- reads |>
  select(record_number, clear, all_of(fields)) |>
  pivot_longer(all_of(fields), names_to = "field", values_to = "read") |>
  left_join(parsed |> select(record_number, all_of(fields)) |>
    pivot_longer(all_of(fields), names_to = "field", values_to = "parsed"),
    by = c("record_number", "field"), relationship = "one-to-one") |>
  left_join(sources, by = c("record_number", "field"), relationship = "one-to-one") |>
  mutate(range = !is.na(read) & is.na(suppressWarnings(as.numeric(read))), read = suppressWarnings(as.numeric(read)),
    parsed = as.numeric(parsed), tolerance = if_else(field == "lot_sqft", lot_tolerance * coalesce(read, 0), 0),
    result = case_when(range ~ "not scored",
      is.na(read) & is.na(parsed) ~ "both blank",
      is.na(read) ~ "parser reads a value the form does not state",
      is.na(parsed) ~ "parser blank",
      abs(parsed - read) <= tolerance ~ "agrees",
      TRUE ~ "differs")) |>
  select(record_number, clear, field, read, parsed, source, result)
SaveData(scores, c("record_number", "field"), "../output/form_holdout_scores.csv")
