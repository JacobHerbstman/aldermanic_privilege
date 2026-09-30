# setwd("tasks/audits/zoning_record_validation/code")
# A sample of amendments whose introduced and substitute ordinances are compared (tasks/compare_substitute_ordinances),
# to read by hand. On September 30, 2026, among applications that passed with the project's floor-area ratio, dwelling
# units or height compared, 20 where the substitute changed any of them and 10 of the others were drawn at random (seed
# 20260930, slice_sample within each group in record-number order). The draw is kept, with each amendment's group at
# the draw, in adjudication/substitute_holdout_sample.csv, so that the reads stay keyed to it as the comparison
# improves. For each amendment and version, the pages to read are those of the files the values are read from that
# hold the text read; where the text runs across pages, every narrative and data-table page of the file.

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

changes <- read_csv("../input/substitute_changes.csv", show_col_types = FALSE) |>
  filter(outcome == "passed", !filed_by_alderman, field != "district")
local_files <- bind_rows(
  read_csv("../input/elms_zoning_legislation_files.csv", show_col_types = FALSE) |> select(url, local_file),
  read_csv("../input/elms_zoning_application_files.csv", show_col_types = FALSE) |> select(url, local_file))
stopifnot(!anyDuplicated(local_files$url))
pages <- bind_rows(arrow::read_parquet("../input/application_form_pages.parquet"),
  arrow::read_parquet("../input/attached_application_pages.parquet")) |>
  filter(data_page, !form_page) |>
  select(url, page, ocr_text) |>
  left_join(local_files, by = "url", relationship = "many-to-one")
stopifnot(!anyNA(pages$local_file))

sample <- read_csv("../adjudication/substitute_holdout_sample.csv", show_col_types = FALSE)

read_from <- changes |>
  semi_join(sample, by = "record_number") |>
  select(record_number, introduced_url, substitute_url, introduced_text, substitute_text) |>
  tidyr::pivot_longer(-record_number, names_to = c("version", ".value"), names_sep = "_") |>
  filter(!is.na(url)) |>
  summarise(texts = list(unique(text)), .by = c(record_number, version, url))
stopifnot(!anyDuplicated(read_from$url))
shown <- read_from |>
  inner_join(pages, by = "url", relationship = "one-to-many") |>
  mutate(holds_text = mapply(function(page_text, texts) any(str_detect(str_squish(page_text), fixed(texts))),
    ocr_text, texts)) |>
  filter(holds_text | !any(holds_text), .by = c(record_number, version, url)) |>
  distinct(record_number, version, local_file, page) |>
  arrange(record_number, version, local_file, page)
stopifnot(all(sample$record_number %in% shown$record_number))
SaveData(shown, c("record_number", "version", "local_file", "page"), "../output/substitute_holdout_pages.csv")
