# setwd("tasks/audits/zoning_record_validation/code")
# A holdout sample of applications to read by hand, for the accuracy of the application-form parser
# (tasks/extract_zoning_application_forms), which was written against its own 60 hand reads: those 60 are excluded.
# Applications filed by applicants whose OCR'd form pages hold item 10 (lot size) or item 13 (proposed use),
# per_year from each year of introduction, then holdout_size of those at random. For each, the pages to read are the
# pages of its first such file that hold either item and the page after each.
per_year <- 3
holdout_size <- 40
sample_seed <- 20260929

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

applications <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  filter(!filed_by_alderman) |>
  select(record_number, introduction_date)
tuned_on <- read_csv("../input/application_form_reads.csv", show_col_types = FALSE)$record_number
files <- read_csv("../input/elms_zoning_legislation_files.csv", show_col_types = FALSE) |>
  select(url, local_file)
pages <- arrow::read_parquet("../input/application_form_pages.parquet") |>
  filter(!data_page) |>
  inner_join(applications, by = "record_number", relationship = "many-to-one") |>
  filter(!record_number %in% tuned_on) |>
  left_join(files, by = "url", relationship = "many-to-one") |>
  mutate(holds_item = str_detect(str_squish(ocr_text), regex("lot size in square|be specific", ignore_case = TRUE)))
stopifnot(!anyNA(pages$local_file))

set.seed(sample_seed)
sample <- pages |>
  filter(holds_item) |>
  distinct(record_number, introduction_date) |>
  mutate(year = as.integer(format(introduction_date, "%Y"))) |>
  arrange(record_number) |>
  slice_sample(n = per_year, by = year) |>
  slice_sample(n = holdout_size)
shown <- pages |>
  semi_join(sample, by = "record_number") |>
  arrange(record_number, file_name, page) |>
  filter(file_name == first(file_name[holds_item]), .by = record_number) |>
  filter(holds_item | lag(holds_item, default = FALSE), .by = record_number) |>
  left_join(select(sample, record_number, year), by = "record_number", relationship = "many-to-one") |>
  select(record_number, year, local_file, page)
stopifnot(n_distinct(shown$record_number) == holdout_size, !any(shown$record_number %in% tuned_on))
SaveData(shown, c("record_number", "page"), "../output/form_holdout_pages.csv")
