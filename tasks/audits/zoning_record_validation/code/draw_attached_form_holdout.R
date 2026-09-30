# setwd("tasks/audits/zoning_record_validation/code")
# A holdout sample of the application forms eLMS attaches separately from 2023, read by the application-form parser
# (tasks/extract_zoning_application_forms, ocr_attached_applications.R), to read by hand as for draw_form_holdout.R:
# applications filed by applicants whose attached form pages hold item 10 (lot size) or item 13 (proposed use),
# excluding the 60 the parser was written against and the 40 of the first holdout, per_year from each year of
# introduction from 2023. For each, the pages to read are the pages of its first such file that hold either item and
# the page after each.
per_year <- 5
first_year <- 2023
sample_seed <- 20260930

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

applications <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE) |>
  filter(!filed_by_alderman) |>
  select(record_number, introduction_date)
already_read <- c(read_csv("../input/application_form_reads.csv", show_col_types = FALSE)$record_number,
  read_csv("../adjudication/form_holdout_reads.csv", show_col_types = FALSE)$record_number)
files <- read_csv("../input/elms_zoning_application_files.csv", show_col_types = FALSE) |>
  select(url, local_file)
pages <- arrow::read_parquet("../input/attached_application_pages.parquet") |>
  filter(form_page | !data_page) |>
  inner_join(applications, by = "record_number", relationship = "many-to-one") |>
  filter(!record_number %in% already_read) |>
  left_join(files, by = "url", relationship = "many-to-one") |>
  mutate(holds_item = str_detect(str_squish(ocr_text), regex("lot size in square|be specific", ignore_case = TRUE)))
stopifnot(!anyNA(pages$local_file))

set.seed(sample_seed)
sample <- pages |>
  filter(holds_item) |>
  distinct(record_number, introduction_date) |>
  mutate(year = as.integer(format(introduction_date, "%Y"))) |>
  filter(year >= first_year) |>
  arrange(record_number) |>
  slice_sample(n = per_year, by = year)
shown <- pages |>
  semi_join(sample, by = "record_number") |>
  arrange(record_number, file_name, page) |>
  filter(file_name == first(file_name[holds_item]), .by = record_number) |>
  filter(holds_item | lag(holds_item, default = FALSE), .by = record_number) |>
  left_join(select(sample, record_number, year), by = "record_number", relationship = "many-to-one") |>
  select(record_number, year, local_file, page)
stopifnot(n_distinct(shown$record_number) == nrow(sample), !any(shown$record_number %in% already_read))
SaveData(shown, c("record_number", "page"), "../output/attached_form_holdout_pages.csv")
