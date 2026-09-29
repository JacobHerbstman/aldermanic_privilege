# setwd("tasks/extract_zoning_application_forms/code")
# Fresh OCR of the pages that describe each zoning map amendment application's project: the application form (present
# and proposed zoning, lot size, current and proposed use, with dwelling units, parking, commercial floor area and
# height), the page after each form page (the proposed use often runs over), and any narrative or planned-development
# data table (units, floor-area ratio, height). The text layers of these PDFs are the City Clerk's own OCR, with
# errors such as "storv" for "story"; each selected page is rendered at ocr_dpi and read again by tesseract. Pages are
# found in the text layer (tasks/extract_zoning_legislation_text) of every matter's files and kept with both texts;
# files read by OCR already (the scans of mid-2023 on) are not selected. Which matters are applications is left to
# the parser.
ocr_dpi <- 300
workers <- 8
form_markers <- "PRESENT ZONING|PROPOSED ZONING|LOT SIZE|BE SPECIFIC|CURRENT USE"
table_markers <- "NARRATIVE|DATA TABLE|BULK REGULATIONS"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

files <- read_csv("../input/elms_zoning_legislation_files.csv", show_col_types = FALSE) |>
  filter(source_status == "received") |>
  select(url, local_file)
pages <- arrow::read_parquet("../input/zoning_legislation_text.parquet") |>
  filter(method == "text_layer") |>
  inner_join(files, by = "url", relationship = "one-to-one") |>
  mutate(embedded_text = strsplit(text, "\f", fixed = TRUE)) |>
  select(matter_id, record_number, file_name, url, local_file, pages, embedded_text) |>
  tidyr::unnest_longer(embedded_text, indices_to = "page") |>
  mutate(squished = toupper(str_squish(embedded_text)), form_page = grepl(form_markers, squished),
    data_page = grepl(table_markers, squished)) |>
  mutate(selected = form_page | lag(form_page, default = FALSE) | data_page, .by = url) |>
  filter(selected)
stopifnot(all(pages$page <= pages$pages))

# One page at a time: render it, read it, remove the image. Tesseract runs single-threaded in each worker.
Sys.setenv(OMP_THREAD_LIMIT = "1")
ocr_page <- function(path, page) {
  prefix <- tempfile("page", tmpdir = "../temp")
  system2("pdftoppm", c("-f", page, "-l", page, "-r", ocr_dpi, "-png", "-singlefile", shQuote(path), shQuote(prefix)))
  image <- paste0(prefix, ".png")
  text <- paste(system2("tesseract", c(shQuote(image), "stdout"), stdout = TRUE, stderr = FALSE), collapse = "\n")
  file.remove(image)
  text
}
ocr_texts <- parallel::mclapply(seq_len(nrow(pages)), function(i) {
  ocr_page(file.path("../input/zoning_legislation_files", basename(pages$local_file[i])), pages$page[i])
}, mc.cores = workers)
stopifnot(length(ocr_texts) == nrow(pages), all(vapply(ocr_texts, is.character, logical(1))))
pages$ocr_text <- unlist(ocr_texts)

SaveData(pages |> select(matter_id, record_number, file_name, url, page, form_page, data_page, embedded_text, ocr_text),
  c("url", "page"), "../output/application_form_pages.parquet")
