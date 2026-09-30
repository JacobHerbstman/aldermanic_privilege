# setwd("tasks/extract_zoning_application_forms/code")
# OCR of the application files eLMS attaches separately from 2023 (tasks/download_elms_matters): each application's
# form ("Application.pdf") and its narrative and plans ("Narrative and Plans.pdf"). They are scans without a text
# layer, so pages cannot be selected from embedded text as in ocr_application_forms.R: every page of a form (a
# "Miscellaneous" attachment, or an exhibit named as an application, application_name, "Exhibit 2 - Application.pdf",
# "22203 App.pdf"), and the first narrative_pages pages of any other exhibit (a narrative, whose plans follow), are
# rendered at ocr_dpi and read by tesseract, and the pages kept are selected from that text by the same markers as
# ocr_application_forms.R: a form page, the page after it, and any narrative or data-table page. Output: the selected
# pages with the same columns as application_form_pages.parquet (embedded_text is empty), one row per file and page.
ocr_dpi <- 300
workers <- 6
narrative_pages <- 3
application_name <- "(?i)ap+li?ca|\\bapp\\b"
form_markers <- "PRESENT ZONING|PROPOSED ZONING|LOT SIZE|BE SPECIFIC|CURRENT USE"
table_markers <- "NARRATIVE|DATA TABLE|BULK REGULATIONS"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

files <- read_csv("../input/elms_zoning_application_files.csv", show_col_types = FALSE) |>
  filter(source_status == "received") |>
  mutate(path = file.path("../input/zoning_application_files", basename(local_file)),
    pages = vapply(path, function(p) {
      info <- system2("pdfinfo", shQuote(p), stdout = TRUE, stderr = FALSE)
      as.integer(sub("^Pages:\\s+", "", grep("^Pages:", info, value = TRUE)))
    }, integer(1), USE.NAMES = FALSE))
stopifnot(nrow(files) > 0, !anyNA(files$pages), all(file.exists(files$path)))
pages <- files |>
  mutate(form_file = attachment_type == "Miscellaneous" | str_detect(file_name, application_name),
    page = lapply(if_else(form_file, pages, pmin(pages, narrative_pages)), seq_len)) |>
  tidyr::unnest_longer(page)

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
ocr_texts <- parallel::mclapply(seq_len(nrow(pages)), function(i) ocr_page(pages$path[i], pages$page[i]),
  mc.cores = workers)
stopifnot(length(ocr_texts) == nrow(pages), all(vapply(ocr_texts, is.character, logical(1))))
pages$ocr_text <- unlist(ocr_texts)

selected <- pages |>
  arrange(url, page) |>
  mutate(squished = toupper(str_squish(ocr_text)), form_page = grepl(form_markers, squished),
    data_page = grepl(table_markers, squished)) |>
  mutate(selected = form_page | lag(form_page, default = FALSE) | data_page, .by = url) |>
  filter(selected) |>
  transmute(matter_id, record_number, file_name, url, page, form_page, data_page, embedded_text = "", ocr_text)
SaveData(selected, c("url", "page"), "../output/attached_application_pages.parquet")
