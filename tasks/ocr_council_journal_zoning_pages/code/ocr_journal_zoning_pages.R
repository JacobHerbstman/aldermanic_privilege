# setwd("tasks/ocr_council_journal_zoning_pages/code")
# year <- "2008"
# Fresh OCR of the year's Journal pages on zoning map amendments (tasks/extract_council_journal_text): each page whose
# text layer mentions them ("to classify as", "Zoning Reclassification", "Reclassification Of Area", "Committee on
# Zoning", "changing all"), read without spaces so that a letter-spaced text layer ("to classify a s") counts, and the
# page after it, where entries run on. The text layers are the City Clerk's OCR, which misreads district codes and words
# ("Cl-2", "82-3", "RMS", "Applicafion"); each page is rendered at ocr_dpi and read again by tesseract. Requires
# pdftoppm (poppler) and tesseract.
ocr_dpi <- 300
workers <- 6
zoning_marker <- "toclassifyas|zoningreclassification|reclassificationofarea|committeeonzoning|changingall"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- c(year)
stopifnot(length(cli_args) == 1, grepl("^[0-9]{4}$", cli_args[1]))
year <- cli_args[1]

pages <- arrow::read_parquet(sprintf("../input/journal_pages_%s.parquet", year)) |>
  select(meeting_date, file, page, text) |>
  arrange(file, page) |>
  mutate(zoning_page = str_detect(str_remove_all(text, "\\s"), regex(zoning_marker, ignore_case = TRUE)),
    selected = zoning_page | lag(zoning_page, default = FALSE), .by = file) |>
  filter(selected)

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
  ocr_page(file.path(sprintf("../input/journals_%s", year), pages$file[i]), pages$page[i])
}, mc.cores = workers)
stopifnot(length(ocr_texts) == nrow(pages), all(vapply(ocr_texts, is.character, logical(1))))
pages$ocr_text <- unlist(ocr_texts)

SaveData(select(pages, meeting_date, file, page, zoning_page, ocr_text), c("file", "page"),
  sprintf("../output/journal_zoning_ocr_%s.parquet", year))
