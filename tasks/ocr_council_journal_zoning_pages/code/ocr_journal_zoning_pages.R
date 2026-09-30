# setwd("tasks/ocr_council_journal_zoning_pages/code")
# year <- "2008"
# Fresh OCR of the year's Journal pages on zoning map amendments (tasks/extract_council_journal_text): each page whose
# text layer mentions them ("to classify as", "Zoning Reclassification", "Reclassification Of Area", "Committee on
# Zoning", "changing all"), read without spaces so that a letter-spaced text layer ("to classify a s") counts, and the
# page after it, where entries run on. The text layers are the City Clerk's OCR, which misreads district codes and words
# ("Cl-2", "82-3", "RMS", "Applicafion"); each page is rendered at ocr_dpi and read again by tesseract. Tesseract finds
# the page's layout itself and now and then splits the lines of a page into two columns, breaking its ordinance
# headings ("Reclassific" on a line of its own, apart from "ation Of Area Shown On Map Number 7-N"); a page with such a
# line, or whose text layer shows more ordinance headings than its reading, is read again as a single column
# (tesseract's page segmentation mode 4), and that reading is kept if it shows more headings (ocr_layout). Requires
# pdftoppm (poppler) and tesseract.
ocr_dpi <- 300
workers <- 6
zoning_marker <- "toclassifyas|zoningreclassification|reclassificationofarea|committeeonzoning|changingall"
ordinance_heading <- "(?i)rec\\w{0,3}assifi\\s?cation\\W+of\\W+areas?\\W+shown"
split_heading <- "(?m)^\\W{0,4}Rec[a-z]{0,12}\\W*$"

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
ocr_page <- function(path, page, layout = character()) {
  prefix <- tempfile("page", tmpdir = "../temp")
  system2("pdftoppm", c("-f", page, "-l", page, "-r", ocr_dpi, "-png", "-singlefile", shQuote(path), shQuote(prefix)))
  image <- paste0(prefix, ".png")
  text <- paste(system2("tesseract", c(shQuote(image), "stdout", layout), stdout = TRUE, stderr = FALSE),
    collapse = "\n")
  file.remove(image)
  text
}
ocr_texts <- parallel::mclapply(seq_len(nrow(pages)), function(i) {
  ocr_page(file.path(sprintf("../input/journals_%s", year), pages$file[i]), pages$page[i])
}, mc.cores = workers)
stopifnot(length(ocr_texts) == nrow(pages), all(vapply(ocr_texts, is.character, logical(1))))
pages$ocr_text <- unlist(ocr_texts)
pages$ocr_layout <- "automatic"
split <- which(str_detect(pages$ocr_text, split_heading) |
  str_count(pages$text, ordinance_heading) > str_count(pages$ocr_text, ordinance_heading))
single_column <- unlist(parallel::mclapply(split, function(i) {
  ocr_page(file.path(sprintf("../input/journals_%s", year), pages$file[i]), pages$page[i], c("--psm", "4"))
}, mc.cores = workers))
better <- str_count(single_column, ordinance_heading) > str_count(pages$ocr_text[split], ordinance_heading)
pages$ocr_text[split[better]] <- single_column[better]
pages$ocr_layout[split[better]] <- "single_column"

SaveData(select(pages, meeting_date, file, page, zoning_page, ocr_layout, ocr_text), c("file", "page"),
  sprintf("../output/journal_zoning_ocr_%s.parquet", year))
