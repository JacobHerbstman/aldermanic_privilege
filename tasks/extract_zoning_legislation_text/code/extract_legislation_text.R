# setwd("tasks/extract_zoning_legislation_text/code")
# Text of every zoning map amendment's legislation file (tasks/download_elms_matters). PDFs are read from their text
# layer with pdftotext (poppler); a PDF whose text layer averages fewer than min_characters_per_page characters per
# page is a scan and is read by rendering each page at ocr_dpi and running tesseract on it. The few Word documents are
# read from their XML, the RTF files by removing control words, and the one image by OCR. Pages are separated by form
# feeds. Requires poppler (pdftotext, pdftoppm, pdfinfo) and tesseract on the PATH.
min_characters_per_page <- 100
ocr_dpi <- 300

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

# Files empty at the source (recorded by the download) have no text.
files <- read_csv("../input/elms_zoning_legislation_files.csv", show_col_types = FALSE) |>
  filter(source_status == "received")
stopifnot(!anyDuplicated(files$url), all(file.exists(file.path("../input/zoning_legislation_files", basename(files$local_file)))))

ocr_image <- function(image) {
  paste(system2("tesseract", c(shQuote(image), "stdout"), stdout = TRUE, stderr = FALSE), collapse = "\n")
}
read_pdf <- function(path) {
  text <- paste(system2("pdftotext", c(shQuote(path), "-"), stdout = TRUE, stderr = FALSE), collapse = "\n")
  info <- system2("pdfinfo", shQuote(path), stdout = TRUE, stderr = FALSE)
  pages <- as.integer(sub("^Pages:\\s+", "", grep("^Pages:", info, value = TRUE)))
  if (nchar(gsub("\\s", "", text)) >= min_characters_per_page * pages) {
    return(tibble(method = "text_layer", pages, text))
  }
  prefix <- file.path("../temp", tools::file_path_sans_ext(basename(path)))
  system2("pdftoppm", c("-r", ocr_dpi, "-png", shQuote(path), shQuote(prefix)))
  images <- sort(Sys.glob(paste0(prefix, "-*.png")))
  stopifnot(length(images) == pages)
  text <- paste(vapply(images, ocr_image, character(1)), collapse = "\f")
  file.remove(images)
  tibble(method = "ocr", pages, text)
}
read_docx <- function(path) {
  document <- xml2::read_xml(unz(path, "word/document.xml"))
  paragraphs <- xml2::xml_find_all(document, ".//w:p", xml2::xml_ns(document))
  tibble(method = "docx", pages = NA_integer_, text = paste(xml2::xml_text(paragraphs), collapse = "\n"))
}
read_rtf <- function(path) {
  rtf <- paste(readLines(path, warn = FALSE), collapse = "\n")
  text <- gsub("\\\\par[d]?", "\n", rtf)
  text <- gsub("\\\\'[0-9a-f]{2}|\\\\[a-z]+-?[0-9]* ?|[{}]", "", text)
  tibble(method = "rtf", pages = NA_integer_, text)
}

texts <- bind_rows(lapply(seq_len(nrow(files)), function(i) {
  path <- file.path("../input/zoning_legislation_files", basename(files$local_file[i]))
  extracted <- switch(tolower(tools::file_ext(path)),
    pdf = read_pdf(path), docx = read_docx(path), rtf = read_rtf(path),
    png = tibble(method = "ocr", pages = 1L, text = ocr_image(path)))
  bind_cols(select(files[i, ], matter_id, record_number, file_name, url), extracted)
})) |>
  mutate(characters = nchar(text))
stopifnot(nrow(texts) == nrow(files), !anyNA(texts$method))

SaveData(texts, "url", "../output/zoning_legislation_text.parquet")
