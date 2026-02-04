#!/usr/bin/env Rscript

# Pilot extractor for rezoning ordinance PDFs:
# - downloads one candidate PDF per bill
# - extracts text (pdftools first, pdftotext fallback)
# - pulls candidate addresses and sponsor names from text

source("../../setup_environment/code/packages.R")

option_list <- list(
  optparse::make_option("--max-bills", type = "integer", default = 300, dest = "max_bills"),
  optparse::make_option("--sleep-seconds", type = "double", default = 0.05, dest = "sleep_seconds"),
  optparse::make_option("--refresh-downloads", action = "store_true", default = FALSE, dest = "refresh_downloads"),
  optparse::make_option("--ocr-pages", type = "integer", default = 3, dest = "ocr_pages")
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

safe_str <- function(x) dplyr::coalesce(as.character(x), "")

safe_filename <- function(x) {
  x %>%
    safe_str() %>%
    stringr::str_replace_all("[^A-Za-z0-9_-]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace("^_|_$", "")
}

extract_pdf_text <- function(pdf_path) {
  if (!file.exists(pdf_path)) {
    return(list(text = NA_character_, parser = NA_character_, status = "missing_file"))
  }

  if (requireNamespace("pdftools", quietly = TRUE)) {
    txt <- tryCatch(
      paste(pdftools::pdf_text(pdf_path), collapse = "\n"),
      error = function(e) NA_character_
    )
    if (!is.na(txt) && nchar(stringr::str_squish(txt)) > 0) {
      return(list(text = txt, parser = "pdftools", status = "ok"))
    }
  }

  has_tesseract <- nzchar(Sys.which("tesseract")) || file.exists("/opt/homebrew/bin/tesseract")
  if (has_tesseract && requireNamespace("pdftools", quietly = TRUE)) {
    tesseract_bin <- if (nzchar(Sys.which("tesseract"))) Sys.which("tesseract") else "/opt/homebrew/bin/tesseract"
    n_pages <- tryCatch(pdftools::pdf_info(pdf_path)$pages, error = function(e) 0)
    n_ocr <- max(0, min(n_pages, opt$ocr_pages))
    if (n_ocr > 0) {
      page_idx <- seq_len(n_ocr)
      png_files <- file.path(
        tempdir(),
        sprintf("ocr_%s_p%%03d.%%s", safe_filename(tools::file_path_sans_ext(basename(pdf_path))))
      )
      rendered <- tryCatch(
        pdftools::pdf_convert(
          pdf = pdf_path,
          format = "png",
          pages = page_idx,
          filenames = png_files,
          dpi = 300,
          verbose = FALSE
        ),
        error = function(e) character(0)
      )
      if (length(rendered) > 0) {
        ocr_txt <- vapply(rendered, function(img) {
          out <- tryCatch(
            system2(tesseract_bin, c(img, "stdout"), stdout = TRUE, stderr = FALSE),
            error = function(e) character(0)
          )
          paste(out, collapse = "\n")
        }, character(1))
        txt <- paste(ocr_txt, collapse = "\n")
        unlink(rendered, force = TRUE)
        if (!is.na(txt) && nchar(stringr::str_squish(txt)) > 0) {
          return(list(text = txt, parser = "tesseract_ocr", status = "ok"))
        }
      }
    }
  }

  if (nzchar(Sys.which("pdftotext"))) {
    txt <- tryCatch(
      paste(system2("pdftotext", c("-layout", pdf_path, "-"), stdout = TRUE, stderr = FALSE), collapse = "\n"),
      error = function(e) NA_character_
    )
    if (!is.na(txt) && nchar(stringr::str_squish(txt)) > 0) {
      return(list(text = txt, parser = "pdftotext", status = "ok"))
    }
  }

  list(text = NA_character_, parser = NA_character_, status = "parse_failed")
}

extract_address_candidates <- function(text) {
  if (is.na(text) || !nzchar(text)) return(character(0))
  text_norm <- stringr::str_squish(stringr::str_replace_all(text, "[\r\n\t]+", " "))

  pattern <- stringr::regex(
    "\\b\\d{1,5}(?:\\s*-\\s*\\d{1,5})?\\s+(?:[NSEW]\\.?\\s+)?[A-Za-z0-9'.-]+(?:\\s+[A-Za-z0-9'.-]+){0,6}\\s(?:AVE(?:NUE)?|ST(?:REET)?|RD|ROAD|BLVD|BOULEVARD|DR(?:IVE)?|LN|LANE|CT|COURT|PL|PLACE|PKWY|PARKWAY|TER|TERRACE|WAY)\\b",
    ignore_case = TRUE
  )

  context_pattern <- paste0(
    "(?:premises(?:\\s+known\\s+as)?|property(?:\\s+known\\s+as)?|located\\s+at|address(?:ed)?\\s+as|common\\s+address\\s+of)\\s+(",
    "\\d{1,5}(?:\\s*-\\s*\\d{1,5})?\\s+(?:[NSEW]\\.?\\s+)?",
    "[A-Za-z0-9'.-]+(?:\\s+[A-Za-z0-9'.-]+){0,6}\\s",
    "(?:AVE(?:NUE)?|ST(?:REET)?|RD|ROAD|BLVD|BOULEVARD|DR(?:IVE)?|LN|LANE|CT|COURT|PL|PLACE|PKWY|PARKWAY|TER|TERRACE|WAY)\\b",
    ")"
  )
  context_hits <- tryCatch(
    stringr::str_match_all(text_norm, stringr::regex(context_pattern, ignore_case = TRUE))[[1]],
    error = function(e) matrix(character(0), nrow = 0, ncol = 2)
  )
  context_addresses <- if (nrow(context_hits) > 0) context_hits[, 2] else character(0)

  generic_addresses <- stringr::str_extract_all(text_norm, pattern)[[1]]

  bad_patterns <- c(
    "121\\s+N(?:ORTH)?\\s+LASALLE",
    "421\\s+N(?:ORTH)?\\s+LASALLE",
    "CITY\\s+HALL",
    "ROOM\\s+3-22"
  )

  all_addresses <- c(context_addresses, generic_addresses) %>%
    stringr::str_squish() %>%
    unique()

  keep <- rep(TRUE, length(all_addresses))
  if (length(all_addresses) > 0) {
    for (bp in bad_patterns) {
      keep <- keep & !stringr::str_detect(stringr::str_to_upper(all_addresses), stringr::regex(bp, ignore_case = TRUE))
    }
  }

  all_addresses[keep]
}

extract_sponsor_candidates <- function(text) {
  if (is.na(text) || !nzchar(text)) return(character(0))
  text_norm <- stringr::str_squish(stringr::str_replace_all(text, "[\r\n\t]+", " "))
  pattern <- stringr::regex(
    "(?:Alderman|Alderperson|Alderwoman|Ald\\.?)[[:space:]]+([A-Z][A-Za-z'.-]+(?:[[:space:]][A-Z][A-Za-z'.-]+){0,3})",
    ignore_case = FALSE
  )
  m <- stringr::str_match_all(text_norm, pattern)[[1]]
  if (nrow(m) == 0) return(character(0))
  unique(stringr::str_squish(m[, 2]))
}

extract_address_from_title <- function(title) {
  if (is.na(title) || !nzchar(title)) return(NA_character_)
  m <- stringr::str_match(
    title,
    stringr::regex("\\bat\\s+(.+?)\\s*-\\s*app\\b", ignore_case = TRUE)
  )
  if (nrow(m) == 0 || is.na(m[1, 2])) return(NA_character_)
  stringr::str_squish(m[1, 2])
}

panel <- readr::read_csv("../input/rezoning_ordinance_panel.csv", show_col_types = FALSE)
docs <- readr::read_csv("../input/rezoning_ordinance_documents.csv", show_col_types = FALSE)

required_panel_cols <- c("bill_id", "identifier", "primary_sponsor", "sponsor_names", "title")
for (col in required_panel_cols) if (!(col %in% names(panel))) panel[[col]] <- NA_character_

required_doc_cols <- c("bill_id", "source_url", "source_media_type", "source_table", "source_row_id", "source_note", "is_pdf")
for (col in required_doc_cols) if (!(col %in% names(docs))) docs[[col]] <- NA_character_

if (nrow(panel) == 0) stop("Input panel is empty.")
if (nrow(docs) == 0) stop("Input document-link table is empty.")

docs <- docs %>%
  mutate(
    source_url = safe_str(source_url),
    source_note = safe_str(source_note),
    source_media_type = safe_str(source_media_type),
    is_pdf = dplyr::if_else(
      !is.na(is_pdf),
      as.logical(is_pdf),
      FALSE
    ) |
      stringr::str_detect(stringr::str_to_lower(source_url), "\\.pdf(\\?|$)") |
      stringr::str_detect(stringr::str_to_lower(source_media_type), "pdf")
  ) %>%
  filter(source_url != "")

candidate_docs <- docs %>%
  left_join(
    panel %>%
      select(
        bill_id,
        identifier,
        primary_sponsor,
        sponsor_names,
        title
      ),
    by = "bill_id"
  ) %>%
  mutate(
    identifier_txt = stringr::str_to_lower(safe_str(identifier)),
    note_txt = stringr::str_to_lower(source_note),
    url_txt = stringr::str_to_lower(source_url),
    identifier_in_note = identifier_txt != "" & stringr::str_detect(note_txt, stringr::fixed(identifier_txt)),
    identifier_in_url = identifier_txt != "" & stringr::str_detect(url_txt, stringr::fixed(identifier_txt)),
    looks_like_report = stringr::str_detect(note_txt, "committee\\s*report|series\\s*of\\s*reports|zoning\\s*committee"),
    score = 0 +
      ifelse(is_pdf, 5, 0) +
      ifelse(source_table == "billversionlink", 2, 0) +
      ifelse(identifier_in_note | identifier_in_url, 3, 0) +
      ifelse(stringr::str_detect(note_txt, "ordinance|app\\s*no|map\\s*no"), 2, 0) -
      ifelse(looks_like_report, 5, 0)
  ) %>%
  arrange(bill_id, desc(score), desc(is_pdf), source_table, source_row_id) %>%
  group_by(bill_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-identifier_txt, -note_txt, -url_txt, -identifier_in_note, -identifier_in_url, -looks_like_report)

candidate_docs <- candidate_docs %>%
  filter(source_url != "")

if (nrow(candidate_docs) == 0) stop("No candidate documents after scoring/filtering.")

if (!is.null(opt$max_bills) && opt$max_bills > 0) {
  candidate_docs <- candidate_docs %>% slice_head(n = opt$max_bills)
}

if (nrow(candidate_docs) == 0) stop("No candidate documents after max-bills slicing.")

dir.create("../temp/pdfs", recursive = TRUE, showWarnings = FALSE)

results <- vector("list", nrow(candidate_docs))

for (i in seq_len(nrow(candidate_docs))) {
  row <- candidate_docs[i, ]
  bill_id <- safe_str(row$bill_id)
  identifier <- safe_str(row$identifier)
  source_url <- safe_str(row$source_url)

  local_pdf <- file.path(
    "../temp/pdfs",
    sprintf("%s_%s.pdf", safe_filename(bill_id), safe_filename(identifier %||% bill_id))
  )

  needs_download <- !file.exists(local_pdf) || isTRUE(opt$refresh_downloads)
  download_status <- "ok"
  download_error <- NA_character_

  if (needs_download) {
    dl <- tryCatch(
      {
        utils::download.file(source_url, local_pdf, mode = "wb", quiet = TRUE)
        TRUE
      },
      error = function(e) {
        download_error <<- e$message
        FALSE
      }
    )
    if (!dl) {
      download_status <- "download_failed"
    }
  }

  parsed <- if (download_status == "ok") extract_pdf_text(local_pdf) else list(
    text = NA_character_,
    parser = NA_character_,
    status = "skipped_parse_download_failed"
  )

  text_norm <- if (!is.na(parsed$text)) stringr::str_squish(stringr::str_replace_all(parsed$text, "[\r\n\t]+", " ")) else NA_character_
  address_candidates <- extract_address_candidates(parsed$text)
  sponsor_candidates <- extract_sponsor_candidates(parsed$text)
  first_address <- if (length(address_candidates) > 0) address_candidates[[1]] else NA_character_
  address_from_title <- extract_address_from_title(safe_str(row$title))
  project_address_best <- dplyr::coalesce(address_from_title, first_address)

  results[[i]] <- tibble::tibble(
    bill_id = bill_id,
    identifier = identifier,
    title = safe_str(row$title),
    source_url = source_url,
    source_table = safe_str(row$source_table),
    sponsor_from_panel = safe_str(row$primary_sponsor),
    sponsors_from_panel = safe_str(row$sponsor_names),
    text_parser = safe_str(parsed$parser),
    download_status = download_status,
    parse_status = safe_str(parsed$status),
    download_error = download_error,
    text_nchar = ifelse(is.na(parsed$text), NA_integer_, nchar(parsed$text)),
    text_sample = ifelse(is.na(text_norm), NA_character_, substr(text_norm, 1, 1200)),
    extracted_sponsors_from_text = paste(head(sponsor_candidates, 8), collapse = " | "),
    extracted_addresses = paste(head(address_candidates, 8), collapse = " | "),
    first_extracted_address = first_address,
    address_from_title = address_from_title,
    project_address_best = project_address_best
  )

  if (!is.null(opt$sleep_seconds) && opt$sleep_seconds > 0) Sys.sleep(opt$sleep_seconds)
}

extraction_sample <- bind_rows(results) %>%
  arrange(bill_id)

address_sponsor_candidates <- extraction_sample %>%
  transmute(
    bill_id,
    identifier,
    source_url,
    sponsor_from_panel,
    sponsors_from_panel,
    extracted_sponsors_from_text,
    address_from_title,
    project_address_best,
    first_extracted_address,
    extracted_addresses,
    text_nchar,
    text_parser,
    download_status,
    parse_status
  )

readr::write_csv(extraction_sample, "../output/rezoning_text_extraction_sample.csv")
readr::write_csv(address_sponsor_candidates, "../output/rezoning_address_sponsor_candidates.csv")

message("Wrote: ../output/rezoning_text_extraction_sample.csv")
message("Wrote: ../output/rezoning_address_sponsor_candidates.csv")
message("Rows processed: ", nrow(extraction_sample))
