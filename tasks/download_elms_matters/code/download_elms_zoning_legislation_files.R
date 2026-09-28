# setwd("tasks/download_elms_matters/code")
# The legislation files attached to every zoning map amendment (the introduced ordinance and, where one exists, the
# substitute that passed), which state the zoning districts before and after the change: PDFs, apart from two RTF
# files, two Word documents and one image. Files are public on the City Clerk's cloud storage, at the path given in
# each matter's attachment list. Each file is saved under the name of its storage object and moved into
# ../output/zoning_legislation_files only after it downloads completely and starts with its format's signature, so
# files already there are complete and an interrupted download resumes with the rest. A few listed storage objects
# are empty (the server reports zero bytes) or absent (404) at the source; they are recorded as such and no file is
# kept. Any other failed request stops the download.
file_signatures <- list(pdf = charToRaw("%PDF"), rtf = charToRaw("{\\rtf"), docx = charToRaw("PK"),
  png = as.raw(c(0x89, 0x50, 0x4e, 0x47)))
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

matters <- lapply(readLines("../output/elms_zoning_matter_details.jsonl"), jsonlite::fromJSON, simplifyVector = FALSE)
attachments <- bind_rows(lapply(matters, function(matter) {
  bind_rows(lapply(matter$attachments, function(attachment) {
    tibble(matter_id = matter$matterId, record_number = matter$recordNumber, file_name = attachment$fileName,
      attachment_type = attachment$attachmentType, url = attachment$path)
  }))
})) |>
  filter(attachment_type == "Legislation") |>
  # A few matters re-filed in 2023 list the same storage object in both the legacy and the current container; files
  # whose name occurs in more than one container are saved under the container name and the file name.
  mutate(format = tolower(tools::file_ext(url)),
    shared_name = basename(url) %in% basename(url)[duplicated(basename(url))],
    local_file = file.path("../output/zoning_legislation_files",
      if_else(shared_name, paste0(basename(dirname(url)), "_", basename(url)), basename(url)))) |>
  select(-shared_name)
stopifnot(nrow(attachments) > 0, !anyDuplicated(attachments$url), all(attachments$format %in% names(file_signatures)))

# Files download in batches of 50, eight at a time.
empty_at_source <- character()
missing_at_source <- character()
pending <- which(!file.exists(attachments$local_file))
for (batch in split(pending, ceiling(seq_along(pending) / 50))) {
  received <- file.path("../temp", basename(attachments$local_file[batch]))
  requests <- lapply(attachments$url[batch], function(url) {
    httr2::request(url) |>
      httr2::req_retry(max_tries = 10,
        is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
        backoff = function(attempt) 30)
  })
  responses <- httr2::req_perform_parallel(requests, paths = received, max_active = 8, on_error = "continue",
    progress = FALSE)
  for (j in seq_along(batch)) {
    i <- batch[j]
    if (inherits(responses[[j]], "httr2_http_404")) {
      missing_at_source <- c(missing_at_source, attachments$url[i])
      next
    }
    if (inherits(responses[[j]], "error")) stop(responses[[j]])
    if (httr2::resp_header(responses[[j]], "Content-Length") == "0") {
      empty_at_source <- c(empty_at_source, attachments$url[i])
      file.remove(received[j])
      next
    }
    signature <- file_signatures[[attachments$format[i]]]
    stopifnot(identical(readBin(received[j], "raw", length(signature)), signature))
    stopifnot(file.rename(received[j], attachments$local_file[i]))
  }
}

attachments <- attachments |>
  mutate(source_status = case_when(url %in% empty_at_source ~ "empty_at_source",
      url %in% missing_at_source ~ "missing_at_source", TRUE ~ "received"),
    local_file = if_else(source_status == "received", local_file, NA_character_),
    bytes = if_else(source_status == "received", file.size(local_file), 0),
    sha256 = vapply(local_file, function(f) if (is.na(f)) NA_character_ else as.character(openssl::sha256(file(f))),
      character(1), USE.NAMES = FALSE),
    retrieved_on = Sys.Date())
stopifnot(all(attachments$source_status != "received" | file.exists(attachments$local_file)))
SaveData(select(attachments, -attachment_type), "url", "../output/elms_zoning_legislation_files.csv")
