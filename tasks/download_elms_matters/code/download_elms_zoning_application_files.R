# setwd("tasks/download_elms_matters/code")
# The application files attached to zoning map amendments apart from their legislation: from 2023 eLMS attaches each
# application's form as a separate file (attachment type "Miscellaneous", "Application.pdf") and its narrative and
# plans as another ("Exhibits", "Narrative and Plans.pdf"); before 2023 both were part of the legislation file
# (download_elms_zoning_legislation_files.R). All are PDFs, public on the City Clerk's cloud storage at the path given
# in each matter's attachment list. As for the legislation files, each is saved under the name of its storage object
# and moved into ../output/zoning_application_files only after it downloads completely and starts with the PDF
# signature, so files already there are complete and an interrupted download resumes with the rest; storage objects
# empty (zero bytes) or absent (404) at the source are recorded as such and no file is kept, and any other failed
# request stops the download.
attachment_types <- c("Miscellaneous", "Exhibits")
pdf_signature <- charToRaw("%PDF")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

matters <- lapply(readLines("../output/elms_zoning_matter_details.jsonl"), jsonlite::fromJSON, simplifyVector = FALSE)
attachments <- bind_rows(lapply(matters, function(matter) {
  bind_rows(lapply(matter$attachments, function(attachment) {
    tibble(matter_id = matter$matterId, record_number = matter$recordNumber, file_name = attachment$fileName,
      attachment_type = attachment$attachmentType, url = attachment$path)
  }))
})) |>
  filter(attachment_type %in% attachment_types) |>
  # Files whose storage name occurs in more than one container are saved under the container name and the file name.
  mutate(shared_name = basename(url) %in% basename(url)[duplicated(basename(url))],
    local_file = file.path("../output/zoning_application_files",
      if_else(shared_name, paste0(basename(dirname(url)), "_", basename(url)), basename(url)))) |>
  select(-shared_name)
stopifnot(nrow(attachments) > 0, !anyDuplicated(attachments$url), !anyDuplicated(attachments$local_file),
  all(tolower(tools::file_ext(attachments$url)) == "pdf"))

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
    stopifnot(identical(readBin(received[j], "raw", length(pdf_signature)), pdf_signature))
    stopifnot(file.rename(received[j], attachments$local_file[i]))
  }
}

attachments <- attachments |>
  mutate(source_status = case_when(url %in% empty_at_source ~ "empty_at_source",
      url %in% missing_at_source ~ "missing_at_source", TRUE ~ "received"),
    local_file = if_else(source_status == "received", local_file, NA_character_),
    bytes = if_else(source_status == "received", file.size(local_file), 0),
    sha256 = vapply(local_file, function(f) if (is.na(f)) NA_character_ else as.character(openssl::sha256(file(f))),
      character(1), USE.NAMES = FALSE))
stopifnot(all(attachments$source_status != "received" | file.exists(attachments$local_file)))
SaveData(attachments, "url", "../output/elms_zoning_application_files.csv")
