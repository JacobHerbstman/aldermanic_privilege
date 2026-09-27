# setwd("tasks/download_elms_matters/code")
# Full eLMS records (actions, votes, sponsors, attachments list) for every zoning map amendment in the matter index:
# matters titled "Zoning Reclassification" or filed under the ZONING RECLASSIFICATIONS category. One request per
# matter to https://api.chicityclerkelms.chicago.gov/matter/{matterId}; each line of the output is one response body
# exactly as received. Records received so far are kept in ../temp, so an interrupted download resumes where it
# stopped; the output is written only when every selected matter has been received.
api_url <- "https://api.chicityclerkelms.chicago.gov/matter/"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

index <- unlist(lapply(readLines("../output/elms_matter_index.jsonl"), function(page) {
  jsonlite::fromJSON(page, simplifyVector = FALSE)$data
}), recursive = FALSE)
zoning <- Filter(function(matter) {
  grepl("zoning reclassification", matter$title %||% "", ignore.case = TRUE) ||
    startsWith(matter$matterCategory %||% "", "ZONING RECLASSIFICATIONS")
}, index)
selected <- tibble(matter_id = vapply(zoning, function(m) m$matterId, character(1)),
  record_number = vapply(zoning, function(m) m$recordNumber %||% NA_character_, character(1)))
stopifnot(nrow(selected) > 0, !anyDuplicated(selected$matter_id))

partial <- "../temp/elms_zoning_matter_details.partial.jsonl"
received <- if (file.exists(partial)) readLines(partial) else character()
received_ids <- vapply(received, function(body) jsonlite::fromJSON(body, simplifyVector = FALSE)$matterId, character(1))
for (matter_id in setdiff(selected$matter_id, received_ids)) {
  body <- httr2::request(paste0(api_url, matter_id)) |>
    httr2::req_retry(max_tries = 60,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
      backoff = function(attempt) 60) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  stopifnot(!grepl("\n", body, fixed = TRUE), jsonlite::fromJSON(body, simplifyVector = FALSE)$matterId == matter_id)
  cat(body, "\n", file = partial, sep = "", append = TRUE)
}

details <- readLines(partial)
detail_ids <- vapply(details, function(body) jsonlite::fromJSON(body, simplifyVector = FALSE)$matterId, character(1),
  USE.NAMES = FALSE)
stopifnot(setequal(detail_ids, selected$matter_id), !anyDuplicated(detail_ids))
details <- details[match(selected$matter_id, detail_ids)]
writeLines(details, "../temp/elms_zoning_matter_details.jsonl", useBytes = TRUE)
stopifnot(file.rename("../temp/elms_zoning_matter_details.jsonl", "../output/elms_zoning_matter_details.jsonl"))
SaveData(selected |> mutate(sha256 = vapply(details, function(body) as.character(openssl::sha256(charToRaw(body))),
  character(1), USE.NAMES = FALSE), retrieved_on = Sys.Date()), "matter_id", "../output/elms_zoning_matter_requests.csv")
