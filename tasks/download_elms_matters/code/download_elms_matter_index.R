# setwd("tasks/download_elms_matters/code")
# Snapshot of the Chicago City Clerk's legislative records: the metadata of every matter (ordinances, orders,
# resolutions and other items) by month of introduction, from the eLMS API (City of Chicago, Office of the City Clerk,
# https://api.chicityclerkelms.chicago.gov; public, no key). The list endpoint returns metadata only; actions and
# roll calls require one request per matter (download_elms_matter_details.R). Records begin in mid-2010.
first_month <- as.Date("2010-01-01")
last_month <- as.Date("2026-09-01")
page_size <- 500L
api_url <- "https://api.chicityclerkelms.chicago.gov/matter"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

# The API allows a limited number of requests per window and answers 429 when it is used up; wait and retry.
get_page <- function(url) {
  response <- httr2::request(url) |>
    httr2::req_retry(max_tries = 30,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
      backoff = function(attempt) 60) |>
    httr2::req_perform()
  httr2::resp_body_string(response)
}
page_url <- function(month, skip) {
  filter <- sprintf("introductionDate ge %sT00:00:00Z and introductionDate lt %sT00:00:00Z", month,
    seq(month, by = "month", length.out = 2)[2])
  paste0(api_url, "?filter=", utils::URLencode(filter, reserved = TRUE), "&top=", page_size, "&skip=", skip,
    "&sort=recordNumber%20asc")
}

# Each line of the snapshot is one API response body exactly as received (responses are single-line JSON).
pages <- character()
requests <- list()
matter_ids <- list()
for (month in as.list(seq(first_month, last_month, by = "month"))) {
  skip <- 0L
  repeat {
    url <- page_url(month, skip)
    body <- get_page(url)
    parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
    stopifnot(!grepl("\n", body, fixed = TRUE), parsed$meta$skip == skip)
    pages <- c(pages, body)
    matter_ids[[length(matter_ids) + 1L]] <- tibble(month = format(month, "%Y-%m"),
      matter_id = vapply(parsed$data, function(matter) matter$matterId, character(1)))
    requests[[length(requests) + 1L]] <- tibble(month = format(month, "%Y-%m"), skip, url,
      matters_in_month = parsed$meta$count, matters_in_page = length(parsed$data),
      sha256 = as.character(openssl::sha256(charToRaw(body))))
    skip <- skip + page_size
    if (skip >= parsed$meta$count) break
  }
}
requests <- bind_rows(requests) |> mutate(retrieved_on = Sys.Date())
by_month <- summarise(requests, received = sum(matters_in_page), expected = first(matters_in_month), .by = month)
# Paging must return every matter of a month exactly once.
matter_ids <- bind_rows(matter_ids)
stopifnot(all(by_month$received == by_month$expected), !anyDuplicated(matter_ids$matter_id))

writeLines(pages, "../temp/elms_matter_index.jsonl", useBytes = TRUE)
stopifnot(file.rename("../temp/elms_matter_index.jsonl", "../output/elms_matter_index.jsonl"))
SaveData(requests, c("month", "skip"), "../output/elms_matter_index_requests.csv")
