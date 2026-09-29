# setwd("tasks/download_council_journals/code")
# year <- "2008"
# The Journal of the Proceedings of the Chicago City Council for one year: one PDF per meeting, regular and special,
# listed on the City Clerk's Journals page. The page's year selector has its own option values (2008 is 8), read
# from the page; the listing shows 25 Journals a page, and its pages are read until one lists none. Each PDF is
# downloaded to ../temp, checked to start with the PDF signature and moved to ../output/journals_<year>, named as at
# the source with spaces replaced by underscores. The record lists each meeting's date and label, URL, file, bytes,
# SHA-256 and retrieval date. The date is the label's ("12/12/2001"), or for a Journal labeled only by its file, the
# file name's ("112801.pdf", "JournalOfTheProceedings_101283.pdf", "1985_12_11_VI_VII.pdf", "03-16-1989 Journal.pdf",
# or "21093.pdf" for February 10, 1993); the Journal's own printed date is read from its pages downstream
# (tasks/extract_council_journal_text). Downloads run one at a time.
listing_url <- "https://www.chicityclerk.com/legislation-records/journals-and-reports/journals-proceedings"
pdf_signature <- charToRaw("%PDF")
max_listing_pages <- 10

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- c(year)
stopifnot(length(cli_args) == 1, grepl("^[0-9]{4}$", cli_args[1]))
year <- cli_args[1]

get_page <- function(request) {
  request |> httr2::req_retry(max_tries = 5) |> httr2::req_timeout(120) |> httr2::req_perform() |>
    httr2::resp_body_string()
}
options <- str_match_all(get_page(httr2::request(listing_url)),
  "<option value=\"([0-9]+)\"[^>]*>\\s*([0-9]{4})\\s*</option>")[[1]]
year_value <- options[options[, 3] == year, 2]
stopifnot(length(year_value) == 1)
links <- NULL
for (page in 0:max_listing_pages) {
  listing <- get_page(httr2::request(listing_url) |> httr2::req_url_query(field_year_value = year_value, page = page))
  page_links <- str_match_all(listing, "<a href=\"(https://chicityclerk[^\"]+\\.pdf)\"[^>]*>([^<]+)</a>")[[1]]
  if (nrow(page_links) == 0) break
  links <- rbind(links, page_links)
}
stopifnot(page < max_listing_pages)

journals <- tibble(url = links[, 2], label = str_squish(links[, 3])) |>
  distinct() |>
  mutate(file_name = utils::URLdecode(basename(url)),
    meeting_date = coalesce(as.Date(str_extract(label, "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}"), "%m/%d/%Y"),
      as.Date(str_extract(file_name, "^[0-9]{4}_[0-9]{2}_[0-9]{2}"), "%Y_%m_%d"),
      as.Date(str_extract(file_name, "^[0-9]{2}-[0-9]{2}-[0-9]{4}"), "%m-%d-%Y"),
      as.Date(str_match(file_name, "(?:^|_)([0-9]{6})")[, 2], "%m%d%y"),
      as.Date(paste0("0", str_match(file_name, "^([0-9]{5})(?:SP)?\\.pdf$")[, 2]), "%m%d%y")),
    special_meeting = grepl("special", label, ignore.case = TRUE) | grepl("^[0-9]{6}_?SP", file_name),
    local_file = file.path(sprintf("../output/journals_%s", year), gsub(" ", "_", file_name))) |>
  select(-file_name) |>
  arrange(meeting_date, url)
stopifnot(nrow(journals) > 0, !anyNA(journals$meeting_date), all(format(journals$meeting_date, "%Y") == year),
  !anyDuplicated(journals$url), !anyDuplicated(journals$local_file))

for (i in seq_len(nrow(journals))) {
  received <- file.path("../temp", basename(journals$local_file[i]))
  httr2::request(utils::URLencode(journals$url[i])) |>
    httr2::req_retry(max_tries = 5) |> httr2::req_timeout(1800) |> httr2::req_perform(path = received)
  stopifnot(identical(readBin(received, "raw", length(pdf_signature)), pdf_signature))
  stopifnot(file.rename(received, journals$local_file[i]))
}

journals <- journals |>
  mutate(bytes = file.size(local_file),
    sha256 = vapply(local_file, function(f) as.character(openssl::sha256(file(f))), character(1), USE.NAMES = FALSE),
    retrieved_on = Sys.Date())
SaveData(journals, "url", sprintf("../output/council_journal_files_%s.csv", year))
