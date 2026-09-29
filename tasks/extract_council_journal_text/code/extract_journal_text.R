# setwd("tasks/extract_council_journal_text/code")
# year <- "2008"
# Text of every page of the year's City Council Journals (tasks/download_council_journals), from each PDF's text
# layer with pdftotext (poppler): once in reading order, for the proceedings, and once with the physical layout
# kept (-layout), for the two-column legislative indexes. One row per page. Requires pdftotext and pdfinfo.
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- c(year)
stopifnot(length(cli_args) == 1, grepl("^[0-9]{4}$", cli_args[1]))
year <- cli_args[1]

journals <- read_csv(sprintf("../input/council_journal_files_%s.csv", year), show_col_types = FALSE)
pdf_pages <- function(path, layout) {
  text <- paste(system2("pdftotext", c(if (layout) "-layout", shQuote(path), "-"), stdout = TRUE, stderr = FALSE),
    collapse = "\n")
  pages <- strsplit(text, "\f", fixed = TRUE)[[1]]
  declared <- as.integer(sub("^Pages:\\s+", "", grep("^Pages:", system2("pdfinfo", shQuote(path), stdout = TRUE),
    value = TRUE)))
  # pdftotext ends the last page with a form feed; trailing empty pages leave no text.
  c(pages, rep("", declared - length(pages)))
}
pages <- bind_rows(lapply(seq_len(nrow(journals)), function(i) {
  path <- file.path(sprintf("../input/journals_%s", year), basename(journals$local_file[i]))
  text <- pdf_pages(path, layout = FALSE)
  layout_text <- pdf_pages(path, layout = TRUE)
  stopifnot(length(text) == length(layout_text))
  tibble(label_date = journals$meeting_date[i], file = basename(journals$local_file[i]), page = seq_along(text),
    text, layout_text)
}))

# The meeting date is the date a Journal prints at the head of its pages ("7/28/2011 REPORTS OF COMMITTEES"), the
# most common date in the first lines of its pages, or for a short special meeting printed without running heads, the
# date on its cover ("Friday, February 1, 2008"). The City Clerk's listing labels three Journals with another date
# (7/08/2011 for July 28, 2011; 11/07/2010 for November 17, 2010; 10/21/2011 for October 12, 2011); label_date keeps
# the listing's.
head_dates <- pages |>
  mutate(printed = as.Date(str_extract(str_sub(text, 1, 250), "\\b[0-9]{1,2}/[0-9]{1,2}/(?:19|20)[0-9]{2}\\b"),
    "%m/%d/%Y")) |>
  filter(!is.na(printed)) |>
  count(file, printed) |>
  slice_max(n, n = 1, by = file, with_ties = FALSE) |>
  select(file, head_date = printed)
cover_dates <- pages |>
  filter(page == 1) |>
  transmute(file, cover_date = as.Date(str_extract(str_squish(text), paste0("(?:", paste(month.name, collapse = "|"),
    ") [0-9]{1,2}, ?(?:19|20)[0-9]{2}")), "%B %d, %Y"))
pages <- pages |>
  left_join(head_dates, by = "file", relationship = "many-to-one") |>
  left_join(cover_dates, by = "file", relationship = "many-to-one") |>
  mutate(meeting_date = coalesce(head_date, cover_date)) |>
  select(-head_date, -cover_date) |>
  relocate(meeting_date)
stopifnot(!anyNA(pages$meeting_date), all(format(pages$meeting_date, "%Y") == year))
SaveData(pages, c("file", "page"), sprintf("../output/journal_pages_%s.parquet", year))
