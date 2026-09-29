# setwd("tasks/audits/zoning_record_validation/code")
# Random samples of Journal pages to read by hand, for the accuracy of the Journal parser
# (tasks/parse_journal_zoning_amendments). Pages are drawn from the fresh OCR (tasks/ocr_council_journal_zoning_pages)
# by what their text shows, not by what the parser found on them, so a page with an entry the parser missed can be
# drawn: introduction pages ("to classify as") and passage pages (a "Reclassification Of Area Shown On Map" heading at
# the start of a line; a page showing both is a passage page). The first sample, of 2008 and 2009, has pages_per_stratum
# of each kind in each year; the second, of 2000 to 2007, drawn after the parser was extended to those years, has
# pages_per_stratum_2000s, with its own seed so that the first sample is unchanged.
pages_per_stratum <- 8
sample_seed <- 20260928
pages_per_stratum_2000s <- 4
sample_seed_2000s <- 20260930

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

pages <- bind_rows(lapply(2000:2009, function(year) {
  arrow::read_parquet(sprintf("../input/journal_zoning_ocr_%d.parquet", year))
})) |>
  transmute(year = as.integer(format(meeting_date, "%Y")), meeting_date, file, page,
    introduction_page = str_detect(ocr_text, regex("classify\\s+as", ignore_case = TRUE)),
    passage_page = str_detect(ocr_text, regex("(^|\\n)\\W{0,4}Rec\\w{0,3}assifi\\s?cation\\W+of\\W+areas?\\W+shown",
      ignore_case = TRUE))) |>
  filter(introduction_page | passage_page) |>
  mutate(kind = if_else(passage_page, "passage", "introduction")) |>
  arrange(year, kind, file, page)

set.seed(sample_seed)
sample_2008 <- pages |>
  filter(year %in% 2008:2009) |>
  slice_sample(n = pages_per_stratum, by = c(year, kind))
set.seed(sample_seed_2000s)
sample_2000s <- pages |>
  filter(year %in% 2000:2007) |>
  slice_sample(n = pages_per_stratum_2000s, by = c(year, kind))
sample <- bind_rows(sample_2008, sample_2000s) |>
  select(year, kind, meeting_date, file, page) |>
  arrange(year, kind, file, page)
stopifnot(nrow(sample) == 4 * pages_per_stratum + 16 * pages_per_stratum_2000s)
SaveData(sample, c("file", "page"), "../output/journal_sample_pages.csv")
