# setwd("tasks/audits/zoning_record_validation/code")
# A search for what became of applications the Journals' records call stalled (tasks/follow_journal_zoning_amendments),
# independent of the parser and the links. On September 29, 2026, 40 applications introduced in 2000--2008 and then
# recorded as never passed were drawn at random, with 10 recorded as passed as controls (seed 20260930), so that the
# readers who look at the pages do not know which are which and the search is shown to find a passage where there is one
# (slice_sample within each group, then labels in random order). The draw is kept, with each application's neutral label
# and its group at the draw, in adjudication/stall_search_sample.csv, so that the readers' reads stay keyed to it as the
# records improve. For each application, every later page of the Journals through 2011 is searched in its own text (the
# fresh OCR where a page has it, tasks/ocr_council_journal_zoning_pages, else the text layer,
# tasks/extract_council_journal_text) for its boundary: the distances it measures ("131.20 feet"), the streets it names
# ("South Shields Avenue", "West 29th Street"), its map sheet ("Map Number 6-F") and the applicant's name. A page is a
# candidate if it repeats two of the distances, or one and two of the streets, or the name or the map sheet and a
# street; the pages_per_application with most matches are kept (distances count most, then the name and map sheet, then
# streets). One row per application and candidate page, ranked (an application with none has one row of rank 0 without a
# page).
pages_per_application <- 4
journal_years <- 2000:2011
name_stopwords <- c("mr", "mrs", "ms", "dr", "and", "the", "of", "in", "care", "llc", "l.l.c", "inc", "corp",
  "corporation", "company", "development", "developers", "properties", "property", "partners", "group", "trust",
  "bank", "chicago", "law", "offices", "office", "samuel", "banks", "james", "gordon", "pikarski", "michas",
  "sylvia", "marino", "associates", "north", "south", "east", "west", "street", "avenue", "holdings", "investments",
  "limited", "partnership", "church", "real", "estate", "attorney", "agent")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

follow_up <- read_csv("../input/journal_amendment_follow_up.csv", col_types = cols(.default = col_character()))
boundaries <- bind_rows(lapply(2000:2008, function(year) {
  read_csv(sprintf("../input/journal_introductions_%d.csv", year), col_types = cols(.default = col_character()))
})) |>
  transmute(introduction = paste0(file, "#", position), introduction_page = as.integer(page), boundary, map_number,
    from_districts, to_districts)
sample <- read_csv("../adjudication/stall_search_sample.csv", col_types = cols(.default = col_character())) |>
  inner_join(follow_up, by = "introduction", relationship = "one-to-one") |>
  inner_join(boundaries, by = "introduction", relationship = "one-to-one") |>
  mutate(
    distances = str_extract_all(str_replace_all(boundary, "(?<=[0-9]),(?=[0-9])", ""),
      "[0-9]+(?:\\.[0-9]+)?(?=\\s*(?:feet|foot|'))"),
    streets = str_match_all(boundary, "\\b(North|South|East|West)\\s+(?:([A-Z][A-Za-z]{3,})|([0-9]{1,3})\\b)"),
    names = str_extract_all(str_to_lower(str_remove(name, "(?i),?\\s*in care of.*$")), "[a-z][a-z.'-]{3,}")) |>
  mutate(distances = lapply(distances, unique),
    streets = lapply(streets, function(m) unique(str_to_lower(if_else(is.na(m[, 3]), paste(m[, 2], m[, 4]), m[, 3])))),
    names = lapply(names, function(n) setdiff(unique(n), name_stopwords)))
stopifnot(nrow(sample) == 50)
escaped <- function(x) str_replace_all(x, "([.()-])", "\\\\\\1")
# A named street is found by its name; a numbered one with its direction and "Street" or "Place" ("west 29th street").
street_pattern <- function(street) {
  if_else(str_detect(street, " "), paste0("\\b", str_replace(street, " ", "\\\\s+"),
    "\\W{0,3}(?:st|nd|rd|th)?\\W{0,2}\\s*(?:street|place)\\b"), paste0("\\b", street, "\\b"))
}

# Page text: the fresh OCR where there is one, else the text layer.
matches <- bind_rows(lapply(journal_years, function(year) {
  ocr <- arrow::read_parquet(sprintf("../input/journal_zoning_ocr_%d.parquet", year), col_select = c("file", "page",
    "ocr_text"))
  pages <- arrow::read_parquet(sprintf("../input/journal_pages_%d.parquet", year), col_select = c("file", "page",
    "meeting_date", "text")) |>
    left_join(ocr, by = c("file", "page"), relationship = "one-to-one") |>
    transmute(file, page, meeting_date = as.Date(meeting_date),
      text = str_squish(str_to_lower(coalesce(ocr_text, text))))
  bind_rows(lapply(seq_len(nrow(sample)), function(i) {
    later <- pages$meeting_date > as.Date(sample$introduction_date[i])
    if (!any(later)) return(NULL)
    count_of <- function(patterns) {
      if (length(patterns) == 0) return(integer(sum(later)))
      Reduce(`+`, lapply(patterns, function(p) as.integer(stringi::stri_detect_regex(pages$text[later], p))))
    }
    distance <- count_of(paste0("\\b", escaped(sample$distances[[i]]), "\\b"))
    street <- count_of(street_pattern(sample$streets[[i]]))
    name <- count_of(paste0("\\b", escaped(sample$names[[i]]), "\\b")) > 0
    map <- if (is.na(sample$map_number[i])) rep(FALSE, sum(later)) else
      count_of(paste0("map\\W+(?:numbers?|nos?\\.?)\\W*", escaped(str_to_lower(sample$map_number[i])), "\\b")) > 0
    tibble(label = sample$label[i], file = pages$file[later], page = pages$page[later],
      meeting_date = pages$meeting_date[later], distances = distance, streets = street, name, map) |>
      filter(distances >= 2 | distances >= 1 & streets >= 2 | (name | map) & streets >= 1)
  }))
}))
candidates <- matches |>
  mutate(score = 3 * distances + 2 * name + 2 * map + streets) |>
  arrange(label, desc(score), meeting_date, file, page) |>
  slice_head(n = pages_per_application, by = label) |>
  mutate(candidate_rank = row_number(), .by = label)
search <- sample |>
  transmute(label, group_at_draw, outcome_now = follow_up_outcome, introduction, introduction_date, file,
    introduction_page, name,
    map_number, from_districts, to_districts, distances = map_chr(distances, paste, collapse = ";"),
    streets = map_chr(streets, paste, collapse = ";"), names = map_chr(names, paste, collapse = ";")) |>
  left_join(candidates |> transmute(label, candidate_rank, candidate_file = file, candidate_page = as.integer(page),
    candidate_date = meeting_date, matched_distances = distances, matched_streets = streets, matched_name = name,
    matched_map = map, score),
    by = "label", relationship = "one-to-many") |>
  mutate(candidate_rank = coalesce(candidate_rank, 0L)) |>
  arrange(label, candidate_rank)
SaveData(search, c("label", "candidate_rank"), "../output/stall_search_pages.csv")
