# setwd("tasks/audits/zoning_record_validation/code")
# The blind search for what became of applications recorded as stalled (draw_stall_search.R), scored. Readers who saw
# only the page images, and not which applications the records call stalled, read each candidate page and said whether
# it prints an ordinance for the same site, a later introduction of it, or neither
# (adjudication/stall_search_reads.csv). An ordinance a reader found is identified among the parser's ordinances
# (tasks/parse_journal_zoning_amendments) by the application number the reader read, if one headed within heading_pages
# before the found page carries it, else as the ordinances headed on the found page and the last one headed before it (a
# long ordinance runs on for pages); its link (tasks/link_journal_zoning_outcomes) is compared with the records
# (tasks/follow_journal_zoning_amendments). One row per application, with the verdict of its best-matched ordinance. For
# an application recorded as stalled at the draw: now_linked (the ordinance is now linked to it, the records having been
# corrected since), unlinked_ordinance (parsed but linked to no introduction), other_introduction (linked to a later
# introduction the records do not hold as its refiling), refiling_passed (linked to its recorded refiling), not_parsed
# (no parsed ordinance found), or no_passage_found. For a control recorded as passed: passage_found (the ordinance
# linked to it), other_ordinance or not_found.
heading_pages <- 15

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

search <- read_csv("../output/stall_search_pages.csv", col_types = cols(.default = col_character()))
reads <- read_csv("../adjudication/stall_search_reads.csv", col_types = cols(.default = col_character()))
follow_up <- read_csv("../input/journal_amendment_follow_up.csv", col_types = cols(.default = col_character()))
links <- read_csv("../input/journal_ordinance_links.csv", col_types = cols(.default = col_character())) |>
  transmute(ordinance, file, page = as.integer(page), action, ordinance_application = application_number,
    linked_introduction = introduction)
applications <- search |> distinct(label, group = group_at_draw, outcome_now, introduction)
stopifnot(!anyDuplicated(applications$label), all(reads$label %in% applications$label),
  all(applications$label %in% reads$label))

# Each ordinance a reader found, matched to the parsed ordinances on its page or the page before.
found <- reads |>
  filter(finding == "ordinance") |>
  transmute(label, file, page = as.integer(page), reader_application = str_remove_all(application_number, "\\s"))
candidates <- bind_rows(lapply(seq_len(nrow(found)), function(i) {
  before <- filter(links, file == found$file[i], page <= found$page[i], page >= found$page[i] - heading_pages)
  by_number <- filter(before, ordinance_application %in% found$reader_application[i])
  matched <- if (nrow(by_number) > 0) by_number else
    bind_rows(filter(before, page == found$page[i]), slice_tail(filter(before, page < found$page[i]), n = 1))
  if (nrow(matched) == 0) return(tibble(label = found$label[i], ordinance = NA_character_))
  mutate(select(matched, ordinance, action, linked_introduction), label = found$label[i])
}))
verdicts <- applications |>
  left_join(select(follow_up, introduction, refiling), by = "introduction", relationship = "one-to-one") |>
  left_join(candidates, by = "label", relationship = "one-to-many") |>
  mutate(verdict = case_when(
    group == "passed" & linked_introduction == introduction ~ "passage_found",
    group == "passed" & !is.na(ordinance) ~ "other_ordinance",
    group == "passed" ~ "not_found",
    !label %in% found$label ~ "no_passage_found",
    is.na(ordinance) ~ "not_parsed",
    linked_introduction == introduction ~ "now_linked",
    is.na(linked_introduction) ~ "unlinked_ordinance",
    linked_introduction == refiling ~ "refiling_passed",
    TRUE ~ "other_introduction")) |>
  mutate(rank = match(verdict, c("passage_found", "now_linked", "unlinked_ordinance", "other_introduction",
    "refiling_passed", "not_parsed", "other_ordinance", "not_found", "no_passage_found"))) |>
  slice_min(rank, n = 1, with_ties = FALSE, by = label) |>
  left_join(reads |> summarise(ordinances_read = sum(finding == "ordinance"),
    introductions_read = sum(finding == "introduction"), .by = label), by = "label", relationship = "one-to-one") |>
  transmute(label, group_at_draw = group, outcome_now, introduction, verdict, ordinance, action, linked_introduction,
    refiling, ordinances_read,
    introductions_read)
SaveData(verdicts, "label", "../output/stall_search_scores.csv")
