# setwd("tasks/audits/zoning_record_validation/code")
# The Journal parser's introductions and ordinances (tasks/parse_journal_zoning_amendments) on the randomly drawn pages
# of 2000 to 2009 (draw_journal_sample.R), against every entry that begins on those pages as read by hand from the page
# images (adjudication/journal_sample_introduction_reads.csv and journal_sample_passage_reads.csv; readers saw only
# the images). A read entry is matched to a parsed entry on the same page by its record or application number, else by
# the filer's name and map, else by its order on the page; every match rule is recorded. An alderman is compared by
# name without the word "Alderman". Districts are compared as sets of the zoning ordinances' codes, the parser's
# coding: a code of the 1957 ordinance is marked "1957:" (readers marked a code by the district name printed with it,
# or by the date where none is printed), a code neither ordinance has (the Journal's "M-1") is expected to be left
# uncoded, and any planned development is PD. Where a reader lists several maps, the first is compared, as the parser
# keeps the first. One row per read entry and per parsed entry that no read matches, with the result for each field:
# agrees, parser blank, differs, or not read (the reader could not see the value).
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

district_table <- read_csv("../input/zoning-code-summary-district-types.csv", show_col_types = FALSE)$district_type_code
district_table_1957 <- read_csv("../input/zoning_conversion_2004_crosswalk.csv", show_col_types = FALSE)$old_code |>
  str_subset("^(?:R[1-8]|[BCM][1-7]-[1-7]|C4)$")
districts <- function(x) {
  vapply(strsplit(toupper(coalesce(x, "")), ";"), function(codes) {
    codes <- str_squish(codes)
    codes <- case_when(str_detect(codes, "PLANNED MANUFACTURING|^PMD\\b") ~ "PMD",
      str_detect(codes, "PLANNED DEVELOPMENT|^M?PD\\b") ~ "PD", TRUE ~ codes)
    codes <- str_replace(str_remove_all(codes, "\\s"), "^(RS|RT|RM|DX|DR|DS|DC|POS)-?([0-9])", "\\1-\\2")
    codes <- codes[codes %in% c(district_table, paste0("1957:", district_table_1957), "PD", "PMD")]
    paste(sort(unique(codes)), collapse = ";")
  }, character(1))
}
plain <- function(x) str_to_lower(str_remove_all(stringi::stri_trans_general(coalesce(x, ""), "Latin-ASCII"),
  "[^A-Za-z0-9]"))
result <- function(parsed, read, not_read = is.na(read)) case_when(
  not_read ~ "not read",
  coalesce(parsed, "") == coalesce(read, "") ~ "agrees",
  coalesce(parsed, "") == "" ~ "parser blank",
  TRUE ~ "differs")
sample <- read_csv("../output/journal_sample_pages.csv", show_col_types = FALSE)
parsed_on <- function(kind) bind_rows(lapply(2000:2009, function(y) {
    read_csv(sprintf("../input/journal_%s_%s.csv", kind, y), col_types = cols(.default = col_character()))
  })) |>
  mutate(page = as.integer(page), position = as.integer(position)) |>
  semi_join(sample, by = c("file", "page")) |>
  mutate(order = rank(position), .by = c(file, page))
# Match read rows to parsed rows by each key in turn, among the rows not yet matched; a key is used only where it is
# unique on both sides.
match_by <- function(reads, parsed, keys) {
  pairs <- tibble(read_id = integer(), parsed_id = integer(), match_rule = character())
  for (rule in names(keys)) {
    by <- keys[[rule]]
    r <- filter(reads, !read_id %in% pairs$read_id, if_all(all_of(by), \(x) !is.na(x) & x != "")) |>
      add_count(across(all_of(by))) |> filter(n == 1) |> select(read_id, all_of(by))
    p <- filter(parsed, !parsed_id %in% pairs$parsed_id, if_all(all_of(by), \(x) !is.na(x) & x != "")) |>
      add_count(across(all_of(by))) |> filter(n == 1) |> select(parsed_id, all_of(by))
    pairs <- bind_rows(pairs, inner_join(r, p, by = by, relationship = "one-to-one") |>
      transmute(read_id, parsed_id, match_rule = rule))
  }
  pairs
}

# 1. Passages.
passage_reads <- read_csv("../adjudication/journal_sample_passage_reads.csv",
    col_types = cols(.default = col_character())) |>
  filter(order != "0") |>
  mutate(read_id = row_number(), page = as.integer(page), order = as.numeric(order),
    application = str_remove_all(toupper(application_number), "\\s"),
    first_map = str_remove(heading_map, ";.*$"),
    map_read = if_else(change_map %in% c(NA, "NOT_SHOWN") | first_map == change_map, first_map, NA))
passages <- parsed_on("ordinances") |>
  mutate(parsed_id = row_number(), application = application_number)
passage_pairs <- match_by(passage_reads, passages, list(application_number = c("file", "page", "application"),
  order_on_page = c("file", "page", "order")))
passage_scores <- passage_reads |>
  left_join(passage_pairs, by = "read_id", relationship = "one-to-one") |>
  left_join(select(passages, parsed_id, parsed_application = application_number, parsed_map = map_number,
    parsed_from = from_districts, parsed_to = to_districts), by = "parsed_id", relationship = "many-to-one") |>
  transmute(table = "passages", file, page, order, match_rule = coalesce(match_rule, "missed by parser"),
    application = result(parsed_application, application),
    map = result(parsed_map, map_read),
    from = result(districts(parsed_from), districts(from_districts), from_districts %in% c(NA, "NOT_SHOWN")),
    to = result(districts(parsed_to), districts(to_districts), to_districts %in% c(NA, "NOT_SHOWN")),
    read_application = application_number, parsed_application, read_map = map_read, parsed_map,
    read_from = from_districts, parsed_from, read_to = to_districts, parsed_to, note) |>
  bind_rows(passages |> filter(!parsed_id %in% passage_pairs$parsed_id) |>
    transmute(table = "passages", file, page, order, match_rule = "no read entry",
      parsed_application = application_number, parsed_map = map_number, parsed_from = from_districts,
      parsed_to = to_districts))

# 2. Introductions.
introduction_reads <- read_csv("../adjudication/journal_sample_introduction_reads.csv",
    col_types = cols(.default = col_character())) |>
  filter(order != "0") |>
  mutate(read_id = row_number(), page = as.integer(page), order = as.numeric(order),
    name_key = str_sub(plain(name), 1, 10), record = record_number, application = application_number,
    map = str_squish(str_remove(map_number, "[,;].*$")))
introductions <- parsed_on("introductions") |>
  mutate(parsed_id = row_number(), name_key = str_sub(plain(name), 1, 10), record = record_number,
    application = application_number, map = map_number)
introduction_pairs <- match_by(introduction_reads, introductions, list(record_number = c("file", "record"),
  application_number = c("file", "application"), name_and_map = c("file", "page", "name_key", "map"),
  name = c("file", "page", "name_key"), order_on_page = c("file", "page", "order")))
introduction_scores <- introduction_reads |>
  left_join(introduction_pairs, by = "read_id", relationship = "one-to-one") |>
  left_join(select(introductions, parsed_id, parsed_filer = filer, parsed_name = name, parsed_ward = ward,
    parsed_application = application_number, parsed_map = map_number, parsed_from = from_districts,
    parsed_to = to_districts, parsed_address = common_address, parsed_record = record_number),
    by = "parsed_id", relationship = "many-to-one") |>
  transmute(table = "introductions", file, page, order, match_rule = coalesce(match_rule, "missed by parser"),
    read_name = name, read_map = map, read_from = from_districts, read_to = to_districts,
    read_address = common_address, read_record = record_number,
    filer = result(parsed_filer, filer),
    name = result(plain(str_remove(parsed_name, "(?i)^alderman\\s+")), plain(str_remove(name, "(?i)^alderman\\s+"))),
    ward = result(parsed_ward, ward, FALSE), application = result(parsed_application, application_number, FALSE),
    map = result(parsed_map, map), from = result(districts(parsed_from), districts(from_districts)),
    to = result(districts(parsed_to), districts(to_districts)),
    address = result(plain(parsed_address), plain(common_address), FALSE),
    record = result(parsed_record, record_number, FALSE),
    parsed_name, parsed_map, parsed_from, parsed_to, parsed_address, parsed_record, note) |>
  bind_rows(introductions |> filter(!parsed_id %in% introduction_pairs$parsed_id) |>
    transmute(table = "introductions", file, page, order, match_rule = "no read entry", parsed_name = name,
      parsed_map = map_number, parsed_from = from_districts, parsed_to = to_districts, parsed_address = common_address,
      parsed_record = record_number))

scores <- bind_rows(passage_scores, introduction_scores) |>
  relocate(table, file, page, order, match_rule, filer, name, ward, application, map, from, to, address, record)
SaveData(scores, c("table", "file", "page", "order", "match_rule"), "../output/journal_sample_scores.csv")
