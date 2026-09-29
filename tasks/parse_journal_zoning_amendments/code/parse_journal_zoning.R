# setwd("tasks/parse_journal_zoning_amendments/code")
# year <- "2008"
# Zoning map amendments in one year's City Council Journals (tasks/extract_council_journal_text, with the zoning pages
# read again by tasks/ocr_council_journal_zoning_pages):
#   introductions: each amendment introduced and referred to the Committee on Zoning, from the two sections headed
#     "Referred - Zoning Reclassifications Of Particular Areas" (or "... Of Areas Shown On Map Number 12-C" for one
#     alderman's amendments): applications, listed by applicant ("Mr. Brett Antonetti - to classify as an RT4.5 ...
#     District instead of an RS3 ... District the area shown on Map Number 1-G bounded by: ... (common address:
#     1417 West Erie Street)."), and aldermen's amendments, listed under "By Alderman Balcer (11th Ward):" or
#     "Alderman Hairston (5th Ward) presented ...". From 2009 an application gives its number ("Alex I. Corp.
#     (Application Number 16950) -- to classify as ..."), and a few are printed alone under their own heading ("The
#     City Clerk transmitted an application (in duplicate) from Asphalt Operating Services of Chicago (Application
#     Number 16990) ... as follows: to classify as ..."). A change in several steps adds clauses ("also, to classify
#     as ...", "and further, to classify as ...", "And To classify as ..."); the first clause gives the district
#     before and the last the district after.
#   ordinances: each map amendment ordinance printed in a report of the Committee on Zoning, with the Council's
#     action on the report (passed, deferred, withdrawn, re-referred, placed on file or failed), printed under
#     "Reclassification Of Area Shown On Map Number 1-G." with its application number ("(Application Number 16606)"),
#     from late 2009 its record number, and text ("changing all the RS3 ... District symbols and indications as shown
#     on Map Number 1-G in the area bounded by: ..., to those of an RM4.5 ... District"), or, reported on its own,
#     under the report's title ("AMENDMENT OF TITLE 17 OF MUNICIPAL CODE BY RECLASSIFICATION OF AREA SHOWN ON MAP NO.
#     8-F. (Application No. 17146) ...");
#   report_notes: applications a report notes as withdrawn or deferred ("Application Number A-7371 was withdrawn by
#     the applicant"), once per meeting.
# Page headers (page number, "JOURNAL--CITY COUNCIL--CHICAGO", the date and the section's running head) are removed
# before pages are joined, so that entries run across pages. District codes are read as in the zoning ordinance, with
# OCR confusions repaired (l or I for 1 in "Cl-2"), and kept only if the ordinance has them: the ordinance of 2004
# (Second City Zoning's table, tasks/download_second_city_zoning_districts) or, marked "1957:", the ordinance of 1957 it
# replaced in November 2004 (the codes the 2004 conversion table lists, data_raw/zoning_conversion_2004_crosswalk.csv).
# The two share codes with different districts (B3-2 was a General Retail District and is a Community Shopping
# District), and Journals of 2004 and 2005 mix the two. Before the 2004 ordinance took effect (ordinance_2004_start), a
# code is the 1957 ordinance's unless only the 2004 ordinance has it (RS3) or it is printed with a 2004 name ("B3-2
# Community Shopping District"). Afterwards a code both have is the 2004 ordinance's (a later "C1-2 Restricted Retail
# District" is a misnamed C1-2), and a code only the 1957 ordinance has is read only with its 1957 name ("instead of an
# R3 General Residence District", in applications of early 2005, or "C3-6 Commercial-Manufacturing District"); printed
# without it, it is a misprint ("B7-6 District" in 2008) and is left uncoded. A planned development is coded PD, and the
# Transportation District T. Each row keeps its text.
running_heads <- c("REPORTSOFCOMMITTEES", "NEWBUSINESSPRESENTEDBYALDERMEN", "COMMUNICATIONSETC", "AGREEDCALENDAR",
  "OFFICEOFTHECITYCLERK", "MISCELLANEOUSBUSINESS", "UNFINISHEDBUSINESS")
# Anchors of the printed sections and entries, allowing for OCR misreadings: "Reciassification" or "Reclassifi cation",
# "{" for "(" or "[" and "}" for "]" ("[PO2009-6084}"), "Apptication", "Number . 16509" or a doubled, stray or missing
# "Number" ("Number Number 16322", "(Application 'No. 16754)", "(Application 16646)"); an amendment of the Chicago Plan
# Commission is numbered "CPC No. 2" and is recorded as CPC-2, "adddress" or "adress", "{[" for "[", "ail" or "alli" for
# "all", and a missing, extra or stray period, space or colon ("RECLASSIFICATIONS OF: PARTICULAR AREAS", "To. classify",
# "ofa", "of the-B1-1", "[0201 1-609]", "[PO2009-21 18]", "[0201 1-7042)}"). A record number read with a zero for its
# letter O ("[02011-5462]"), a 5 or $ for its S ("[50201 1-6698]", "[S$02010-3116]") or a doubled O ("[S0O2010-6992]",
# "[SOO2009-1073]") is repaired. A section heading may carry stray marks or digits ("ZONING -RECLASSIFICATIONS",
# "RECLASSIFICATIONS _ OF", "RECLASSIFICATIONS 7 OF", "GF" for "OF") or, on a damaged scan, lose its "Referred --"; it
# is then read only as printed, "ZONING RECLASSIFICATIONS OF PARTICULAR AREAS" in capitals (a notice "OBJECTION TO
# PROPOSED ZONING RECLASSIFICATION OF AREA ..." is not a heading). A change may end ", to a Residential Planned
# Development" or "... in Section 1 to an Institutional Planned Development" without "those of", and the map may be
# "Map No. 1-F" or "Map Number11-L".
heading_words <- "ZONING[\\W_]+RECLASSIFICATIONS?[\\W_0-9]{1,6}[OG0]F[\\W_]+(?:PARTICULAR[\\W_]+)?AREAS?"
section_heading <- paste0("(?:(?i:R\\s*e\\s*f\\s*e\\s*r\\s*r\\s*e\\s*d\\s*[-\u2013\u2014~]+\\s*", heading_words,
  ")|ZONING[\\W_]+RECLASSIFICATIONS[\\W_0-9]{1,6}[OG0]F[\\W_]+PARTICULAR[\\W_]+AREAS)",
  "(?i:\\s+SHOWN\\s+ON\\s+MAP\\s+(?:NUMBERS?|NOS?\\.)\\s+[0-9]{1,2}\\s*-\\s*[A-Z0-9])?\\.?")
section_end <- "\\n\\s*(?:Referred\\s*[-\u2013\u2014~]|[0-9]\\.\\s+[A-Z]{4})"
application_label <- "[({]\\s*App\\w*\\W+(?:(?:Number|No)\\W*){0,2}([A-Z]?-?[0-9]{4,})\\s*[)}]?"
plan_commission_label <- "[({]\\s*(?:Application\\W+Number\\W+)?CPC\\W*(?:No\\W*|Number\\W*)?([0-9]+)"
record_label <- "[\\[{]{1,2}\\s*([A-Z0-9$]{1,3}\\s?[0-9](?:\\s?[0-9]){3}\\s?-\\s?[0-9](?:\\s?[0-9])*)\\s*[\\])}]{1,2}"
record_id <- function(x) {
  x <- str_replace(str_replace(str_remove_all(x, "\\s"), "^S\\$", "S"), "^[5$](?=[O0][0-9]{4}-)", "S")
  str_replace(str_replace(x, "^([A-Z]*)0(?=[0-9]{4}-)", "\\1O"), "^S[0O]O(?=[0-9]{4}-)", "SO")
}
classify_clause <- "to\\W{0,3}classify\\.?\\s+as?"
common_address_label <- "\\(\\s*common\\s+ad+res+(?:es)?\\s*:?"
map_label <- "Map\\W+(?:Numbers?|Nos?\\.?)\\s*([0-9](?:\\s?[0-9]){0,2}\\s*-{0,2}\\s*[A-Za-z0-9|!/])"
to_those_of <- paste0("(?:to\\.?\\s+(?:those|that|the designation)\\s+of|,?\\s*\\bto(?=\\s+an?\\s+",
  "(?:[\\w./-]+\\s+){0,6}?(?:District|Planned Development)\\b))")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- c(year)
stopifnot(length(cli_args) == 1, grepl("^[0-9]{4}$", cli_args[1]))
year <- cli_args[1]

district_table <- read_csv("../input/zoning-code-summary-district-types.csv", show_col_types = FALSE)$district_type_code
district_table_1957 <- read_csv("../input/zoning_conversion_2004_crosswalk.csv", show_col_types = FALSE)$old_code |>
  str_subset("^(?:R[1-8]|[BCM][1-7]-[1-7]|C4)$")
ordinance_2004_start <- as.Date("2004-11-01")
name_1957 <- paste0("^\\W*(?:SINGLE-FAMILY RESIDENCE|GENERAL|RESTRICTED|LOCAL RETAIL|",
  "COMMERCIAL-? ?MANUFACTURING DISTRICT|MOTOR FREIGHT)")
name_2004 <- paste0("^\\W*(?:NEIGHBORHOOD|COMMUNITY|MOTOR VEHICLE|COMMERCIAL, MANUFACTURING|LIMITED MANUFACTURING|",
  "LIGHT IND|HEAVY IND)")
district_codes <- function(text, meeting_date) {
  text <- toupper(str_squish(coalesce(text, "")))
  # OCR confusions: l or I for 1 ("Cl-2", "RSI", "Rl"), or 1 doubled ("C1l-2", "Ml1-1", "Cl-1l"), 8 for B ("82-3",
  # "84-2"), S for 5 ("RMS", "BS-1" for B5-1, "RS General Residence" for R5) or 5 doubled ("RMS5", "B5S-2"), 7 for T
  # ("R74"), an underscore or a space inside or after the code ("M1-2_", "M 1-1", "B2-1 .5"), an article run into it
  # ("aC1-1"), and misspelled names of the 1957 ordinance's districts ("Genered Residence", "Restricted Retafl",
  # "Restricted Serrice").
  text <- str_replace_all(text, c("_" = " ", "\\b([BCM])(?:1L|L1)\\s*-" = "\\11-", "-1L\\b" = "-1",
    "\\bR[LI](?=\\s+SINGLE)" = "R1", "\\b(RM|B)(?:5S|S5)" = "\\15", "\\bRTS(?=[0-9])" = "RT",
    "\\b([BCM])[LI]\\s*-" = "\\11-", "\\b(RS|RT|RM)[LI]\\b" = "\\11",
    "\\b(AN?)((?:RS|RT|RM|B[1-7]|C[1-5]|M[1-3])-?[0-9])" = "\\1 \\2", "\\b8([1-7])\\s*-" = "B\\1-",
    "\\bRMS\\b" = "RM5", "\\bBS5?-(?=[0-9])" = "B5-", "\\bR7(?=[0-9])" = "RT",
    "\\b([BCM])\\s+([1-7])\\s*-" = "\\1\\2-", "-([0-9]) \\.([0-9])\\b" = "-\\1.\\2",
    "\\bGENER[A-Z]{1,2}\\b" = "GENERAL", "\\bRESTR[A-Z]{3,6}\\b" = "RESTRICTED", "\\bRETA[A-Z]L\\b" = "RETAIL",
    "\\bRS(?=\\s+GENERAL RESIDENCE)" = "R5"))
  found <- str_match_all(text, paste0("\\b(RS|RT|RM|R|B[1-7]|C[1-5]|M[1-3]|DC|DX|DR|DS|POS)\\s*-?\\s*",
    "([0-9]{1,2}(?:\\.[0-9])?A?)\\b(?=(.{0,40}))"))[[1]]
  only_2004 <- found[, 2] %in% c("RS", "RT", "RM", "DC", "DX", "DR", "DS", "POS")
  in_both <- found[, 2] %in% paste0(rep(c("B", "C", "M"), each = 3), 1:3)
  is_1957 <- if (meeting_date < ordinance_2004_start) {
    !only_2004 & !(in_both & str_detect(found[, 4], name_2004))
  } else {
    !only_2004 & !paste0(found[, 2], "-", found[, 3]) %in% district_table & str_detect(found[, 4], name_1957)
  }
  codes <- if_else(is_1957, paste0("1957:", found[, 2], if_else(found[, 2] == "R", "", "-"), found[, 3]),
    paste0(found[, 2], "-", found[, 3]))
  codes <- codes[codes %in% c(district_table, paste0("1957:", district_table_1957))]
  if (grepl("\\bC4\\s+MOTOR FREIGHT", text)) codes <- c(codes, "1957:C4")
  if (grepl("\\bT\\s*\\(?TRANSPORTATION\\)?\\s+DISTRICT", text)) codes <- c(codes, "T")
  if (grepl("PLANNED MANUFACTURING", text) || grepl("\\bPMD\\b", text)) codes <- c(codes, "PMD")
  if (grepl("PLANNED DEVELOPMENT", text) && !grepl("PLANNED MANUFACTURING", text)) codes <- c(codes, "PD")
  paste(unique(codes), collapse = ";")
}
# Map sheets are a number and a capital letter ("7-I"), numbered 1 to 20 and, further south, 22 to 32 in even numbers,
# with further sheets in column B numbered past 100 ("Map No. 153-B"); the hyphen may be missing or doubled ("13 --
# G"). OCR reads the letter I as 1, l, i, |, ! or /, J as d or u, B as 8 and O as 0, and splits the number ("1 1-d" for
# 11-J). A reading with another number ("76-K") or with a digit for its letter ("1-9") is not a sheet.
map_sheet_numbers <- c(1:20, seq(22, 32, by = 2))
map_sheet <- function(x) {
  parts <- str_match(str_remove_all(x, "\\s"), "^([0-9]{1,3})-{0,2}(.)$")
  number <- as.integer(parts[, 2])
  letter <- toupper(chartr("1li|!/du80", "IIIIIIJJBO", parts[, 3]))
  if_else(str_detect(letter, "^[A-Z]$") & (number %in% map_sheet_numbers | (letter %in% "B" & number %in% 1:199)),
    paste0(number, "-", letter), NA_character_)
}

# 1. Pages, read from the fresh OCR of the zoning pages (tasks/ocr_council_journal_zoning_pages) and from the text
# layer elsewhere, without their headers and with recurring OCR errors repaired (in the text layer "ti" read as "f",
# missing spaces and a letter-spaced "t o classify"; in both an article joined to the district code, "of aC1-5"),
# joined by meeting, with the offset at which each page starts. A page's printed number is read from its running
# head, the first line of its text as laid out ("18302 JOURNAL--CITY COUNCIL--CHICAGO 1/9/2008", "1/9/2008 REPORTS OF
# COMMITTEES 18303"). The numbers run on one to a page and restart with each council term, and jump only where a
# volume begins, so each page takes its position plus the offset (printed number less position) that most of the
# pages within page_offset_window of it show; a misread or missing number (a map, a page OCR garbles) is then set from
# its neighbours.
text_repairs <- c("(?i)applicafion" = "Application", "(?i)residenfial" = "Residential",
  "(?i)institufional" = "Institutional", "(?i)indicafions" = "indications", "(?i)mulfi" = "Multi",
  "\\bofthe\\b" = "of the", "\\btothose\\b" = "to those",
  "(?i)\\bt\\s+o\\s+(?=classify)" = "to ",
  "\\b(of|as) (an?)(?=(?:RS|RT|RM|DC|DX|DR|DS|POS|[BCM])\\s?[0-9-])" = "\\1 \\2 ")
page_offset_window <- 5
fresh_ocr <- arrow::read_parquet(sprintf("../input/journal_zoning_ocr_%s.parquet", year)) |>
  select(file, page, ocr_text)
pages <- arrow::read_parquet(sprintf("../input/journal_pages_%s.parquet", year)) |>
  left_join(fresh_ocr, by = c("file", "page"), relationship = "one-to-one") |>
  mutate(text_source = if_else(is.na(ocr_text), "text_layer", "fresh_ocr"), text = coalesce(ocr_text, text)) |>
  arrange(meeting_date, file, page)
strip_header <- function(text, journal_page) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  top <- head(which(nchar(str_squish(lines)) > 0), 3)
  if (length(top) == 0) return(text)
  squished <- str_squish(lines[top])
  letters_only <- gsub("[^A-Z]", "", toupper(squished))
  number <- str_match(squished, "^[^A-Za-z0-9]*([0-9]{1,6})[^A-Za-z0-9]*$")[, 2]
  header <- coalesce(nchar(number) >= 5 | number == as.character(journal_page), FALSE) |
    str_detect(squished, "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$|^[^A-Za-z]*JOURNAL") |
    vapply(letters_only, function(l) nchar(l) > 0 && min(utils::adist(l, running_heads)) <= 2, logical(1))
  if (any(header)) lines <- lines[-top[header]]
  paste(lines, collapse = "\n")
}
local_offset <- function(offset) vapply(seq_along(offset), function(i) {
  near <- offset[max(1, i - page_offset_window):min(length(offset), i + page_offset_window)]
  near <- near[!is.na(near)]
  if (length(near) == 0) NA_integer_ else as.integer(names(which.max(table(near))))
}, integer(1))
pages <- pages |>
  mutate(running_head = str_squish(str_extract(layout_text, "^\\s*[^\\n]*\\S[^\\n]*")),
    printed_page = as.integer(coalesce(
      str_match(running_head, "^[^A-Za-z0-9]*([0-9]{1,6})\\s+\\S*\\s*J\\s?O\\s?U\\s?R")[, 2],
      str_match(running_head, "[A-Z]{3}[^0-9/]{0,6}\\s([0-9]{1,6})[^A-Za-z0-9/]*$")[, 2]))) |>
  mutate(journal_page = page + local_offset(printed_page - page), .by = file) |>
  mutate(clean_text = str_replace_all(unlist(Map(strip_header, text, journal_page), use.names = FALSE),
    text_repairs))
meetings <- pages |>
  mutate(start = cumsum(lag(nchar(clean_text) + 1L, default = 0L)) + 1L, .by = file) |>
  select(meeting_date, file, page, journal_page, text_source, start)
meeting_text <- pages |> summarise(text = paste(clean_text, collapse = "\n"), .by = c(meeting_date, file))
page_at <- function(file_name, position) {
  starts <- meetings[meetings$file == file_name, ]
  starts[findInterval(position, starts$start), c("page", "journal_page", "text_source")]
}

# Fields of an amendment's clauses, read with whitespace collapsed: districts before and after, map number, boundary,
# common address and record number. Each clause changes some districts to another ("to classify as <to> instead of
# <from>"). Clauses may chain (a district, then a planned development) or cover further parcels ("also, ..."), so the
# districts before are those of every clause not produced by an earlier clause, and the districts after those of
# every clause not changed by a later one. OCR misreads "instead of" as "instead ef" and "shown" as "shewn" or "showri",
# and sets stray marks inside "to classify as" ("to 'classify as", "to classify. as").
# Address ranges are written with a hyphen ("3206 - 3208"), which OCR also reads as two. The boundary is the first
# clause's, up to its common address, record number or the next clause.
clause_fields <- function(entry, meeting_date) {
  entry <- str_squish(entry)
  clauses <- str_split(entry, regex(classify_clause, ignore_case = TRUE))[[1]][-1]
  to_text <- str_match(clauses, regex("^(.*?)\\binstead\\s*[eo]f", ignore_case = TRUE))[, 2]
  from_text <- str_match(clauses, regex(paste0("instead\\s*[eo]f\\s*(.*?)",
    "(?:\\s*the area sh\\w*|\\s*and further|,?\\s*also,|$)"), ignore_case = TRUE))[, 2]
  to_codes <- strsplit(vapply(to_text, district_codes, character(1), meeting_date = meeting_date), ";")
  from_codes <- strsplit(vapply(from_text, district_codes, character(1), meeting_date = meeting_date), ";")
  k <- seq_along(clauses)
  before <- unique(unlist(lapply(k, function(i) setdiff(from_codes[[i]], unlist(to_codes[k < i])))))
  after <- unique(unlist(lapply(k, function(i) setdiff(to_codes[[i]], unlist(from_codes[k > i])))))
  tibble(
    steps = length(clauses),
    from_text = paste(from_text, collapse = " | "),
    to_text = paste(to_text, collapse = " | "),
    map_number = map_sheet(str_match(entry, regex(map_label, ignore_case = TRUE))[, 2]),
    boundary = str_match(entry, regex(paste0("bounded by:?(.*?)(?:", common_address_label, "|also,|", record_label,
      "|[,;]?\\s*(?:and\\s+)?(?:further,?\\s*)?", classify_clause, "|$)"), ignore_case = TRUE))[, 2],
    common_address = str_replace_all(str_match(entry, regex(paste0(common_address_label,
      "\\s*(.*?)(?:\\)[^\\[{)]{0,16}(?:[\\[{]|$)|\\.?\\s*[\\[{])"), ignore_case = TRUE))[, 2],
      "\\s*(?:-{2,}|[\u2013\u2014])\\s*|\\s+-\\s+", " - "),
    record_number = record_id(str_match(entry, record_label)[, 2]),
    from_districts = paste(before, collapse = ";"),
    to_districts = paste(after, collapse = ";"))
}

# Where an application entry ends: after its common address (whose own parentheses may nest, as in "(common address:
# 2541 North Sawyer Avenue (rear))", or go unclosed before the record number) and any record number that follows,
# perhaps after dot leaders or a stray mark ("... 55th Street) 7 [O2010-5174]"); or, for an entry without them that is
# followed by another applicant ("... South Western Avenue. Ms. Martha E. Franco -"), at the first period followed only
# by a short name without semicolons, after a word that is not an abbreviation or initial (the letter of a lettered
# street, "South Avenue M.", is not an initial) or after a parenthesis
# ("... perpendicular thereto). Ashland and Waveland, L.L.C. --"). OCR reads the period as several ("Avenue...") or,
# failing any period, as a semicolon ("... South Kedvale Avenue; SDO Development, L.L.C --").
name_abbreviations <- c("MR", "MRS", "MS", "DR", "INC", "CO", "CORP", "LTD", "JR", "SR", "LLC", "LP", "ST", "BROS",
  "NO")
entry_end <- function(chunk) {
  opening <- str_locate_all(chunk, regex(common_address_label, ignore_case = TRUE))[[1]]
  record <- str_locate_all(chunk, record_label)[[1]]
  if (nrow(opening) > 0) {
    characters <- strsplit(substr(chunk, opening[nrow(opening), 1], nchar(chunk)), "")[[1]]
    close <- opening[nrow(opening), 1] + which(cumsum((characters == "(") - (characters == ")")) == 0)[1] - 1L
    if (is.na(close)) {
      return(if (nrow(record) > 0 && record[nrow(record), 1] > opening[nrow(opening), 1]) record[nrow(record), 2] else
        NA_integer_)
    }
    after <- str_match(substr(chunk, close + 1L, nchar(chunk)), paste0("^(?:[^\\[{]{0,16}?", record_label,
      "|[^\\w\\[{]{0,16})"))[1, 1]
    return(close + nchar(after))
  }
  if (nrow(record) > 0) return(record[nrow(record), 2])
  if (!grepl("[-\u2013\u2014~][^A-Za-z0-9]*$", chunk)) return(NA_integer_)
  periods <- str_locate_all(chunk, "(?:[A-Za-z]+|\\))\\.+\\s")[[1]]
  if (nrow(periods) > 0) {
    words <- toupper(str_match(substring(chunk, periods[, 1], periods[, 2]), "^([A-Za-z]*)")[, 2])
    remainder <- substring(chunk, periods[, 2] + 1L, nchar(chunk))
    lettered_street <- grepl("Avenue\\s$", substring(chunk, pmax(periods[, 1] - 7L, 1L), periods[, 1] - 1L))
    candidate <- which((words == "" | nchar(words) > 1 & !words %in% name_abbreviations | lettered_street) &
      nchar(remainder) < 250 &
      !grepl(";", remainder))
    if (length(candidate) > 0) return(periods[candidate[1], 2] - 1L)
  }
  semicolon <- tail(str_locate_all(chunk, ";\\s")[[1]][, 1], 1)
  if (length(semicolon) == 0 || nchar(chunk) - semicolon >= 250) return(NA_integer_)
  semicolon
}

# 2. Introductions. A section is the aldermen's if it opens with them ("The aldermen named below presented ...",
# "Alderman Daley (43rd Ward) presented ...") or with an alderman's heading ("BY ALDERMAN", where a damaged scan leaves
# the opening illegible), and otherwise the applicants'. It is cut into one chunk per amendment, from its first clause
# to the next amendment's. An application's chunk ends with its common address; what follows, up to the next clause, is
# the next applicant's name and application number, or for the first application, what follows the section's opening "as
# follows:" (or the applicant the City Clerk "transmitted an application ... from"). A name loses the stray marks and
# record number OCR sets before it ("_ BMT-I, L.L.C.", "[PO2010- -5190] Target Corporation"). Aldermen's amendments take
# the alderman and ward of the last "By Alderman" heading before them.
application_number_text <- paste0("\\s*", application_label)
introductions <- bind_rows(lapply(seq_len(nrow(meeting_text)), function(m) {
  text <- meeting_text$text[m]
  headings <- str_locate_all(text, section_heading)[[1]]
  bind_rows(lapply(seq_len(nrow(headings)), function(h) {
    rest <- substr(text, headings[h, 2] + 1L, nchar(text))
    section <- substr(rest, 1, regexpr(section_end, substr(rest, 60, nchar(rest)), perl = TRUE) + 58L)
    filer <- if (grepl("aldermen[\\W_]+named[\\W_]+below|alderman\\s[^()]{1,40}\\([^()]{1,12}ward\\)\\s+presented",
      substr(section, 1, 300), ignore.case = TRUE, perl = TRUE) || grepl("BY\\s+ALDERMAN", substr(section, 1, 400)))
      "alderman" else "applicant"
    clauses <- str_locate_all(section, regex(classify_clause, ignore_case = TRUE))[[1]][, 1]
    if (length(clauses) == 0) return(NULL)
    continuation <- str_detect(substring(section, pmax(clauses - 12L, 1L), clauses - 1L),
      regex("also,?\\s*$|further,?\\s*$|\\band\\s*$", ignore_case = TRUE))
    starts <- clauses[!continuation]
    if (length(starts) == 0) return(NULL)
    chunks <- substring(section, starts, c(starts[-1] - 1L, nchar(section)))
    if (filer == "applicant") {
      ends <- vapply(chunks, entry_end, integer(1), USE.NAMES = FALSE)
      intro_end <- coalesce(str_locate(section, "as follows\\s*:")[, 2], 0L)
      name_from <- c(intro_end + 1L, starts[-length(starts)] + ends[-length(ends)])
      names <- substring(section, name_from, starts - 1L)
      transmitted <- str_match(substr(section, 1, intro_end), regex(paste0("transmitted an application \\(in \\w+\\) ",
        "from (.+?)(", application_number_text, ")?,?\\s*together"), ignore_case = TRUE, dotall = TRUE))
      if (str_squish(names[1]) == "" && !is.na(transmitted[, 1])) names[1] <- paste(transmitted[, 2],
        coalesce(transmitted[, 3], ""))
      applications <- str_match(names, regex(application_number_text, ignore_case = TRUE))[, 2]
      names <- str_squish(str_remove(str_remove(names, regex(application_number_text, ignore_case = TRUE)),
        "[-\u2013\u2014~][^A-Za-z0-9]*$")) |>
        str_remove("^[^A-Za-z0-9\\[{]*(?:[\\[{][^\\]}]{0,24}[\\]}])?[^A-Za-z0-9]*")
      entries <- if_else(is.na(ends), chunks, substr(chunks, 1, ends))
      wards <- NA_integer_
    } else {
      applications <- NA_character_
      ends <- rep(NA_integer_, length(starts))
      entries <- str_remove(chunks, regex("\\n\\s*BY ALDERMAN.*$", ignore_case = TRUE, dotall = TRUE))
      aldermen <- str_locate_all(section, regex("ALDERMAN\\s+[A-Z][A-Z .'\u2019-]+?\\s*\\(\\s*[0-9]{1,2}",
        ignore_case = TRUE))[[1]]
      heading_text <- str_match(substring(section, aldermen[, 1], aldermen[, 2]),
        regex("ALDERMAN\\s+(.+?)\\s*\\(\\s*([0-9]{1,2})", ignore_case = TRUE))
      last_heading <- findInterval(starts, aldermen[, 1])
      names <- if_else(last_heading > 0, str_squish(heading_text[pmax(last_heading, 1), 2]), NA_character_)
      wards <- if_else(last_heading > 0, as.integer(heading_text[pmax(last_heading, 1), 3]), NA_integer_)
    }
    position <- headings[h, 2] + starts
    bind_cols(tibble(meeting_date = meeting_text$meeting_date[m], file = meeting_text$file[m], position),
      page_at(meeting_text$file[m], position),
      tibble(filer, name = names, ward = wards, application_number = applications,
        entry_complete = filer == "alderman" | !is.na(ends)),
      bind_rows(lapply(entries, clause_fields, meeting_date = meeting_text$meeting_date[m])),
      tibble(entry_text = str_squish(entries)))
  }))
}))

# 3. Reports of the Committee on Zoning. The committee's reports are printed one after another, each under a title in
# capitals ("AMENDMENT OF TITLE 17 OF MUNICIPAL CODE BY RECLASSIFICATION OF AREAS SHOWN ON MAP NOS. 2-I, 3-G, 3-H AND
# 11-H.", perhaps under "COMMITTEE ON ZONING." or after "Action Deferred --"), then "The Committee on Zoning submitted
# the following report:", the report, the Council's action and the ordinances. A report deferred at one meeting is taken
# up at a later one ("the City Council took up for consideration the report of the Committee on Zoning, deferred and
# published ..."); a few are printed without their opening sentence, and open with the report's own first sentence
# ("Reporting for your Committee on Zoning, ..."), which is not an opening where it follows the report's opening or,
# within a few lines, a note that the report resumes ("(Continued from page 48521) CHICAGO, December 13, 2000. To the
# President ..."). A report's title is the run of heading lines just above its opening, and the report runs to the next
# report's title or the next section of the Journal (the Agreed Calendar, New Business, Unfinished Business,
# Miscellaneous Business). Only reports of map amendments are kept: their titles reclassify areas. The Council's action
# is read from the report's opening or title ("Deferred and ordered published", "Action Deferred --"), else its motion
# ("On motion of Alderman Banks, the said proposed ordinances ... were Passed"; or Withdrawn, Re-Referred, Placed on
# File, Failed to Pass, with or without "was", as in "the committee's recommendation was Concurred In and the said
# proposed ordinance ... Failed To Pass"; OCR reads "Re-Refemed" and "Defemed"), else the sentence that introduces its
# ordinances ("The following is said withdrawn ordinance"), else its title ("Withdrawn --"), else, for a report that the
# application be voted "Do Not Pass", the Council's concurring in the recommendation, which fails it; a report without
# one stops the parse.
report_opening <- regex("submitted\\W+the\\W+following\\W+report|took\\W+up\\W+for\\W+consideration\\W+the\\W+report",
  ignore_case = TRUE)
report_first_sentence <- regex("^\\W*Reporting\\W+for\\W+your\\W+Committee\\W+on\\W+Zoning", ignore_case = TRUE)
report_heading <- regex(paste0("^\\W{0,3}(?:(?:Action \\w+|Withdrawn|Re-Referred|Placed On File)\\s*-+\\s*)?",
  "(?:COMMITTEE ON [A-Z]|AMENDMENT|APPOINTMENT|REAPPOINTMENT|ISSUANCE|DESIGNATION|APPROVAL|AUTHORIZATION|RECLASSIF)"))
journal_section <- regex(paste0("^\\s*(?:AGREED CALENDAR|NEW BUSINESS|UNFINISHED BUSINESS|MISCELLANEOUS BUSINESS|",
  "[0-9]{1,2}\\.\\s+TRAFFIC REGULATIONS)"))
map_amendment_title <- paste0("(?:RE)?CLASSIF\\w*\\W+(?:OF\\W+)?(?:PARTICULAR\\W+)?AREAS?\\W+(?:SHOWN|$)|",
  "RECLASSIFICATION\\W+OF\\W+PARTICULAR")
council_motion <- regex(paste0("On motion of Alderm[ae]n.{0,300}?\\b(?:(?:was|were)\\s+)?(Passed|Withdrawn|",
  "Re-Ref\\w{2,4}d|Placed on File|Failed to Pass)"), ignore_case = TRUE)
reports <- bind_rows(lapply(seq_len(nrow(meeting_text)), function(m) {
  lines <- strsplit(meeting_text$text[m], "\n", fixed = TRUE)[[1]]
  line_start <- cumsum(c(1L, nchar(lines[-length(lines)]) + 1L))
  continued_from <- str_detect(lines, regex("\\(\\s*Continued\\s+from\\s+page", ignore_case = TRUE))
  openings <- which(str_detect(lines, report_opening))
  reporting <- which(str_detect(lines, report_first_sentence))
  reporting <- reporting[!vapply(reporting, \(k) any(openings < k & openings >= k - 12) ||
    any(continued_from[max(1L, k - 8L):k]), logical(1))]
  openings <- sort(c(openings, reporting))
  if (length(openings) == 0) return(NULL)
  heads <- which(str_detect(lines, report_heading))
  sections <- which(str_detect(lines, journal_section))
  previous <- c(0L, openings[-length(openings)])
  heading_above <- function(k) (k - 1L) %in% heads || ((k - 2L) %in% heads && str_squish(lines[k - 1L]) == "")
  title_start <- vapply(seq_along(openings), function(i) {
    k <- heads[heads < openings[i] & heads > previous[i] & heads >= openings[i] - 60]
    if (length(k) == 0) return(NA_integer_)
    k <- max(k)
    while (heading_above(k) && max(heads[heads < k]) > previous[i]) k <- max(heads[heads < k])
    as.integer(k)
  }, integer(1))
  last_line <- vapply(seq_along(openings), function(i) {
    later <- c(title_start[-seq_len(i)], openings[-seq_len(i)], sections[sections > openings[i]])
    later <- later[!is.na(later)]
    as.integer(if (length(later) > 0) min(later) - 1L else length(lines))
  }, integer(1))
  first_line <- coalesce(title_start, openings)
  # A report interrupted before its ordinances ("(Continued on page 55345)") resumes on that Journal page, after
  # "(Continued from page 55338)", and runs to the next report or section.
  boundaries <- sort(c(title_start[!is.na(title_start)], openings, sections))
  journal_pages <- meetings[meetings$file == meeting_text$file[m], ]
  continuation <- vapply(seq_along(openings), function(i) {
    span <- openings[i]:last_line[i]
    enacted <- span[str_detect(lines[span], regex("Ordained", ignore_case = TRUE))]
    marks <- span[str_detect(lines[span], regex("\\(\\s*Continued\\s+on\\s+page\\s+[0-9]{1,6}", ignore_case = TRUE))]
    marks <- marks[marks < min(c(enacted, Inf))]
    if (length(marks) == 0) return(c(NA_integer_, NA_integer_, NA_integer_))
    target <- str_match(lines[marks[1]], "page\\s+([0-9]{1,6})")[, 2]
    page_start <- journal_pages$start[match(target, journal_pages$journal_page)]
    if (is.na(page_start)) return(c(marks[1], NA_integer_, NA_integer_))
    resumes <- findInterval(page_start, line_start)
    later <- boundaries[boundaries > resumes]
    c(marks[1], resumes, as.integer(if (length(later) > 0) min(later) - 1L else length(lines)))
  }, integer(3))
  tibble(meeting_date = meeting_text$meeting_date[m], file = meeting_text$file[m],
    title = vapply(seq_along(openings), function(i) {
      if (is.na(title_start[i])) "" else str_squish(paste(lines[title_start[i]:(openings[i] - 1L)], collapse = " "))
    }, character(1)),
    opener = str_squish(paste(lines[pmax(openings - 1L, 1L)], lines[openings],
      lines[pmin(openings + 1L, length(lines))])),
    start = line_start[first_line], body_start = line_start[openings],
    end = if_else(is.na(continuation[1, ]), line_start[last_line] + nchar(lines[last_line]) - 1L,
      line_start[continuation[1, ]] - 2L),
    resumes = line_start[continuation[2, ]],
    resumed_end = line_start[continuation[3, ]] + nchar(lines[continuation[3, ]]) - 1L,
    continued_to_missing_page = !is.na(continuation[1, ]) & is.na(continuation[2, ]))
})) |>
  filter(str_detect(opener, regex("Committee\\W+on\\W+Zoning", ignore_case = TRUE)),
    str_detect(title, map_amendment_title)) |>
  mutate(text = meeting_text$text[match(file, meeting_text$file)],
    report_text = if_else(is.na(resumes), str_sub(text, start, end),
      paste(str_sub(text, start, end), str_sub(text, resumes, resumed_end), sep = "\n")),
    body = str_squish(str_sub(report_text, body_start - start + 1L)),
    motion = tolower(str_match(body, council_motion)[, 2]),
    introduction = str_extract(body, regex(paste0("The following (?:is|are) said[^:]{0,120}:|Said proposed[^:]{0,120}",
      "reads as follows:"), ignore_case = TRUE)),
    action = case_when(
      str_detect(str_sub(body, 1, 300), regex("Def\\w{2,4}d and ordered published", ignore_case = TRUE)) |
        str_detect(title, "^\\W*Action Deferred") ~ "deferred",
      !is.na(motion) ~ if_else(str_starts(motion, "re-ref"), "re-referred",
        recode(motion, "failed to pass" = "failed")),
      str_detect(introduction, regex("withdrawn", ignore_case = TRUE)) ~ "withdrawn",
      str_detect(introduction, regex("as passed", ignore_case = TRUE)) ~ "passed",
      str_detect(title, "^\\W*Withdrawn") ~ "withdrawn",
      str_detect(title, "^\\W*Re-Referred") ~ "re-referred",
      str_detect(body, regex("Do Not Pass", ignore_case = TRUE)) &
        str_detect(body, regex("recommendation was Conc\\w+ In", ignore_case = TRUE)) ~ "failed"),
    report = row_number()) |>
  select(-text)
if (anyNA(reports$action)) print(select(filter(reports, is.na(action)), file, title))
stopifnot(!anyNA(reports$action), !any(reports$continued_to_missing_page))

# The reports also note applications withdrawn or deferred without printing them ("Please let the record reflect that
# Application Number A-7371 was withdrawn by the applicant"); the notes, repeated in each report of a meeting, are
# kept once per meeting and map amendment (text amendments in the same note, "TAD-451", are left out).
report_note <- regex(paste0("Application\\s+Numbers?\\s+((?:[A-Z]{0,3}-?\\s?[0-9]{3,5}\\s*(?:,|and|&)?\\s*)+)",
  "[^.]{0,80}?",
  "\\b(?:was|were|has been|have been|is|are)\\s+(withdrawn|deferred|placed on file)"), ignore_case = TRUE)
report_notes <- reports |>
  mutate(found = str_match_all(str_remove(body, regex("Be\\W+\\S{1,3}\\W+Ordained.*$", ignore_case = TRUE)),
    report_note)) |>
  select(meeting_date, file, found) |>
  mutate(found = lapply(found, \(x) tibble(numbers = x[, 2], note = tolower(x[, 3]), note_text = x[, 1]))) |>
  unnest(found) |>
  mutate(application_number = str_extract_all(numbers, "(?<![A-Z-])(?:A-)?[0-9]{4,5}\\b")) |>
  unnest(application_number) |>
  distinct(meeting_date, file, application_number, note, .keep_all = TRUE) |>
  select(meeting_date, file, application_number, note, note_text)

# Each report's ordinances are printed under headings ("Reclassification Of Area Shown On Map Number 1-G.", sometimes
# without "On"); a report of a single ordinance has none, and its ordinance, under the report's title, follows "Be It
# Ordained". The change reads "changing all the <from> District symbols and indications as shown on Map Number <map> in
# the area bounded by: ..., to those of (or to that of, or to the designation of) a <to> District", the area also
# written "in an area bound by", "in the area generally bounded by" or "in the following area"; a change in several
# steps has several such sentences, of which a later one may leave out the map ("changing all of the B3-5 ... District
# symbols to those of a Business Planned Development", "... symbols and indications established in Section 1 above to
# the designation of an Institutional Planned Development"). A planned development's statements, which follow it, are
# not read for the change. The map number is printed in the heading (or title) and in the first change, and OCR misreads
# either ("Map Number 17-L" in a heading for 1-L, "8-H" in a change for 9-H): it is their common reading, or the one
# that is legible, and is left blank where the two conflict.
ordinance_area <- "\\b(?:area\\s+(?:generally\\s+)?bound(?:ed)?\\s+by|following\\s+area)\\s*:?"
ordinance_heading <- paste0("(?<=\\n[ \\t_.,:;'\u2018\u2019~-]{0,10})[Rr]ec[l1i]assifi\\s?cation\\W+[Oo]f\\W+",
  "Areas?\\W+[Ss]hown\\W+(?:[Oo]n\\W+)?")
change_sentence <- paste0("changing (?:a\\S{1,3}\\s*)?(?:of\\s*)?the\\W(.{0,400}?)",
  "(?:\\bas shown|\\bsymbols?(?=\\W+(?:and\\W+indications\\W+)?(?:shown\\W+)?(?:on|to|established|within|in)\\b))",
  "(?:.{0,300}?",
  map_label, ")?.{0,20000}?",
  to_those_of, "\\W*(?:an?\\s|the\\s)?(.{0,400}?)",
  "(?: and ?a corresponding|[.,]\\W*SECTION|,? which is hereby|\\.\\s*$)")
ordinances <- bind_rows(lapply(seq_len(nrow(reports)), function(r) {
  report_text <- reports$report_text[r]
  first_span <- reports$end[r] - reports$start[r] + 1L
  headings <- str_locate_all(report_text, ordinance_heading)[[1]][, 1]
  if (length(headings) == 0) {
    if (!grepl("Ordained", report_text, ignore.case = TRUE)) return(NULL)
    headings <- 1L
  }
  chunks <- str_squish(substring(report_text, headings, c(headings[-1] - 1L, nchar(report_text))))
  bind_rows(lapply(seq_along(chunks), function(i) {
    chunk <- chunks[i]
    ordinance <- str_remove(str_remove(chunk, regex("^.*?(?=Be\\W+\\S{1,3}\\W+Ordained)", ignore_case = TRUE)),
      regex("Plan of Development Statements.*$", ignore_case = TRUE))
    changes <- str_match_all(ordinance, regex(change_sentence, ignore_case = TRUE))[[1]]
    position <- if (headings[i] <= first_span) reports$start[r] + headings[i] - 1L else
      reports$resumes[r] + headings[i] - first_span - 2L
    bind_cols(
      tibble(meeting_date = reports$meeting_date[r], file = reports$file[r], position),
      page_at(reports$file[r], position),
      tibble(
        report = reports$report[r], report_title = reports$title[r], action = reports$action[r],
        heading_map = map_sheet(str_match(str_sub(chunk, 1, 400), regex(map_label, ignore_case = TRUE))[, 2]),
        change_map = if (nrow(changes) > 0) map_sheet(changes[1, 3]) else NA_character_,
        application_number = coalesce(str_match(chunk, regex(application_label, ignore_case = TRUE))[, 2],
          paste0("CPC-", str_match(str_sub(chunk, 1, 300), regex(plan_commission_label, ignore_case = TRUE))[, 2]) |>
            na_if("CPC-NA")),
        record_number = record_id(str_match(str_sub(chunk, 1, 600), record_label)[, 2]),
        as_amended = grepl("\\(\\s*As Amended\\s*\\)", str_sub(chunk, 1, 300), ignore.case = TRUE),
        steps = nrow(changes),
        from_text = if (nrow(changes) > 0) changes[1, 2] else NA_character_,
        to_text = if (nrow(changes) > 0) changes[nrow(changes), 4] else NA_character_,
        boundary = str_match(ordinance, regex(paste0(ordinance_area, "(.*?),?\\s*", to_those_of),
          ignore_case = TRUE))[, 2],
        ordinance_text = chunk)) |>
      mutate(map_number = case_when(is.na(heading_map) ~ change_map, is.na(change_map) | heading_map == change_map ~
          heading_map), from_districts = district_codes(from_text, reports$meeting_date[r]),
        to_districts = district_codes(to_text, reports$meeting_date[r]))
  }))
}))

# 4. Hand checks: entries read from the printed pages (adjudication/journal_parse_checks.csv) must be parsed the same,
# an ordinance with the Council's action and a report's note with its kind. Each check names its file and page and
# the entry's application number or the start of its filer's name; where one filer has several entries on the page,
# the map number picks the entry.
checks <- read_csv("../adjudication/journal_parse_checks.csv", show_col_types = FALSE,
  col_types = cols(.default = col_character())) |>
  filter(file %in% meeting_text$file)
checked_entry <- function(table, file, page, key, map) {
  rows <- if (table == "introductions") {
    rows <- filter(introductions, file == .env$file, page == as.integer(.env$page), str_starts(name, fixed(key)))
    if (nrow(rows) > 1) rows <- filter(rows, map_number == .env$map)
    rows
  } else if (table == "ordinances") {
    filter(ordinances, file == .env$file, page == as.integer(.env$page), application_number == key)
  } else {
    filter(report_notes, file == .env$file, application_number == key) |> mutate(action = note)
  }
  stopifnot(nrow(rows) == 1)
  select(rows, any_of(c("map_number", "from_districts", "to_districts", "common_address", "action")))
}
parsed <- bind_rows(purrr::pmap(select(checks, table, file, page, key, map = map_number), checked_entry))
agrees <- function(a, b) coalesce(a, "") == coalesce(b, "")
entry_agrees <- agrees(parsed$map_number, checks$map_number) & agrees(parsed$from_districts, checks$from_districts) &
  agrees(parsed$to_districts, checks$to_districts)
mismatch <- !(if_else(checks$table == "report_notes", agrees(parsed$action, checks$action), entry_agrees) &
  (checks$table != "introductions" | agrees(parsed$common_address, checks$common_address)) &
  (checks$table != "ordinances" | agrees(parsed$action, checks$action)))
if (any(mismatch)) print(bind_cols(select(checks, table, file, page, key)[mismatch, ], parsed[mismatch, ]))
stopifnot(!any(mismatch))

SaveData(introductions, c("file", "position"), sprintf("../output/journal_introductions_%s.csv", year))
SaveData(ordinances, c("file", "position"), sprintf("../output/journal_ordinances_%s.csv", year))
SaveData(report_notes, c("file", "application_number", "note"), sprintf("../output/journal_report_notes_%s.csv", year))
