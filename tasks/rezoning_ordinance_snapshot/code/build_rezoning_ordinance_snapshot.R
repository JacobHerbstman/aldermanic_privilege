#!/usr/bin/env Rscript

# Build a rezoning-focused Councilmatic snapshot and outputs:
# - ../output/chicago_council.db (subset sqlite snapshot for rezoning workflow)
# - ../output/rezoning_ordinance_panel.csv
# - ../output/rezoning_ordinance_documents.csv

source("../../setup_environment/code/packages.R")

ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but could not be loaded.", pkg))
  }
}

ensure_package("RSQLite")
ensure_package("jsonlite")
ensure_package("httr2")

`%||%` <- function(a, b) if (is.null(a)) b else a

as_chr <- function(df, col) {
  if (is.null(col) || length(col) == 0 || is.na(col) || !(col %in% names(df))) {
    return(rep(NA_character_, nrow(df)))
  }
  as.character(df[[col]])
}

escape_sql_string <- function(x) {
  stringr::str_replace_all(as.character(x), "'", "''")
}

chunk_vec <- function(x, chunk_size = 80) {
  if (length(x) == 0) return(list())
  split(x, ceiling(seq_along(x) / chunk_size))
}

pick_primary <- function(name_vec, primary_vec) {
  if (length(name_vec) == 0) return(NA_character_)
  idx <- which(primary_vec %in% TRUE)
  if (length(idx) > 0) return(name_vec[[idx[[1]]]])
  name_vec[[1]]
}

base_url <- Sys.getenv("COUNCILMATIC_DATASETTE_BASE", unset = "https://puddle.datamade.us/chicago_council")
snapshot_path <- "../output/chicago_council.db"
bill_limit <- suppressWarnings(as.integer(Sys.getenv("REZONING_BILL_LIMIT", unset = "0")))
if (is.na(bill_limit) || bill_limit < 0) bill_limit <- 0

datasette_get <- function(path, query = list(), max_tries = 5) {
  req <- httr2::request(path) %>%
    httr2::req_user_agent("aldermanic-privilege-rezoning/1.0") %>%
    httr2::req_retry(
      max_tries = max_tries,
      retry_on_failure = TRUE,
      backoff = function(attempt) min(20, 2 ^ attempt)
    ) %>%
    httr2::req_timeout(seconds = 120)

  if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)
  resp <- httr2::req_perform(req)
  txt <- httr2::resp_body_string(resp)
  jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)
}

rows_to_tibble <- function(rows) {
  if (is.null(rows)) return(tibble::tibble())
  if (is.data.frame(rows)) return(tibble::as_tibble(rows))
  if (is.list(rows)) return(tibble::as_tibble(rows))
  tibble::tibble()
}

fetch_table_where <- function(table_name, where_clause = NULL, max_rows = 0) {
  endpoint <- paste0(base_url, "/", table_name, ".json")
  next_token <- NULL
  out <- list()
  done <- FALSE

  while (!done) {
    q <- list(`_size` = "max")
    if (!is.null(where_clause) && nzchar(where_clause)) q$`_where` <- where_clause
    if (!is.null(next_token) && nzchar(next_token)) q$`_next` <- next_token

    obj <- datasette_get(endpoint, query = q)
    rows <- rows_to_tibble(obj$rows)
    if (nrow(rows) > 0) out[[length(out) + 1]] <- rows

    n_so_far <- if (length(out) == 0) 0 else sum(vapply(out, nrow, integer(1)))
    if (max_rows > 0 && n_so_far >= max_rows) {
      done <- TRUE
    } else {
      next_token <- as.character(obj[["next"]] %||% "")
      done <- !nzchar(next_token)
    }
  }

  all_rows <- dplyr::bind_rows(out)
  if (max_rows > 0 && nrow(all_rows) > max_rows) {
    all_rows <- dplyr::slice_head(all_rows, n = max_rows)
  }
  all_rows
}

fetch_table_by_ids <- function(table_name, id_col, ids, chunk_size = 80) {
  ids <- unique(stats::na.omit(as.character(ids)))
  if (length(ids) == 0) return(tibble::tibble())

  parts <- lapply(chunk_vec(ids, chunk_size), function(ch) {
    q <- paste(sprintf("'%s'", escape_sql_string(ch)), collapse = ", ")
    where_clause <- sprintf("%s in (%s)", id_col, q)
    fetch_table_where(table_name, where_clause = where_clause, max_rows = 0)
  })

  dplyr::bind_rows(parts) %>%
    distinct()
}

rezoning_where <- paste(
  "lower(classification) like '%ordinance%'",
  "and (lower(subject) like '%zoning reclassification%'",
  "or lower(title) like '%rezoning%'",
  "or lower(title) like '%map amendment%')"
)

message("Fetching rezoning bills from Datasette...")
bill <- fetch_table_where("bill", where_clause = rezoning_where, max_rows = bill_limit)
if (nrow(bill) == 0) stop("No rezoning bills found from Datasette API.")

bill_ids <- as_chr(bill, "id")
session_ids <- as_chr(bill, "legislative_session_id")

message("Fetching legislative sessions...")
legislative_session <- fetch_table_by_ids("legislativesession", "id", session_ids)

message("Fetching sponsorship rows...")
billsponsorship <- fetch_table_by_ids("billsponsorship", "bill_id", bill_ids)
person_ids <- as_chr(billsponsorship, "person_id")

message("Fetching person rows...")
person <- fetch_table_by_ids("person", "id", person_ids)

message("Fetching bill document rows...")
billdocument <- fetch_table_by_ids("billdocument", "bill_id", bill_ids)
document_ids <- as_chr(billdocument, "id")

message("Fetching bill document links...")
billdocumentlink <- fetch_table_by_ids("billdocumentlink", "document_id", document_ids)

message("Fetching bill version rows...")
billversion <- fetch_table_by_ids("billversion", "bill_id", bill_ids)
version_ids <- as_chr(billversion, "id")

message("Fetching bill version links...")
billversionlink <- fetch_table_by_ids("billversionlink", "version_id", version_ids)

message("Fetching bill source rows...")
billsource <- fetch_table_by_ids("billsource", "bill_id", bill_ids)

if (file.exists(snapshot_path)) file.remove(snapshot_path)
con <- DBI::dbConnect(RSQLite::SQLite(), snapshot_path)
on.exit(DBI::dbDisconnect(con), add = TRUE)

snapshot_tables <- list(
  bill = bill,
  legislativesession = legislative_session,
  billsponsorship = billsponsorship,
  person = person,
  billdocument = billdocument,
  billdocumentlink = billdocumentlink,
  billversion = billversion,
  billversionlink = billversionlink,
  billsource = billsource
)

for (nm in names(snapshot_tables)) {
  DBI::dbWriteTable(con, nm, as.data.frame(snapshot_tables[[nm]]), overwrite = TRUE)
}

rezoning_bill <- bill %>%
  transmute(
    bill_id = as_chr(., "id"),
    identifier = as_chr(., "identifier"),
    title = as_chr(., "title"),
    classification = as_chr(., "classification"),
    subject = as_chr(., "subject"),
    status = as_chr(., "status"),
    introduced_date = dplyr::coalesce(as_chr(., "introduced_date"), as_chr(., "created_at")),
    created_date = as_chr(., "created_at"),
    updated_date = as_chr(., "updated_at"),
    passed_date = as_chr(., "passed_date"),
    legislative_session_id = as_chr(., "legislative_session_id"),
    extras = as_chr(., "extras")
  ) %>%
  distinct(bill_id, .keep_all = TRUE)

if (nrow(legislative_session) > 0 && all(c("id", "identifier") %in% names(legislative_session))) {
  rezoning_bill <- rezoning_bill %>%
    left_join(
      legislative_session %>%
        transmute(
          legislative_session_id = as_chr(., "id"),
          legislative_session = as_chr(., "identifier"),
          session_start_date = as_chr(., "start_date"),
          session_end_date = as_chr(., "end_date")
        ),
      by = "legislative_session_id"
    )
}

sponsor_rollup <- tibble::tibble(
  bill_id = character(),
  primary_sponsor = character(),
  sponsor_names = character(),
  n_sponsors = integer()
)

if (nrow(billsponsorship) > 0 && "bill_id" %in% names(billsponsorship)) {
  sponsor_df <- billsponsorship %>%
    transmute(
      bill_id = as_chr(., "bill_id"),
      person_id = as_chr(., "person_id"),
      sponsor_name_raw = as_chr(., "name"),
      sponsor_role = dplyr::coalesce(as_chr(., "classification"), as_chr(., "role")),
      sponsor_primary_raw = stringr::str_to_lower(as_chr(., "primary"))
    )

  if (nrow(person) > 0 && all(c("id", "name") %in% names(person))) {
    sponsor_df <- sponsor_df %>%
      left_join(
        person %>%
          transmute(
            person_id = as_chr(., "id"),
            person_name = as_chr(., "name")
          ),
        by = "person_id"
      )
  } else {
    sponsor_df <- sponsor_df %>% mutate(person_name = NA_character_)
  }

  sponsor_df <- sponsor_df %>%
    mutate(
      sponsor_name = dplyr::coalesce(person_name, sponsor_name_raw),
      sponsor_is_primary = sponsor_primary_raw %in% c("1", "true", "t", "yes") |
        stringr::str_detect(sponsor_role, regex("primary", ignore_case = TRUE))
    ) %>%
    filter(!is.na(bill_id), !is.na(sponsor_name), sponsor_name != "")

  if (nrow(sponsor_df) > 0) {
    sponsor_rollup <- sponsor_df %>%
      arrange(bill_id, desc(sponsor_is_primary), sponsor_name) %>%
      group_by(bill_id) %>%
      summarise(
        primary_sponsor = pick_primary(sponsor_name, sponsor_is_primary),
        sponsor_names = paste(unique(sponsor_name), collapse = " | "),
        n_sponsors = dplyr::n_distinct(sponsor_name),
        .groups = "drop"
      )
  }
}

doc_links <- tibble::tibble()
if (nrow(billdocument) > 0 && nrow(billdocumentlink) > 0) {
  doc_links <- billdocument %>%
    transmute(
      document_id = as_chr(., "id"),
      bill_id = as_chr(., "bill_id")
    ) %>%
    inner_join(
      billdocumentlink %>%
        transmute(
          document_id = as_chr(., "document_id"),
          source_row_id = as_chr(., "id"),
          source_url = as_chr(., "url"),
          source_note = dplyr::coalesce(as_chr(., "text"), as_chr(., "note")),
          source_media_type = dplyr::coalesce(as_chr(., "media_type"), as_chr(., "mimetype"))
        ),
      by = "document_id"
    ) %>%
    transmute(
      bill_id,
      source_table = "billdocumentlink",
      source_row_id,
      source_url,
      source_note,
      source_media_type
    )
}

version_links <- tibble::tibble()
if (nrow(billversion) > 0 && nrow(billversionlink) > 0) {
  version_links <- billversion %>%
    transmute(
      version_id = as_chr(., "id"),
      bill_id = as_chr(., "bill_id")
    ) %>%
    inner_join(
      billversionlink %>%
        transmute(
          version_id = as_chr(., "version_id"),
          source_row_id = as_chr(., "id"),
          source_url = as_chr(., "url"),
          source_note = dplyr::coalesce(as_chr(., "text"), as_chr(., "note")),
          source_media_type = dplyr::coalesce(as_chr(., "media_type"), as_chr(., "mimetype"))
        ),
      by = "version_id"
    ) %>%
    transmute(
      bill_id,
      source_table = "billversionlink",
      source_row_id,
      source_url,
      source_note,
      source_media_type
    )
}

source_links <- tibble::tibble()
if (nrow(billsource) > 0 && "bill_id" %in% names(billsource)) {
  source_links <- billsource %>%
    transmute(
      bill_id = as_chr(., "bill_id"),
      source_table = "billsource",
      source_row_id = as_chr(., "id"),
      source_url = dplyr::coalesce(as_chr(., "url"), as_chr(., "link"), as_chr(., "uri")),
      source_note = dplyr::coalesce(as_chr(., "note"), as_chr(., "title"), as_chr(., "text")),
      source_media_type = dplyr::coalesce(as_chr(., "media_type"), as_chr(., "mimetype"))
    )
}

rezoning_doc_links <- bind_rows(doc_links, version_links, source_links) %>%
  mutate(
    bill_id = as_chr(., "bill_id"),
    source_url = stringr::str_squish(source_url),
    is_pdf = stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(source_url, "")), "\\.pdf(\\?|$)") |
      stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(source_media_type, "")), "pdf")
  ) %>%
  filter(!is.na(bill_id), !is.na(source_url), source_url != "") %>%
  semi_join(rezoning_bill %>% select(bill_id), by = "bill_id") %>%
  distinct()

link_rollup <- rezoning_doc_links %>%
  arrange(bill_id, desc(is_pdf), source_table, source_url) %>%
  group_by(bill_id) %>%
  summarise(
    n_links = n(),
    n_pdf_links = sum(is_pdf, na.rm = TRUE),
    first_pdf_url = if (any(is_pdf, na.rm = TRUE)) source_url[which(is_pdf)[1]] else NA_character_,
    first_link_url = source_url[[1]],
    all_pdf_urls = paste(unique(source_url[is_pdf]), collapse = " | "),
    .groups = "drop"
  )

rezoning_panel <- rezoning_bill %>%
  left_join(sponsor_rollup, by = "bill_id") %>%
  left_join(link_rollup, by = "bill_id") %>%
  arrange(introduced_date, bill_id)

readr::write_csv(rezoning_panel, "../output/rezoning_ordinance_panel.csv")
readr::write_csv(rezoning_doc_links, "../output/rezoning_ordinance_documents.csv")

message("Wrote: ../output/chicago_council.db")
message("Wrote: ../output/rezoning_ordinance_panel.csv")
message("Wrote: ../output/rezoning_ordinance_documents.csv")
message("Rezoning bills in panel: ", nrow(rezoning_panel))
