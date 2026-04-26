source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

first_nonmissing_chr <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  x[1]
}

first_nonmissing_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.Date(NA))
  }
  x[1]
}

first_nonmissing_int <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_integer_)
  }
  as.integer(x[1])
}

first_nonmissing_num <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(x[1])
}

any_true_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  any(x %in% TRUE, na.rm = TRUE)
}

max_int_or_na <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) {
    return(NA_integer_)
  }
  as.integer(max(x))
}

collapse_list <- function(x) {
  x <- sort(unique(x[!is.na(x) & nzchar(x)]))
  if (length(x) == 0) {
    return(NA_character_)
  }
  paste(x, collapse = ";")
}

parse_bool <- function(x) {
  x <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    x == "true" ~ TRUE,
    x == "false" ~ FALSE,
    TRUE ~ NA
  )
}

clean_chr <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[x == ""] <- NA_character_
  x
}

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/prepare_land_transaction_sales/code")
# sales_input <- "../input/Assessor_-_Parcel_Sales_20251123.csv"
# output_events <- "../output/land_transaction_sales_pin10_events.parquet"
# output_schema <- "../output/land_transaction_sales_schema_check.csv"
# output_diag <- "../output/land_transaction_sales_cleaning_diagnostics.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(sales_input, output_events, output_schema, output_diag)
}

if (length(cli_args) != 4) {
  stop(
    "FATAL: Script requires 4 args: <sales_input_csv> <output_events_parquet> <output_schema_csv> <output_diag_csv>",
    call. = FALSE
  )
}

sales_input <- cli_args[1]
output_events <- cli_args[2]
output_schema <- cli_args[3]
output_diag <- cli_args[4]

required_columns <- c(
  "pin",
  "year",
  "sale_date",
  "sale_price",
  "sale_document_num",
  "sale_deed_type",
  "mydec_deed_type",
  "sale_seller_name",
  "sale_buyer_name",
  "sale_type",
  "is_multisale",
  "num_parcels_sale",
  "sale_filter_same_sale_within_365",
  "sale_filter_less_than_10k",
  "sale_filter_deed_type",
  "row_id"
)

message("Inspecting raw sales schema...")
sales_header <- names(
  data.table::fread(sales_input, nrows = 0L, showProgress = FALSE)
)

schema_check <- tibble(
  column_name = required_columns,
  present = required_columns %in% sales_header
)

readr::write_csv(schema_check, output_schema)

missing_required <- schema_check %>%
  filter(!present) %>%
  pull(column_name)

if (length(missing_required) > 0) {
  stop(
    sprintf(
      "Raw parcel-sales file is missing required columns: %s",
      paste(missing_required, collapse = ", ")
    ),
    call. = FALSE
  )
}

message("Reading raw parcel sales...")
sales_dt <- data.table::fread(
  sales_input,
  select = required_columns,
  colClasses = "character",
  showProgress = FALSE
)

sales_dt[, row_seq := sprintf("%09d", seq_len(.N))]
sales_dt[, pin_digits := stringr::str_remove_all(pin, "[^0-9]")]
sales_dt[!is.na(pin_digits) & nchar(pin_digits) == 0L, pin_digits := NA_character_]
sales_dt[, pin14 := data.table::fifelse(
  !is.na(pin_digits) & nchar(pin_digits) >= 10L,
  stringr::str_pad(pin_digits, width = 14, side = "left", pad = "0"),
  NA_character_
)]
sales_dt[, pin10 := substr(pin14, 1L, 10L)]
sales_dt[, `:=`(
  sale_year_raw = suppressWarnings(as.integer(year)),
  sale_date = as.Date(sale_date, format = "%B %d, %Y"),
  sale_price = suppressWarnings(as.numeric(gsub("[$,]", "", sale_price))),
  sale_document_num = clean_chr(sale_document_num),
  sale_deed_type = clean_chr(sale_deed_type),
  mydec_deed_type = clean_chr(mydec_deed_type),
  sale_seller_name = clean_chr(sale_seller_name),
  sale_buyer_name = clean_chr(sale_buyer_name),
  sale_type = clean_chr(toupper(sale_type)),
  is_multisale = parse_bool(is_multisale),
  num_parcels_sale = suppressWarnings(as.integer(num_parcels_sale)),
  sale_filter_same_sale_within_365 = parse_bool(sale_filter_same_sale_within_365),
  sale_filter_less_than_10k = parse_bool(sale_filter_less_than_10k),
  sale_filter_deed_type = parse_bool(sale_filter_deed_type),
  row_id = clean_chr(row_id)
)]

sales_dt[, sale_year := dplyr::coalesce(sale_year_raw, lubridate::year(sale_date))]
sales_dt[, `:=`(
  raw_land_sale_flag = sale_type == "LAND",
  market_deed_flag = sale_deed_type %in% c("Warranty", "Trustee"),
  sale_year_date_mismatch_flag = !is.na(sale_year_raw) &
    !is.na(sale_date) &
    sale_year_raw != lubridate::year(sale_date),
  sale_document_num_clean = data.table::fifelse(
    !is.na(sale_document_num),
    sale_document_num,
    paste0("missing_doc_", row_seq)
  ),
  sale_date_key = data.table::fifelse(
    !is.na(sale_date),
    format(sale_date, "%Y-%m-%d"),
    paste0("missing_date_", row_seq)
  ),
  sale_price_key = data.table::fifelse(
    is.finite(sale_price),
    sprintf("%.2f", sale_price),
    paste0("missing_price_", row_seq)
  )
)]

n_raw_rows_total <- nrow(sales_dt)
n_invalid_pin10 <- sales_dt[is.na(pin10) | nchar(pin10) != 10L, .N]

sales_dt <- sales_dt[!is.na(pin10) & nchar(pin10) == 10L]

message("Collapsing to pin10 sale events...")
events_dt <- sales_dt[, .(
  sale_event_id = paste(
    .BY$pin10,
    first_nonmissing_chr(sale_document_num_clean),
    first_nonmissing_chr(sale_date_key),
    first_nonmissing_chr(sale_price_key),
    sep = "__"
  ),
  sale_date = first_nonmissing_date(sale_date),
  sale_year = first_nonmissing_int(sale_year),
  sale_price = first_nonmissing_num(sale_price),
  sale_type = first_nonmissing_chr(sale_type),
  sale_deed_type = first_nonmissing_chr(sale_deed_type),
  mydec_deed_type = first_nonmissing_chr(mydec_deed_type),
  sale_seller_name = first_nonmissing_chr(sale_seller_name),
  sale_buyer_name = first_nonmissing_chr(sale_buyer_name),
  is_multisale = any_true_or_na(is_multisale),
  num_parcels_sale = max_int_or_na(num_parcels_sale),
  sale_filter_same_sale_within_365 = any_true_or_na(sale_filter_same_sale_within_365),
  sale_filter_less_than_10k = any_true_or_na(sale_filter_less_than_10k),
  sale_filter_deed_type = any_true_or_na(sale_filter_deed_type),
  raw_land_sale_flag = any_true_or_na(raw_land_sale_flag),
  market_deed_flag = any_true_or_na(market_deed_flag),
  sale_year_date_mismatch_flag = any_true_or_na(sale_year_date_mismatch_flag),
  n_raw_rows_in_event = .N,
  n_pin14_in_pin10_event = uniqueN(pin14),
  pin14_list = collapse_list(pin14)
), by = .(
  pin10,
  sale_document_num_clean,
  sale_date_key,
  sale_price_key
)]

events_dt[, `:=`(
  package_sale_flag = (is_multisale %in% TRUE) |
    (!is.na(num_parcels_sale) & num_parcels_sale > 1L),
  single_parcel_sale_flag = !is.na(num_parcels_sale) &
    num_parcels_sale == 1L &
    !(is_multisale %in% TRUE),
  positive_price_flag = !is.na(sale_price) & sale_price > 0
)]

data.table::setorder(events_dt, sale_year, sale_date, pin10, sale_document_num_clean)

if (anyDuplicated(events_dt$sale_event_id) > 0) {
  stop("Collapsed land-transaction sale events are not unique by sale_event_id.", call. = FALSE)
}

diagnostics <- tibble(
  diagnostic_group = c(
    rep("raw_rows", 9),
    rep("collapsed_events", 11)
  ),
  metric = c(
    "n_rows_total",
    "n_rows_invalid_pin10",
    "n_rows_missing_sale_year",
    "n_rows_missing_sale_date",
    "n_rows_missing_sale_price",
    "n_rows_sale_year_date_mismatch",
    "n_rows_raw_land_tag",
    "n_rows_market_deed",
    "n_rows_package_sale",
    "n_events_total",
    "n_events_missing_sale_year",
    "n_events_missing_sale_date",
    "n_events_missing_sale_price",
    "n_events_raw_land_tag",
    "n_events_market_deed",
    "n_events_package_sale",
    "n_events_collapsed_from_duplicate_rows",
    "n_events_with_multiple_pin14",
    "n_events_sale_year_date_mismatch",
    "share_events_positive_price"
  ),
  value = c(
    n_raw_rows_total,
    n_invalid_pin10,
    sales_dt[is.na(sale_year), .N],
    sales_dt[is.na(sale_date), .N],
    sales_dt[is.na(sale_price), .N],
    sales_dt[sale_year_date_mismatch_flag %in% TRUE, .N],
    sales_dt[raw_land_sale_flag %in% TRUE, .N],
    sales_dt[market_deed_flag %in% TRUE, .N],
    sales_dt[(is_multisale %in% TRUE) | (!is.na(num_parcels_sale) & num_parcels_sale > 1L), .N],
    nrow(events_dt),
    events_dt[is.na(sale_year), .N],
    events_dt[is.na(sale_date), .N],
    events_dt[is.na(sale_price), .N],
    events_dt[raw_land_sale_flag %in% TRUE, .N],
    events_dt[market_deed_flag %in% TRUE, .N],
    events_dt[package_sale_flag %in% TRUE, .N],
    events_dt[n_raw_rows_in_event > 1L, .N],
    events_dt[n_pin14_in_pin10_event > 1L, .N],
    events_dt[sale_year_date_mismatch_flag %in% TRUE, .N],
    mean(events_dt$positive_price_flag, na.rm = TRUE)
  )
)

arrow::write_parquet(events_dt, output_events)
readr::write_csv(diagnostics, output_diag)

message(sprintf("Saved %s", output_events))
message(sprintf("Saved %s", output_schema))
message(sprintf("Saved %s", output_diag))
