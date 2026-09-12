# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/clean_residential_sales/code")
# start_year <- 2006
# end_year <- 2022

source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) {
  cli_args <- c(start_year, end_year)
}
if (length(cli_args) != 2) {
  stop("Script requires start and end sale years.", call. = FALSE)
}

start_year <- as.integer(cli_args[1])
end_year <- as.integer(cli_args[2])
if (any(!is.finite(c(start_year, end_year))) || start_year > end_year) {
  stop("Sale-year range is invalid.", call. = FALSE)
}

sales <- fread(
  "../input/parcel_sales_city.csv",
  colClasses = list(
    character = c("pin", "sale_date", "sale_price", "sale_document_num", "row_id")
  )
)

sales[, `:=`(
  pin = gsub("[^0-9]", "", trimws(pin)),
  year = suppressWarnings(as.integer(year)),
  class = suppressWarnings(as.integer(class)),
  sale_date = as.Date(sale_date),
  sale_price_nominal = suppressWarnings(as.numeric(gsub("[$,]", "", sale_price))),
  seller_name_upper = toupper(trimws(fifelse(is.na(sale_seller_name), "", sale_seller_name))),
  buyer_name_upper = toupper(trimws(fifelse(is.na(sale_buyer_name), "", sale_buyer_name)))
)]
sales[nchar(pin) == 13L, pin := paste0("0", pin)]
sales[, `:=`(
  seller_name_usable = !seller_name_upper %chin% c("", "-", "UNKNOWN", ".."),
  buyer_name_usable = !buyer_name_upper %chin% c("", "-", "UNKNOWN", ".."),
  seller_name_normalized = gsub("[^A-Z0-9]", "", seller_name_upper),
  buyer_name_normalized = gsub("[^A-Z0-9]", "", buyer_name_upper)
)]
sales[, normalized_same_party :=
  seller_name_usable & buyer_name_usable &
    seller_name_normalized != "" &
    seller_name_normalized == buyer_name_normalized]

sales <- sales[
  year %between% c(start_year, end_year) &
    class %in% c(202:211, 234, 278, 295) &
    is.finite(sale_price_nominal) & sale_price_nominal > 10000 &
    (is.na(sale_type) | sale_type != "LAND") &
    num_parcels_sale == 1 &
    sale_deed_type %chin% c("Warranty", "Trustee") &
    sale_filter_same_sale_within_365 == FALSE &
    sale_filter_less_than_10k == FALSE &
    sale_filter_deed_type == FALSE &
    !normalized_same_party
]

if (nrow(sales) == 0) {
  stop("No residential sales passed the canonical transaction filters.", call. = FALSE)
}
if (any(nchar(sales$pin) != 14L)) {
  stop("A cleaned residential sale has an invalid full PIN.", call. = FALSE)
}
if (anyNA(sales$sale_date) || any(year(sales$sale_date) != sales$year)) {
  stop("Cleaned residential sales require valid sale dates in the recorded sale year.", call. = FALSE)
}
if (anyNA(sales$sale_document_num) || any(trimws(sales$sale_document_num) == "")) {
  stop("A cleaned residential sale is missing its document number.", call. = FALSE)
}
if (anyDuplicated(sales$row_id) > 0) {
  stop("Cleaned residential sales must be unique by source row_id.", call. = FALSE)
}
if (anyDuplicated(sales$sale_document_num) > 0) {
  stop("Cleaned residential sales must be unique by sale document number.", call. = FALSE)
}
if (anyDuplicated(sales[, .(pin, sale_date, sale_price_nominal)]) > 0) {
  stop("Cleaned residential sales contain a duplicate PIN-date-price transaction.", call. = FALSE)
}

sales <- sales[, .(
  row_id,
  sale_document_num,
  pin,
  year,
  sale_date,
  sale_price_nominal,
  class,
  township_code,
  neighborhood_code,
  is_mydec_date,
  sale_deed_type,
  mydec_deed_type,
  sale_type,
  sale_seller_name,
  sale_buyer_name,
  seller_name_usable,
  buyer_name_usable,
  num_parcels_sale,
  sale_filter_same_sale_within_365,
  sale_filter_less_than_10k,
  sale_filter_deed_type
)]
setorder(sales, year, sale_date, pin, row_id)

SaveData(sales, c("row_id"), "../output/residential_sales_clean.parquet")
