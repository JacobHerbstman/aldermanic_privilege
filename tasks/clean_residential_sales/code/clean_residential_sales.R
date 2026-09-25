# setwd("tasks/clean_residential_sales/code")
start_year <- 2006L
end_year <- 2022L

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

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
  sale_price_nominal = suppressWarnings(as.numeric(gsub("[$,]", "", sale_price)))
)]
sales[nchar(pin) == 13L, pin := paste0("0", pin)]
# Sale screens follow tasks/clean_home_sales in school_closures_house_prices. A market sale passes the county's three
# sale-quality flags and is a single-parcel, non-land sale above $10,000.
sales[, market_sale := num_parcels_sale == 1 & sale_filter_same_sale_within_365 == FALSE &
  sale_filter_less_than_10k == FALSE & sale_filter_deed_type == FALSE & is.finite(sale_price_nominal) &
  sale_price_nominal > 10000 & (is.na(sale_type) | sale_type != "LAND")]

# Quick resales are often flips whose renovation the assessor characteristics do not record: flagged, not dropped.
# Gaps use every market sale of the parcel, including foreclosure auctions removed below.
recorded_sales <- sales[market_sale == TRUE, .(row_id, pin, sale_date)]
setorder(recorded_sales, pin, sale_date, row_id)
recorded_sales[, days_since_previous_sale := as.integer(sale_date - shift(sale_date)), by = pin]
sales[recorded_sales, on = "row_id", days_since_previous_sale := i.days_since_previous_sale]
sales[, resale_within_365 := !is.na(days_since_previous_sale) & days_since_previous_sale <= 365L]

raw_records <- nrow(sales)
sales <- sales[market_sale == TRUE & year %between% c(start_year, end_year) & class %in% c(202:211, 234, 278, 295)]

# Foreclosure auctions and transfers to a lender or land bank are not market sales, and REO resales by lenders,
# servicers, securitization trustees, Fannie Mae, Freddie Mac, HUD, the VA or the land bank are sales of distressed
# homes priced by the distress rather than the location; all three are removed. (school_closures_house_prices flags
# REO resales instead; here they are 72 percent of sales without a warranty or trustee deed, at a third of the typical
# price.) Banks acting as Chicago land trustees convey ordinary sales; national banks named as trustees of mortgage
# securitizations are REO sellers. MyDec deed types (from 2013) classify a sale only when its seller name is missing.
auction_seller <- "JUDI[A-Z]* +SALE|JUDICIAL CORP|INTER ?COUNTY JUDI|KALLEN R|^JSC$|SHERIFF|SELLING OFFICER|SPECIAL COMMISSIONER"
agency <- paste0("FANNIE|FREDDIE|FEDERAL (NATIONAL|NATL) (MORTGAGE|MTG)|FEDERAL HOM|HOUSING (AND|&) URBAN|",
  "SECRETARY OF HOUSING|VETERANS AFFAIRS|SECRETARY OF VETERANS|LAND BANK|COUNTY OF COOK")
lender <- paste0(
  "(^| )(BANK|BK|BANC|MORTGAGE|MTG|FSB|REO)( |,|$)|CITIMORTGAGE|CITIBANK|LOAN SERV|SERVICING|HOMESALES INC|SAVINGS FUND|",
  "MTGLQ|PENNYMAC|REVERSE|LIQUIDATION PROP|HOME LENDERS|LENDING SERV|HOME LOANS?|(^| )LOAN LLC|INV & LOAN|",
  "RESIDENTIAL FUNDG|RESIDENTIAL FUNDING|SUTTON FUNDG|BROAD STREET FUNDING|FINANCE OF AMERICA|",
  "(HOUSEHOLD|BENEFICIAL|CONSUMER|HOME|AMERICAN GEN|WELLS FARGO|WILMINGTON|HOMECOMINGS|SELENE) FIN")
land_trustee <- "LAND TRUST|TRUST NO|TRUST NUMBER|U/T/A|TRUST AGREEMENT|AS TR|TRUSTEE|(AND|&|&AMP;) TRUST|TR [0-9]"
securitization_trustee <- paste0("DEUTSCHE|U\\.? ?S\\.? (BANK|BK)|WELLS FARGO|NEW YORK|BANK NY|HSBC|CITIBANK|WILMINGTON|",
  "CHRISTIANA|POOLING|CERTIFICATE|PASS.THROUGH|ASSET.BACKED")
auction_deeds <- c("Judicial Sale", "Sheriff's Deed", "Selling Officer's Deed", "Special Commissioner's Deed",
  "Judge's Deed", "Court Officer's Deed", "Master's Deed")
sales[, `:=`(seller = toupper(fifelse(is.na(sale_seller_name), "", sale_seller_name)),
  buyer = toupper(fifelse(is.na(sale_buyer_name), "", sale_buyer_name)))]
sales[, `:=`(
  lender_seller = grepl(agency, seller) | (grepl(lender, seller) & (!grepl(land_trustee, seller) | grepl(securitization_trustee, seller))),
  lender_buyer = grepl(agency, buyer) | (grepl(lender, buyer) & (!grepl(land_trustee, buyer) | grepl(securitization_trustee, buyer))),
  seller_missing = !grepl("[A-Z]", gsub("UNKNOWN|N/A", "", seller)),
  buyer_missing = !grepl("[A-Z]", gsub("UNKNOWN|N/A", "", buyer)))]
sales[, `:=`(
  foreclosure_auction = grepl(auction_seller, seller) | (seller_missing & mydec_deed_type %chin% auction_deeds),
  transfer_to_lender = lender_buyer | (seller_missing & mydec_deed_type %chin% "Deed in lieu of Foreclosure"),
  reo_sale = lender_seller | (seller_missing & mydec_deed_type %chin% "Special Warranty Deed"),
  # Identical buyer and seller names suggest a related-party transfer; land trustees appear on both sides of ordinary
  # sales. Flagged, not removed.
  same_party_names = !seller_missing & !buyer_missing & !grepl(land_trustee, seller) &
    gsub("[^A-Z0-9]", "", seller) == gsub("[^A-Z0-9]", "", buyer),
  # Only warranty and trustee deeds convey ordinary sales. Special warranty deeds and sales without a deed record
  # include REO resales the party names miss (sellers such as "PB IL OREO LLC" or mortgage servicers).
  warranty_or_trustee_deed = sale_deed_type %chin% c("Warranty", "Trustee"))]
# Sample flow quoted in the paper's data appendix; each removal counts sales not already removed above it, and the
# flags count retained sales.
sample_flow <- data.table(
  step = c("raw_records", "market_sales", "foreclosure_auctions", "transfers_to_lenders", "reo_resales",
    "not_warranty_or_trustee_deed", "clean_sales", "flag_resale_within_365", "flag_same_party_names"),
  count = c(raw_records, nrow(sales), sum(sales$foreclosure_auction),
    sum(sales$transfer_to_lender & !sales$foreclosure_auction),
    sum(sales$reo_sale & !sales$foreclosure_auction & !sales$transfer_to_lender),
    sum(!sales$warranty_or_trustee_deed & !sales$reo_sale & !sales$foreclosure_auction & !sales$transfer_to_lender),
    NA, NA, NA)
)
sales <- sales[foreclosure_auction == FALSE & transfer_to_lender == FALSE & reo_sale == FALSE & warranty_or_trustee_deed]
sample_flow[step == "clean_sales", count := nrow(sales)]
sample_flow[step == "flag_resale_within_365", count := sum(sales$resale_within_365)]
sample_flow[step == "flag_same_party_names", count := sum(sales$same_party_names)]

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
  num_parcels_sale,
  same_party_names,
  resale_within_365,
  days_since_previous_sale,
  sale_filter_same_sale_within_365,
  sale_filter_less_than_10k,
  sale_filter_deed_type
)]
setorder(sales, year, sale_date, pin, row_id)

SaveData(sales, c("row_id"), "../output/residential_sales_clean.parquet")
SaveData(sample_flow, "step", "../output/residential_sales_sample_flow.csv")
