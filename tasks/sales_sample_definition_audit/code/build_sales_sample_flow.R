# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_sample_definition_audit/code")

source("../../setup_environment/code/packages.R")

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
  sale_date = as.IDate(sale_date),
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

flow <- data.table(
  stage = "Transaction file",
  restriction = "Raw Assessor parcel-sale rows, eight Chicago townships, 2006--2022",
  retained = nrow(sales),
  dropped = NA_integer_
)

keep <- sales$year %between% c(2006L, 2022L) &
  sales$class %in% c(202:211, 234, 278, 295)
keep[is.na(keep)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Transaction file",
  restriction = "Keep Assessor residential classes 202--211, 234, 278, and 295",
  retained = sum(keep),
  dropped = flow$retained[nrow(flow)] - sum(keep)
))

previous_n <- sum(keep)
keep <- keep & is.finite(sales$sale_price_nominal) & sales$sale_price_nominal > 10000
keep[is.na(keep)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Transaction file",
  restriction = "Require nominal sale price above $10,000",
  retained = sum(keep),
  dropped = previous_n - sum(keep)
))

previous_n <- sum(keep)
keep <- keep & (is.na(sales$sale_type) | sales$sale_type != "LAND")
keep[is.na(keep)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Transaction file",
  restriction = "Exclude records explicitly identified as land-only sales",
  retained = sum(keep),
  dropped = previous_n - sum(keep)
))

previous_n <- sum(keep)
keep <- keep & sales$num_parcels_sale == 1
keep[is.na(keep)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Transaction file",
  restriction = "Require a single-parcel deed",
  retained = sum(keep),
  dropped = previous_n - sum(keep)
))

previous_n <- sum(keep)
keep <- keep & sales$sale_deed_type %chin% c("Warranty", "Trustee")
keep[is.na(keep)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Transaction file",
  restriction = "Require a warranty or trustee deed",
  retained = sum(keep),
  dropped = previous_n - sum(keep)
))

previous_n <- sum(keep)
keep <- keep &
  sales$sale_filter_same_sale_within_365 == FALSE &
  sales$sale_filter_less_than_10k == FALSE &
  sales$sale_filter_deed_type == FALSE
keep[is.na(keep)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Transaction file",
  restriction = "Pass all three Assessor sale-quality flags",
  retained = sum(keep),
  dropped = previous_n - sum(keep)
))

previous_n <- sum(keep)
keep <- keep & !sales$normalized_same_party
keep[is.na(keep)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Transaction file",
  restriction = "Exclude normalized same-party transfers; missing party names remain eligible",
  retained = sum(keep),
  dropped = previous_n - sum(keep)
))

clean_sales <- as.data.table(read_parquet(
  "../input/residential_sales_clean.parquet",
  col_select = c("row_id", "pin", "year")
))
clean_sales[, row_id := as.character(row_id)]
if (
  nrow(clean_sales) != sum(keep) ||
    !setequal(clean_sales$row_id, sales$row_id[keep])
) {
  stop("Sample-flow transaction filters do not reproduce the canonical clean-sales file.", call. = FALSE)
}

sales_locations <- fread(
  "../input/sales_pre_scores.csv",
  select = c("row_id", "coordinate_source", "ward", "neighbor_ward", "ward_pair_id", "dist_m")
)
sales_locations[, row_id := as.character(row_id)]
if (
  nrow(sales_locations) != nrow(clean_sales) ||
    !setequal(sales_locations$row_id, clean_sales$row_id) ||
    any(sales_locations$coordinate_source != "historical_exact_pin_year") ||
    any(is.na(sales_locations$ward)) ||
    any(is.na(sales_locations$neighbor_ward)) ||
    any(is.na(sales_locations$ward_pair_id) | sales_locations$ward_pair_id == "") ||
    any(!is.finite(sales_locations$dist_m))
) {
  stop("Canonical clean sales do not all have exact historical locations and ward-boundary assignments.", call. = FALSE)
}

improvements <- as.data.table(read_parquet(
  "../input/residential_improvements_panel.parquet"
))
improvements[, `:=`(
  pin = as.character(pin),
  hedonic_tax_year = tax_year
)]
if (anyDuplicated(improvements[, .(pin, tax_year)]) > 0L) {
  stop("Residential improvements must be unique by PIN-tax year.", call. = FALSE)
}
setkey(improvements, pin, tax_year)

sales_properties <- improvements[
  clean_sales,
  on = .(pin, tax_year = year),
  roll = FALSE
]

keep_property <- !is.na(sales_properties$hedonic_tax_year)
keep_property[is.na(keep_property)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Comparable-property panel",
  restriction = "Match an Assessor improvement record for the same full PIN and tax year",
  retained = sum(keep_property),
  dropped = nrow(clean_sales) - sum(keep_property)
))

previous_n <- sum(keep_property)
keep_property <- keep_property &
  sales_properties$num_buildings == 1 &
  !sales_properties$is_multibuilding
keep_property[is.na(keep_property)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Comparable-property panel",
  restriction = "Require exactly one recorded residential building",
  retained = sum(keep_property),
  dropped = previous_n - sum(keep_property)
))

previous_n <- sum(keep_property)
keep_property <- keep_property &
  is.finite(sales_properties$tieback_proration_rate) &
  abs(sales_properties$tieback_proration_rate - 1) < 0.000001
keep_property[is.na(keep_property)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Comparable-property panel",
  restriction = "Require a non-prorated PIN (tieback proration rate equals one)",
  retained = sum(keep_property),
  dropped = previous_n - sum(keep_property)
))

previous_n <- sum(keep_property)
keep_property <- keep_property &
  is.finite(sales_properties$building_sqft) &
  sales_properties$building_sqft > 0
keep_property[is.na(keep_property)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Comparable-property panel",
  restriction = "Require positive recorded building area",
  retained = sum(keep_property),
  dropped = previous_n - sum(keep_property)
))

sales_with_hedonics <- as.data.table(read_parquet(
  "../input/sales_with_hedonics.parquet",
  col_select = "row_id"
))
sales_with_hedonics[, row_id := as.character(row_id)]
if (
  nrow(sales_with_hedonics) != sum(keep_property) ||
    !setequal(sales_with_hedonics$row_id, sales_properties$row_id[keep_property])
) {
  stop("Sample-flow property filters do not reproduce the comparable sales panel.", call. = FALSE)
}

analysis_sales <- as.data.table(read_parquet(
  "../input/production_sales_with_hedonics_amenities.parquet"
))
analysis_sales[, `:=`(
  row_id = as.character(row_id),
  sale_date = as.IDate(sale_date),
  signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
  score_tie = strictness_own == strictness_neighbor
)]
if (
  nrow(analysis_sales) != nrow(sales_with_hedonics) ||
    !setequal(analysis_sales$row_id, sales_with_hedonics$row_id)
) {
  stop("Amenity enrichment changed the comparable sales panel.", call. = FALSE)
}

keep_analysis <-
  is.finite(analysis_sales$signed_dist_ft) &
  is.finite(analysis_sales$strictness_own) &
  is.finite(analysis_sales$strictness_neighbor) &
  !is.na(analysis_sales$score_tie) &
  !analysis_sales$score_tie
keep_analysis[is.na(keep_analysis)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Boundary-price regression",
  restriction = "Require finite, unequal stringency scores on both sides of the assigned boundary",
  retained = sum(keep_analysis),
  dropped = nrow(analysis_sales) - sum(keep_analysis)
))

previous_n <- sum(keep_analysis)
keep_analysis <- keep_analysis &
  abs(analysis_sales$signed_dist_ft) < 1500 &
  !is.na(analysis_sales$segment_id) &
  analysis_sales$segment_id != "" &
  !is.na(analysis_sales$ward_pair_id) &
  analysis_sales$ward_pair_id != "" &
  is.finite(analysis_sales$longitude) &
  is.finite(analysis_sales$latitude)
keep_analysis[is.na(keep_analysis)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Boundary-price regression",
  restriction = "Keep sales within 1,500 ft and assign the nearest 1,320-ft boundary segment",
  retained = sum(keep_analysis),
  dropped = previous_n - sum(keep_analysis)
))

control_columns <- c(
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage",
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)
previous_n <- sum(keep_analysis)
for (control_i in control_columns) {
  keep_analysis <- keep_analysis & is.finite(analysis_sales[[control_i]])
}
keep_analysis[is.na(keep_analysis)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Boundary-price regression",
  restriction = "Require all logged property characteristics and amenity-distance controls",
  retained = sum(keep_analysis),
  dropped = previous_n - sum(keep_analysis)
))

previous_n <- sum(keep_analysis)
keep_analysis <- keep_analysis & abs(analysis_sales$signed_dist_ft) < 500
keep_analysis[is.na(keep_analysis)] <- FALSE
flow <- rbind(flow, data.table(
  stage = "Boundary-price regression",
  restriction = "Keep the preferred 500-ft boundary window",
  retained = sum(keep_analysis),
  dropped = previous_n - sum(keep_analysis)
))

price_estimates <- fread("../input/price_boundary_property_type_fe_estimates.csv")
preferred_n <- price_estimates[
  market == "sales" & property_type_fe == TRUE,
  unique(n)
]
if (length(preferred_n) != 1L || preferred_n != sum(keep_analysis)) {
  stop("Sample-flow count does not match the preferred property-class-FE estimate.", call. = FALSE)
}

fwrite(flow, "../output/sales_sample_flow.csv")

latex_rows <- character()
for (stage_i in unique(flow$stage)) {
  latex_rows <- c(
    latex_rows,
    "\\addlinespace",
    sprintf("\\multicolumn{3}{l}{\\textit{%s}} \\\\", stage_i)
  )
  stage_rows <- flow[stage == stage_i]
  for (row_i in seq_len(nrow(stage_rows))) {
    retained_label <- format(stage_rows$retained[row_i], big.mark = ",", scientific = FALSE)
    dropped_label <- if (is.na(stage_rows$dropped[row_i])) {
      "--"
    } else {
      format(stage_rows$dropped[row_i], big.mark = ",", scientific = FALSE)
    }
    restriction_label <- gsub("\\$", "\\\\$", stage_rows$restriction[row_i])
    latex_rows <- c(
      latex_rows,
      sprintf(
        "\\raggedright %s & %s & %s \\tabularnewline",
        restriction_label,
        retained_label,
        dropped_label
      )
    )
  }
}

writeLines(
  c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Home-Sales Sample Construction}",
    "\\label{tab:sales_sample_flow}",
    "\\footnotesize",
    "\\begin{threeparttable}",
    "\\begin{tabular}{p{0.58\\textwidth}@{\\hspace{1.25em}}r@{\\hspace{1.25em}}r}",
    "\\toprule",
    "Restriction & Retained & Dropped \\\\",
    "\\midrule",
    latex_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    "\\item Notes: Counts are cumulative within the production home-sales pipeline. The transaction-clean file is unique by sale document number. The 1,500-ft row is the common support used to construct the main and shifted-cutoff samples; the preferred estimate uses the final 500-ft sample. No price observations are winsorized.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  ),
  "../output/sales_sample_flow.tex"
)
