source("../../setup_environment/code/packages.R")
options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/parcel_land_value_panel/code")
# history_dir <- "../input/assessor_history"
# assessor_current_dir <- "../input/assessor_current"
# parcels_input <- "../input/parcels_current.csv"
# out_panel <- "../output/parcel_land_value_panel.parquet"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(history_dir, assessor_current_dir, parcels_input, out_panel)
}

if (length(cli_args) != 4) {
  stop(
    "FATAL: Script requires 4 args: <assessor_history_dir> <assessor_current_dir> <parcels_current_csv> <out_panel_parquet>",
    call. = FALSE
  )
}

history_dir <- cli_args[1]
assessor_current_dir <- cli_args[2]
parcels_input <- cli_args[3]
out_panel <- cli_args[4]

if (!dir.exists(history_dir)) {
  stop(sprintf("Assessor history directory not found: %s", history_dir), call. = FALSE)
}

if (!dir.exists(assessor_current_dir)) {
  stop(sprintf("Current assessor directory not found: %s", assessor_current_dir), call. = FALSE)
}

if (!file.exists(parcels_input)) {
  stop(sprintf("Parcel universe input not found: %s", parcels_input), call. = FALSE)
}

history_files <- list.files(history_dir, pattern = "\\.csv$", full.names = TRUE)
history_files <- history_files[!grepl("\\.DS_Store$", history_files)]
if (length(history_files) == 0) {
  stop(sprintf("No assessor history CSV files found in %s.", history_dir), call. = FALSE)
}

assessor_current_files <- list.files(assessor_current_dir, pattern = "\\.csv$", full.names = TRUE)
assessor_current_files <- assessor_current_files[!grepl("\\.DS_Store$", assessor_current_files)]
if (length(assessor_current_files) == 0) {
  stop(sprintf("No assessor current CSV files found in %s.", assessor_current_dir), call. = FALSE)
}

head_bin <- Sys.which("head")
if (!nzchar(head_bin)) {
  stop("Could not find the system `head` command needed to preflight raw CSV inputs.", call. = FALSE)
}

touch_csv <- function(path, label, retries = 3) {
  for (attempt in seq_len(retries)) {
    status <- suppressWarnings(system2(head_bin, c("-c", "1", path), stdout = FALSE, stderr = FALSE))
    if (identical(status, 0L)) {
      return(invisible(NULL))
    }

    if (attempt < retries) {
      Sys.sleep(attempt)
    }
  }

  stop(
    sprintf("Could not read %s CSV after %d attempts: %s", label, retries, path),
    call. = FALSE
  )
}

message(sprintf("Preflighting %s assessor history CSV files...", length(history_files)))
invisible(lapply(history_files, touch_csv, label = "assessor history"))

message(sprintf("Preflighting %s current assessor CSV files...", length(assessor_current_files)))
invisible(lapply(assessor_current_files, touch_csv, label = "current assessor"))

normalize_names <- function(x) {
  x %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}

choose_col <- function(schema_df, candidates, patterns = NULL, label, required = TRUE) {
  idx_exact <- match(candidates, schema_df$norm_name)
  idx_exact <- idx_exact[!is.na(idx_exact)]
  if (length(idx_exact) > 0) {
    return(schema_df$column_name[idx_exact[1]])
  }

  if (!is.null(patterns)) {
    for (pattern_i in patterns) {
      idx_pattern <- which(grepl(pattern_i, schema_df$norm_name))
      if (length(idx_pattern) > 0) {
        return(schema_df$column_name[idx_pattern[1]])
      }
    }
  }

  if (!required) {
    return(NA_character_)
  }

  stop(
    sprintf(
      "Could not find a '%s' column in assessor history files. Available normalized names include: %s",
      label,
      paste(utils::head(schema_df$norm_name, 40), collapse = ", ")
    ),
    call. = FALSE
  )
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit({
  DBI::dbDisconnect(con, shutdown = TRUE)
}, add = TRUE)

history_files_sql <- paste(as.character(DBI::dbQuoteString(con, history_files)), collapse = ", ")
assessor_current_files_sql <- paste(as.character(DBI::dbQuoteString(con, assessor_current_files)), collapse = ", ")

history_scan_sql <- paste0(
  "read_csv_auto([",
  history_files_sql,
  "], union_by_name = TRUE, all_varchar = TRUE, sample_size = 20000)"
)

assessor_current_scan_sql <- paste0(
  "read_csv_auto([",
  assessor_current_files_sql,
  "], union_by_name = TRUE, all_varchar = TRUE, sample_size = 20000)"
)

message("Inspecting assessor history schema...")
history_header <- names(
  data.table::fread(history_files[1], nrows = 0L, showProgress = FALSE)
)

history_schema <- tibble(
  column_name = history_header,
  norm_name = normalize_names(history_header)
)

pin_col <- choose_col(
  history_schema,
  c("pin", "sa_parcel_nbr_primary"),
  patterns = c("^pin$", "^sa_parcel_nbr_primary$"),
  label = "pin"
)
history_year_col <- choose_col(
  history_schema,
  c("ah_history_yr", "history_year"),
  patterns = c("^ah_history_yr$", "^history_.*year$"),
  label = "history year",
  required = FALSE
)
tax_year_col <- choose_col(
  history_schema,
  c("tax_year", "taxyear"),
  patterns = c("^tax_year$", "^taxyear$"),
  label = "tax_year",
  required = FALSE
)
assr_year_col <- choose_col(
  history_schema,
  c("assr_year", "year"),
  patterns = c("^assr_year$", "^year$"),
  label = "assessment year",
  required = FALSE
)
class_col <- choose_col(
  history_schema,
  c("class", "property_class", "property_class_code", "use_code_std"),
  patterns = c("^class$", "^property_class", "class_code$", "^use_code_std$"),
  label = "class/use code",
  required = FALSE
)
land_col <- choose_col(
  history_schema,
  c("certified_land", "certified_land_value", "land_certified", "sa_val_assd_land"),
  patterns = c("^certified_.*land", "^land_.*certified", "^sa_val_assd_land$"),
  label = "certified land"
)
bldg_col <- choose_col(
  history_schema,
  c("certified_bldg", "certified_building", "certified_building_value", "building_certified", "sa_val_assd_imprv"),
  patterns = c("^certified_.*(bldg|building)", "^(bldg|building)_.*certified", "^sa_val_assd_imprv$"),
  label = "certified building"
)

message("Inspecting current assessor schema for lot size...")
assessor_current_header <- names(
  data.table::fread(assessor_current_files[1], nrows = 0L, showProgress = FALSE)
)

assessor_current_schema <- tibble(
  column_name = assessor_current_header,
  norm_name = normalize_names(assessor_current_header)
)

assessor_pin_col <- choose_col(
  assessor_current_schema,
  c("parcelnumberraw"),
  patterns = c("^parcelnumberraw$"),
  label = "raw parcel number"
)
assessor_alt_pin_col <- choose_col(
  assessor_current_schema,
  c("parcelnumberalternate"),
  patterns = c("^parcelnumberalternate$"),
  label = "alternate parcel number",
  required = FALSE
)
lot_sqft_col <- choose_col(
  assessor_current_schema,
  c("arealotsf", "sa_lotsize"),
  patterns = c("^arealotsf$", "^sa_lotsize$"),
  label = "lot square feet"
)
property_use_group_col <- choose_col(
  assessor_current_schema,
  c("propertyusegroup"),
  patterns = c("^propertyusegroup$"),
  label = "property use group",
  required = FALSE
)
property_use_standardized_col <- choose_col(
  assessor_current_schema,
  c("propertyusestandardized"),
  patterns = c("^propertyusestandardized$"),
  label = "property use standardized",
  required = FALSE
)
property_use_muni_col <- choose_col(
  assessor_current_schema,
  c("propertyusemuni"),
  patterns = c("^propertyusemuni$"),
  label = "property use muni",
  required = FALSE
)
zoned_code_local_col <- choose_col(
  assessor_current_schema,
  c("zonedcodelocal"),
  patterns = c("^zonedcodelocal$"),
  label = "zoned code local",
  required = FALSE
)
owner_type_description1_col <- choose_col(
  assessor_current_schema,
  c("ownertypedescription1"),
  patterns = c("^ownertypedescription1$"),
  label = "owner type description 1",
  required = FALSE
)
tax_exemption_homeowner_col <- choose_col(
  assessor_current_schema,
  c("taxexemptionhomeownerflag"),
  patterns = c("^taxexemptionhomeownerflag$"),
  label = "homeowner exemption flag",
  required = FALSE
)
tax_exemption_disabled_col <- choose_col(
  assessor_current_schema,
  c("taxexemptiondisabledflag"),
  patterns = c("^taxexemptiondisabledflag$"),
  label = "disabled exemption flag",
  required = FALSE
)
tax_exemption_senior_col <- choose_col(
  assessor_current_schema,
  c("taxexemptionseniorflag"),
  patterns = c("^taxexemptionseniorflag$"),
  label = "senior exemption flag",
  required = FALSE
)
tax_exemption_veteran_col <- choose_col(
  assessor_current_schema,
  c("taxexemptionveteranflag"),
  patterns = c("^taxexemptionveteranflag$"),
  label = "veteran exemption flag",
  required = FALSE
)
tax_exemption_widow_col <- choose_col(
  assessor_current_schema,
  c("taxexemptionwidowflag"),
  patterns = c("^taxexemptionwidowflag$"),
  label = "widow exemption flag",
  required = FALSE
)
tax_exemption_additional_col <- choose_col(
  assessor_current_schema,
  c("taxexemptionadditional"),
  patterns = c("^taxexemptionadditional$"),
  label = "additional exemption text",
  required = FALSE
)
shell_record_col <- choose_col(
  assessor_current_schema,
  c("parcelshellrecord"),
  patterns = c("^parcelshellrecord$"),
  label = "parcel shell record flag",
  required = FALSE
)

sql_ident <- function(x) {
  as.character(DBI::dbQuoteIdentifier(con, x))
}

trim_expr <- function(col_name) {
  if (is.na(col_name) || !nzchar(col_name)) {
    return("NULL")
  }
  sprintf("NULLIF(TRIM(%s), '')", sql_ident(col_name))
}

upper_trim_expr <- function(col_name) {
  if (is.na(col_name) || !nzchar(col_name)) {
    return("NULL")
  }
  sprintf("NULLIF(UPPER(TRIM(%s)), '')", sql_ident(col_name))
}

int_expr <- function(col_name) {
  sprintf("TRY_CAST(%s AS INTEGER)", trim_expr(col_name))
}

double_expr <- function(col_name) {
  sprintf("TRY_CAST(%s AS DOUBLE)", trim_expr(col_name))
}

logical_flag_expr <- function(col_name) {
  if (is.na(col_name) || !nzchar(col_name)) {
    return("NULL")
  }

  upper_trimmed <- sprintf("UPPER(COALESCE(%s, ''))", trim_expr(col_name))

  paste(
    "CASE",
    sprintf("WHEN %s IN ('1', 'Y', 'YES', 'TRUE', 'T') THEN TRUE", upper_trimmed),
    sprintf("WHEN %s IN ('0', 'N', 'NO', 'FALSE', 'F') THEN FALSE", upper_trimmed),
    sprintf("WHEN %s <> '' THEN TRUE", upper_trimmed),
    "ELSE NULL END"
  )
}

create_modal_text_table <- function(source_table, value_col, out_table, out_col) {
  value_n_values <- paste0(out_col, "_n_values")
  value_n_top_ties <- paste0(out_col, "_n_top_ties")
  value_rank <- paste0(out_col, "_rank")

  DBI::dbExecute(
    con,
    paste(
      "CREATE OR REPLACE TEMP TABLE", out_table, "AS",
      "WITH clean AS (",
      "  SELECT pin10,", value_col, "AS value",
      "  FROM", source_table,
      "  WHERE pin10 IS NOT NULL",
      "    AND", value_col, "IS NOT NULL",
      "),",
      "value_freq AS (",
      "  SELECT pin10, value, COUNT(*) AS n_records",
      "  FROM clean",
      "  GROUP BY 1, 2",
      "),",
      "value_summary AS (",
      "  SELECT pin10, COUNT(*) AS", value_n_values, ", MAX(n_records) AS max_n_records",
      "  FROM value_freq",
      "  GROUP BY 1",
      "),",
      "value_ranked AS (",
      "  SELECT",
      "    f.pin10,",
      "    f.value,",
      "    f.n_records,",
      "    s.", value_n_values, ",",
      "    SUM(CASE WHEN f.n_records = s.max_n_records THEN 1 ELSE 0 END) OVER (PARTITION BY f.pin10) AS", value_n_top_ties, ",",
      "    ROW_NUMBER() OVER (PARTITION BY f.pin10 ORDER BY f.n_records DESC, f.value DESC) AS", value_rank,
      "  FROM value_freq f",
      "  INNER JOIN value_summary s USING (pin10)",
      ")",
      "SELECT",
      "  pin10,",
      "  CASE",
      "    WHEN", value_n_values, "= 1 THEN value",
      "    WHEN", value_n_top_ties, "= 1 THEN value",
      "    ELSE NULL",
      "  END AS", out_col, ",",
      "  CASE",
      "    WHEN", value_n_values, "= 1 THEN 'exact'",
      "    WHEN", value_n_top_ties, "= 1 THEN 'modal'",
      "    ELSE 'unresolved_tie'",
      "  END AS", paste0(out_col, "_resolution"), ",",
      " ", value_n_values, ",",
      " ", value_n_top_ties,
      "FROM value_ranked",
      "WHERE", value_rank, "= 1"
    )
  )
}

year_component_expr <- function(col_name) {
  if (is.na(col_name) || !nzchar(col_name)) {
    return(NULL)
  }
  sprintf("NULLIF(%s, 0)", int_expr(col_name))
}

year_component_minus_one_expr <- function(col_name) {
  if (is.na(col_name) || !nzchar(col_name)) {
    return(NULL)
  }
  base_expr <- year_component_expr(col_name)
  sprintf(
    "CASE WHEN %s IS NOT NULL THEN %s - 1 ELSE NULL END",
    base_expr,
    base_expr
  )
}

pin_expr <- trim_expr(pin_col)
tax_year_admin_parts <- Filter(
  Negate(is.null),
  list(
    year_component_expr(tax_year_col),
    year_component_expr(assr_year_col),
    year_component_minus_one_expr(history_year_col)
  )
)

if (length(tax_year_admin_parts) == 0) {
  stop("Could not identify any usable year column in assessor history files.", call. = FALSE)
}

tax_year_expr <- sprintf("COALESCE(%s)", paste(tax_year_admin_parts, collapse = ", "))

tax_year_source_expr <- paste(
  "CASE",
  if (!is.na(tax_year_col) && nzchar(tax_year_col)) {
    sprintf("WHEN %s IS NOT NULL THEN 'taxyear'", year_component_expr(tax_year_col))
  } else {
    NULL
  },
  if (!is.na(assr_year_col) && nzchar(assr_year_col)) {
    sprintf("WHEN %s IS NOT NULL THEN 'assr_year'", year_component_expr(assr_year_col))
  } else {
    NULL
  },
  if (!is.na(history_year_col) && nzchar(history_year_col)) {
    sprintf("WHEN %s IS NOT NULL THEN 'ah_history_yr_minus_1'", year_component_minus_one_expr(history_year_col))
  } else {
    NULL
  },
  "ELSE NULL END"
)
class_expr <- trim_expr(class_col)
land_expr <- double_expr(land_col)
bldg_expr <- double_expr(bldg_col)

lot_sqft_expr <- double_expr(lot_sqft_col)
assessor_pin_expr <- trim_expr(assessor_pin_col)
assessor_alt_pin_expr <- trim_expr(assessor_alt_pin_col)
shell_record_expr <- int_expr(shell_record_col)
property_use_group_expr <- upper_trim_expr(property_use_group_col)
property_use_standardized_expr <- trim_expr(property_use_standardized_col)
property_use_muni_expr <- upper_trim_expr(property_use_muni_col)
zoned_code_local_expr <- upper_trim_expr(zoned_code_local_col)
owner_type_description1_expr <- upper_trim_expr(owner_type_description1_col)
tax_exemption_homeowner_expr <- logical_flag_expr(tax_exemption_homeowner_col)
tax_exemption_disabled_expr <- logical_flag_expr(tax_exemption_disabled_col)
tax_exemption_senior_expr <- logical_flag_expr(tax_exemption_senior_col)
tax_exemption_veteran_expr <- logical_flag_expr(tax_exemption_veteran_col)
tax_exemption_widow_expr <- logical_flag_expr(tax_exemption_widow_col)
tax_exemption_additional_expr <- trim_expr(tax_exemption_additional_col)

message("Creating cleaned assessor history view...")
DBI::dbExecute(
  con,
  paste(
    "CREATE OR REPLACE TEMP VIEW history_selected AS",
    "SELECT",
    sprintf("%s AS pin_raw,", pin_expr),
    sprintf("%s AS tax_year,", tax_year_expr),
    sprintf("%s AS tax_year_source,", tax_year_source_expr),
    sprintf("%s AS class_value,", class_expr),
    sprintf("%s AS certified_land,", land_expr),
    sprintf("%s AS certified_bldg", bldg_expr),
    "FROM",
    history_scan_sql
  )
)

message("Creating current assessor use/zoning/lot-size view...")
DBI::dbExecute(
  con,
  paste(
    "CREATE OR REPLACE TEMP VIEW assessor_current_selected AS",
    "SELECT",
    sprintf("  %s AS parcel_raw,", assessor_pin_expr),
    sprintf("  %s AS parcel_alternate,", assessor_alt_pin_expr),
    sprintf("  %s AS lot_sqft_raw,", lot_sqft_expr),
    sprintf("  %s AS property_use_group_raw,", property_use_group_expr),
    sprintf("  %s AS property_use_standardized_raw,", property_use_standardized_expr),
    sprintf("  %s AS property_use_muni_raw,", property_use_muni_expr),
    sprintf("  %s AS zoned_code_local_raw,", zoned_code_local_expr),
    sprintf("  %s AS owner_type_description1_raw,", owner_type_description1_expr),
    sprintf("  %s AS homeowner_exemption_flag_raw,", tax_exemption_homeowner_expr),
    sprintf("  %s AS disabled_exemption_flag_raw,", tax_exemption_disabled_expr),
    sprintf("  %s AS senior_exemption_flag_raw,", tax_exemption_senior_expr),
    sprintf("  %s AS veteran_exemption_flag_raw,", tax_exemption_veteran_expr),
    sprintf("  %s AS widow_exemption_flag_raw,", tax_exemption_widow_expr),
    sprintf("  %s AS tax_exemption_additional_raw,", tax_exemption_additional_expr),
    sprintf("  %s AS shell_record", shell_record_expr),
    "FROM",
    assessor_current_scan_sql
  )
)

message("Collapsing assessor history to pin10 x tax_year...")
DBI::dbExecute(
  con,
  paste(
    "CREATE OR REPLACE TEMP TABLE history_pin10_year AS",
    "SELECT",
    "  SUBSTR(pin_raw, 1, 10) AS pin10,",
    "  tax_year,",
    "  CASE",
    "    WHEN COUNT(DISTINCT tax_year_source) FILTER (WHERE tax_year_source IS NOT NULL) = 1 THEN MIN(tax_year_source)",
    "    WHEN COUNT(DISTINCT tax_year_source) FILTER (WHERE tax_year_source IS NOT NULL) > 1 THEN 'mixed'",
    "    ELSE NULL",
    "  END AS tax_year_source,",
    "  COUNT(DISTINCT tax_year_source) FILTER (WHERE tax_year_source IS NOT NULL) AS tax_year_source_n,",
    "  MIN(class_value) AS class_primary,",
    "  COUNT(DISTINCT class_value) FILTER (WHERE class_value IS NOT NULL) AS class_n,",
    "  COUNT(DISTINCT pin_raw) AS n_pins,",
    "  SUM(COALESCE(certified_land, 0)) AS land_sum,",
    "  SUM(COALESCE(certified_bldg, 0)) AS bldg_sum",
    "FROM history_selected",
    "WHERE pin_raw IS NOT NULL",
    "  AND tax_year IS NOT NULL",
    "  AND tax_year BETWEEN 2001 AND 2023",
    "  AND (certified_land IS NOT NULL OR certified_bldg IS NOT NULL)",
    "GROUP BY 1, 2"
  )
)

message("Normalizing current assessor fields to pin10...")
DBI::dbExecute(
  con,
  paste(
    "CREATE OR REPLACE TEMP TABLE assessor_current_pin10_clean AS",
    "SELECT",
    "  SUBSTR(",
    "    REGEXP_REPLACE(",
    "      COALESCE(parcel_alternate, parcel_raw),",
    "      '[^0-9]',",
    "      '',",
    "      'g'",
    "    ),",
    "    1,",
    "    10",
    "  ) AS pin10,",
    "  lot_sqft_raw AS lot_sqft,",
    "  property_use_group_raw AS property_use_group,",
    "  property_use_standardized_raw AS property_use_standardized,",
    "  property_use_muni_raw AS property_use_muni,",
    "  zoned_code_local_raw AS zoned_code_local,",
    "  owner_type_description1_raw AS owner_type_description1,",
    "  homeowner_exemption_flag_raw AS homeowner_exemption_flag,",
    "  disabled_exemption_flag_raw AS disabled_exemption_flag,",
    "  senior_exemption_flag_raw AS senior_exemption_flag,",
    "  veteran_exemption_flag_raw AS veteran_exemption_flag,",
    "  widow_exemption_flag_raw AS widow_exemption_flag,",
    "  tax_exemption_additional_raw AS tax_exemption_additional,",
    "  CASE",
    "    WHEN COALESCE(homeowner_exemption_flag_raw, FALSE)",
    "      OR COALESCE(disabled_exemption_flag_raw, FALSE)",
    "      OR COALESCE(senior_exemption_flag_raw, FALSE)",
    "      OR COALESCE(veteran_exemption_flag_raw, FALSE)",
    "      OR COALESCE(widow_exemption_flag_raw, FALSE)",
    "      OR tax_exemption_additional_raw IS NOT NULL THEN TRUE",
    "    ELSE FALSE",
    "  END AS any_exemption_current,",
    "  shell_record",
    "FROM assessor_current_selected",
    "WHERE COALESCE(parcel_alternate, parcel_raw) IS NOT NULL"
  )
)

message("Collapsing current assessor lot sizes to pin10...")
DBI::dbExecute(
  con,
  paste(
    "CREATE OR REPLACE TEMP TABLE lot_size_pin10 AS",
    "WITH clean AS (",
    "  SELECT pin10, lot_sqft",
    "  FROM assessor_current_pin10_clean",
    "  WHERE pin10 IS NOT NULL",
    "    AND LENGTH(pin10) = 10",
    "    AND lot_sqft IS NOT NULL",
    "    AND lot_sqft > 0",
    "    AND (shell_record IS NULL OR shell_record = 0)",
    "),",
    "lot_freq AS (",
    "  SELECT pin10, lot_sqft, COUNT(*) AS n_records",
    "  FROM clean",
    "  GROUP BY 1, 2",
    "),",
    "lot_summary AS (",
    "  SELECT",
    "    pin10,",
    "    COUNT(*) AS lot_sqft_n_values,",
    "    MAX(n_records) AS max_n_records",
    "  FROM lot_freq",
    "  GROUP BY 1",
    "),",
    "lot_ranked AS (",
    "  SELECT",
    "    f.pin10,",
    "    f.lot_sqft,",
    "    f.n_records,",
    "    s.lot_sqft_n_values,",
    "    SUM(CASE WHEN f.n_records = s.max_n_records THEN 1 ELSE 0 END) OVER (PARTITION BY f.pin10) AS lot_sqft_n_top_ties,",
    "    ROW_NUMBER() OVER (PARTITION BY f.pin10 ORDER BY f.n_records DESC, f.lot_sqft DESC) AS lot_sqft_rank",
    "  FROM lot_freq f",
    "  INNER JOIN lot_summary s USING (pin10)",
    ")",
    "SELECT",
    "  pin10,",
    "  CASE",
    "    WHEN lot_sqft_n_values = 1 THEN lot_sqft",
    "    WHEN lot_sqft_n_top_ties = 1 THEN lot_sqft",
    "    ELSE NULL",
    "  END AS lot_sqft_current,",
    "  CASE",
    "    WHEN lot_sqft_n_values = 1 THEN 'exact'",
    "    WHEN lot_sqft_n_top_ties = 1 THEN 'modal'",
    "    ELSE 'unresolved_tie'",
    "  END AS lot_sqft_resolution,",
    "  lot_sqft_n_values,",
    "  lot_sqft_n_top_ties",
    "FROM lot_ranked",
    "WHERE lot_sqft_rank = 1"
  )
)

message("Collapsing current assessor use/zoning fields to pin10...")
create_modal_text_table("assessor_current_pin10_clean", "property_use_group", "property_use_group_pin10", "property_use_group_current")
create_modal_text_table("assessor_current_pin10_clean", "property_use_standardized", "property_use_standardized_pin10", "property_use_standardized_current")
create_modal_text_table("assessor_current_pin10_clean", "property_use_muni", "property_use_muni_pin10", "property_use_muni_current")
create_modal_text_table("assessor_current_pin10_clean", "zoned_code_local", "zoned_code_local_pin10", "zoned_code_local_current")
create_modal_text_table("assessor_current_pin10_clean", "owner_type_description1", "owner_type_description1_pin10", "owner_type_description1_current")

DBI::dbExecute(
  con,
  paste(
    "CREATE OR REPLACE TEMP TABLE any_exemption_pin10 AS",
    "SELECT",
    "  pin10,",
    "  CASE",
    "    WHEN MAX(CASE WHEN any_exemption_current THEN 1 ELSE 0 END) = 1 THEN TRUE",
    "    WHEN COUNT(*) FILTER (WHERE any_exemption_current IS NOT NULL) > 0 THEN FALSE",
    "    ELSE NULL",
    "  END AS any_exemption_current,",
    "  COUNT(DISTINCT any_exemption_current) FILTER (WHERE any_exemption_current IS NOT NULL) AS any_exemption_current_n_values",
    "FROM assessor_current_pin10_clean",
    "WHERE pin10 IS NOT NULL",
    "  AND LENGTH(pin10) = 10",
    "  AND (shell_record IS NULL OR shell_record = 0)",
    "GROUP BY 1"
  )
)

message("Reading current parcel universe and creating pin10 parcel-geometry lookup...")
parcel_cols <- c(
  "pin10", "class", "triad_name", "township_name", "municipality_name",
  "longitude", "latitude", "centroid_x_crs_3435", "centroid_y_crs_3435"
)

parcels_current <- data.table::fread(
  parcels_input,
  select = parcel_cols,
  colClasses = "character",
  fill = TRUE,
  showProgress = FALSE
)

parcels_current <- as_tibble(parcels_current) %>%
  mutate(
    pin10 = stringr::str_sub(stringr::str_trim(pin10), 1, 10),
    class = na_if(stringr::str_trim(class), ""),
    triad_name = na_if(stringr::str_trim(triad_name), ""),
    township_name = na_if(stringr::str_trim(township_name), ""),
    municipality_name = na_if(stringr::str_trim(municipality_name), ""),
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude = suppressWarnings(as.numeric(latitude)),
    centroid_x_crs_3435 = suppressWarnings(as.numeric(centroid_x_crs_3435)),
    centroid_y_crs_3435 = suppressWarnings(as.numeric(centroid_y_crs_3435))
  ) %>%
  filter(!is.na(pin10), pin10 != "")

coord_conflicts <- parcels_current %>%
  group_by(pin10) %>%
  summarise(
    n_lon = n_distinct(longitude[!is.na(longitude)]),
    n_lat = n_distinct(latitude[!is.na(latitude)]),
    n_x = n_distinct(centroid_x_crs_3435[!is.na(centroid_x_crs_3435)]),
    n_y = n_distinct(centroid_y_crs_3435[!is.na(centroid_y_crs_3435)]),
    .groups = "drop"
  ) %>%
  filter(n_lon > 1 | n_lat > 1 | n_x > 1 | n_y > 1)

if (nrow(coord_conflicts) > 0) {
  stop(
    sprintf(
      "Parcel universe has %d pin10 values with conflicting coordinates. Fix upstream instead of silently collapsing.",
      nrow(coord_conflicts)
    ),
    call. = FALSE
  )
}

parcel_pin10 <- parcels_current %>%
  group_by(pin10) %>%
  summarise(
    class_current = dplyr::first(na.omit(class), default = NA_character_),
    triad_name = dplyr::first(na.omit(triad_name), default = NA_character_),
    township_name = dplyr::first(na.omit(township_name), default = NA_character_),
    municipality_name = dplyr::first(na.omit(municipality_name), default = NA_character_),
    longitude = dplyr::first(na.omit(longitude), default = NA_real_),
    latitude = dplyr::first(na.omit(latitude), default = NA_real_),
    centroid_x_crs_3435 = dplyr::first(na.omit(centroid_x_crs_3435), default = NA_real_),
    centroid_y_crs_3435 = dplyr::first(na.omit(centroid_y_crs_3435), default = NA_real_),
    .groups = "drop"
  )

DBI::dbWriteTable(con, "parcel_pin10", parcel_pin10, temporary = TRUE, overwrite = TRUE)

history_counts <- DBI::dbGetQuery(
  con,
  "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT pin10) AS n_pin10, MIN(tax_year) AS min_tax_year, MAX(tax_year) AS max_tax_year FROM history_pin10_year"
)
message(sprintf(
  "Collapsed assessor history rows: %s | distinct pin10: %s | tax years: %s-%s",
  format(history_counts$n_rows[[1]], big.mark = ","),
  format(history_counts$n_pin10[[1]], big.mark = ","),
  history_counts$min_tax_year[[1]],
  history_counts$max_tax_year[[1]]
))

tax_year_source_counts <- DBI::dbGetQuery(
  con,
  paste(
    "SELECT tax_year_source, COUNT(*) AS n_rows",
    "FROM history_selected",
    "WHERE pin_raw IS NOT NULL",
    "  AND tax_year IS NOT NULL",
    "  AND tax_year BETWEEN 2001 AND 2023",
    "  AND (certified_land IS NOT NULL OR certified_bldg IS NOT NULL)",
    "GROUP BY tax_year_source",
    "ORDER BY n_rows DESC"
  )
)

message(
  paste0(
    "Administrative year source usage: ",
    paste(
      sprintf(
        "%s=%s",
        tax_year_source_counts$tax_year_source,
        format(tax_year_source_counts$n_rows, big.mark = ",")
      ),
      collapse = "; "
    )
  )
)

joined_counts <- DBI::dbGetQuery(
  con,
  paste(
    "SELECT",
    "  COUNT(*) AS n_rows,",
    "  COUNT(DISTINCT h.pin10) AS n_pin10,",
    "  SUM(CASE WHEN p.longitude IS NOT NULL AND p.latitude IS NOT NULL AND p.centroid_x_crs_3435 IS NOT NULL AND p.centroid_y_crs_3435 IS NOT NULL THEN 1 ELSE 0 END) AS n_with_coords",
    "FROM history_pin10_year h",
    "LEFT JOIN parcel_pin10 p USING (pin10)"
  )
)
message(sprintf(
  "Rows with current parcel coordinates: %s / %s",
  format(joined_counts$n_with_coords[[1]], big.mark = ","),
  format(joined_counts$n_rows[[1]], big.mark = ",")
))

lot_size_counts <- DBI::dbGetQuery(
  con,
  paste(
    "SELECT",
    "  COUNT(*) AS n_pin10,",
    "  SUM(CASE WHEN lot_sqft_current IS NOT NULL THEN 1 ELSE 0 END) AS n_with_lot_sqft,",
    "  SUM(CASE WHEN lot_sqft_resolution = 'exact' THEN 1 ELSE 0 END) AS n_exact,",
    "  SUM(CASE WHEN lot_sqft_resolution = 'modal' THEN 1 ELSE 0 END) AS n_modal,",
    "  SUM(CASE WHEN lot_sqft_resolution = 'unresolved_tie' THEN 1 ELSE 0 END) AS n_unresolved",
    "FROM lot_size_pin10"
  )
)
message(sprintf(
  paste(
    "Collapsed current lot sizes by pin10: %s rows, %s with usable lot sqft,",
    "%s exact, %s modal, %s unresolved ties."
  ),
  format(lot_size_counts$n_pin10[[1]], big.mark = ","),
  format(lot_size_counts$n_with_lot_sqft[[1]], big.mark = ","),
  format(lot_size_counts$n_exact[[1]], big.mark = ","),
  format(lot_size_counts$n_modal[[1]], big.mark = ","),
  format(lot_size_counts$n_unresolved[[1]], big.mark = ",")
))

property_use_counts <- DBI::dbGetQuery(
  con,
  paste(
    "SELECT",
    "  COUNT(*) AS n_pin10,",
    "  SUM(CASE WHEN property_use_group_current IS NOT NULL THEN 1 ELSE 0 END) AS n_with_property_use_group,",
    "  SUM(CASE WHEN property_use_group_current = 'VACANT LAND' THEN 1 ELSE 0 END) AS n_vacant_land_group,",
    "  SUM(CASE WHEN property_use_group_current_resolution = 'unresolved_tie' THEN 1 ELSE 0 END) AS n_unresolved",
    "FROM property_use_group_pin10"
  )
)
message(sprintf(
  paste(
    "Collapsed property-use group by pin10: %s rows, %s with nonmissing group,",
    "%s in VACANT LAND, %s unresolved ties."
  ),
  format(property_use_counts$n_pin10[[1]], big.mark = ","),
  format(property_use_counts$n_with_property_use_group[[1]], big.mark = ","),
  format(property_use_counts$n_vacant_land_group[[1]], big.mark = ","),
  format(property_use_counts$n_unresolved[[1]], big.mark = ",")
))

message("Writing parcel land-value panel parquet...")
DBI::dbExecute(
  con,
  paste(
    "COPY (",
    "  SELECT",
    "    h.pin10,",
    "    h.tax_year,",
    "    h.tax_year_source,",
    "    h.tax_year_source_n,",
    "    h.class_primary,",
    "    h.class_n,",
    "    h.n_pins,",
    "    h.land_sum,",
    "    h.bldg_sum,",
    "    h.land_sum + h.bldg_sum AS total_av,",
    "    CASE",
    "      WHEN h.land_sum + h.bldg_sum > 0 THEN h.land_sum / (h.land_sum + h.bldg_sum)",
    "      ELSE NULL",
    "    END AS land_share_pin10,",
    "    CASE",
    "      WHEN h.land_sum > 0 THEN LN(h.land_sum)",
    "      ELSE NULL",
    "    END AS log_land_sum,",
    "    l.lot_sqft_current,",
    "    l.lot_sqft_resolution,",
    "    l.lot_sqft_n_values,",
    "    l.lot_sqft_n_top_ties,",
    "    CASE",
    "      WHEN l.lot_sqft_current > 0 THEN h.land_sum / l.lot_sqft_current",
    "      ELSE NULL",
    "    END AS land_psf,",
    "    CASE",
    "      WHEN h.land_sum > 0 AND l.lot_sqft_current > 0 THEN LN(h.land_sum / l.lot_sqft_current)",
    "      ELSE NULL",
    "    END AS log_land_psf,",
    "    CASE",
    "      WHEN h.bldg_sum = 0 THEN TRUE",
    "      ELSE FALSE",
    "    END AS land_only,",
    "    p.class_current,",
    "    pug.property_use_group_current,",
    "    pug.property_use_group_current_resolution,",
    "    pug.property_use_group_current_n_values,",
    "    pug.property_use_group_current_n_top_ties,",
    "    pus.property_use_standardized_current,",
    "    pus.property_use_standardized_current_resolution,",
    "    pus.property_use_standardized_current_n_values,",
    "    pus.property_use_standardized_current_n_top_ties,",
    "    pum.property_use_muni_current,",
    "    pum.property_use_muni_current_resolution,",
    "    pum.property_use_muni_current_n_values,",
    "    pum.property_use_muni_current_n_top_ties,",
    "    zcl.zoned_code_local_current,",
    "    zcl.zoned_code_local_current_resolution,",
    "    zcl.zoned_code_local_current_n_values,",
    "    zcl.zoned_code_local_current_n_top_ties,",
    "    otd.owner_type_description1_current,",
    "    otd.owner_type_description1_current_resolution,",
    "    otd.owner_type_description1_current_n_values,",
    "    otd.owner_type_description1_current_n_top_ties,",
    "    ex.any_exemption_current,",
    "    ex.any_exemption_current_n_values,",
    "    p.triad_name,",
    "    p.township_name,",
    "    p.municipality_name,",
    "    p.longitude,",
    "    p.latitude,",
    "    p.centroid_x_crs_3435,",
    "    p.centroid_y_crs_3435",
    "  FROM history_pin10_year h",
    "  LEFT JOIN parcel_pin10 p USING (pin10)",
    "  LEFT JOIN lot_size_pin10 l USING (pin10)",
    "  LEFT JOIN property_use_group_pin10 pug USING (pin10)",
    "  LEFT JOIN property_use_standardized_pin10 pus USING (pin10)",
    "  LEFT JOIN property_use_muni_pin10 pum USING (pin10)",
    "  LEFT JOIN zoned_code_local_pin10 zcl USING (pin10)",
    "  LEFT JOIN owner_type_description1_pin10 otd USING (pin10)",
    "  LEFT JOIN any_exemption_pin10 ex USING (pin10)",
    "  WHERE p.longitude IS NOT NULL",
    "    AND p.latitude IS NOT NULL",
    "    AND p.centroid_x_crs_3435 IS NOT NULL",
    "    AND p.centroid_y_crs_3435 IS NOT NULL",
    "  ORDER BY h.pin10, h.tax_year",
    ") TO",
    as.character(DBI::dbQuoteString(con, out_panel)),
    "(FORMAT PARQUET, COMPRESSION ZSTD)"
  )
)

message(sprintf("Saved %s", out_panel))
