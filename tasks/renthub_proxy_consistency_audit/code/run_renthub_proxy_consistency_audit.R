# Audit RentHub property/floorplan proxies before rental geometry and RD.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/renthub_proxy_consistency_audit/code")
# start_date <- "2014-01-01"
# end_date <- "2022-12-31"

source("../../setup_environment/code/packages.R")

library(DBI)
library(duckdb)
library(data.table)
library(sf)

sf_use_s2(FALSE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_date, end_date)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <start_date> <end_date>", call. = FALSE)
}

start_date <- as.Date(cli_args[1])
end_date <- as.Date(cli_args[2])
if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
  stop("start_date and end_date must define a valid date window.", call. = FALSE)
}

address_location_radius_ft <- 200
primary_location_share_cutoff <- 0.85
far_secondary_share_cutoff <- 0.10
far_secondary_distance_ft <- 500
manual_review_top_n <- 250L

sql_escape <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

clean_address_stem_r <- function(x) {
  x <- toupper(trimws(x))
  x <- gsub("[.,]", "", x)
  x <- gsub("\\bNORTH\\b", "N", x)
  x <- gsub("\\bSOUTH\\b", "S", x)
  x <- gsub("\\bEAST\\b", "E", x)
  x <- gsub("\\bWEST\\b", "W", x)
  x <- gsub(
    " (STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|PLACE|PLAZA|PLZ|PL|COURT|CT|DRIVE|DR|TERRACE|TER|LANE|LN)$",
    "",
    x
  )
  x <- gsub(" +", " ", trimws(x))
  fifelse(x == "", NA_character_, x)
}

assign_location_groups <- function(x_ft, y_ft, radius_ft) {
  n <- length(x_ft)
  if (n == 1L) {
    return(1L)
  }
  as.integer(cutree(
    hclust(dist(cbind(x_ft, y_ft)), method = "complete"),
    h = radius_ft
  ))
}

raw_glob <- "../input/renthub_raw/*.parquet"
rent_panel_path <- "../input/chicago_rent_panel.parquet"
manual_location_path <- "manual_verified_address_locations.csv"

message("=== RentHub Proxy Consistency Audit ===")
message(sprintf("Window: %s through %s", start_date, end_date))

unlink(c(
  "../output/address_coordinate_stability.csv",
  "../output/address_location_group_stability.csv",
  "../output/unstable_address_external_review_queue.csv",
  "../output/unstable_address_external_review_workbook.xlsx",
  "../output/property_proxy_stability.csv",
  "../output/floorplan_same_day_rent_spread.csv",
  "../output/coordinate_only_pile_audit.csv",
  "../output/one_day_bulk_audit.csv",
  "../output/building_type_conflict_audit.csv",
  "../output/stale_posted_audit.csv",
  "../output/renthub_quality_flag_summary.csv",
  "../output/harrison_221_spotcheck.csv",
  "../output/chicago_rent_panel_quality_flags.parquet"
), recursive = TRUE, force = TRUE)

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")
dbExecute(
  con,
  "
  CREATE OR REPLACE MACRO clean_address_stem(address_value) AS (
    NULLIF(TRIM(REGEXP_REPLACE(
      REGEXP_REPLACE(
        REGEXP_REPLACE(
          REGEXP_REPLACE(
            REGEXP_REPLACE(
              REGEXP_REPLACE(
                REPLACE(REPLACE(UPPER(TRIM(CAST(address_value AS VARCHAR))), '.', ''), ',', ''),
                '\\bNORTH\\b', 'N'
              ),
              '\\bSOUTH\\b', 'S'
            ),
            '\\bEAST\\b', 'E'
          ),
          '\\bWEST\\b', 'W'
        ),
        ' (STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|PLACE|PLAZA|PLZ|PL|COURT|CT|DRIVE|DR|TERRACE|TER|LANE|LN)$',
        ''
      ),
      ' +', ' '
    )), '')
  )
  "
)

collect_query <- function(sql) {
  as.data.table(dbGetQuery(con, sql))
}

message("Loading cleaned monthly panel...")
dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE rent_panel AS
    SELECT
      *,
      clean_address_stem(address_norm) AS address_stem,
      PRINTF('%%.4f|%%.4f', latitude, longitude) AS panel_coord_key,
      CASE
        WHEN sqft IS NOT NULL AND sqft > 0 THEN rent_price / sqft
        ELSE NULL
      END AS rent_per_sqft
    FROM read_parquet('%s')
    ",
    sql_escape(rent_panel_path)
  )
)

panel_n <- collect_query("SELECT COUNT(*) AS n FROM rent_panel")$n[1]
panel_unique_n <- collect_query("SELECT COUNT(DISTINCT rent_panel_id) AS n FROM rent_panel")$n[1]
if (panel_n != panel_unique_n) {
  stop("chicago_rent_panel.parquet must be unique by rent_panel_id.", call. = FALSE)
}

message("Building raw keyed RentHub view...")
dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE raw_clean AS
    WITH raw_source AS (
      SELECT
        *,
        UPPER(TRIM(CAST(ID AS VARCHAR))) AS id_raw,
        UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR))) AS property_id_raw,
        UPPER(TRIM(CAST(UNIT_ID AS VARCHAR))) AS unit_id_raw,
        UPPER(TRIM(CAST(ADDRESS AS VARCHAR))) AS address_raw,
        UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) AS building_type_raw,
        UPPER(TRIM(CAST(CITY AS VARCHAR))) AS city_raw
      FROM read_parquet('%s', union_by_name = true)
      WHERE TRY_CAST(SCRAPED_TIMESTAMP AS DATE) >= CAST('%s' AS DATE)
        AND TRY_CAST(SCRAPED_TIMESTAMP AS DATE) <= CAST('%s' AS DATE)
    ),
    cleaned AS (
      SELECT
        CASE
          WHEN id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
          ELSE id_raw
        END AS id,
        CASE
          WHEN property_id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
          ELSE property_id_raw
        END AS property_id,
        CASE
          WHEN unit_id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
          ELSE unit_id_raw
        END AS unit_id,
        CASE
          WHEN address_raw IS NULL
            OR address_raw IN ('', '0', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
          ELSE NULLIF(REGEXP_REPLACE(address_raw, ' +', ' '), '')
        END AS address_norm,
        CASE
          WHEN address_raw IS NULL
            OR address_raw IN ('', '0', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN 1
          ELSE 0
        END AS address_missing,
        CASE WHEN address_raw = '0' THEN 1 ELSE 0 END AS address_zero,
        TRY_CAST(SCRAPED_TIMESTAMP AS DATE) AS file_date,
        DATE_TRUNC('month', TRY_CAST(SCRAPED_TIMESTAMP AS DATE))::DATE AS month_start,
        TRY_CAST(DATE_POSTED AS DATE) AS posted_date,
        TRY_CAST(RENT_PRICE AS DOUBLE) AS rent_price,
        TRY_CAST(BEDS AS DOUBLE) AS beds,
        TRY_CAST(BATHS AS DOUBLE) AS baths,
        TRY_CAST(SQFT AS DOUBLE) AS sqft,
        TRY_CAST(LATITUDE AS DOUBLE) AS latitude,
        TRY_CAST(LONGITUDE AS DOUBLE) AS longitude,
        CAST(BUILDING_TYPE AS VARCHAR) AS building_type,
        CAST(ZIP AS VARCHAR) AS zip,
        CAST(NEIGHBORHOOD AS VARCHAR) AS neighborhood,
        CASE
          WHEN building_type_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN 'other'
          WHEN building_type_raw = 'TH' OR building_type_raw LIKE '%%TOWN%%' THEN 'townhouse'
          WHEN building_type_raw IN ('CON', 'CONDO') OR building_type_raw LIKE '%%CONDO%%'
            OR building_type_raw LIKE '%%CONDOMINIUM%%' THEN 'condo'
          WHEN building_type_raw IN ('COMM', 'COMMERCIAL') OR building_type_raw LIKE '%%COMMERCIAL%%' THEN 'commercial'
          WHEN building_type_raw LIKE '%%MULTI%%'
            OR building_type_raw LIKE '%%APART%%'
            OR building_type_raw LIKE '%%APT%%'
            OR building_type_raw LIKE '%%DUPLEX%%'
            OR building_type_raw LIKE '%%TRIPLEX%%'
            OR building_type_raw LIKE '%%FOURPLEX%%' THEN 'multi_family'
          WHEN building_type_raw LIKE '%%SINGLE%%'
            OR building_type_raw LIKE '%%HOUSE%%'
            OR building_type_raw LIKE '%%DETACHED%%'
            OR building_type_raw LIKE '%%SFR%%' THEN 'single_family'
          ELSE 'other'
        END AS building_type_clean,
        CASE
          WHEN TRY_CAST(DATE_POSTED AS DATE) IS NULL
            OR TRY_CAST(SCRAPED_TIMESTAMP AS DATE) IS NULL THEN NULL
          ELSE DATE_DIFF('day', TRY_CAST(DATE_POSTED AS DATE), TRY_CAST(SCRAPED_TIMESTAMP AS DATE))
        END AS posted_lag_days
      FROM raw_source
      WHERE city_raw = 'CHICAGO'
    ),
    keyed_base AS (
      SELECT
        *,
        clean_address_stem(address_norm) AS address_stem,
        CASE
          WHEN latitude IS NOT NULL
            AND longitude IS NOT NULL
            AND latitude BETWEEN -90 AND 90
            AND longitude BETWEEN -180 AND 180 THEN PRINTF('%%.4f|%%.4f', latitude, longitude)
          ELSE NULL
        END AS coord_key
      FROM cleaned
      WHERE latitude BETWEEN 41.55 AND 42.10
        AND longitude BETWEEN -88.10 AND -87.40
    ),
    property_keys AS (
      SELECT
        *,
        CASE
          WHEN coord_key IS NULL THEN NULL
          ELSE COALESCE(address_norm, 'NO_ADDRESS') || '|' || coord_key
        END AS property_key
      FROM keyed_base
    )
    SELECT
      *,
      CASE
        WHEN property_key IS NULL THEN NULL
        ELSE property_key || '|' ||
          COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(building_type_clean, 'other')
      END AS floorplan_key,
      CASE
        WHEN property_key IS NULL OR rent_price IS NULL THEN NULL
        ELSE property_key || '|' ||
          COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(building_type_clean, 'other') || '|' ||
          CAST(rent_price AS VARCHAR)
      END AS rent_cell_key,
      CASE
        WHEN unit_id IS NOT NULL THEN unit_id
        WHEN property_key IS NOT NULL THEN property_key || '|' ||
          COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(building_type_clean, 'other')
        ELSE NULL
      END AS analysis_key
    FROM property_keys
    ",
    sql_escape(raw_glob),
    start_date,
    end_date
  )
)

raw_n <- collect_query("SELECT COUNT(*) AS n FROM raw_clean")$n[1]
message(sprintf(
  "Raw Chicago rows in bbox: %s | monthly panel rows: %s",
  format(raw_n, big.mark = ","),
  format(panel_n, big.mark = ",")
))

manual_locations <- fread(manual_location_path, na.strings = c("", "NA", "N/A", "NULL"))
manual_required <- c(
  "address_stem",
  "verification_status",
  "verified_latitude",
  "verified_longitude",
  "source_name",
  "source_url",
  "note"
)
if (!all(manual_required %in% names(manual_locations))) {
  stop("manual_verified_address_locations.csv is missing required columns.", call. = FALSE)
}
manual_locations[, address_stem := toupper(trimws(address_stem))]
manual_locations[, address_stem := clean_address_stem_r(address_stem)]
manual_locations[, verification_status := tolower(trimws(verification_status))]
manual_locations <- manual_locations[address_stem != "" & !is.na(address_stem)]
if (anyDuplicated(manual_locations$address_stem) > 0) {
  stop("manual_verified_address_locations.csv must be unique by address_stem.", call. = FALSE)
}
valid_manual_status <- c("verified", "exclude", "needs_review")
if (any(!manual_locations$verification_status %in% valid_manual_status)) {
  stop("Manual location verification status must be verified, exclude, or needs_review.", call. = FALSE)
}
manual_locations[, verified_latitude := suppressWarnings(as.numeric(verified_latitude))]
manual_locations[, verified_longitude := suppressWarnings(as.numeric(verified_longitude))]
verified_missing_coords <- manual_locations[
  verification_status == "verified" &
    (!is.finite(verified_latitude) | !is.finite(verified_longitude))
]
if (nrow(verified_missing_coords) > 0) {
  stop("Verified manual location rows must include finite latitude and longitude.", call. = FALSE)
}
manual_locations[, `:=`(
  manual_location_x_ft = NA_real_,
  manual_location_y_ft = NA_real_
)]
if (nrow(manual_locations[verification_status == "verified"]) > 0) {
  manual_sf <- st_as_sf(
    manual_locations[verification_status == "verified"],
    coords = c("verified_longitude", "verified_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(3435)
  manual_xy <- st_coordinates(manual_sf)
  manual_locations[
    verification_status == "verified",
    `:=`(
      manual_location_x_ft = manual_xy[, 1],
      manual_location_y_ft = manual_xy[, 2]
    )
  ]
}

message("Auditing address-coordinate stability in EPSG 3435...")
address_clusters <- collect_query(
  "
  SELECT
    address_stem,
    coord_key,
    STRING_AGG(DISTINCT address_norm, ';') AS address_variants,
    COUNT(DISTINCT address_norm) AS n_address_variants,
    QUANTILE_CONT(latitude, 0.50) AS latitude,
    QUANTILE_CONT(longitude, 0.50) AS longitude,
    COUNT(*) AS raw_rows,
    COUNT(DISTINCT file_date) AS active_days,
    MIN(file_date) AS first_seen,
    MAX(file_date) AS last_seen,
    COUNT(DISTINCT zip) FILTER (WHERE zip IS NOT NULL AND zip != '') AS n_zips,
    COUNT(DISTINCT neighborhood) FILTER (WHERE neighborhood IS NOT NULL AND neighborhood != '') AS n_neighborhoods,
    QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) AS rent_p50,
    MIN(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_min,
    MAX(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_max
  FROM raw_clean
  WHERE address_stem IS NOT NULL
    AND coord_key IS NOT NULL
  GROUP BY 1, 2
  "
)

setorder(address_clusters, address_stem, -raw_rows, coord_key)
address_clusters[, modal_coord_key := first(coord_key), by = address_stem]
address_clusters[, modal_latitude := first(latitude), by = address_stem]
address_clusters[, modal_longitude := first(longitude), by = address_stem]
address_clusters[, modal_raw_rows := first(raw_rows), by = address_stem]

cluster_sf <- st_as_sf(
  address_clusters,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)
cluster_xy <- st_coordinates(cluster_sf)
address_clusters[, `:=`(
  x_ft = cluster_xy[, 1],
  y_ft = cluster_xy[, 2]
)]
modal_sf <- st_as_sf(
  address_clusters,
  coords = c("modal_longitude", "modal_latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)
address_clusters[, address_coord_distance_ft := as.numeric(st_distance(cluster_sf, modal_sf, by_element = TRUE))]
address_clusters[, flag_address_coord_gt100ft := address_coord_distance_ft > 100]
address_clusters[, flag_address_coord_gt250ft := address_coord_distance_ft > 250]
address_clusters[, flag_address_coord_gt500ft := address_coord_distance_ft > 500]
address_clusters[, is_modal_cluster := coord_key == modal_coord_key]

address_clusters[, location_group_local := assign_location_groups(
  x_ft,
  y_ft,
  address_location_radius_ft
), by = address_stem]
address_location_groups <- address_clusters[, .(
  location_group_raw_rows = sum(raw_rows),
  location_group_coord_clusters = .N,
  location_group_latitude = weighted.mean(latitude, raw_rows),
  location_group_longitude = weighted.mean(longitude, raw_rows),
  location_group_x_ft = weighted.mean(x_ft, raw_rows),
  location_group_y_ft = weighted.mean(y_ft, raw_rows),
  first_seen = min(first_seen),
  last_seen = max(last_seen),
  address_variants = paste(sort(unique(unlist(strsplit(address_variants, ";", fixed = TRUE)))), collapse = ";")
), by = .(address_stem, location_group_local)]
setorder(address_location_groups, address_stem, -location_group_raw_rows, location_group_local)
address_location_groups[, address_total_raw_rows := sum(location_group_raw_rows), by = address_stem]
address_location_groups[, location_group_rank := seq_len(.N), by = address_stem]
address_location_groups[, location_group_share := location_group_raw_rows / address_total_raw_rows]
address_location_groups[, address_location_group_count := .N, by = address_stem]

primary_locations <- address_location_groups[location_group_rank == 1L, .(
  address_stem,
  primary_location_group_local = location_group_local,
  primary_location_latitude = location_group_latitude,
  primary_location_longitude = location_group_longitude,
  primary_location_x_ft = location_group_x_ft,
  primary_location_y_ft = location_group_y_ft,
  primary_location_raw_rows = location_group_raw_rows,
  primary_location_share = location_group_share,
  address_location_group_count
)]
second_locations <- address_location_groups[location_group_rank == 2L, .(
  address_stem,
  second_location_latitude = location_group_latitude,
  second_location_longitude = location_group_longitude,
  second_location_x_ft = location_group_x_ft,
  second_location_y_ft = location_group_y_ft,
  second_location_raw_rows = location_group_raw_rows,
  second_location_share = location_group_share
)]
location_contract <- merge(primary_locations, second_locations, by = "address_stem", all.x = TRUE)
location_contract[, primary_second_distance_ft := fifelse(
  is.finite(second_location_x_ft),
  sqrt((primary_location_x_ft - second_location_x_ft)^2 + (primary_location_y_ft - second_location_y_ft)^2),
  NA_real_
)]
location_contract[, flag_address_location_unstable_cluster := primary_location_share < primary_location_share_cutoff |
  (!is.na(second_location_share) &
    second_location_share >= far_secondary_share_cutoff &
    primary_second_distance_ft > far_secondary_distance_ft)]
location_contract <- merge(
  location_contract,
  manual_locations[, .(
    address_stem,
    verification_status,
    verified_latitude,
    verified_longitude,
    manual_location_x_ft,
    manual_location_y_ft,
    source_name,
    source_url,
    note
  )],
  by = "address_stem",
  all.x = TRUE,
  sort = FALSE
)
location_contract[, flag_manual_location_verified := verification_status == "verified"]
location_contract[is.na(flag_manual_location_verified), flag_manual_location_verified := FALSE]
location_contract[, flag_manual_location_exclude := verification_status == "exclude"]
location_contract[is.na(flag_manual_location_exclude), flag_manual_location_exclude := FALSE]
location_contract[, flag_address_location_unstable := fifelse(
  flag_manual_location_verified,
  FALSE,
  flag_manual_location_exclude | flag_address_location_unstable_cluster
)]
location_contract[, flag_address_location_stable := flag_manual_location_verified |
  (!flag_manual_location_exclude & !flag_address_location_unstable_cluster)]
location_contract[, `:=`(
  effective_location_latitude = fifelse(flag_manual_location_verified, verified_latitude, primary_location_latitude),
  effective_location_longitude = fifelse(flag_manual_location_verified, verified_longitude, primary_location_longitude),
  effective_location_x_ft = fifelse(flag_manual_location_verified, manual_location_x_ft, primary_location_x_ft),
  effective_location_y_ft = fifelse(flag_manual_location_verified, manual_location_y_ft, primary_location_y_ft)
)]

address_clusters <- merge(
  address_clusters,
  address_location_groups[, .(
    address_stem,
    location_group_local,
    location_group_rank,
    location_group_share,
    location_group_raw_rows,
    location_group_coord_clusters,
    location_group_latitude,
    location_group_longitude
  )],
  by = c("address_stem", "location_group_local"),
  all.x = TRUE,
  sort = FALSE
)
address_clusters <- merge(
  address_clusters,
  location_contract[, .(
    address_stem,
    primary_location_group_local,
    primary_location_latitude,
    primary_location_longitude,
    primary_location_x_ft,
    primary_location_y_ft,
    primary_location_share,
    flag_address_location_unstable_cluster,
    second_location_latitude,
    second_location_longitude,
    second_location_share,
    primary_second_distance_ft,
    address_location_group_count,
    verification_status,
    verified_latitude,
    verified_longitude,
    source_name,
    source_url,
    note,
    flag_manual_location_verified,
    flag_manual_location_exclude,
    effective_location_latitude,
    effective_location_longitude,
    effective_location_x_ft,
    effective_location_y_ft,
    flag_address_location_stable,
    flag_address_location_unstable
  )],
  by = "address_stem",
  all.x = TRUE,
  sort = FALSE
)
address_clusters[, distance_to_primary_location_ft := sqrt(
  (x_ft - primary_location_x_ft)^2 + (y_ft - primary_location_y_ft)^2
)]
address_clusters[, distance_to_corrected_location_ft := sqrt(
  (x_ft - effective_location_x_ft)^2 + (y_ft - effective_location_y_ft)^2
)]
address_clusters[, flag_address_location_fix_candidate := flag_address_location_stable &
  location_group_rank != 1L &
  distance_to_corrected_location_ft > address_location_radius_ft]
address_clusters[, corrected_latitude := fifelse(
  flag_address_location_stable,
  effective_location_latitude,
  latitude
)]
address_clusters[, corrected_longitude := fifelse(
  flag_address_location_stable,
  effective_location_longitude,
  longitude
)]

address_location_groups <- merge(
  address_location_groups,
  location_contract[, .(
    address_stem,
    primary_location_group_local,
    primary_location_share,
    second_location_share,
    primary_second_distance_ft,
    flag_address_location_unstable_cluster,
    verification_status,
    verified_latitude,
    verified_longitude,
    source_name,
    source_url,
    note,
    flag_manual_location_verified,
    flag_manual_location_exclude,
    flag_address_location_stable,
    flag_address_location_unstable
  )],
  by = "address_stem",
  all.x = TRUE,
  sort = FALSE
)
address_location_groups[, address_location_radius_ft := address_location_radius_ft]
address_location_groups[, primary_location_share_cutoff := primary_location_share_cutoff]
address_location_groups[, far_secondary_share_cutoff := far_secondary_share_cutoff]
address_location_groups[, far_secondary_distance_ft := far_secondary_distance_ft]
setorder(address_location_groups, address_stem, location_group_rank)
fwrite(address_location_groups, "../output/address_location_group_stability.csv")

candidate_groups <- address_location_groups[
  location_group_rank <= 5,
  .(
    candidate_location_groups = paste(
      sprintf(
        "rank=%d rows=%d share=%.3f lat=%.6f lon=%.6f first=%s last=%s",
        location_group_rank,
        location_group_raw_rows,
        location_group_share,
        location_group_latitude,
        location_group_longitude,
        first_seen,
        last_seen
      ),
      collapse = " | "
    )
  ),
  by = address_stem
]
review_queue <- merge(
  location_contract[
    flag_address_location_unstable_cluster == TRUE &
      flag_manual_location_verified == FALSE
  ],
  candidate_groups,
  by = "address_stem",
  all.x = TRUE,
  sort = FALSE
)
setorder(review_queue, -primary_location_raw_rows, address_stem)
review_queue[, manual_review_priority := seq_len(.N)]
review_queue[, suggested_search_query := paste(address_stem, "Chicago apartment building")]
review_queue[, google_search_url := paste0(
  "https://www.google.com/search?q=",
  utils::URLencode(suggested_search_query, reserved = TRUE)
)]
review_queue[, google_maps_url := paste0(
  "https://www.google.com/maps/search/?api=1&query=",
  utils::URLencode(paste(address_stem, "Chicago IL"), reserved = TRUE)
)]
review_queue[, manual_resolution_template := paste(
  address_stem,
  "verified",
  "<lat>",
  "<lon>",
  "<source_name>",
  "<source_url>",
  "<note>",
  sep = ","
)]
review_queue_export <- head(review_queue, manual_review_top_n)[
  ,
  .(
    manual_review_priority,
    address_stem,
    primary_location_raw_rows,
    primary_location_share,
    second_location_raw_rows,
    second_location_share,
    primary_second_distance_ft,
    address_location_group_count,
    verification_status,
    source_name,
    source_url,
    suggested_search_query,
    google_search_url,
    google_maps_url,
    candidate_location_groups,
    manual_resolution_template
  )
]
fwrite(review_queue_export, "../output/unstable_address_external_review_queue.csv")
write_xlsx(
  list(
    review_queue = review_queue_export,
    manual_verified_locations = manual_locations
  ),
  "../output/unstable_address_external_review_workbook.xlsx"
)

setorder(address_clusters, -address_coord_distance_ft, -raw_rows)
fwrite(address_clusters, "../output/address_coordinate_stability.csv")

dbWriteTable(
  con,
  "address_cluster_flags",
  as.data.frame(address_clusters[, .(
    address_stem,
    address_variants,
    coord_key,
    modal_coord_key,
    modal_latitude,
    modal_longitude,
    primary_location_latitude,
    primary_location_longitude,
    corrected_latitude,
    corrected_longitude,
    location_group_local,
    location_group_rank,
    location_group_share,
    primary_location_share,
    flag_address_location_unstable_cluster,
    second_location_share,
    primary_second_distance_ft,
    address_location_group_count,
    distance_to_primary_location_ft,
    distance_to_corrected_location_ft,
    verification_status,
    verified_latitude,
    verified_longitude,
    source_name,
    source_url,
    note,
    flag_manual_location_verified,
    flag_manual_location_exclude,
    address_coord_distance_ft,
    flag_address_coord_gt100ft,
    flag_address_coord_gt250ft,
    flag_address_coord_gt500ft,
    flag_address_location_stable,
    flag_address_location_unstable,
    flag_address_location_fix_candidate,
    is_modal_cluster
  )]),
  overwrite = TRUE
)

message("Auditing property rent stability...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE property_month AS
  SELECT
    property_key,
    month_start,
    COUNT(*) AS floorplan_months,
    QUANTILE_CONT(rent_price, 0.50) AS rent_p50,
    MIN(rent_price) AS rent_min,
    MAX(rent_price) AS rent_max,
    AVG(CASE WHEN rent_per_sqft < 0.5 THEN 1.0 ELSE 0.0 END) AS share_rent_per_sqft_lt050,
    AVG(CASE WHEN rent_per_sqft > 10 THEN 1.0 ELSE 0.0 END) AS share_rent_per_sqft_gt10
  FROM rent_panel
  GROUP BY 1, 2
  "
)
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE property_month_jumps AS
  SELECT
    *,
    LAG(rent_p50) OVER (PARTITION BY property_key ORDER BY month_start) AS prior_rent_p50,
    CASE
      WHEN rent_p50 > 0 AND LAG(rent_p50) OVER (PARTITION BY property_key ORDER BY month_start) > 0
        THEN ABS(LOG(rent_p50 / LAG(rent_p50) OVER (PARTITION BY property_key ORDER BY month_start)))
      ELSE NULL
    END AS abs_log_adjacent_month_jump
  FROM property_month
  "
)
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE property_proxy_stability AS
  WITH property_address AS (
    SELECT
      property_key,
      ANY_VALUE(address_norm) AS address_norm
    FROM rent_panel
    GROUP BY 1
  )
  SELECT
    p.property_key,
    ANY_VALUE(r.address_norm) AS address_norm,
    COUNT(DISTINCT p.month_start) AS active_months,
    SUM(p.floorplan_months) AS floorplan_months,
    QUANTILE_CONT(p.rent_p50, 0.01) AS rent_p01,
    QUANTILE_CONT(p.rent_p50, 0.50) AS rent_p50,
    QUANTILE_CONT(p.rent_p50, 0.99) AS rent_p99,
    MIN(p.rent_min) AS rent_min,
    MAX(p.rent_max) AS rent_max,
    MAX(p.floorplan_months) AS max_floorplan_months,
    MAX(p.abs_log_adjacent_month_jump) AS max_abs_log_adjacent_month_jump,
    EXP(MAX(p.abs_log_adjacent_month_jump)) AS max_adjacent_month_rent_ratio,
    MAX(p.share_rent_per_sqft_lt050) AS max_share_rent_per_sqft_lt050,
    MAX(p.share_rent_per_sqft_gt10) AS max_share_rent_per_sqft_gt10,
    CASE WHEN QUANTILE_CONT(p.rent_p50, 0.01) > 0
      AND QUANTILE_CONT(p.rent_p50, 0.99) / QUANTILE_CONT(p.rent_p50, 0.01) > 3 THEN TRUE ELSE FALSE END AS flag_property_rent_p99_p1_gt3,
    CASE WHEN MAX(p.abs_log_adjacent_month_jump) > LOG(2) THEN TRUE ELSE FALSE END AS flag_property_adjacent_jump_gt2,
    CASE WHEN MAX(p.abs_log_adjacent_month_jump) > LOG(3) THEN TRUE ELSE FALSE END AS flag_property_adjacent_jump_gt3,
    CASE WHEN MAX(p.share_rent_per_sqft_lt050) > 0 THEN TRUE ELSE FALSE END AS flag_property_any_rent_per_sqft_lt050,
    CASE WHEN MAX(p.share_rent_per_sqft_gt10) > 0 THEN TRUE ELSE FALSE END AS flag_property_any_rent_per_sqft_gt10
  FROM property_month_jumps p
  LEFT JOIN property_address r
    ON p.property_key = r.property_key
  GROUP BY 1
  "
)
dbExecute(
  con,
  "
  COPY property_proxy_stability
  TO '../output/property_proxy_stability.csv'
  (HEADER TRUE, DELIMITER ',')
  "
)

message("Auditing same-day floorplan rent spreads...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE floorplan_day_spread AS
  SELECT
    analysis_key,
    property_key,
    floorplan_key,
    ANY_VALUE(address_norm) AS address_norm,
    month_start,
    file_date,
    COUNT(*) AS raw_rows_day,
    COUNT(DISTINCT rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_values_day,
    MIN(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_min,
    QUANTILE_CONT(rent_price, 0.25) FILTER (WHERE rent_price IS NOT NULL) AS rent_p25,
    QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) AS rent_p50,
    QUANTILE_CONT(rent_price, 0.75) FILTER (WHERE rent_price IS NOT NULL) AS rent_p75,
    MAX(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_max,
    CASE
      WHEN MIN(rent_price) FILTER (WHERE rent_price IS NOT NULL) > 0
        THEN MAX(rent_price) FILTER (WHERE rent_price IS NOT NULL) /
          MIN(rent_price) FILTER (WHERE rent_price IS NOT NULL)
      ELSE NULL
    END AS rent_max_min_ratio,
    CASE
      WHEN QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) > 0
        THEN (
          QUANTILE_CONT(rent_price, 0.75) FILTER (WHERE rent_price IS NOT NULL) -
          QUANTILE_CONT(rent_price, 0.25) FILTER (WHERE rent_price IS NOT NULL)
        ) / QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL)
      ELSE NULL
    END AS rent_iqr_over_median
  FROM raw_clean
  WHERE analysis_key IS NOT NULL
    AND rent_price IS NOT NULL
  GROUP BY 1, 2, 3, 5, 6
  "
)
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE floorplan_month_spread_flags AS
  SELECT
    analysis_key,
    month_start,
    MAX(CASE WHEN rent_values_day >= 5 AND rent_max_min_ratio > 2 THEN TRUE ELSE FALSE END) AS flag_same_day_rent_spread_gt2,
    MAX(CASE WHEN rent_max_min_ratio > 3 THEN TRUE ELSE FALSE END) AS flag_same_day_rent_spread_gt3,
    MAX(CASE WHEN rent_iqr_over_median > 0.35 THEN TRUE ELSE FALSE END) AS flag_same_day_iqr_over_median_gt035,
    MAX(rent_max_min_ratio) AS max_same_day_rent_ratio,
    MAX(rent_iqr_over_median) AS max_same_day_iqr_over_median
  FROM floorplan_day_spread
  GROUP BY 1, 2
  "
)
dbExecute(
  con,
  "
  COPY (
    SELECT *
    FROM floorplan_day_spread
    WHERE (rent_values_day >= 5 AND rent_max_min_ratio > 2)
      OR rent_max_min_ratio > 3
      OR rent_iqr_over_median > 0.35
    ORDER BY rent_max_min_ratio DESC NULLS LAST, raw_rows_day DESC
    LIMIT 50000
  )
  TO '../output/floorplan_same_day_rent_spread.csv'
  (HEADER TRUE, DELIMITER ',')
  "
)

message("Auditing one-day bulk property-days...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE property_day AS
  SELECT
    property_key,
    ANY_VALUE(address_norm) AS address_norm,
    file_date,
    month_start,
    COUNT(*) AS raw_rows_day,
    COUNT(DISTINCT floorplan_key) AS floorplans_day,
    COUNT(DISTINCT rent_cell_key) AS rent_cells_day,
    QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) AS rent_p50,
    MIN(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_min,
    MAX(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_max
  FROM raw_clean
  WHERE property_key IS NOT NULL
  GROUP BY 1, 3, 4
  "
)
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE property_day_context AS
  WITH lifetime AS (
    SELECT property_key, COUNT(*) AS property_days
    FROM property_day
    GROUP BY 1
  ),
  ordered AS (
    SELECT
      d.*,
      l.property_days,
      LAG(file_date) OVER (PARTITION BY d.property_key ORDER BY d.file_date) AS prev_file_date,
      LEAD(file_date) OVER (PARTITION BY d.property_key ORDER BY d.file_date) AS next_file_date
    FROM property_day d
    LEFT JOIN lifetime l
      ON d.property_key = l.property_key
  )
  SELECT
    *,
    CASE
      WHEN floorplans_day > 25 OR raw_rows_day > 100 THEN TRUE ELSE FALSE
    END AS flag_one_day_bulk,
    CASE
      WHEN property_days = 1 THEN TRUE ELSE FALSE
    END AS flag_one_day_only_property,
    CASE
      WHEN (prev_file_date IS NULL OR DATE_DIFF('day', prev_file_date, file_date) > 7)
        AND (next_file_date IS NULL OR DATE_DIFF('day', file_date, next_file_date) > 7)
        THEN TRUE ELSE FALSE
    END AS flag_no_adjacent_support_7d
  FROM ordered
  "
)
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE property_month_bulk_flags AS
  SELECT
    property_key,
    month_start,
    MAX(flag_one_day_bulk) AS flag_one_day_bulk,
    MAX(flag_one_day_bulk AND (flag_one_day_only_property OR flag_no_adjacent_support_7d)) AS flag_one_day_bulk_no_support,
    MAX(floorplans_day) AS max_floorplans_day,
    MAX(raw_rows_day) AS max_raw_rows_day
  FROM property_day_context
  GROUP BY 1, 2
  "
)
dbExecute(
  con,
  "
  COPY (
    SELECT *
    FROM property_day_context
    WHERE flag_one_day_bulk
    ORDER BY raw_rows_day DESC, floorplans_day DESC
    LIMIT 50000
  )
  TO '../output/one_day_bulk_audit.csv'
  (HEADER TRUE, DELIMITER ',')
  "
)

message("Auditing coordinate-only generic piles...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE coordinate_only_pile_flags AS
  SELECT
    coord_key,
    QUANTILE_CONT(latitude, 0.50) AS latitude,
    QUANTILE_CONT(longitude, 0.50) AS longitude,
    COUNT(*) AS total_rows,
    SUM(address_missing) AS missing_address_rows,
    COUNT(DISTINCT address_norm) FILTER (WHERE address_norm IS NOT NULL) AS n_valid_addresses,
    COUNT(DISTINCT zip) FILTER (WHERE zip IS NOT NULL AND zip != '') AS n_zips,
    COUNT(DISTINCT neighborhood) FILTER (WHERE neighborhood IS NOT NULL AND neighborhood != '') AS n_neighborhoods,
    MIN(file_date) AS first_seen,
    MAX(file_date) AS last_seen,
    CASE
      WHEN SUM(address_missing) > 0
        AND (
          COUNT(*) > 1000
          OR COUNT(DISTINCT address_norm) FILTER (WHERE address_norm IS NOT NULL) > 50
          OR COUNT(DISTINCT zip) FILTER (WHERE zip IS NOT NULL AND zip != '') > 20
          OR COUNT(DISTINCT neighborhood) FILTER (WHERE neighborhood IS NOT NULL AND neighborhood != '') > 20
        ) THEN TRUE
      ELSE FALSE
    END AS flag_coordinate_only_generic_pile
  FROM raw_clean
  WHERE coord_key IS NOT NULL
  GROUP BY 1
  "
)
dbExecute(
  con,
  "
  COPY (
    SELECT *
    FROM coordinate_only_pile_flags
    WHERE missing_address_rows > 0
    ORDER BY flag_coordinate_only_generic_pile DESC, total_rows DESC
  )
  TO '../output/coordinate_only_pile_audit.csv'
  (HEADER TRUE, DELIMITER ',')
  "
)

message("Auditing building type conflicts...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE building_type_conflict_flags AS
  WITH type_counts AS (
    SELECT
      property_key,
      ANY_VALUE(address_norm) AS address_norm,
      COUNT(*) AS raw_rows,
      COUNT(DISTINCT building_type_clean) AS n_building_type_clean,
      SUM(CASE WHEN building_type_clean = 'single_family' THEN 1 ELSE 0 END) AS single_family_rows,
      SUM(CASE WHEN building_type_clean = 'multi_family' THEN 1 ELSE 0 END) AS multi_family_rows,
      SUM(CASE WHEN building_type_clean = 'condo' THEN 1 ELSE 0 END) AS condo_rows,
      SUM(CASE WHEN building_type_clean = 'townhouse' THEN 1 ELSE 0 END) AS townhouse_rows,
      SUM(CASE WHEN building_type_clean = 'commercial' THEN 1 ELSE 0 END) AS commercial_rows,
      STRING_AGG(DISTINCT building_type_clean, ';') AS building_type_clean_values
    FROM raw_clean
    WHERE property_key IS NOT NULL
    GROUP BY 1
  ),
  scale AS (
    SELECT
      property_key,
      MAX(floorplans_day) AS max_floorplans_day,
      MAX(raw_rows_day) AS max_raw_rows_day,
      COUNT(*) AS active_property_days
    FROM property_day
    GROUP BY 1
  )
  SELECT
    t.*,
    s.max_floorplans_day,
    s.max_raw_rows_day,
    s.active_property_days,
    CASE WHEN t.n_building_type_clean > 1 THEN TRUE ELSE FALSE END AS flag_building_type_conflict,
    CASE
      WHEN t.single_family_rows > 0
        AND (s.max_floorplans_day > 10 OR s.max_raw_rows_day > 100)
        THEN TRUE
      ELSE FALSE
    END AS flag_large_property_has_single_family
  FROM type_counts t
  LEFT JOIN scale s
    ON t.property_key = s.property_key
  "
)
dbExecute(
  con,
  "
  COPY (
    SELECT *
    FROM building_type_conflict_flags
    WHERE flag_building_type_conflict OR flag_large_property_has_single_family
    ORDER BY flag_large_property_has_single_family DESC, max_raw_rows_day DESC NULLS LAST
  )
  TO '../output/building_type_conflict_audit.csv'
  (HEADER TRUE, DELIMITER ',')
  "
)

message("Auditing stale DATE_POSTED lags...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE stale_month_flags AS
  SELECT
    analysis_key,
    month_start,
    COUNT(*) AS raw_rows,
    AVG(CASE WHEN posted_lag_days < 0 THEN 1.0 ELSE 0.0 END) AS share_posted_lag_negative,
    AVG(CASE WHEN posted_lag_days > 90 THEN 1.0 ELSE 0.0 END) AS share_posted_lag_gt90,
    AVG(CASE WHEN posted_lag_days > 180 THEN 1.0 ELSE 0.0 END) AS share_posted_lag_gt180,
    MAX(CASE WHEN posted_lag_days < 0 THEN TRUE ELSE FALSE END) AS flag_posted_lag_negative,
    MAX(CASE WHEN posted_lag_days > 90 THEN TRUE ELSE FALSE END) AS flag_posted_lag_gt90,
    MAX(CASE WHEN posted_lag_days > 180 THEN TRUE ELSE FALSE END) AS flag_posted_lag_gt180
  FROM raw_clean
  WHERE analysis_key IS NOT NULL
  GROUP BY 1, 2
  "
)
dbExecute(
  con,
  "
  COPY (
    SELECT
      CAST(YEAR(file_date) AS INTEGER) AS year,
      COUNT(*) AS raw_rows,
      AVG(CASE WHEN posted_lag_days IS NULL THEN 1.0 ELSE 0.0 END) AS share_missing_posted_lag,
      AVG(CASE WHEN posted_lag_days < 0 THEN 1.0 ELSE 0.0 END) AS share_posted_lag_negative,
      AVG(CASE WHEN posted_lag_days > 90 THEN 1.0 ELSE 0.0 END) AS share_posted_lag_gt90,
      AVG(CASE WHEN posted_lag_days > 180 THEN 1.0 ELSE 0.0 END) AS share_posted_lag_gt180,
      QUANTILE_CONT(posted_lag_days, 0.50) FILTER (WHERE posted_lag_days IS NOT NULL) AS posted_lag_p50,
      QUANTILE_CONT(posted_lag_days, 0.90) FILTER (WHERE posted_lag_days IS NOT NULL) AS posted_lag_p90,
      QUANTILE_CONT(posted_lag_days, 0.99) FILTER (WHERE posted_lag_days IS NOT NULL) AS posted_lag_p99
    FROM raw_clean
    GROUP BY 1
    ORDER BY 1
  )
  TO '../output/stale_posted_audit.csv'
  (HEADER TRUE, DELIMITER ',')
  "
)

message("Writing 221 W Harrison spotcheck...")
dbExecute(
  con,
  "
  COPY (
    SELECT
      address_norm,
      coord_key,
      ROUND(QUANTILE_CONT(latitude, 0.50), 6) AS latitude,
      ROUND(QUANTILE_CONT(longitude, 0.50), 6) AS longitude,
      file_date,
      COUNT(*) AS raw_rows,
      COUNT(DISTINCT floorplan_key) AS floorplans,
      COUNT(DISTINCT rent_cell_key) AS rent_cells,
      MIN(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_min,
      QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) AS rent_p50,
      MAX(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_max,
      STRING_AGG(DISTINCT building_type_clean, ';') AS building_types
    FROM raw_clean
    WHERE address_norm LIKE '221 W HARRISON%%'
    GROUP BY 1, 2, 5
    ORDER BY address_norm, coord_key, file_date
  )
  TO '../output/harrison_221_spotcheck.csv'
  (HEADER TRUE, DELIMITER ',')
  "
)

message("Building panel-level quality flags...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE chicago_rent_panel_quality_flags AS
  SELECT
    p.rent_panel_id,
    p.analysis_key,
    p.property_key,
    p.floorplan_key,
    p.address_norm,
    p.address_stem,
    p.address_missing,
    p.month_start,
    p.year,
    p.latitude,
    p.longitude,
    CASE
      WHEN COALESCE(p.address_missing, FALSE) = FALSE
        AND COALESCE(ac.flag_address_location_stable, FALSE)
        THEN ac.corrected_latitude
      ELSE p.latitude
    END AS geometry_latitude,
    CASE
      WHEN COALESCE(p.address_missing, FALSE) = FALSE
        AND COALESCE(ac.flag_address_location_stable, FALSE)
        THEN ac.corrected_longitude
      ELSE p.longitude
    END AS geometry_longitude,
    ac.modal_latitude,
    ac.modal_longitude,
    ac.primary_location_latitude,
    ac.primary_location_longitude,
    ac.corrected_latitude,
    ac.corrected_longitude,
    ac.location_group_local,
    ac.location_group_rank,
    ac.location_group_share,
    ac.primary_location_share,
    ac.second_location_share,
    ac.primary_second_distance_ft,
    ac.address_location_group_count,
    ac.distance_to_primary_location_ft,
    ac.distance_to_corrected_location_ft,
    ac.address_coord_distance_ft,
    ac.verification_status AS manual_location_verification_status,
    ac.verified_latitude AS manual_verified_latitude,
    ac.verified_longitude AS manual_verified_longitude,
    ac.source_name AS manual_location_source_name,
    ac.source_url AS manual_location_source_url,
    ac.note AS manual_location_note,
    COALESCE(ac.flag_address_coord_gt100ft, FALSE) AS flag_address_coord_gt100ft,
    COALESCE(ac.flag_address_coord_gt250ft, FALSE) AS flag_address_coord_gt250ft,
    COALESCE(ac.flag_address_coord_gt500ft, FALSE) AS flag_address_coord_gt500ft,
    COALESCE(ac.flag_address_location_unstable_cluster, FALSE) AS flag_address_location_unstable_cluster,
    COALESCE(ac.flag_address_location_stable, FALSE) AS flag_address_location_stable,
    COALESCE(ac.flag_address_location_unstable, FALSE) AS flag_address_location_unstable,
    COALESCE(ac.flag_address_location_fix_candidate, FALSE) AS flag_address_location_fix_candidate,
    COALESCE(ac.flag_manual_location_verified, FALSE) AS flag_manual_location_verified,
    COALESCE(ac.flag_manual_location_exclude, FALSE) AS flag_manual_location_exclude,
    COALESCE(ac.distance_to_primary_location_ft > 100, FALSE) AS flag_distance_to_primary_location_gt100ft,
    COALESCE(ac.distance_to_primary_location_ft > 200, FALSE) AS flag_distance_to_primary_location_gt200ft,
    COALESCE(ac.distance_to_corrected_location_ft > 100, FALSE) AS flag_distance_to_corrected_location_gt100ft,
    COALESCE(ac.distance_to_corrected_location_ft > 200, FALSE) AS flag_distance_to_corrected_location_gt200ft,
    COALESCE(p.address_missing, FALSE) = FALSE
      AND COALESCE(ac.flag_address_location_stable, FALSE)
      AND COALESCE(ac.distance_to_primary_location_ft > 1, FALSE) AS flag_geometry_uses_address_location_correction,
    COALESCE(cop.flag_coordinate_only_generic_pile, FALSE)
      AND COALESCE(p.address_missing, FALSE) = TRUE AS flag_coordinate_only_generic_pile,
    CASE WHEN p.rent_price <= 0 OR p.rent_price IS NULL THEN TRUE ELSE FALSE END AS flag_rent_nonpositive,
    CASE WHEN p.beds < 0 OR p.beds > 8 THEN TRUE ELSE FALSE END AS flag_beds_impossible,
    CASE WHEN p.baths <= 0 OR p.baths > 8 THEN TRUE ELSE FALSE END AS flag_baths_impossible,
    CASE WHEN p.sqft > 0 AND (p.sqft < 150 OR p.sqft > 10000) THEN TRUE ELSE FALSE END AS flag_sqft_impossible,
    CASE WHEN p.rent_per_sqft < 0.5 THEN TRUE ELSE FALSE END AS flag_rent_per_sqft_lt050,
    CASE WHEN p.rent_per_sqft > 10 THEN TRUE ELSE FALSE END AS flag_rent_per_sqft_gt10,
    COALESCE(prop.flag_property_rent_p99_p1_gt3, FALSE) AS flag_property_rent_p99_p1_gt3,
    COALESCE(prop.flag_property_adjacent_jump_gt2, FALSE) AS flag_property_adjacent_jump_gt2,
    COALESCE(prop.flag_property_adjacent_jump_gt3, FALSE) AS flag_property_adjacent_jump_gt3,
    COALESCE(prop.flag_property_any_rent_per_sqft_lt050, FALSE) AS flag_property_any_rent_per_sqft_lt050,
    COALESCE(prop.flag_property_any_rent_per_sqft_gt10, FALSE) AS flag_property_any_rent_per_sqft_gt10,
    COALESCE(sp.flag_same_day_rent_spread_gt2, FALSE) AS flag_same_day_rent_spread_gt2,
    COALESCE(sp.flag_same_day_rent_spread_gt3, FALSE) AS flag_same_day_rent_spread_gt3,
    COALESCE(sp.flag_same_day_iqr_over_median_gt035, FALSE) AS flag_same_day_iqr_over_median_gt035,
    COALESCE(bulk.flag_one_day_bulk, FALSE) AS flag_one_day_bulk,
    COALESCE(bulk.flag_one_day_bulk_no_support, FALSE) AS flag_one_day_bulk_no_support,
    COALESCE(bt.flag_building_type_conflict, FALSE) AS flag_building_type_conflict,
    COALESCE(bt.flag_large_property_has_single_family, FALSE) AS flag_large_property_has_single_family,
    COALESCE(st.flag_posted_lag_negative, FALSE) AS flag_posted_lag_negative,
    COALESCE(st.flag_posted_lag_gt90, FALSE) AS flag_posted_lag_gt90,
    COALESCE(st.flag_posted_lag_gt180, FALSE) AS flag_posted_lag_gt180,
    COALESCE(sp.max_same_day_rent_ratio, NULL) AS max_same_day_rent_ratio,
    COALESCE(sp.max_same_day_iqr_over_median, NULL) AS max_same_day_iqr_over_median,
    COALESCE(prop.max_adjacent_month_rent_ratio, NULL) AS max_adjacent_month_rent_ratio,
    COALESCE(bulk.max_floorplans_day, NULL) AS max_floorplans_day,
    COALESCE(bulk.max_raw_rows_day, NULL) AS max_raw_rows_day,
    CASE
      WHEN COALESCE(ac.flag_manual_location_exclude, FALSE)
        OR COALESCE(ac.flag_address_location_unstable, FALSE)
        OR (
          COALESCE(cop.flag_coordinate_only_generic_pile, FALSE)
          AND COALESCE(p.address_missing, FALSE) = TRUE
        ) THEN TRUE
      ELSE FALSE
    END AS flag_location_questionable,
    CASE
      WHEN COALESCE(ac.flag_manual_location_exclude, FALSE) THEN 'manual_exclude'
      WHEN COALESCE(ac.flag_address_location_unstable, FALSE) THEN 'unstable_address_location'
      WHEN COALESCE(cop.flag_coordinate_only_generic_pile, FALSE)
        AND COALESCE(p.address_missing, FALSE) = TRUE THEN 'coordinate_only_generic_pile'
      WHEN COALESCE(ac.flag_manual_location_verified, FALSE) THEN 'manual_verified'
      WHEN COALESCE(ac.flag_address_location_stable, FALSE) THEN 'stable_address_location'
      WHEN COALESCE(p.address_missing, FALSE) = TRUE THEN 'coordinate_only_not_flagged'
      ELSE 'not_flagged'
    END AS location_quality_status,
    (
      p.rent_price <= 0 OR p.rent_price IS NULL
      OR p.beds < 0 OR p.beds > 8
      OR p.baths <= 0 OR p.baths > 8
      OR (p.sqft > 0 AND (p.sqft < 150 OR p.sqft > 10000))
    ) AS flag_hard_invalid_candidate,
    CASE
      WHEN (
        COALESCE(ac.flag_address_location_unstable, FALSE)
        OR (
          COALESCE(cop.flag_coordinate_only_generic_pile, FALSE)
          AND COALESCE(p.address_missing, FALSE) = TRUE
        )
        OR p.rent_price <= 0 OR p.rent_price IS NULL
        OR p.beds < 0 OR p.beds > 8
        OR p.baths <= 0 OR p.baths > 8
        OR (p.sqft > 0 AND (p.sqft < 150 OR p.sqft > 10000))
        OR COALESCE(prop.flag_property_adjacent_jump_gt3, FALSE)
        OR COALESCE(sp.flag_same_day_rent_spread_gt3, FALSE)
        OR COALESCE(bulk.flag_one_day_bulk_no_support, FALSE)
      ) THEN 'severe'
      WHEN (
        COALESCE(ac.flag_address_location_fix_candidate, FALSE)
        OR COALESCE(prop.flag_property_adjacent_jump_gt2, FALSE)
        OR COALESCE(prop.flag_property_rent_p99_p1_gt3, FALSE)
        OR COALESCE(sp.flag_same_day_rent_spread_gt2, FALSE)
        OR COALESCE(sp.flag_same_day_iqr_over_median_gt035, FALSE)
        OR COALESCE(bulk.flag_one_day_bulk, FALSE)
        OR COALESCE(bt.flag_large_property_has_single_family, FALSE)
        OR p.rent_per_sqft < 0.5
        OR p.rent_per_sqft > 10
      ) THEN 'high'
      WHEN (
        COALESCE(ac.distance_to_corrected_location_ft > 100, FALSE)
        OR COALESCE(bt.flag_building_type_conflict, FALSE)
        OR COALESCE(st.flag_posted_lag_negative, FALSE)
        OR COALESCE(st.flag_posted_lag_gt90, FALSE)
        OR COALESCE(st.flag_posted_lag_gt180, FALSE)
      ) THEN 'low'
      ELSE 'none'
    END AS quality_flag_severity
  FROM rent_panel p
  LEFT JOIN address_cluster_flags ac
    ON p.address_stem = ac.address_stem
    AND p.panel_coord_key = ac.coord_key
  LEFT JOIN coordinate_only_pile_flags cop
    ON p.panel_coord_key = cop.coord_key
  LEFT JOIN property_proxy_stability prop
    ON p.property_key = prop.property_key
  LEFT JOIN floorplan_month_spread_flags sp
    ON p.analysis_key = sp.analysis_key
    AND p.month_start = sp.month_start
  LEFT JOIN property_month_bulk_flags bulk
    ON p.property_key = bulk.property_key
    AND p.month_start = bulk.month_start
  LEFT JOIN building_type_conflict_flags bt
    ON p.property_key = bt.property_key
  LEFT JOIN stale_month_flags st
    ON p.analysis_key = st.analysis_key
    AND p.month_start = st.month_start
  "
)

quality_n <- collect_query("SELECT COUNT(*) AS n FROM chicago_rent_panel_quality_flags")$n[1]
quality_unique_n <- collect_query("SELECT COUNT(DISTINCT rent_panel_id) AS n FROM chicago_rent_panel_quality_flags")$n[1]
if (quality_n != panel_n || quality_unique_n != panel_n) {
  stop("Quality flags must be one-to-one with the monthly rent panel.", call. = FALSE)
}

dbExecute(
  con,
  "
  COPY chicago_rent_panel_quality_flags
  TO '../output/chicago_rent_panel_quality_flags.parquet'
  (FORMAT PARQUET, COMPRESSION ZSTD)
  "
)

flag_cols <- collect_query("DESCRIBE chicago_rent_panel_quality_flags")$column_name
flag_cols <- flag_cols[grepl("^flag_", flag_cols)]

summary_parts <- lapply(flag_cols, function(flag_col) {
  rbindlist(list(
    collect_query(sprintf(
      "
      SELECT
        'overall' AS scope,
        NULL AS year,
        '%s' AS flag,
        COUNT(*) AS n_rows,
        SUM(CASE WHEN %s THEN 1 ELSE 0 END) AS n_flagged,
        AVG(CASE WHEN %s THEN 1.0 ELSE 0.0 END) AS share_flagged
      FROM chicago_rent_panel_quality_flags
      ",
      flag_col,
      flag_col,
      flag_col
    )),
    collect_query(sprintf(
      "
      SELECT
        'year' AS scope,
        year,
        '%s' AS flag,
        COUNT(*) AS n_rows,
        SUM(CASE WHEN %s THEN 1 ELSE 0 END) AS n_flagged,
        AVG(CASE WHEN %s THEN 1.0 ELSE 0.0 END) AS share_flagged
      FROM chicago_rent_panel_quality_flags
      GROUP BY 1, 2, 3
      ",
      flag_col,
      flag_col,
      flag_col
    ))
  ), fill = TRUE)
})
flag_summary <- rbindlist(summary_parts, fill = TRUE)
severity_summary <- collect_query(
  "
  SELECT
    'overall' AS scope,
    NULL AS year,
    quality_flag_severity AS flag,
    COUNT(*) AS n_rows,
    COUNT(*) AS n_flagged,
    COUNT(*) * 1.0 / SUM(COUNT(*)) OVER () AS share_flagged
  FROM chicago_rent_panel_quality_flags
  GROUP BY 1, 2, 3
  UNION ALL
  SELECT
    'year' AS scope,
    year,
    quality_flag_severity AS flag,
    COUNT(*) AS n_rows,
    COUNT(*) AS n_flagged,
    COUNT(*) * 1.0 / SUM(COUNT(*)) OVER (PARTITION BY year) AS share_flagged
  FROM chicago_rent_panel_quality_flags
  GROUP BY 1, 2, 3
  "
)
fwrite(rbindlist(list(flag_summary, severity_summary), fill = TRUE), "../output/renthub_quality_flag_summary.csv")

message(sprintf(
  "Wrote quality flags for %s monthly rent observations.",
  format(quality_n, big.mark = ",")
))
message("RentHub proxy consistency audit complete.")
