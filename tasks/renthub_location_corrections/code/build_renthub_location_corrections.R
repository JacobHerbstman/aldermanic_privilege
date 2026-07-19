# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/renthub_location_corrections/code")
# start_date <- "2014-01-01"
# end_date <- "2022-12-31"

source("../../setup_environment/code/packages.R")

library(DBI)
library(duckdb)
library(data.table)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_date, end_date)
}
if (length(cli_args) != 2) {
  stop("Script requires a start date and end date.", call. = FALSE)
}

start_date <- as.Date(cli_args[1])
end_date <- as.Date(cli_args[2])
if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
  stop("Start and end dates must define a valid window.", call. = FALSE)
}

address_location_radius_ft <- 200
primary_location_share_cutoff <- 0.85
far_secondary_share_cutoff <- 0.10
far_secondary_distance_ft <- 500

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
invisible(dbExecute(con, "PRAGMA threads=4"))

invisible(dbExecute(
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
))

invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE rent_panel AS
  SELECT
    rent_panel_id,
    clean_address_stem(address_norm) AS address_stem,
    address_missing,
    year,
    latitude,
    longitude,
    PRINTF('%.4f|%.4f', latitude, longitude) AS panel_coord_key
  FROM read_parquet('../input/chicago_rent_panel.parquet')
  "
))

panel_counts <- dbGetQuery(
  con,
  "SELECT COUNT(*) AS n, COUNT(DISTINCT rent_panel_id) AS n_unique FROM rent_panel"
)
if (panel_counts$n != panel_counts$n_unique) {
  stop("The monthly rental panel must be unique by rent_panel_id.", call. = FALSE)
}

invisible(dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE raw_locations AS
    WITH raw_source AS (
      SELECT
        UPPER(TRIM(CAST(ADDRESS AS VARCHAR))) AS address_raw,
        UPPER(TRIM(CAST(CITY AS VARCHAR))) AS city_raw,
        TRY_CAST(SCRAPED_TIMESTAMP AS DATE) AS file_date,
        TRY_CAST(LATITUDE AS DOUBLE) AS latitude,
        TRY_CAST(LONGITUDE AS DOUBLE) AS longitude,
        CAST(ZIP AS VARCHAR) AS zip,
        CAST(NEIGHBORHOOD AS VARCHAR) AS neighborhood
      FROM read_parquet('../input/renthub_raw/*.parquet', union_by_name = true)
      WHERE TRY_CAST(SCRAPED_TIMESTAMP AS DATE) BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)
    ),
    cleaned AS (
      SELECT
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
        file_date,
        latitude,
        longitude,
        zip,
        neighborhood
      FROM raw_source
      WHERE city_raw IN ('CHICAGO', 'CHGO')
    )
    SELECT
      clean_address_stem(address_norm) AS address_stem,
      address_norm,
      address_missing,
      file_date,
      latitude,
      longitude,
      zip,
      neighborhood,
      PRINTF('%%.4f|%%.4f', latitude, longitude) AS coord_key
    FROM cleaned
    WHERE latitude BETWEEN 41.55 AND 42.10
      AND longitude BETWEEN -88.10 AND -87.40
    ",
    start_date,
    end_date
  )
))

manual_locations <- fread(
  "../input/manual_verified_address_locations.csv",
  na.strings = c("", "NA", "N/A", "NULL")
)
required_manual_columns <- c(
  "address_stem", "verification_status", "verified_latitude", "verified_longitude"
)
if (!all(required_manual_columns %in% names(manual_locations))) {
  stop("Manual location decisions are missing required columns.", call. = FALSE)
}

manual_locations[, address_stem := toupper(trimws(address_stem))]
manual_locations[, address_stem := gsub("[.,]", "", address_stem)]
manual_locations[, address_stem := gsub("\\bNORTH\\b", "N", address_stem)]
manual_locations[, address_stem := gsub("\\bSOUTH\\b", "S", address_stem)]
manual_locations[, address_stem := gsub("\\bEAST\\b", "E", address_stem)]
manual_locations[, address_stem := gsub("\\bWEST\\b", "W", address_stem)]
manual_locations[, address_stem := gsub(
  " (STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|PLACE|PLAZA|PLZ|PL|COURT|CT|DRIVE|DR|TERRACE|TER|LANE|LN)$",
  "",
  address_stem
)]
manual_locations[, address_stem := gsub(" +", " ", trimws(address_stem))]
manual_locations[, verification_status := tolower(trimws(verification_status))]
manual_locations[, verified_latitude := suppressWarnings(as.numeric(verified_latitude))]
manual_locations[, verified_longitude := suppressWarnings(as.numeric(verified_longitude))]
manual_locations <- manual_locations[address_stem != "" & !is.na(address_stem)]

if (anyDuplicated(manual_locations$address_stem) > 0) {
  stop("Manual location decisions must be unique by address.", call. = FALSE)
}
if (any(!manual_locations$verification_status %in% c("verified", "exclude", "needs_review"))) {
  stop("Manual location status must be verified, exclude, or needs_review.", call. = FALSE)
}
if (nrow(manual_locations[
  verification_status == "verified" &
    (!is.finite(verified_latitude) | !is.finite(verified_longitude))
]) > 0) {
  stop("Verified manual locations must have finite coordinates.", call. = FALSE)
}

manual_locations[, `:=`(manual_x_ft = NA_real_, manual_y_ft = NA_real_)]
if (nrow(manual_locations[verification_status == "verified"]) > 0) {
  manual_sf <- st_as_sf(
    manual_locations[verification_status == "verified"],
    coords = c("verified_longitude", "verified_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
    st_transform(3435)
  manual_xy <- st_coordinates(manual_sf)
  manual_locations[verification_status == "verified", `:=`(
    manual_x_ft = manual_xy[, 1],
    manual_y_ft = manual_xy[, 2]
  )]
}

address_clusters <- as.data.table(dbGetQuery(
  con,
  "
  SELECT
    address_stem,
    coord_key,
    QUANTILE_CONT(latitude, 0.50) AS latitude,
    QUANTILE_CONT(longitude, 0.50) AS longitude,
    COUNT(*) AS raw_rows
  FROM raw_locations
  WHERE address_stem IS NOT NULL AND coord_key IS NOT NULL
  GROUP BY 1, 2
  "
))
if (nrow(address_clusters) == 0) {
  stop("No address-coordinate clusters were found.", call. = FALSE)
}

setorder(address_clusters, address_stem, -raw_rows, coord_key)
address_clusters[, modal_latitude := first(latitude), by = address_stem]
address_clusters[, modal_longitude := first(longitude), by = address_stem]

cluster_sf <- st_as_sf(
  address_clusters,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)
cluster_xy <- st_coordinates(cluster_sf)
address_clusters[, `:=`(x_ft = cluster_xy[, 1], y_ft = cluster_xy[, 2])]

address_clusters[, location_group := {
  if (.N == 1L) {
    1L
  } else {
    as.integer(cutree(
      hclust(dist(cbind(x_ft, y_ft)), method = "complete"),
      h = address_location_radius_ft
    ))
  }
}, by = address_stem]

location_groups <- address_clusters[, .(
  group_rows = sum(raw_rows),
  group_latitude = weighted.mean(latitude, raw_rows),
  group_longitude = weighted.mean(longitude, raw_rows),
  group_x_ft = weighted.mean(x_ft, raw_rows),
  group_y_ft = weighted.mean(y_ft, raw_rows)
), by = .(address_stem, location_group)]
setorder(location_groups, address_stem, -group_rows, location_group)
location_groups[, total_rows := sum(group_rows), by = address_stem]
location_groups[, group_rank := seq_len(.N), by = address_stem]
location_groups[, group_share := group_rows / total_rows]

primary_locations <- location_groups[group_rank == 1L, .(
  address_stem,
  primary_latitude = group_latitude,
  primary_longitude = group_longitude,
  primary_x_ft = group_x_ft,
  primary_y_ft = group_y_ft,
  primary_share = group_share
)]
second_locations <- location_groups[group_rank == 2L, .(
  address_stem,
  second_x_ft = group_x_ft,
  second_y_ft = group_y_ft,
  second_share = group_share
)]

location_contract <- merge(primary_locations, second_locations, by = "address_stem", all.x = TRUE)
location_contract[, primary_second_distance_ft := fifelse(
  is.finite(second_x_ft),
  sqrt((primary_x_ft - second_x_ft)^2 + (primary_y_ft - second_y_ft)^2),
  NA_real_
)]
location_contract[, unstable_cluster := primary_share < primary_location_share_cutoff |
  (!is.na(second_share) &
    second_share >= far_secondary_share_cutoff &
    primary_second_distance_ft > far_secondary_distance_ft)]

location_contract <- merge(
  location_contract,
  manual_locations[, .(
    address_stem,
    verification_status,
    verified_latitude,
    verified_longitude,
    manual_x_ft,
    manual_y_ft
  )],
  by = "address_stem",
  all.x = TRUE,
  sort = FALSE
)
location_contract[, manual_verified := verification_status == "verified"]
location_contract[is.na(manual_verified), manual_verified := FALSE]
location_contract[, manual_exclude := verification_status == "exclude"]
location_contract[is.na(manual_exclude), manual_exclude := FALSE]
location_contract[, unstable_location := fifelse(
  manual_verified,
  FALSE,
  manual_exclude | unstable_cluster
)]
location_contract[, stable_location := manual_verified | (!manual_exclude & !unstable_cluster)]
location_contract[, `:=`(
  effective_latitude = fifelse(manual_verified, verified_latitude, primary_latitude),
  effective_longitude = fifelse(manual_verified, verified_longitude, primary_longitude),
  effective_x_ft = fifelse(manual_verified, manual_x_ft, primary_x_ft),
  effective_y_ft = fifelse(manual_verified, manual_y_ft, primary_y_ft)
)]

address_clusters <- merge(
  address_clusters,
  location_contract[, .(
    address_stem,
    primary_latitude,
    primary_longitude,
    primary_x_ft,
    primary_y_ft,
    effective_latitude,
    effective_longitude,
    effective_x_ft,
    effective_y_ft,
    manual_verified,
    manual_exclude,
    stable_location,
    unstable_location
  )],
  by = "address_stem",
  all.x = TRUE,
  sort = FALSE
)
address_clusters[, distance_to_primary_ft := sqrt(
  (x_ft - primary_x_ft)^2 + (y_ft - primary_y_ft)^2
)]
address_clusters[, corrected_latitude := fifelse(
  stable_location,
  effective_latitude,
  latitude
)]
address_clusters[, corrected_longitude := fifelse(
  stable_location,
  effective_longitude,
  longitude
)]

invisible(dbWriteTable(
  con,
  "address_locations",
  as.data.frame(address_clusters[, .(
    address_stem,
    coord_key,
    modal_latitude,
    modal_longitude,
    corrected_latitude,
    corrected_longitude,
    distance_to_primary_ft,
    manual_verified,
    manual_exclude,
    stable_location,
    unstable_location
  )]),
  overwrite = TRUE
))

invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE coordinate_piles AS
  SELECT
    coord_key,
    CASE
      WHEN SUM(address_missing) > 0
        AND (
          COUNT(*) > 1000
          OR COUNT(DISTINCT address_norm) FILTER (WHERE address_norm IS NOT NULL) > 50
          OR COUNT(DISTINCT zip) FILTER (WHERE zip IS NOT NULL AND zip != '') > 20
          OR COUNT(DISTINCT neighborhood) FILTER (WHERE neighborhood IS NOT NULL AND neighborhood != '') > 20
        ) THEN TRUE
      ELSE FALSE
    END AS generic_coordinate_pile
  FROM raw_locations
  WHERE coord_key IS NOT NULL
  GROUP BY 1
  "
))

location_output <- dbGetQuery(
  con,
  "
  SELECT
    p.rent_panel_id,
    p.year,
    CASE
      WHEN COALESCE(p.address_missing, FALSE) = FALSE
        AND COALESCE(a.stable_location, FALSE) THEN a.corrected_latitude
      ELSE p.latitude
    END AS geometry_latitude,
    CASE
      WHEN COALESCE(p.address_missing, FALSE) = FALSE
        AND COALESCE(a.stable_location, FALSE) THEN a.corrected_longitude
      ELSE p.longitude
    END AS geometry_longitude,
    a.modal_latitude,
    a.modal_longitude,
    COALESCE(p.address_missing, FALSE) = FALSE
      AND COALESCE(a.stable_location, FALSE)
      AND COALESCE(a.distance_to_primary_ft > 1, FALSE)
      AS flag_geometry_uses_address_location_correction,
    COALESCE(a.manual_verified, FALSE) AS flag_manual_location_verified,
    COALESCE(a.manual_exclude, FALSE) AS flag_manual_location_exclude,
    COALESCE(a.unstable_location, FALSE) AS flag_address_location_unstable,
    COALESCE(c.generic_coordinate_pile, FALSE)
      AND COALESCE(p.address_missing, FALSE) = TRUE
      AS flag_coordinate_only_generic_pile,
    CASE
      WHEN COALESCE(a.manual_exclude, FALSE)
        OR COALESCE(a.unstable_location, FALSE)
        OR (
          COALESCE(c.generic_coordinate_pile, FALSE)
          AND COALESCE(p.address_missing, FALSE) = TRUE
        ) THEN TRUE
      ELSE FALSE
    END AS flag_location_questionable,
    CASE
      WHEN COALESCE(a.manual_exclude, FALSE) THEN 'manual_exclude'
      WHEN COALESCE(a.unstable_location, FALSE) THEN 'unstable_address_location'
      WHEN COALESCE(c.generic_coordinate_pile, FALSE)
        AND COALESCE(p.address_missing, FALSE) = TRUE THEN 'coordinate_only_generic_pile'
      WHEN COALESCE(a.manual_verified, FALSE) THEN 'manual_verified'
      WHEN COALESCE(a.stable_location, FALSE) THEN 'stable_address_location'
      WHEN COALESCE(p.address_missing, FALSE) = TRUE THEN 'coordinate_only_not_flagged'
      ELSE 'not_flagged'
    END AS location_quality_status
  FROM rent_panel p
  LEFT JOIN address_locations a
    ON p.address_stem = a.address_stem
    AND p.panel_coord_key = a.coord_key
  LEFT JOIN coordinate_piles c
    ON p.panel_coord_key = c.coord_key
  "
)

if (nrow(location_output) != panel_counts$n ||
    anyDuplicated(location_output$rent_panel_id) > 0 ||
    any(is.na(location_output$location_quality_status))) {
  stop("Location corrections must match the monthly rental panel one-to-one.", call. = FALSE)
}

write_parquet(location_output, "../output/chicago_rent_panel_location_corrections.parquet")
