# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

con <- dbConnect(duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
invisible(dbExecute(con, "PRAGMA threads=4"))

raw_glob <- "../../../download_rent_data/output/*.parquet"

coordinate_cells <- dbGetQuery(
  con,
  sprintf(
    "
    WITH raw AS (
      SELECT
        COALESCE(NULLIF(UPPER(TRIM(CAST(CITY AS VARCHAR))), ''), '<MISSING>') AS city_raw,
        ROUND(TRY_CAST(LATITUDE AS DOUBLE), 5) AS latitude,
        ROUND(TRY_CAST(LONGITUDE AS DOUBLE), 5) AS longitude
      FROM read_parquet(%s, union_by_name = true)
      WHERE TRY_CAST(SCRAPED_TIMESTAMP AS DATE) BETWEEN CAST('2014-01-01' AS DATE) AND CAST('2022-12-31' AS DATE)
    )
    SELECT city_raw, latitude, longitude, COUNT(*) AS raw_rows
    FROM raw
    WHERE latitude BETWEEN 41.55 AND 42.10
      AND longitude BETWEEN -88.10 AND -87.40
    GROUP BY 1, 2, 3
    ",
    dbQuoteString(con, raw_glob)
  )
) %>%
  as_tibble()

ward_panel <- st_read("../../../ward_panel_create/output/ward_panel.gpkg", quiet = TRUE) %>%
  filter(year == 2015) %>%
  st_transform(3435)
chicago_union <- st_union(st_make_valid(ward_panel))

coordinate_sf <- st_as_sf(
  coordinate_cells,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)
coordinate_cells$inside_chicago_ward_union <- lengths(
  st_within(coordinate_sf, chicago_union)
) > 0

city_spatial_summary <- coordinate_cells %>%
  group_by(city_raw) %>%
  summarise(
    raw_rows_in_broad_bbox = sum(raw_rows),
    coordinate_cells_in_broad_bbox = n(),
    raw_rows_inside_chicago = sum(raw_rows[inside_chicago_ward_union]),
    coordinate_cells_inside_chicago = sum(inside_chicago_ward_union),
    accepted_by_production_city_filter = first(city_raw) %in% c("CHICAGO", "CHGO"),
    included_in_quality_raw_history = first(city_raw) == "CHICAGO",
    .groups = "drop"
  ) %>%
  arrange(desc(raw_rows_inside_chicago), desc(raw_rows_in_broad_bbox))
write_csv(city_spatial_summary, "../output/rental_city_raw_spatial_summary.csv")

panel_city_lineage <- dbGetQuery(
  con,
  sprintf(
    "
    WITH raw_source AS (
      SELECT
        UPPER(TRIM(CAST(CITY AS VARCHAR))) AS city_raw,
        UPPER(TRIM(CAST(ID AS VARCHAR))) AS id_raw,
        UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR))) AS property_id_raw,
        UPPER(TRIM(CAST(UNIT_ID AS VARCHAR))) AS unit_id_raw,
        UPPER(TRIM(CAST(ADDRESS AS VARCHAR))) AS address_raw,
        UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) AS building_type_raw,
        TRY_CAST(SCRAPED_TIMESTAMP AS DATE) AS file_date,
        TRY_CAST(BEDS AS DOUBLE) AS beds,
        TRY_CAST(BATHS AS DOUBLE) AS baths,
        TRY_CAST(SQFT AS DOUBLE) AS sqft,
        TRY_CAST(LATITUDE AS DOUBLE) AS latitude,
        TRY_CAST(LONGITUDE AS DOUBLE) AS longitude
      FROM read_parquet(%s, union_by_name = true)
      WHERE TRY_CAST(SCRAPED_TIMESTAMP AS DATE) BETWEEN CAST('2014-01-01' AS DATE) AND CAST('2022-12-31' AS DATE)
        AND UPPER(TRIM(CAST(CITY AS VARCHAR))) IN ('CHICAGO', 'CHGO')
    ),
    cleaned AS (
      SELECT
        city_raw,
        file_date,
        CASE
          WHEN unit_id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
          ELSE unit_id_raw
        END AS unit_id,
        CASE
          WHEN address_raw IN ('', '0', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
          ELSE NULLIF(REGEXP_REPLACE(address_raw, ' +', ' '), '')
        END AS address_norm,
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
        beds,
        baths,
        sqft,
        latitude,
        longitude
      FROM raw_source
      WHERE latitude BETWEEN -90 AND 90
        AND longitude BETWEEN -180 AND 180
    ),
    keyed AS (
      SELECT
        city_raw,
        file_date,
        CASE
          WHEN unit_id IS NOT NULL THEN unit_id
          ELSE COALESCE(address_norm, 'NO_ADDRESS') || '|' || PRINTF('%%.4f|%%.4f', latitude, longitude) || '|' ||
            COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
            COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' ||
            COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' ||
            COALESCE(building_type_clean, 'other')
        END AS analysis_key
      FROM cleaned
    ),
    panel_keys AS (
      SELECT
        analysis_key || '__' || STRFTIME(DATE_TRUNC('month', file_date), '%%Y-%%m') AS rent_panel_id,
        SUM(CASE WHEN city_raw = 'CHICAGO' THEN 1 ELSE 0 END) AS chicago_raw_rows,
        SUM(CASE WHEN city_raw = 'CHGO' THEN 1 ELSE 0 END) AS chgo_raw_rows
      FROM keyed
      WHERE analysis_key IS NOT NULL
      GROUP BY 1
    )
    SELECT
      p.rent_panel_id,
      COALESCE(k.chicago_raw_rows, 0) AS chicago_raw_rows,
      COALESCE(k.chgo_raw_rows, 0) AS chgo_raw_rows
    FROM read_parquet(%s) p
    LEFT JOIN panel_keys k USING (rent_panel_id)
    WHERE COALESCE(k.chgo_raw_rows, 0) > 0
    ",
    dbQuoteString(con, raw_glob),
    dbQuoteString(con, "../../../process_rent_data/output/chicago_rent_panel.parquet")
  )
) %>%
  as_tibble() %>%
  mutate(
    city_lineage = case_when(
      chicago_raw_rows > 0 & chgo_raw_rows > 0 ~ "both_chicago_and_chgo",
      chicago_raw_rows > 0 ~ "chicago_only",
      chgo_raw_rows > 0 ~ "chgo_only",
      TRUE ~ "unmatched"
    )
  )

quality_flags <- read_parquet(
  "../../renthub_quality_diagnostics/output/chicago_rent_panel_quality_flags.parquet"
) %>%
  as_tibble() %>%
  select(
    rent_panel_id,
    quality_flag_severity,
    flag_location_questionable,
    flag_address_location_unstable,
    flag_coordinate_only_generic_pile
  )

rent <- read_parquet(
  "../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet"
) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = year(file_date),
    signed_dist_ft = as.numeric(signed_dist),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    year_month = format(file_date, "%Y-%m"),
    beds_factor = factor(beds),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  ) %>%
  filter(
    !is.na(file_date),
    between(year, 2014, 2022),
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair_id),
    flag_clean_location_sample,
    is.finite(beds),
    beds >= 0,
    !is.na(log_sqft),
    !is.na(log_baths),
    if_all(
      all_of(c(
        "nearest_school_dist_kft",
        "nearest_park_dist_kft",
        "nearest_major_road_dist_kft",
        "nearest_cta_stop_dist_kft",
        "lake_michigan_dist_kft"
      )),
      is.finite
    )
  )

chgo_panel_rows <- panel_city_lineage %>%
  left_join(quality_flags, by = "rent_panel_id", relationship = "one-to-one") %>%
  mutate(in_main_rental_model = rent_panel_id %in% rent$rent_panel_id)
write_csv(chgo_panel_rows, "../output/rental_city_chgo_panel_rows.csv")

rent_rhs <- "right + log_sqft + beds_factor + log_baths"
rent <- rent %>% mutate(right = as.integer(signed_dist_ft >= 0))
if (n_distinct(rent$building_type_factor) > 1) {
  rent_rhs <- paste(rent_rhs, "+ building_type_factor")
}
rent_rhs <- paste(
  rent_rhs,
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft",
  sep = " + "
)

model_rows <- list()
for (sample_i in c("production", "exclude_chgo_only", "exclude_any_chgo")) {
  model_data <- rent
  if (sample_i == "exclude_chgo_only") {
    model_data <- model_data %>%
      filter(!rent_panel_id %in% chgo_panel_rows$rent_panel_id[
        chgo_panel_rows$city_lineage == "chgo_only"
      ])
  } else if (sample_i == "exclude_any_chgo") {
    model_data <- model_data %>%
      filter(!rent_panel_id %in% chgo_panel_rows$rent_panel_id)
  }

  model <- feols(
    as.formula(paste0("log(rent_price) ~ ", rent_rhs, " | segment_id^year_month")),
    data = model_data,
    cluster = ~segment_id,
    warn = FALSE
  )
  table <- coeftable(model)
  p_value_column <- grep("^Pr\\(", colnames(table), value = TRUE)[1]
  model_rows[[length(model_rows) + 1L]] <- tibble(
    sample = sample_i,
    estimate = unname(table["right", "Estimate"]),
    se = unname(table["right", "Std. Error"]),
    p_value = unname(table["right", p_value_column]),
    n = nobs(model)
  )
}
write_csv(bind_rows(model_rows), "../output/rental_city_model_sensitivity.csv")

lineage_summary <- bind_rows(
  chgo_panel_rows %>%
    count(city_lineage, name = "value") %>%
    transmute(section = "chgo_panel_lineage", metric = city_lineage, value),
  chgo_panel_rows %>%
    filter(in_main_rental_model) %>%
    count(city_lineage, name = "value") %>%
    transmute(section = "chgo_rows_in_main_model", metric = city_lineage, value),
  chgo_panel_rows %>%
    count(quality_flag_severity, name = "value") %>%
    transmute(
      section = "chgo_panel_quality_severity",
      metric = coalesce(as.character(quality_flag_severity), "missing"),
      value
    ),
  tibble(
    section = "main_model",
    metric = c("all_rows", "rows_with_any_chgo_lineage", "rows_with_chgo_only_lineage"),
    value = c(
      nrow(rent),
      sum(rent$rent_panel_id %in% chgo_panel_rows$rent_panel_id),
      sum(rent$rent_panel_id %in% chgo_panel_rows$rent_panel_id[
        chgo_panel_rows$city_lineage == "chgo_only"
      ])
    )
  )
)
write_csv(lineage_summary, "../output/rental_city_panel_lineage_summary.csv")
