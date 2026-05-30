# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_sale_distances/code")
# sample <- FALSE

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(sample)
}

if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <sample>.", call. = FALSE)
}
run_sample <- as.logical(cli_args[1])
if (is.na(run_sample)) {
    stop("sample must be TRUE or FALSE.", call. = FALSE)
}

crs_projected <- 3435

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
    st_transform(crs_projected)
canonical_ward_maps <- load_canonical_ward_maps(ward_panel)
canonical_boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
    mutate(month = as.yearmon(month))
ald_lookup <- alderman_panel %>%
    select(ward, month, alderman) %>%
    distinct()
if (anyDuplicated(ald_lookup[, c("ward", "month")]) > 0) {
    stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

sales_raw <- fread("../input/parcel_sales.csv")

sales <- sales_raw %>%
    filter(class %in% c(202, 203, 204, 205, 206, 207, 208, 209, 210, 211)) %>%
    mutate(
        sale_price_nominal = as.numeric(gsub("[$,]", "", sale_price)),
        year = as.numeric(year),
        pin = as.character(pin),
        sale_date = coalesce(
            as.Date(as.character(sale_date), format = "%B %d, %Y"),
            as.Date(substr(as.character(sale_date), 1, 10), format = "%Y-%m-%d")
        )
    ) %>%
    filter(!is.na(sale_price_nominal), sale_price_nominal > 10000, !is.na(year)) %>%
    filter(year >= 1999, year <= 2025) %>%
    filter(sale_deed_type %in% c("Warranty", "Trustee")) %>%
    filter(sale_type != "LAND") %>%
    filter(!sale_seller_name %in% c("", "-", "UNKNOWN", "..")) %>%
    filter(sale_seller_name != sale_buyer_name) %>%
    filter(num_parcels_sale == 1) %>%
    mutate(
        sale_date_for_price = if_else(
            !is.na(sale_date),
            sale_date,
            as.Date(paste0(as.integer(year), "-06-15"))
        ),
        sale_year_month = format(sale_date_for_price, "%Y-%m")
    )

cpi_raw <- read_csv("../input/fred_cpi_cuura207sa0.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
if (!all(c("observation_date", "CUURA207SA0") %in% names(cpi_raw))) {
    stop("CPI input missing expected columns for series CUURA207SA0.", call. = FALSE)
}

cpi_lookup <- cpi_raw %>%
    transmute(
        observation_date = as.Date(observation_date),
        cpi_value = suppressWarnings(as.numeric(CUURA207SA0))
    ) %>%
    filter(!is.na(observation_date))
if (anyDuplicated(cpi_lookup$observation_date) > 0) {
    stop("CPI input must be unique by observation_date.", call. = FALSE)
}

cpi_start_month <- as.Date(format(min(sales$sale_date_for_price, na.rm = TRUE), "%Y-%m-01"))
cpi_end_month <- as.Date(format(max(sales$sale_date_for_price, na.rm = TRUE), "%Y-%m-01"))
cpi <- tibble(observation_date = seq(cpi_start_month, cpi_end_month, by = "month")) %>%
    left_join(cpi_lookup, by = "observation_date", relationship = "one-to-one") %>%
    arrange(observation_date)

n_missing_cpi <- sum(is.na(cpi$cpi_value))
if (n_missing_cpi > 0) {
    idx_known <- which(!is.na(cpi$cpi_value))
    if (length(idx_known) < 2) {
        stop("Not enough non-missing CPI observations to interpolate.", call. = FALSE)
    }
    cpi_interp <- approx(
        x = idx_known,
        y = cpi$cpi_value[idx_known],
        xout = seq_len(nrow(cpi)),
        method = "linear",
        rule = 1
    )$y
    cpi$cpi_value <- if_else(is.na(cpi$cpi_value), cpi_interp, cpi$cpi_value)
}
if (anyNA(cpi$cpi_value)) {
    stop("CPI has unresolved endpoint gaps after interpolation.", call. = FALSE)
}

cpi_2022 <- cpi %>%
    filter(format(observation_date, "%Y") == "2022") %>%
    pull(cpi_value)
if (length(cpi_2022) == 0 || !all(is.finite(cpi_2022))) {
    stop("Unable to compute base CPI for year 2022.", call. = FALSE)
}
base_cpi <- mean(cpi_2022)
if (!is.finite(base_cpi) || base_cpi <= 0) {
    stop("Computed invalid base CPI for year 2022.", call. = FALSE)
}

cpi_deflator <- cpi %>%
    transmute(
        sale_year_month = format(observation_date, "%Y-%m"),
        sale_price_cpi_chi_all_items = cpi_value,
        sale_price_deflator_to_2022 = base_cpi / cpi_value
    )

sales <- sales %>%
    left_join(cpi_deflator, by = "sale_year_month", relationship = "many-to-one")

n_missing_deflator <- sum(!is.finite(sales$sale_price_deflator_to_2022))
if (n_missing_deflator > 0) {
    stop(sprintf(
        "Missing/invalid sale CPI deflator for %d observations.",
        n_missing_deflator
    ), call. = FALSE)
}

sales <- sales %>%
    mutate(
        sale_price_real_2022_raw = sale_price_nominal * sale_price_deflator_to_2022
    )

p01 <- quantile(sales$sale_price_real_2022_raw, 0.01, na.rm = TRUE)
p99 <- quantile(sales$sale_price_real_2022_raw, 0.99, na.rm = TRUE)

sales <- sales %>%
    mutate(sale_price = pmin(pmax(sale_price_real_2022_raw, p01), p99))

if (run_sample) {
    sales <- sales %>% slice_sample(prop = 0.05)
}

parcels <- fread("../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv",
    select = c("pin", "pin10", "latitude", "longitude")
)
parcels[, `:=`(pin = as.character(pin), pin10 = as.character(pin10))]
if (anyDuplicated(parcels$pin) > 0) {
    stop("Parcel universe must be unique by pin.", call. = FALSE)
}

sales_geo <- sales %>%
    left_join(parcels, by = "pin", relationship = "many-to-one")

missing_coords <- sum(is.na(sales_geo$latitude))

if (missing_coords > 0) {
    sales_geo <- sales_geo %>%
        mutate(pin10_sales = substr(pin, 1, 10))

    sales_found <- sales_geo %>% filter(!is.na(latitude))
    sales_missing <- sales_geo %>%
        filter(is.na(latitude)) %>%
        select(-latitude, -longitude, -pin10)

    parcels_pin10 <- parcels %>%
        select(pin10, latitude, longitude) %>%
        distinct(pin10, .keep_all = TRUE)

    sales_missing_fixed <- sales_missing %>%
        left_join(parcels_pin10, by = c("pin10_sales" = "pin10"), relationship = "many-to-one")

    sales_geo <- bind_rows(sales_found, sales_missing_fixed)
}

sales_sf <- sales_geo %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs_projected)

rm(sales_raw, sales, sales_geo, parcels)
invisible(gc())

sales_sf <- sales_sf %>%
    mutate(
        sale_date_use = sale_date_for_price,
        boundary_year = canonical_boundary_year_from_date(sale_date_use),
        era = canonical_era_from_boundary_year(boundary_year)
    )

boundary_assignments <- assign_points_to_boundaries(
    points_sf = sales_sf,
    era_values = sales_sf$era,
    ward_maps = canonical_ward_maps,
    boundary_lines = canonical_boundaries,
    chunk_n = 5000L
)

results_sf <- bind_cols(sales_sf, boundary_assignments) %>%
    filter(!is.na(ward), !is.na(ward_pair_id))

final_df <- results_sf %>%
    st_transform(4326) %>%
    mutate(
        longitude = st_coordinates(.)[, 1],
        latitude = st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry() %>%
    as_tibble()

final_df <- final_df %>%
    mutate(month_join = as.yearmon(sale_date_use))

final_df <- final_df %>%
    left_join(
        ald_lookup,
        by = c("ward" = "ward", "month_join" = "month"),
        relationship = "many-to-one"
    ) %>%
    rename(alderman_own = alderman)

final_df <- final_df %>%
    left_join(
        ald_lookup,
        by = c("neighbor_ward" = "ward", "month_join" = "month"),
        relationship = "many-to-one"
    ) %>%
    rename(alderman_neighbor = alderman)

final_output <- final_df %>%
    select(
        pin, year,
        sale_date = sale_date_use,
        boundary_year,
        sale_price,
        sale_price_nominal,
        sale_price_real_2022_raw,
        sale_price_cpi_chi_all_items,
        sale_price_deflator_to_2022,
        class,
        latitude, longitude, ward, neighbor_ward, ward_pair_id,
        dist_m,
        alderman_own, alderman_neighbor
    )

suffix <- if (run_sample) "_sample" else ""
output_path <- sprintf("../output/sales_pre_scores%s.csv", suffix)

write_csv(final_output, output_path)
