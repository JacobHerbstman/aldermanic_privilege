# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_sale_distances/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")

crs_projected <- 3435
analysis_eras <- c("2003_2014", "2015_2023")

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
    st_transform(crs_projected)
canonical_ward_maps <- load_canonical_ward_maps(ward_panel, analysis_eras)
canonical_boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", analysis_eras)

alderman_terms <- read_csv("../input/chicago_alderman_terms.csv", show_col_types = FALSE) %>%
    mutate(
        start_date = as.Date(start_date),
        end_date = as.Date(end_date)
    ) %>%
    arrange(ward, start_date)
term_overlaps <- alderman_terms %>%
    group_by(ward) %>%
    mutate(next_start_date = lead(start_date)) %>%
    ungroup() %>%
    filter(!is.na(next_start_date), next_start_date <= end_date)
if (nrow(term_overlaps) > 0) {
    stop("Alderman terms overlap within a ward.", call. = FALSE)
}

sales <- read_parquet("../input/residential_sales_clean.parquet") %>%
    mutate(
        year = as.numeric(year),
        pin = as.character(pin),
        sale_date = as.Date(sale_date)
    )

if (any(nchar(sales$pin) != 14L)) {
    stop("Residential sales contain an invalid full PIN.", call. = FALSE)
}

sales <- sales %>%
    mutate(
        sale_date_for_price = sale_date,
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

sales <- sales %>%
    mutate(sale_price = sale_price_real_2022_raw)

parcels <- fread(
    "../input/parcel_universe_2025_city.csv",
    select = c("pin", "latitude", "longitude"),
    colClasses = list(character = "pin")
)
parcels[, pin := gsub("[^0-9]", "", trimws(pin))]
parcels[nchar(pin) == 13L, pin := paste0("0", pin)]
parcels[, `:=`(
    current_latitude = suppressWarnings(as.numeric(latitude)),
    current_longitude = suppressWarnings(as.numeric(longitude))
)]
parcels[, c("latitude", "longitude") := NULL]
if (any(nchar(parcels$pin) != 14L)) {
    stop("Current parcel universe contains an invalid full PIN.", call. = FALSE)
}
if (anyDuplicated(parcels$pin) > 0) {
    stop("Parcel universe must be unique by pin.", call. = FALSE)
}

historical_parcels <- fread(
    "../input/historical_sale_parcel_coordinates_2006_2022.csv",
    colClasses = list(character = "pin")
)
historical_parcels[, `:=`(
    historical_latitude = suppressWarnings(as.numeric(latitude)),
    historical_longitude = suppressWarnings(as.numeric(longitude))
)]
historical_parcels[, c("latitude", "longitude") := NULL]
if (anyDuplicated(historical_parcels[, .(pin, year)]) > 0) {
    stop("Historical parcel coordinates must be unique by pin-year.", call. = FALSE)
}

sales_geo <- sales %>%
    left_join(
        historical_parcels %>%
            select(pin, year, historical_latitude, historical_longitude),
        by = c("pin", "year"),
        relationship = "many-to-one"
    ) %>%
    left_join(parcels, by = "pin", relationship = "many-to-one") %>%
    mutate(
        has_historical_coordinates = is.finite(historical_latitude) &
            is.finite(historical_longitude),
        has_current_coordinates = is.finite(current_latitude) &
            is.finite(current_longitude),
        latitude = if_else(
            has_historical_coordinates,
            historical_latitude,
            current_latitude
        ),
        longitude = if_else(
            has_historical_coordinates,
            historical_longitude,
            current_longitude
        ),
        coordinate_source = case_when(
            has_historical_coordinates ~ "historical_exact_pin_year",
            has_current_coordinates ~ "current_2025_fallback",
            TRUE ~ NA_character_
        ),
        coordinate_year = if_else(has_historical_coordinates, year, 2025)
    )

unresolved_coordinates <- !is.finite(sales_geo$latitude) | !is.finite(sales_geo$longitude)
if (any(unresolved_coordinates & sales_geo$year >= 2006 & sales_geo$year <= 2022)) {
    stop("A 2006-2022 sale has unresolved exact-PIN coordinates.", call. = FALSE)
}

sales_sf <- sales_geo %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs_projected)

rm(sales, sales_geo, parcels)
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

# Unrefined first-of-month dates do not identify the serving alderman when
# either side of the boundary changes office during that month. Drop those
# sales instead of assigning them mechanically to the first day.
transition_months <- bind_rows(
    alderman_terms %>% transmute(ward, transition_date = start_date),
    alderman_terms %>% transmute(ward, transition_date = end_date + days(1))
) %>%
    filter(day(transition_date) > 1L) %>%
    mutate(year_month = format(transition_date, "%Y-%m")) %>%
    distinct(ward, year_month)

final_df <- final_df %>%
    mutate(
        year_month = format(sale_date_use, "%Y-%m"),
        unrefined_first_of_month = !is_mydec_date & day(sale_date_use) == 1L,
        map_can_change_within_month = canonical_era_from_date(floor_date(sale_date_use, "month")) !=
            canonical_era_from_date(ceiling_date(sale_date_use, "month") - days(1))
    ) %>%
    left_join(
        transition_months %>% mutate(own_transition = TRUE),
        by = c("ward", "year_month"),
        relationship = "many-to-one"
    ) %>%
    left_join(
        transition_months %>%
            rename(neighbor_ward = ward) %>%
            mutate(neighbor_transition = TRUE),
        by = c("neighbor_ward", "year_month"),
        relationship = "many-to-one"
    ) %>%
    filter(!(
        unrefined_first_of_month &
            (coalesce(own_transition, FALSE) | coalesce(neighbor_transition, FALSE) |
                map_can_change_within_month)
    )) %>%
    select(-year_month, -unrefined_first_of_month, -map_can_change_within_month,
           -own_transition, -neighbor_transition)

final_df <- final_df %>%
    select(-any_of(c(
        "historical_latitude", "historical_longitude",
        "current_latitude", "current_longitude",
        "has_historical_coordinates", "has_current_coordinates"
    )))

alderman_daily <- alderman_terms %>%
    mutate(
        start_date = pmax(start_date, min(final_df$sale_date_use, na.rm = TRUE)),
        end_date = pmin(end_date, max(final_df$sale_date_use, na.rm = TRUE))
    ) %>%
    filter(start_date <= end_date) %>%
    rowwise() %>%
    mutate(sale_date_use = list(seq(start_date, end_date, by = "day"))) %>%
    ungroup() %>%
    select(ward, sale_date_use, alderman) %>%
    tidyr::unnest(sale_date_use)
if (anyDuplicated(alderman_daily[, c("ward", "sale_date_use")]) > 0) {
    stop("Exact-date alderman lookup must be unique by ward-date.", call. = FALSE)
}

final_df <- final_df %>%
    left_join(
        alderman_daily,
        by = c("ward", "sale_date_use"),
        relationship = "many-to-one"
    ) %>%
    rename(alderman_own = alderman)

final_df <- final_df %>%
    left_join(
        alderman_daily,
        by = c("neighbor_ward" = "ward", "sale_date_use"),
        relationship = "many-to-one"
    ) %>%
    rename(alderman_neighbor = alderman)

final_output <- final_df %>%
    select(
        row_id, sale_document_num, pin, year,
        sale_date = sale_date_use,
        boundary_year,
        sale_price,
        sale_price_nominal,
        sale_price_real_2022_raw,
        sale_price_cpi_chi_all_items,
        sale_price_deflator_to_2022,
        class, coordinate_source, coordinate_year,
        latitude, longitude, ward, neighbor_ward, ward_pair_id,
        dist_m,
        alderman_own, alderman_neighbor
    )

write_csv(final_output, "../output/sales_pre_scores.csv")
