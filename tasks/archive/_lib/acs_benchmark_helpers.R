load_annual_cpi_deflators <- function(cpi_input) {
  raw <- suppressWarnings(read_csv(cpi_input, show_col_types = FALSE, progress = FALSE))
  raw <- raw[, 1:2]
  setDT(raw)
  setnames(raw, names(raw), c("date", "cpi_value"))
  raw[, month_start := as.Date(date)]
  raw[, cpi_value := as.numeric(cpi_value)]
  raw <- raw[!is.na(month_start), .(month_start, cpi_value)]

  month_grid <- data.table(month_start = seq.Date(
    from = min(raw$month_start, na.rm = TRUE),
    to = max(raw$month_start, na.rm = TRUE),
    by = "month"
  ))
  cpi_dt <- merge(month_grid, raw, by = "month_start", all.x = TRUE, sort = TRUE)

  if (cpi_dt[!is.finite(cpi_value), .N] > 0) {
    idx_known <- which(is.finite(cpi_dt$cpi_value))
    if (length(idx_known) < 2) {
      stop("CPI input does not have enough non-missing observations to interpolate.", call. = FALSE)
    }
    interpolated <- approx(
      x = idx_known,
      y = cpi_dt$cpi_value[idx_known],
      xout = seq_len(nrow(cpi_dt)),
      method = "linear",
      rule = 2
    )$y
    cpi_dt[, cpi_value := fifelse(is.finite(cpi_value), cpi_value, interpolated)]
  }

  cpi_dt[, year := as.integer(format(month_start, "%Y"))]
  annual_cpi <- cpi_dt[, .(cpi_annual = mean(cpi_value, na.rm = TRUE)), by = year]

  base_2022 <- annual_cpi[year == 2022, cpi_annual][1]
  base_2024 <- annual_cpi[year == 2024, cpi_annual][1]
  if (!is.finite(base_2022) || !is.finite(base_2024)) {
    stop("Unable to compute annual CPI bases for 2022 and 2024.", call. = FALSE)
  }

  annual_cpi[, `:=`(
    deflator_to_2022 = base_2022 / cpi_annual,
    deflator_to_2024 = base_2024 / cpi_annual
  )]

  annual_cpi[]
}

dedupe_chicago_bg_panel <- function(bg_panel) {
  dt <- as.data.table(copy(bg_panel))
  duplicate_keys <- dt[, .N, by = .(GEOID, year)][N > 1]
  if (nrow(duplicate_keys) == 0) {
    return(dt)
  }

  value_cols <- setdiff(names(dt), c("GEOID", "year", "ward"))
  duplicate_values <- dt[duplicate_keys, on = .(GEOID, year)]
  if (nrow(duplicate_values) == 0) {
    return(dt)
  }

  consistency <- duplicate_values[
    ,
    lapply(.SD, function(x) uniqueN(x[!is.na(x)])),
    by = .(GEOID, year),
    .SDcols = value_cols
  ]
  inconsistent <- consistency[
    ,
    apply(.SD, 1, function(x) any(x > 1)),
    .SDcols = value_cols
  ]
  if (any(inconsistent)) {
    stop("Duplicate GEOID-year rows in the ACS block-group panel are not identical outside ward.", call. = FALSE)
  }

  message(sprintf(
    "Dropping %d duplicated GEOID-year ACS block-group rows after confirming non-ward fields are identical.",
    nrow(duplicate_keys)
  ))

  first_nonmissing <- function(x) {
    idx <- which(!is.na(x))
    if (length(idx) == 0) {
      return(x[1])
    }
    x[idx[1]]
  }

  dt[
    ,
    lapply(.SD, first_nonmissing),
    by = .(GEOID, year),
    .SDcols = value_cols
  ][]
}

build_bg_to_community_area_crosswalk <- function(bg_geometry_input, community_area_input, chicago_geoids) {
  block_groups <- st_read(bg_geometry_input, quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(3435) %>%
    mutate(GEOID = as.character(GEOID)) %>%
    filter(GEOID %in% chicago_geoids) %>%
    select(GEOID)

  community_areas <- st_read(community_area_input, quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(3435) %>%
    transmute(
      community_area = as.integer(area_numbe),
      community_name = as.character(community)
    )

  overlaps <- suppressWarnings(
    st_intersection(block_groups, community_areas)
  )
  overlaps$overlap_area <- as.numeric(st_area(overlaps))
  overlaps <- overlaps %>%
    st_drop_geometry() %>%
    as.data.table()

  crosswalk <- overlaps[order(GEOID, -overlap_area, community_area), .SD[1], by = GEOID][
    ,
    .(GEOID, community_area, community_name)
  ]

  missing_geoids <- setdiff(chicago_geoids, crosswalk$GEOID)
  if (length(missing_geoids) > 0) {
    missing_points <- block_groups %>%
      filter(GEOID %in% missing_geoids) %>%
      st_point_on_surface()

    fallback <- st_join(missing_points, community_areas, join = st_within, left = TRUE)
    fallback <- st_drop_geometry(fallback)
    fallback <- as.data.table(fallback)[, .(GEOID, community_area, community_name)]

    still_missing <- fallback[is.na(community_area), GEOID]
    if (length(still_missing) > 0) {
      nearest_idx <- st_nearest_feature(
        missing_points %>% filter(GEOID %in% still_missing),
        community_areas
      )
      nearest_tbl <- data.table(
        GEOID = still_missing,
        community_area = community_areas$community_area[nearest_idx],
        community_name = community_areas$community_name[nearest_idx]
      )
      fallback <- rbindlist(
        list(
          fallback[!is.na(community_area)],
          nearest_tbl
        ),
        use.names = TRUE,
        fill = TRUE
      )
    }

    crosswalk <- rbindlist(list(crosswalk, fallback), use.names = TRUE, fill = TRUE)
  }

  if (crosswalk[, uniqueN(GEOID)] != length(unique(chicago_geoids))) {
    stop("Community-area crosswalk does not cover all Chicago block groups.", call. = FALSE)
  }

  crosswalk[order(GEOID)][]
}

build_acs_benchmark_panel <- function(ward_controls_input,
                                      bg_controls_input,
                                      bg_geometry_input,
                                      community_area_input,
                                      cpi_input,
                                      start_year,
                                      end_year) {
  annual_cpi <- load_annual_cpi_deflators(cpi_input)

  ward_controls <- fread(ward_controls_input)[
    year >= start_year & year <= end_year,
    .(
      geography_level = "ward",
      geography_id = as.integer(ward),
      geography_name = paste("Ward", as.integer(ward)),
      year = as.integer(year),
      acs_rent_nominal = as.numeric(avg_rent),
      acs_home_value_nominal = as.numeric(avg_home_value),
      owner_occ = as.numeric(NA),
      renter_occ = as.numeric(NA)
    )
  ]

  bg_panel <- fread(
    bg_controls_input,
    select = c("GEOID", "year", "ward", "agg_value", "agg_rent", "owner_occ", "renter_occ")
  )[year >= start_year & year <= end_year]
  bg_panel[, GEOID := as.character(GEOID)]
  bg_panel <- dedupe_chicago_bg_panel(bg_panel)

  chicago_geoids <- sort(unique(bg_panel$GEOID))
  crosswalk <- build_bg_to_community_area_crosswalk(
    bg_geometry_input = bg_geometry_input,
    community_area_input = community_area_input,
    chicago_geoids = chicago_geoids
  )

  bg_panel <- merge(bg_panel, crosswalk, by = "GEOID", all.x = TRUE)
  if (bg_panel[is.na(community_area), .N] > 0) {
    stop("One or more ACS Chicago block groups are missing a community-area assignment.", call. = FALSE)
  }

  citywide_panel <- bg_panel[
    ,
    .(
      acs_rent_nominal = sum(agg_rent, na.rm = TRUE) / sum(renter_occ, na.rm = TRUE),
      acs_home_value_nominal = sum(agg_value, na.rm = TRUE) / sum(owner_occ, na.rm = TRUE),
      owner_occ = sum(owner_occ, na.rm = TRUE),
      renter_occ = sum(renter_occ, na.rm = TRUE)
    ),
    by = year
  ][
    ,
    `:=`(
      geography_level = "citywide",
      geography_id = NA_integer_,
      geography_name = "Chicago"
    )
  ][
    ,
    .(
      geography_level,
      geography_id,
      geography_name,
      year,
      acs_rent_nominal,
      acs_home_value_nominal,
      owner_occ,
      renter_occ
    )
  ]

  community_area_panel <- bg_panel[
    ,
    .(
      acs_rent_nominal = sum(agg_rent, na.rm = TRUE) / sum(renter_occ, na.rm = TRUE),
      acs_home_value_nominal = sum(agg_value, na.rm = TRUE) / sum(owner_occ, na.rm = TRUE),
      owner_occ = sum(owner_occ, na.rm = TRUE),
      renter_occ = sum(renter_occ, na.rm = TRUE)
    ),
    by = .(community_area, community_name, year)
  ][
    ,
    .(
      geography_level = "community_area",
      geography_id = as.integer(community_area),
      geography_name = community_name,
      year,
      acs_rent_nominal,
      acs_home_value_nominal,
      owner_occ,
      renter_occ
    )
  ]

  acs_panel <- rbindlist(
    list(citywide_panel, ward_controls, community_area_panel),
    use.names = TRUE,
    fill = TRUE
  )
  acs_panel <- merge(acs_panel, annual_cpi[, .(year, deflator_to_2022, deflator_to_2024)], by = "year", all.x = TRUE)

  if (acs_panel[!is.finite(deflator_to_2022) | !is.finite(deflator_to_2024), .N] > 0) {
    stop("ACS benchmark panel is missing annual CPI deflators.", call. = FALSE)
  }

  acs_panel[!is.finite(acs_rent_nominal) | acs_rent_nominal <= 0, acs_rent_nominal := NA_real_]
  acs_panel[!is.finite(acs_home_value_nominal) | acs_home_value_nominal <= 0, acs_home_value_nominal := NA_real_]

  acs_panel[, `:=`(
    acs_rent_real_2024 = acs_rent_nominal * deflator_to_2024,
    acs_home_value_real_2022 = acs_home_value_nominal * deflator_to_2022
  )]

  coverage_summary <- acs_panel[
    ,
    .(n_geographies = .N),
    by = .(geography_level, year)
  ]
  coverage_summary[, dataset := "acs"]
  coverage_summary[, expected_geographies := fifelse(
    geography_level == "citywide",
    1L,
    fifelse(geography_level == "ward", 50L, 77L)
  )]
  coverage_summary[, share_covered := n_geographies / expected_geographies]

  list(
    panel = acs_panel[order(geography_level, geography_id, year)],
    coverage_summary = coverage_summary[order(geography_level, year)]
  )
}
