# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

missing <- readr::read_csv(
  "../output/historical_project_parcel_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(coverage_status == "missing") %>%
  mutate(request_id = row_number(), .before = 1)

if (anyDuplicated(missing$request_id) > 0) {
  stop("Missing project parcel request IDs are not unique.", call. = FALSE)
}

target_pins <- sort(unique(missing$component_pin))
pin_chunks <- split(target_pins, ceiling(seq_along(target_pins) / 50))
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"
history_records <- list()

for (i in seq_along(pin_chunks)) {
  parameters <- c(
    "$select" = "pin,pin10,year,lon,lat,x_3435,y_3435,row_id",
    "$where" = sprintf(
      "year between 1999 and 2025 and pin in(%s)",
      paste(sprintf("'%s'", pin_chunks[[i]]), collapse = ",")
    ),
    "$order" = "pin,year",
    "$limit" = "50000"
  )
  query <- paste0(
    base_url,
    "?",
    paste(
      paste0(
        URLencode(names(parameters), reserved = TRUE),
        "=",
        URLencode(unname(parameters), reserved = TRUE)
      ),
      collapse = "&"
    )
  )

  response <- NULL
  for (attempt in 1:5) {
    response <- tryCatch(curl::curl_fetch_memory(query), error = function(e) NULL)
    if (!is.null(response) && response$status_code == 200L) {
      break
    }
    Sys.sleep(attempt)
  }
  if (is.null(response) || response$status_code != 200L) {
    stop("Historical Parcel Universe request failed for chunk ", i, call. = FALSE)
  }

  payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
  if (is.data.frame(payload) && nrow(payload) > 0) {
    history_records[[length(history_records) + 1]] <- as_tibble(payload)
  }
}

pin_history <- bind_rows(history_records) %>%
  transmute(
    pin = as.character(pin),
    pin10 = as.character(pin10),
    year = suppressWarnings(as.integer(year)),
    longitude = suppressWarnings(as.numeric(lon)),
    latitude = suppressWarnings(as.numeric(lat)),
    x_3435 = suppressWarnings(as.numeric(x_3435)),
    y_3435 = suppressWarnings(as.numeric(y_3435)),
    row_id = as.character(row_id)
  ) %>%
  filter(is.finite(x_3435), is.finite(y_3435)) %>%
  distinct(pin, year, .keep_all = TRUE) %>%
  arrange(pin, year)

if (nrow(anti_join(pin_history, tibble::tibble(pin = target_pins), by = "pin")) > 0) {
  stop("Historical Parcel Universe returned an unrequested PIN.", call. = FALSE)
}
if (anyDuplicated(pin_history[c("pin", "year")]) > 0) {
  stop("Historical Parcel Universe is not unique by PIN-year.", call. = FALSE)
}

history_by_pin <- split(pin_history, pin_history$pin)
nearest_history_points <- purrr::map_dfr(
  seq_len(nrow(missing)),
  function(i) {
    request <- missing[i, ]
    candidates <- history_by_pin[[request$component_pin]]
    if (is.null(candidates) || nrow(candidates) == 0) {
      return(tibble::tibble(request_id = request$request_id))
    }
    candidates <- candidates %>%
      mutate(
        absolute_year_gap = abs(year - request$target_year),
        prior_or_same_year = year <= request$target_year
      ) %>%
      arrange(absolute_year_gap, desc(prior_or_same_year), desc(year), row_id)
    selected <- candidates[1, ]
    tibble::tibble(
      request_id = request$request_id,
      history_reference_year = selected$year,
      history_reference_year_gap = selected$year - request$target_year,
      history_x_3435 = selected$x_3435,
      history_y_3435 = selected$y_3435,
      history_row_id = selected$row_id
    )
  }
)

current_parcels <- data.table::fread(
  "../input/parcel_universe_2025_city.csv",
  select = c("pin", "centroid_x_crs_3435", "centroid_y_crs_3435")
) %>%
  as_tibble() %>%
  transmute(
    pin = as.character(pin),
    current_x_3435 = as.numeric(centroid_x_crs_3435),
    current_y_3435 = as.numeric(centroid_y_crs_3435)
  )

if (anyDuplicated(current_parcels$pin) > 0) {
  stop("Current Parcel Universe is not unique by PIN.", call. = FALSE)
}

permit_points <- readr::read_csv(
  "../output/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_family = readr::col_character(),
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    permit_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(plausible_application_window, plausible_issue_window) %>%
  distinct(source_family, project_id, permit_id, permit_x_3435, permit_y_3435) %>%
  filter(is.finite(permit_x_3435), is.finite(permit_y_3435)) %>%
  group_by(source_family, project_id) %>%
  mutate(
    permit_center_x = mean(permit_x_3435),
    permit_center_y = mean(permit_y_3435),
    permit_distance_from_center = sqrt(
      (permit_x_3435 - permit_center_x)^2 +
        (permit_y_3435 - permit_center_y)^2
    )
  ) %>%
  summarise(
    permit_ids = paste(sort(unique(permit_id)), collapse = "/"),
    permit_count = n_distinct(permit_id),
    permit_x_3435 = first(permit_center_x),
    permit_y_3435 = first(permit_center_y),
    permit_coordinate_spread_ft = max(permit_distance_from_center),
    .groups = "drop"
  ) %>%
  mutate(permit_location_unambiguous = permit_coordinate_spread_ft <= 100)

reference_points <- missing %>%
  left_join(nearest_history_points, by = "request_id", relationship = "one-to-one") %>%
  left_join(
    current_parcels,
    by = c("component_pin" = "pin"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    permit_points,
    by = c("source_family", "project_id"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    reference_source = case_when(
      is.finite(history_x_3435) & is.finite(history_y_3435) ~ "nearest_exact_pin_history",
      is.finite(current_x_3435) & is.finite(current_y_3435) ~ "current_exact_pin",
      permit_location_unambiguous &
        is.finite(permit_x_3435) & is.finite(permit_y_3435) ~ "exact_pin_permit_cluster",
      TRUE ~ "unresolved"
    ),
    reference_x_3435 = case_when(
      reference_source == "nearest_exact_pin_history" ~ history_x_3435,
      reference_source == "current_exact_pin" ~ current_x_3435,
      reference_source == "exact_pin_permit_cluster" ~ permit_x_3435,
      TRUE ~ NA_real_
    ),
    reference_y_3435 = case_when(
      reference_source == "nearest_exact_pin_history" ~ history_y_3435,
      reference_source == "current_exact_pin" ~ current_y_3435,
      reference_source == "exact_pin_permit_cluster" ~ permit_y_3435,
      TRUE ~ NA_real_
    )
  ) %>%
  arrange(target_year, source_family, project_id, component_pin)

summary <- bind_rows(
  reference_points %>%
    count(reference_source, name = "value") %>%
    transmute(metric = paste0("missing_component_year_reference_", reference_source), value),
  tibble::tibble(metric = "missing_component_year_requests", value = nrow(reference_points)),
  tibble::tibble(metric = "unique_missing_component_pins", value = length(target_pins)),
  tibble::tibble(metric = "historical_pin_year_records", value = nrow(pin_history))
)

readr::write_csv(summary, "../output/missing_project_parcel_history_summary.csv")
readr::write_csv(pin_history, "../output/missing_project_pin_history.csv")
readr::write_csv(reference_points, "../output/missing_project_reference_points.csv")
