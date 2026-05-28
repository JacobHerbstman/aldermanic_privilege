# process_rent_data

Purpose: Processes the Illinois RentHub download into a cleaned Chicago property/floorplan-month panel for the 2014-2022 rental RD.

## Scope

The raw Dewey/RentHub export is Illinois-wide. This task is the first Chicago rental-cleaning stage. It does not assign wards, distances, sides, segments, or alderman scores; those happen downstream in `calculate_rent_distances`, `renthub_proxy_consistency_audit`, `assign_segment_ids_sales_rental`, and `merge_event_study_scores`.

The active window is controlled by Makefile scalar arguments and defaults to `2014-01-01` through `2022-12-31`.

## Main Output Unit

The main rental unit is a cleaned `floorplan_proxy x month` observation, not a raw scrape row and not a claimed physical unit. RentHub `ID` is treated as row-level source metadata, and `PROPERTY_ID`/`UNIT_ID` are mostly unavailable. The task therefore builds transparent proxy keys and neutralizes daily scrape repetition by aggregating before any RD work.

`output/chicago_rent_panel.parquet` is the main monthly panel. Each row is unique by `analysis_key x month_start`, and the saved `rent_panel_id` is `analysis_key__YYYY-MM`.

## City Filter

The raw files are loaded through DuckDB from `input/renthub_raw/*.parquet`. Rows are restricted to `SCRAPED_TIMESTAMP` inside the requested date window and `CITY` equal to `CHICAGO` or the verified alias `CHGO`.

Final Chicago geographic validity is not decided by city string alone. This task applies only a broad Chicago bounding-box screen after geocoding. Downstream ward assignment is the binding spatial filter for RD work.

## Field Cleaning

Address strings are uppercased, trimmed, whitespace-normalized, and treated as missing when they are blank, `0`, `NA`, `N/A`, `NAN`, `NULL`, `NONE`, or `UNKNOWN`. `ADDRESS == 0` is never allowed to survive as a valid address.

Building type is recoded before key construction:
- `TH` and strings containing `TOWN` become `townhouse`.
- `CON`, `CONDO`, `CONDOMINIUM`, and related strings become `condo`.
- `COMM` and commercial strings become `commercial`.
- apartment, multi-family, duplex, triplex, and fourplex strings become `multi_family`.
- single-family, house, detached, and SFR strings become `single_family`.
- missing or unrecognized values become `other`.

The script validates that `CON` maps to `condo` and townhouse/`TH` values map to `townhouse`.

Numeric fields are parsed with explicit casts: `RENT_PRICE`, `BEDS`, `BATHS`, `SQFT`, `YEAR_BUILT`, `LATITUDE`, and `LONGITUDE`. Coordinates must be finite longitude/latitude values and then pass the broad Chicago bounding-box check for the main day sample.

`SCRAPED_TIMESTAMP` is the observation date. `DATE_POSTED` is retained as source metadata, not used for panel timing.

## Key Construction

The task builds three transparent keys:

- `property_key`: valid normalized address plus rounded coordinates. If the address is missing, the key uses `NO_ADDRESS` plus rounded coordinates. Rows without usable coordinates cannot form a property key.
- `floorplan_key`: `property_key + beds + baths + sqft + building_type_clean`.
- `rent_cell_key`: `floorplan_key + rent_price`, used to track same-day rent variation, not as the final analysis unit.

The analysis key is `UNIT_ID` when present. Otherwise it is the floorplan fingerprint. Rows without either are treated as unkeyed and do not enter the floorplan-day panel.

Coordinates in the property key are rounded to four decimal places. That is a transparent grouping device for raw RentHub rows, not the final spatial standard. The downstream proxy-consistency audit reviews address-coordinate stability and can provide corrected geometry coordinates for RD assignment.

## Daily Collapse

Raw rows are first collapsed to `analysis_key x file_date`. This stage removes repeated daily scrapes while preserving within-day rent variation:

- `rent_price` is the median rent within the floorplan-day.
- `rent_price_mean`, `rent_p25`, `rent_p75`, `rent_min`, and `rent_max` summarize within-day rents.
- `raw_rows_day` counts raw rows behind the floorplan-day.
- `rent_values_day` and `rent_cells_day` count same-day rent variation.
- `multi_rent_day` flags days where the same floorplan proxy has more than one rent.
- `same_rent_repeat_day` flags repeated same-rent scrape rows.

Same-day multi-rent groups are not dropped in the main sample. They often reflect large multifamily buildings or multiple active offers that share the same floorplan proxy.

## Outlier And Main-Day Rules

The main day sample requires:

- valid coordinates inside the broad Chicago bounding box,
- positive rent not outside year-by-bedroom-bin 1st and 99th percentile thresholds,
- sqft either missing or inside year-by-bedroom-bin 1st and 99th percentile thresholds.

The trim is applied at the floorplan-day level, after raw rows have already been collapsed. This avoids raw scrape repetition affecting final RD weights while still removing extreme rent and sqft records.

## Monthly Collapse

The main panel is then collapsed to one `analysis_key x month_start` row:

- `rent_price` is the median of daily median rents in the month.
- `rent_price_mean`, `rent_price_p25`, `rent_price_p75`, `rent_price_min`, and `rent_price_max` are saved.
- `first_observed_rent` and `first_observed_date` identify the first retained day in the month.
- `active_days`, `distinct_daily_rents`, `raw_rows_month`, `multi_rent_days`, and `same_rent_repeat_days` summarize the raw observations behind each month.
- beds, baths, sqft, year built, latitude, and longitude use within-month medians.
- binary amenities from RentHub use within-month maxima.

The script hard-fails if duplicate `analysis_key x month_start` rows remain.

## Output

The task writes only `output/chicago_rent_panel.parquet`. Robustness samples and cleaning audits should be handled in dedicated downstream tasks when needed, not emitted as side products of the active producer.

## Built-In Validation

The script fails if:

- no raw RentHub parquet files are available,
- no Chicago rows are found in the requested window,
- duplicate main-panel `analysis_key x month_start` rows remain,
- `ADDRESS=0` survives as a valid address,
- `CON` does not map completely to `condo`,
- townhouse/`TH` values do not map completely to `townhouse`,
- the main monthly panel does not span the requested complete month window.

Approx. runtime: ~1-10 minutes.
