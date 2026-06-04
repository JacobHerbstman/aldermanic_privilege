# renthub_quality_flags

Purpose: Builds RentHub geometry and source-quality flags for downstream rental distance and RD tasks.

This task does not drop rental observations. It standardizes stable address-location groups to their primary coordinates and flags unstable addresses, implausible rents, stale listings, repeated one-day scrape artifacts, and proxy conflicts for downstream analysis.

Address-location rule: address stems are canonicalized before grouping. Rounded coordinate clusters within the same address stem are grouped with complete-link clustering at 200ft. Downstream geometry uses the primary location-group coordinate only when the primary group has at least 85% of raw rows and no secondary group has at least 10% of raw rows more than 500ft away. Addresses that fail that rule remain flagged.

Manual coordinates: `input/manual_verified_address_locations.csv` records externally verified coordinates for unstable high-volume addresses. A manual coordinate is only applied when the row is marked `verified` with a source URL.

Produces: `output/chicago_rent_panel_quality_flags.parquet`.

Approx. runtime: ~2-8 minutes.
