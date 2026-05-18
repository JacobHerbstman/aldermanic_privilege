# renthub_proxy_consistency_audit

Purpose: Audits RentHub address/property/floorplan proxies before rental geometry and RD.

This task does not drop rental observations. It writes diagnostic flags for bad geocodes, unstable property proxies, implausible rents, stale listings, and one-day scrape artifacts. Downstream tasks can carry these flags into geometry and RD balance checks.

Address-location rule: address stems are canonicalized before auditing by removing punctuation, standardizing full cardinal directions, and stripping common street suffixes such as `ST`, `AVE`, `DR`, `PL`, `PLZ`, and `PLAZA`. Rounded coordinate clusters within the same address stem are grouped with complete-link clustering at 200ft. Downstream geometry uses the primary location-group coordinate only when the primary group has at least 85% of raw rows and no secondary group has at least 10% of raw rows more than 500ft away. Addresses that fail that rule are flagged as unstable rather than silently corrected.

Manual rescue workflow: `code/manual_verified_address_locations.csv` records externally verified coordinates for unstable high-volume addresses. The script writes `output/unstable_address_external_review_queue.csv` and `output/unstable_address_external_review_workbook.xlsx`, sorted by raw-row volume, with Google Search/Maps URLs and candidate coordinate groups. A manual coordinate is only applied when the row is marked `verified` with a source URL; otherwise unstable addresses remain flagged.

Produces: `output/chicago_rent_panel_quality_flags.parquet` and CSV audits for address-coordinate stability, address-location groups, external review queue, property rent stability, same-day rent spreads, coordinate-only piles, one-day bulk records, building-type conflicts, stale posted dates, and flag summaries.

Approx. runtime: ~2-8 minutes.
