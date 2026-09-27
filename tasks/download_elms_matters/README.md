# Chicago City Clerk legislative records (eLMS)

Snapshot of the City Clerk's electronic Legislative Management System, retrieved September 27, 2026 from its public
API (City of Chicago, Office of the City Clerk, https://api.chicityclerkelms.chicago.gov; no key; documentation at
`/swagger.json`). Run `make` in `code/`. The source is live, so a rerun is a deliberate refresh.

- `download_elms_matter_index.R` requests `/matter` month by month of introduction date, 500 records per page
  (the maximum), sorted by record number so paging is stable. `elms_matter_index.jsonl` holds each response body
  exactly as received, one per line; `elms_matter_index_requests.csv` records each request, its page and month
  counts and the SHA-256 of its body. The script requires every month's pages to add up to the API's count and every
  matter to appear once. Records begin in February 2010: 180,928 matters through September 2026 (none in August
  2026, a council recess). Matters with no introduction date cannot be selected by month and are not included.
- `download_elms_zoning_details.R` requests `/matter/{matterId}` for every zoning map amendment in the index (title
  containing "Zoning Reclassification" or category ZONING RECLASSIFICATIONS): 6,750 matters, with their actions,
  roll calls, sponsors and attachment list; the list endpoint returns metadata only. Bodies are kept as received, in
  index order, with a request record in `elms_zoning_matter_requests.csv`. Records received so far are kept in
  `../temp`, so an interrupted download resumes.

The API limits requests per window and returns 429 when the limit is used up; both scripts wait a minute and retry.
Zoning map amendments are ordinances; most are applications transmitted through the Zoning Administrator (filing
office "Misc."), and 1,347 were filed by an alderman (filing office a ward number). The districts before and after a
change appear only in the attached ordinance PDFs.
