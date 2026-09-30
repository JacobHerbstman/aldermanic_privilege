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
- `download_elms_zoning_legislation_files.R` downloads the files attached to those matters as "Legislation" (the
  introduced ordinance and, where one exists, the substitute that passed), from the public cloud storage paths in the
  attachment lists: 9,344 files, 9,339 of them PDFs, plus two RTF files, two Word documents and one image (13 GB, in
  `output/zoning_legislation_files`). Each is saved under its storage object's name, prefixed with the container name
  for the 25 objects listed in both the legacy and the current container (matters re-filed in 2023). Files are
  checked against their format's signature and moved into the output folder only when complete, so an interrupted
  download resumes. `elms_zoning_legislation_files.csv` records every file with its SHA-256; three are recorded
  rather than kept: one object that is empty at the source (the introduced version of SO2011-2264) and two that return
  404 (the RTF files of SO2015-6397 and SO2019-328).

- `download_elms_zoning_application_files.R` downloads the other application files of those matters: from 2023
  eLMS attaches each application's form separately (attachment type "Miscellaneous", "Application.pdf") and its
  narrative and plans as another file ("Exhibits", "Narrative and Plans.pdf"); before 2023 both were part of the
  legislation file. 2,291 PDFs of 1,034 matters (964 forms, 1,327 narratives; 2.8 GB, in
  `output/zoning_application_files`), retrieved September 29, 2026 with the same checks, recorded with their SHA-256
  in `elms_zoning_application_files.csv`; none was empty or absent at the source.

The API limits requests per window and returns 429 when the limit is used up; the scripts wait and retry.
Zoning map amendments are ordinances; most are applications transmitted through the Zoning Administrator (filing
office "Misc."), and 1,347 were filed by an alderman (filing office a ward number). The districts before and after a
change appear only in the attached ordinance PDFs.
