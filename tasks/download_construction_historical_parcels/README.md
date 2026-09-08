# Historical parcel polygons for initial construction candidates

The ordinary Make entry point combines two pinned Cook County historical parcel
extracts. The July 27, 2026 source contains 8,444 polygons from 11,017 requested
year/PIN10 pairs. Updated construction candidates required 29 additional queries;
the September 7 supplement contains their 28 returned polygons. The combined
source has 8,472 polygons and 11,046 queried pairs. Requests with no returned
polygon remain in the query table. Construction cleaning recomputes project
coverage from these inputs; it does not read archived match decisions.

The source service is Cook County's `parcelHistorical/MapServer`. The preserved
layer manifest records the year-to-layer mapping and July 27 retrieval times.
Source geometry is already in EPSG:3435. These are the administrative features
saved by the original downloader, with normalized identifiers, rather than
verbatim HTTP responses. The source file is copied without changing its bytes.
The query table was recovered from the original coverage export and checked
against the original requested-PIN10 counts in every annual layer manifest.

The five pinned files live in `data_raw/construction_review/`:
`historical_project_parcel_source_2026-07-27.gpkg`,
`historical_project_parcel_queries_2026-07-27.csv`,
`historical_project_parcel_layers_2026-07-27.csv`,
`historical_project_parcel_source_2026-09-07.gpkg`, and
`historical_project_parcel_queries_2026-09-07.csv`.
`code/source_snapshot.sha256` identifies their exact bytes. The July files were
recovered from the original construction audit, whose acquisition code is
preserved at commit `010a1f8497c1f32e2c79b5933d1c5baf9af44be3`. The supplement was
retrieved through this task's public API recipes. The two query sets are disjoint;
combining them preserves every original feature's attributes and geometry.

Run `make` in `code/` to reproduce the pinned inputs and their standard reports.
An ordinary build never refreshes a historical source silently. A current public
query cannot guarantee the July bytes. For an intentional expansion, run
`make -f download_recipes.make` in `code/`. Make obtains the current construction
request table through its upstream producer and queries only year/PIN10 pairs
absent from the July scope. It saves annual extracts and their combined supplement.
Download responses must contain every requested object ID and valid geometry;
errors or partial responses fail before publication of the annual output.

An ordinary replication uses the five committed files and needs no network or
manual run order. Updating a pinned source is a separate, reviewed data change:
compare the generated query table and supplement, preserve them under dated raw
filenames, and update the declared inputs and hashes together. Keep the July
baseline unchanged. Coverage stops explicitly if a new candidate requires a query
outside the pinned combined scope, instead of classifying an unqueried parcel as
missing. The single September query with no result is retained as missing; it
does not by itself exclude a construction observation.

Predecessor recovery also needs parcel location history for candidates absent
from the annual polygon lookup. The intentional acquisition entry point derives
the requested PINs from current coverage and retrieves 1999–2025 records from
Cook County Parcel Universe (`nj4t-kc8j`). It saves every returned record, including
missing coordinates and duplicate parcel-years, before construction cleaning
checks keys and selects the nearest assessment year. This replaces selection
inside the old downloader. Both the response table and requested PINs must be
pinned before ordinary production uses them. The September 7 snapshot contains
31,571 rows for 2,576 PINs, without duplicate PIN-years. It preserves all 31,552
records used by the earlier location calculation with unchanged values, adds 18
years for the new PIN, and retains one row with missing coordinates that the old
downloader removed. `history_snapshot.sha256` records the source and query files.

The predecessor polygon inputs are the unchanged July 27 extract (1,050 features)
and its 2,576 distinct year-and-coordinate queries, plus one September query and
returned polygon. The July query scope was recovered from the original reference
point export; matching decisions are recalculated locally. The July GPKG records
its write time as `2026-07-27T05:58:36.224Z`. The four dated inputs are named
`historical_predecessor_parcels_2026-07-27.gpkg`,
`historical_predecessor_queries_2026-07-27.csv`,
`historical_predecessor_parcels_2026-09-07.gpkg`, and
`historical_predecessor_queries_2026-09-07.csv`, under
`data_raw/construction_review/`; `predecessor_snapshot.sha256` fixes their bytes.

Ordinary Make combines those polygon and query inputs and reproduces their reports.
The intentional download recipes derive additional spatial queries from the
current reference points and request intersecting polygons in EPSG:3435. They
preserve the July responses. Source points outside the recorded query scope fail
in construction cleaning rather than being interpreted as unmatched.

The later preferred-project parcel lookup now also has a reproducible source
union. It retains the original `preferred_historical_parcel_source.gpkg` and
query table unchanged, then uses the initial parcel source for queries outside
that original scope. Current preferred candidates required 17 additional
year/PIN10 pairs: six were already queried in the initial source and 11 required
new queries. The latter are pinned as
`preferred_parcel_supplement_2026-09-07.gpkg` and
`preferred_parcel_supplement_queries_2026-09-07.csv`. All four preferred inputs
are fixed by `preferred_snapshot.sha256`. The original query scope takes
priority, including its empty results; combining sources never silently refreshes
an already queried parcel. `download_recipes.make` prepares and downloads only
the additional preferred queries, using the same annual API rules as the initial
lookup.

The preferred predecessor lookup preserves its original 1,693 polygons and spatial
queries, then adds one September 7 query and one returned 2007 polygon (object ID
641141, PIN 17073250410000). Its four pinned files are
`preferred_predecessor_parcel_source.gpkg`,
`preferred_predecessor_source_queries.csv`,
`preferred_predecessor_parcels_2026-09-07.gpkg`, and
`preferred_predecessor_queries_2026-09-07.csv`, under
`data_raw/construction_review/`. `preferred_predecessor_snapshot.sha256` fixes
their bytes. Ordinary Make combines the two sources and query scopes. Intentional
acquisition derives only unqueried points from current preferred reference points.
The polygon downloader uses an explicit `initial` or `preferred` scope; its spatial
query and source-validation rules are the same for both.

A separate September 7 extract preserves all 447 Parcel Universe records in
1999–2025 for the 216 exact PINs requested by address geocoding. The pinned files
are `geocoding_parcel_history_2026-09-07.csv` and
`geocoding_history_queries_2026-09-07.csv`; `geocoding_history_snapshot.sha256`
fixes their bytes. The history downloader's `geocoding` scope retrieves the same
fields as its `initial` scope and does not select coordinates during acquisition.
The release audit evaluates nearest-year coordinate availability. Adopting these
locations ahead of address geocoding remains a proposed method change, not an
implemented production priority.

The approved South Troy address correction requires another spatial query: its
Chicago address point in the 2008 layer. The existing Make downloader returned
one polygon, PIN 16241000360000 (object ID 169033). The full polygon extract and
query are pinned as `preferred_predecessor_troy_parcels_2026-09-07.gpkg` and
`preferred_predecessor_troy_queries_2026-09-07.csv`. Both are included in
`preferred_predecessor_snapshot.sha256`. The ordinary source union retains the
earlier 1,694 polygons and adds this one. The query was submitted in EPSG:3435 at
x=1155481.6903695965, y=1894116.3050945636; construction cleaning must independently
produce that point from the corrected address response before using this lookup.

## September 8 historical-coordinate parcel queries

The consolidated review queries every finite exact-PIN location in the pinned
September 7 geocoding coordinate history against the requested construction-year
maps, plus next-year coordinates from the earlier history extract for failed
initial locations.
`prepare_history_reference_queries.R` produces 238 distinct coordinate/year
queries; the existing parcel downloader's `history_reference` scope returns 87
polygons. The 62 objects also present in the earlier predecessor source have
spatially identical shapes. No project decision is encoded in these source files.

The received polygons and query scope are preserved as
`data_raw/construction_review/history_reference_parcels_complete_2026-09-08.gpkg` and
`history_reference_queries_complete_2026-09-08.csv`. `history_reference_snapshot.sha256`
checks both. Ordinary Make restores `history_reference_parcels.gpkg` and
`history_reference_queries.csv` and their reports without contacting the service.
The deliberate acquisition command is `make -f Makefile -f download_recipes.make
../report/history_reference_parcels_download.gpkg.log
../report/history_reference_spatial_queries_download.csv.log` from `code/`.
It uses Cook County's `parcelHistorical/MapServer` service and the recorded
year-to-layer table, and refuses incomplete API responses. A refresh is a new
source vintage, not part of an unchanged build.

Jacob approved using the exact PIN's coordinate from one year after construction
when the original predecessor lookup has no reference point or finds no polygon.
Construction cleaning applies this general fallback to the construction-year map;
multiple unequal polygons remain unresolved. The broader nearest-year priority
proposal remains unadopted.
